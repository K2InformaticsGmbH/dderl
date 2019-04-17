-module(dderl_dal).
-behavior(gen_server).

-include("dderl.hrl").

-export([init/1
        ,start_link/0
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,format_status/2
        ]).

-export([get_adapters/1
        ,add_adapter/2
        ,add_command/7
        ,update_command/6
        ,update_command/7
        ,add_view/5
        ,update_view/4
        ,rename_view/3
        ,delete_view/2
        ,add_connect/2
        ,get_connects/2
        ,del_conn/3
        ,get_command/2
        ,get_view/4
        ,get_view/2
        ,is_local_query/1
        ,can_connect_locally/1
        ,save_dashboard/5
        ,rename_dashboard/3
        ,delete_dashboard/2
        ,get_dashboards/2
        ,add_adapter_to_cmd/3
        ,user_name/1
        ,get_restartable_apps/0
        ,process_login/3
        ,rows_from/3
        ,expand_rows/4
        ,add_d3_templates_path/2
        ,get_d3_templates/0
        ,get_d3_templates_path/1
        ,get_host_app/0
        ,is_proxy/2
        ]).

-record(state, { schema :: term()
               , sess :: {atom(), pid()}
               , d3_templates :: list()
               , host_app :: binary()
    }).

%% Privileges
-define(MANAGE_CONNS, {dderl, conn, manage}).
-define(CREATE_CONNS, {dderl, conn, create}).
-define(USE_SYS_CONNS, {dderl, conn, {owner, system}, use}).
-define(USE_CONN(__ConnId), {dderl, conn, {conn, __ConnId}, use}).
-define(USE_LOCAL_CONN, {dderl, conn, local, use}).

%% Validate this permission.
-define(USE_ADAPTER, {dderl, adapter, {id, __AdaptId}, use}).

-spec add_adapter(atom(), binary()) -> ok.
add_adapter(Id, FullName) -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).

-spec add_connect({atom(), pid()} | undefined, #ddConn{}) -> integer() | {error, binary()}.
add_connect(undefined, #ddConn{} = Conn) -> gen_server:call(?MODULE, {add_connect, Conn});
add_connect(Sess, #ddConn{} = Conn) -> gen_server:call(?MODULE, {add_connect, Sess, Conn}).

-spec del_conn({atom(), pid()}, ddEntityId(), integer()) -> ok | no_permission.
del_conn(Sess, UserId, ConId) ->
    HasAll = (Sess:run_cmd(have_permission, [[?MANAGE_CONNS]]) == true),
    if HasAll ->
        ok = Sess:run_cmd(delete, [ddConn, ConId]),
        ?Info("user ~p deleted connection ~p", [UserId, ConId]),
        ok;
    true ->
        case Sess:run_cmd(select, [ddConn, [{#ddConn{id=ConId,owner=UserId,_='_'},[],['$_']}]]) of
            {[_|_], true} ->
                ok = Sess:run_cmd(delete, [ddConn, ConId]),
                ?Info("user ~p deleted connection ~p", [UserId, ConId]),
                ok;
            _ ->
                ?Error("user ~p doesn't have permission to delete connection ~p", [UserId, ConId]),
                no_permission
        end
    end.

-spec get_connects({atom(), pid()}, ddEntityId()) -> [#ddConn{}] | {error, binary()}.
get_connects(Sess, UserId) -> gen_server:call(?MODULE, {get_connects, Sess, UserId}).

-spec add_command({atom(), pid()} | undefined, ddEntityId(), atom(), binary(), binary(), list() | undefined, term()) -> ddEntityId().
add_command(undefined, Owner, Adapter, Name, Cmd, Conn, Opts) ->
    gen_server:call(?MODULE, {add_command, Owner, Adapter, Name, Cmd, Conn, Opts});
add_command(Sess, Owner, Adapter, Name, Cmd, undefined, Opts) ->
    case is_local_query(Cmd) of
        true -> Conn = local;
        false -> Conn = []
    end,
    add_command(Sess, Owner, Adapter, Name, Cmd, Conn, Opts);
add_command(Sess, Owner, Adapter, Name, Cmd, Conn, Opts) ->
    Id = erlang:phash2(make_ref()),
    ?Debug("add_command ~p new id ~p", [Name, Id]),
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = Owner
                 , adapters  = [Adapter]
                 , command   = Cmd
                 , conns     = Conn
                 , opts      = Opts},
    Sess:run_cmd(insert, [ddCmd, NewCmd]),
    ?Debug("add_command inserted ~p", [NewCmd]),
    Id.

-spec update_command({atom(), pid()} | undefined, ddEntityId(), ddEntityId(), binary(), binary(), term()) -> ddEntityId().
update_command(undefined, Id, Owner, Name, Sql, Opts) -> gen_server:call(?MODULE, {update_command, Id, Owner, Name, Sql, Opts});
update_command(Sess, Id, Owner, Name, Sql, Opts) ->
    ?Debug("update command ~p replacing id ~p", [Name, Id]),
    {[Cmd], true} = Sess:run_cmd(select, [ddCmd, [{#ddCmd{id = Id, _='_'}, [], ['$_']}]]),
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = Owner
                 , adapters  = Cmd#ddCmd.adapters
                 , command   = Sql
                 , conns     = Cmd#ddCmd.conns
                 , opts      = Opts},
    Sess:run_cmd(write, [ddCmd, NewCmd]),
    Id.

-spec update_command({atom(), pid()} | undefined, ddEntityId(), ddEntityId(), binary(), binary(), list(), term()) -> ddEntityId().
update_command(undefined, Id, Owner, Name, Sql, Conns, Opts) -> gen_server:call(?MODULE, {update_command, Id, Owner, Name, Sql, Conns, Opts});
update_command(Sess, Id, Owner, Name, Sql, Conns, Opts) ->
    ?Debug("update command ~p replacing id ~p", [Name, Id]),
    {[Cmd], true} = Sess:run_cmd(select, [ddCmd, [{#ddCmd{id = Id, _='_'}, [], ['$_']}]]),
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = Owner
                 , adapters  = Cmd#ddCmd.adapters
                 , command   = Sql
                 , conns     = Conns
                 , opts      = Opts},
    Sess:run_cmd(write, [ddCmd, NewCmd]),
    Id.

-spec user_name(atom() | integer() | binary()) -> binary().
user_name(system) -> <<"system">>;
user_name(Name) when is_binary(Name) -> Name;
user_name(Id) when is_integer(Id) ->
    {[Name],true} = imem_meta:select(ddAccount, [{#ddAccount{id=Id,name='$1',_='_'},[],['$1']}]),
    Name.

-spec add_view({atom(), pid()} | undefined, ddEntityId(), binary(), ddEntityId(), #viewstate{}) -> ddEntityId().
add_view(undefined, Owner, Name, CmdId, ViewsState) ->
    gen_server:call(?MODULE, {add_view, Owner, Name, CmdId, ViewsState});
add_view(Sess, Owner, Name, CmdId, ViewsState) ->
    Id = case Sess:run_cmd(select, [ddView, [{#ddView{name=Name, cmd = CmdId, id='$1', owner=Owner, _='_'}
                                            , []
                                            , ['$1']}]]) of
        {[Id0|_], true} ->
            ?Debug("add_view ~p replacing id ~p ~p~n", [Name, Id0, Owner]),
            Id0;
        _ ->
            Id1 = erlang:phash2(make_ref()),
            ?Debug("add_view ~p new id ~p", [Name, Id1]),
            Id1
    end,
    NewView = #ddView { id      = Id
                     , name     = Name
                     , owner    = Owner
                     , cmd      = CmdId
                     , state    = ViewsState},
    Sess:run_cmd(write, [ddView, NewView]), %% TODO: Validate result...
    ?Debug("add_view written ~p", [NewView]),
    Id.

-spec update_view({atom(), pid()}, integer(), #viewstate{}, binary()) -> integer() | {error, binary()}.
update_view(Sess, ViewId, ViewsState, Qry) when is_integer(ViewId) ->
    %% TODO: At the moment the command and the view always have the same owner.
    %%       Check for authorization.
    case Sess:run_cmd(select, [ddView, [{#ddView{id=ViewId, _='_'}, [], ['$_']}]]) of
        {[OldView], true} ->
            ?Debug("The oldView ~p and the session ~p", [OldView, Sess]),
            Cmd = get_command(Sess, OldView#ddView.cmd),
            ?Debug("The old cmd ~p", [Cmd]),
            %% TODO: Handle multiple adapters.
            update_command(Sess, Cmd#ddCmd.id, Cmd#ddCmd.owner, Cmd#ddCmd.name, Qry, Cmd#ddCmd.opts),
            case ViewsState of
                #viewstate{table_layout=[], column_layout=[]} ->
                    NewView = OldView;
                #viewstate{table_layout=[]} ->
                    #viewstate{table_layout=OldTableLay} = OldView#ddView.state,
                    NewState = ViewsState#viewstate{table_layout=OldTableLay},
                    NewView = OldView#ddView{state=NewState};
                #viewstate{column_layout=[]} ->
                    #viewstate{column_layout=OldColumLay} = OldView#ddView.state,
                    NewState = ViewsState#viewstate{column_layout=OldColumLay},
                    NewView = OldView#ddView{state=NewState};
                #viewstate{} ->
                    NewView = OldView#ddView{state=ViewsState}
            end,
            Sess:run_cmd(write, [ddView, NewView]),
            ?Debug("update_view written ~p", [NewView]),
            ViewId;
        _ ->
            ?Error("Unable to get the view to update ~p", [ViewId]),
            {error, <<"Unable to get the view to update">>}
    end.

-spec rename_view({atom(), pid()}, integer(), binary()) -> ok | {error, binary()}.
rename_view(Sess, ViewId, ViewName) ->
    %% TODO: At the moment the command and the view always have the same owner.
    %%       Check for authorization.
    case Sess:run_cmd(select, [ddView, [{#ddView{id=ViewId, _='_'}, [], ['$_']}]]) of
        {[OldView], true} ->
            case Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=OldView#ddView.cmd, _='_'}, [], ['$_']}]]) of
                {[OldCmd], true} ->
                    Sess:run_cmd(write, [ddCmd, OldCmd#ddCmd{name = ViewName}]),
                    Sess:run_cmd(write, [ddView, OldView#ddView{name = ViewName}]),
                    ok;
                _ ->
                    ?Error("Unable to get the command to rename ~p", [OldView#ddView.cmd]),
                    {error, <<"Unable to find the command to rename">>}
            end;
        _ ->
            ?Error("Unable to get the view to rename ~p", [ViewId]),
            {error, <<"Unable to find the view to rename">>}
    end.

-spec delete_view({atom(), pid()}, integer()) -> integer() | {error, binary()}.
delete_view(Sess, ViewId) ->
    %% TODO: At the moment the command and the view always have the same owner.
    %%       Check for authorization.
    case Sess:run_cmd(select, [ddView, [{#ddView{id=ViewId, _='_'}, [], ['$_']}]]) of
        {[OldView], true} ->
            case Sess:run_cmd(select, [ddView, [{#ddView{cmd=OldView#ddView.cmd, _='_'}, [], ['$_']}]]) of
                {[OldView], true} ->
                    % Only one view with the command, so the command is also deleted
                    case Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=OldView#ddView.cmd, _='_'}, [], ['$_']}]]) of
                        {[OldCmd], true} ->
                            ok = Sess:run_cmd(delete, [ddCmd, OldCmd#ddCmd.id]);
                        _ -> ?Info("No command found to delete ~p", [OldView#ddView.cmd])
                    end;
                _ -> ok
            end,
            ok = Sess:run_cmd(delete, [ddView, OldView#ddView.id]),
            ok;
        _ ->
            ?Error("Unable to get the view to delete ~p", [ViewId]),
            {error, <<"Unable to find the view to delete">>}
    end.

-spec get_adapters({atom(), pid()}) -> {error, binary()} | [#ddAdapter{}].
get_adapters(Sess) ->
    ?Debug("get_adapters"),
    check_cmd_select(Sess, [ddAdapter, [{'$1', [], ['$_']}]]).

-spec get_command({atom(), pid()}, ddEntityId() | binary()) -> #ddCmd{} | {error, binary()}.
get_command(Sess, IdOrName) ->
    ?Debug("get_command for id ~p", [IdOrName]),
    CmdsResult =
        case IdOrName of
            Id when is_integer(Id) ->
                check_cmd_select(Sess, [ddCmd, [{#ddCmd{id=Id, _='_'}, [], ['$_']}]]);
            Name ->
                check_cmd_select(Sess, [ddCmd, [{#ddCmd{name=Name, _='_'}, [], ['$_']}]])
        end,
    case CmdsResult of
        {error, _} = Error -> Error;
        [Cmd | _Ignored] -> Cmd;
        [] ->  #ddCmd{opts=[]}
    end.

-spec get_view({atom(), pid()}, ddEntityId()) -> {error, binary()} | #ddView{} | undefined .
get_view(undefined, ViewId) -> gen_server:call(?MODULE, {get_view, ViewId});
get_view(Sess, ViewId) ->
    ?Debug("get ddView by id ~p", [ViewId]),
    case check_cmd_select(Sess, [ddView, [{#ddView{id = ViewId, _ = '_'}, [], ['$_']}]]) of
        {error, _} = Error -> Error;
        [View] -> View;
        Result ->
            ?Error("ddView with the id ~p was not found, select result: ~n~p", [ViewId, Result]),
            undefined
    end.

-spec get_view({atom(), pid()} | undefined, binary(), atom(), ddEntityId() | '_') -> #ddView{} | undefined.
get_view(undefined, Name, Adapter, Owner) -> gen_server:call(?MODULE, {get_view, Name, Adapter, Owner});
get_view(Sess, Name, Adapter, Owner) ->
    ?Debug("get_view ~p", [Name]),
    case check_cmd_select(Sess, [ddView,[{#ddView{name=Name, owner=Owner, _='_'}, [], ['$_']}]]) of
        {error, _} = Error -> Error;
        Views ->
            case filter_view_result(Views, Sess, Adapter) of
                {error, _} = Error -> Error;
                View -> View
            end
    end.

-spec save_dashboard({atom(), pid()}, ddEntityId(), integer(), binary(), list()) -> integer() | {error, binary()}.
save_dashboard(Sess, Owner, -1, Name, Views) ->
    NewId = erlang:phash2(make_ref()),
    case Sess:run_cmd(select, [ddDash, [{#ddDash{id=NewId, name='$1', _='_'}, [], ['$1']}]]) of
        {[DashName], true} ->
            ?Debug("Save dashboard colision saving the dashboard ~p with id ~p", [DashName, NewId]),
            save_dashboard(Sess, Owner, -1, Name, Views);
        _ ->
            save_dashboard(Sess, Owner, NewId, Name, Views)
    end;
save_dashboard(Sess, Owner, DashId, Name, Views) ->
    NewDash = #ddDash{id = DashId, name = Name, owner = Owner, views = Views},
    Sess:run_cmd(write, [ddDash, NewDash]),
    ?Debug("dashboard saved ~p", [NewDash]),
    DashId.

-spec rename_dashboard({atom(), pid()}, integer(), binary()) -> binary() | {error, binary()}.
rename_dashboard(Sess, Id, Name) ->
    case check_cmd_select(Sess, [ddDash, [{#ddDash{id=Id, _='_'}, [], ['$_']}]]) of
        {error, _} = Error -> Error;
        [] -> {error, <<"Dashboard not found">>};
        [OldDash = #ddDash{}] ->
            Sess:run_cmd(write, [ddDash, OldDash#ddDash{name=Name}]),
            Name
    end.

-spec delete_dashboard({atom(), pid()}, integer()) -> integer() | {error, binary()}.
delete_dashboard(Sess, Id) ->
    case check_cmd_select(Sess, [ddDash, [{#ddDash{id=Id, _='_'}, [], ['$_']}]]) of
        {error, _} = Error -> Error;
        [] -> {error, <<"Dashboard not found">>};
        [#ddDash{id = Id}] ->
            ok = Sess:run_cmd(delete, [ddDash, Id]),
            Id
    end.

-spec get_dashboards({atom(), pid()}, ddEntityId()) -> {error, binary()} | [#ddDash{}].
get_dashboards(Sess, Owner) ->
    check_cmd_select(Sess, [ddDash, [{#ddDash{owner = Owner, _='_'}, [], ['$_']}]]).

-spec get_name({atom(), pid()}, ddEntityId()) -> ddIdentity().
get_name(Sess, UserId) ->
    case Sess:run_cmd(select, [ddAccount, [{#ddAccount{id=UserId, name='$1', _='_'}, [], ['$1']}]]) of
        {[Username], true} -> Username;
        _ -> <<"">>
    end.

-spec add_adapter_to_cmd({atom(), pid()} | undefined, ddEntityId(), atom()) -> ok | {error, binary()}.
add_adapter_to_cmd(undefined, CmdId, Adapter) ->
    gen_server:call(?MODULE, {add_adapter_to_cmd, CmdId, Adapter});
add_adapter_to_cmd(Sess, CmdId, Adapter) ->
    {[Cmd], true} = Sess:run_cmd(select, [ddCmd, [{#ddCmd{id = CmdId, _='_'}, [], ['$_']}]]),
    case lists:member(Adapter, Cmd#ddCmd.adapters) of
        false ->
            NewAdapters = [Adapter|Cmd#ddCmd.adapters];
        true ->
            NewAdapters = Cmd#ddCmd.adapters
    end,
    Sess:run_cmd(write, [ddCmd, Cmd#ddCmd{adapters = NewAdapters}]),
    CmdId.

-spec add_d3_templates_path(atom(), string()) -> ok.
add_d3_templates_path(Application, Path) ->
    gen_server:call(?MODULE, {add_d3_templates_path, {Application, Path}}).

-spec get_d3_templates() -> [{atom(), string()}].
get_d3_templates() ->
    gen_server:call(?MODULE, get_d3_templates).

-spec get_d3_templates_path(atom()) -> string().
get_d3_templates_path(Application) ->
    gen_server:call(?MODULE, {get_d3_templates_path, Application}).

-spec get_host_app() -> binary().
get_host_app() ->
    gen_server:call(?MODULE, get_host_app).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

init([]) ->
    SchemaName = imem_meta:schema(),
    case erlimem:open(local, SchemaName) of
        {ok, Sess} ->
            {ok, Vsn} = application:get_key(dderl,vsn),
            case code:lib_dir(dderl) of
                {error,bad_name} -> ?Info("Application not running from installation", []);
                LibDir ->
                    ConfigPath = filename:join([LibDir,"..","..","releases",Vsn]),
                    case filelib:is_dir(ConfigPath) of
                        true ->
                            ?Info("Adding system schema: ~p", [ConfigPath]),
                            Sess:run_cmd(create_sys_conf, [ConfigPath]);
                        false ->
                            ?Info("Application not running from installation", [])
                    end
            end,
            TablesToBuild =  [
                  {ddAdapter, record_info(fields, ddAdapter), ?ddAdapter, #ddAdapter{}}
                , {ddInterface, record_info(fields, ddInterface), ?ddInterface, #ddInterface{}}
                , {ddConn, record_info(fields, ddConn), ?ddConn, #ddConn{}}
                , {ddCmd, record_info(fields, ddCmd), ?ddCmd, #ddCmd{}}
                , {ddView, record_info(fields, ddView), ?ddView, #ddView{}}
                , {ddDash, record_info(fields, ddDash), ?ddDash, #ddDash{}}
            ],
            ?Debug("tables to build: ~p", [TablesToBuild]),
            build_tables_on_boot(Sess, TablesToBuild),
            Sess:run_cmd(write, [ddInterface, #ddInterface{id = ddjson, fullName = <<"DDerl">>}]),
            % Initializing adapters (all the *_adapter modules compiled with dderl)
            %  doesn't include dynamically built adapters
            {ok, AdaptMods} = application:get_key(dderl, modules),
            Adapters = [A || A <- AdaptMods, re:run(erlang:atom_to_binary(A, utf8), ".*_adapter$") =/= nomatch],
            [gen_server:cast(?MODULE, {init_adapter, Adapter}) || Adapter <- Adapters],
            ?Info("Available adapters ~p", [Adapters]),
            D3Templates = [{dderl, filename:join(dderl:priv_dir(), "d3_templates")}],
            ?Info("Default d3 templates directory ~p", [D3Templates]),
            {ok, #state{sess=Sess, schema=SchemaName, d3_templates=D3Templates}};
        {error, Reason} ->
             ?Error("Failed to start : ~p", [Reason]),
            {stop, Reason};
        Else ->
             ?Error("Failed to start : ~p", [Else]),
             {stop, Else}
    end.

-spec build_tables_on_boot({atom(), pid()}, [tuple()]) -> ok.
build_tables_on_boot(_, []) -> ok;
build_tables_on_boot(Sess, [{N, Cols, Types, Default}|R]) ->
    Sess:run_cmd(init_create_check_table, [N, {Cols, Types, Default}, []]),
    build_tables_on_boot(Sess, R).

handle_call({add_connect, Conn}, _From, #state{sess=Sess} = State) ->
    {reply, add_connect_internal(Sess, Sess, Conn), State};
handle_call({add_connect, UserSess, Conn}, _From, #state{sess=DalSess} = State) ->
    {reply, add_connect_internal(UserSess, DalSess, Conn), State};

handle_call({get_connects, UserSess, UserId}, _From, #state{sess = DalSess} = State) ->
    case check_cmd_select(UserSess, [ddConn, [{'$1', [], ['$_']}]]) of
        {error, _} = Error ->
            {reply, Error, State};
        AllCons ->
            case UserSess:run_cmd(have_permission, [[?MANAGE_CONNS]]) of
                true -> Cons = [C#ddConn{owner = get_name(DalSess, C#ddConn.owner)} || C <- AllCons];
                _ ->
                    Cons = [C#ddConn{owner = get_name(DalSess, C#ddConn.owner)}
                            || C <- AllCons, conn_permission(UserSess, UserId, C)]
            end,
            {reply, Cons, State}
    end;

handle_call({update_command, Id, Owner, Name, Sql, Opts}, _From, #state{sess=Sess} = State) ->
    {reply, update_command(Sess, Id, Owner, Name, Sql, Opts), State};
handle_call({update_command, Id, Owner, Name, Sql, Conns, Opts}, _From, #state{sess=Sess} = State) ->
    {reply, update_command(Sess, Id, Owner, Name, Sql, Conns, Opts), State};

handle_call({add_command, Owner, Adapter, Name, Cmd, Conn, Opts}, _From, #state{sess=Sess} = State) ->
    {reply, add_command(Sess, Owner, Adapter, Name, Cmd, Conn, Opts), State};

handle_call({add_adapter_to_cmd, CmdId, Adapter}, _From, #state{sess=Sess} = State) ->
    {reply, add_adapter_to_cmd(Sess, CmdId, Adapter), State};

handle_call({add_view, Owner, Name, CmdId, ViewsState}, _From, #state{sess=Sess} = State) ->
    {reply, add_view(Sess, Owner, Name, CmdId, ViewsState), State};

handle_call({get_view, ViewId}, _From, #state{sess = Sess} = State) ->
    {reply, get_view(Sess, ViewId), State};

handle_call({get_view, Name, Adapter, Owner}, _From, #state{sess=Sess} = State) ->
    {reply, get_view(Sess, Name, Adapter, Owner), State};

handle_call({add_d3_templates_path, Entry}, _From, #state{d3_templates=D3Templates} = State) ->
    {reply, ok, State#state{d3_templates=[Entry | D3Templates]}};

handle_call(get_d3_templates, _From, #state{d3_templates=D3Templates} = State) ->
    {reply, D3Templates, State};

handle_call({get_d3_templates_path, Application}, _From, #state{d3_templates=D3Templates} = State) ->
    Entry = proplists:get_value(Application, D3Templates),
    {reply, Entry, State};

handle_call(get_host_app, _From, #state{host_app = undefined} = State) ->
    HostApp =
        lists:foldl(
            fun({App,_,_}, <<>>) ->
                    {ok, Apps} = application:get_key(App, applications),
                    case lists:member(dderl, Apps) of
                        true -> atom_to_binary(App, utf8);
                        _ -> <<>>
                    end;
                (_, App) -> App
            end, <<>>, application:which_applications()),
    {reply, HostApp, State#state{host_app = HostApp}};
handle_call(get_host_app, _From, #state{host_app = HostApp} = State) ->
    {reply, HostApp, State};

handle_call(Req,From,State) ->
    ?Info("unknown call req ~p from ~p~n", [Req, From]),
    {reply, ok, State}.

handle_cast({add_adapter, Id, FullName}, #state{sess=Sess} = State) ->
    Adp = #ddAdapter{id=Id,fullName=FullName},
    Sess:run_cmd(write, [ddAdapter, Adp]),
    ?Debug("add_adapter written ~p", [Adp]),
    {noreply, State};
handle_cast({init_adapter, Adapter}, State) ->
    spawn(fun() ->
        Deps = Adapter:get_deps(),
        ?Info("checking for ~p dependencies: ~p", [Adapter, Deps]),
        case check_dependencies(Deps) of
            true ->
                ?Info("Dependencies found, initializing adapter ~p", [Adapter]),
                Adapter:init();
            false ->
                ?Info("Some dependencies of ~p missing, it will be excluded", [Adapter])
        end
    end),
    {noreply, State};
handle_cast(Req,State) ->
    ?Debug("unknown cast ~p", [Req]),
    {noreply, State}.

handle_info(Req,State) ->
    ?Debug("unknown info ~p", [Req]),
    {noreply, State}.

terminate(Reason, #state{sess = Sess}) ->
    ?Debug("terminating, reason ~p", [Reason]),
    Sess:close(),
    ok.

code_change(_OldVsn, State, _Extra)     -> {ok, State}.
format_status(_Opt, [_PDict, _State])   -> ok.


%% Helper functions %%
-spec is_local_query(binary()) -> boolean().
is_local_query(Qry) ->
    SysTabs = [erlang:atom_to_binary(Dt, utf8) || Dt <- [ddAdapter,ddInterface,ddConn,ddCmd,ddView,ddDash]],

%   Example estructure for a query with joins and alias
%   SELECT * from tab1 a INNER JOIN tab2 on a = b INNER JOIN tab3 on c = d, tab4 f, tab2, tab3 b INNER JOIN tab2 on a1 = b4, tab5 INNER JOIN tab2 on a1 = b4
%   {select,[{hints,<<>>},
%            {opt,<<>>},
%            {fields,[<<"*">>]},
%            {into,[]},
%            {from,[{{as,<<"tab1">>,<<"a">>},
%                    [{join_inner,<<"tab2">>,{on,{'=',<<"a">>,<<"b">>}}},
%                     {join_inner,<<"tab3">>,{on,{'=',<<"c">>,<<"d">>}}}]},
%                   {as,<<"tab4">>,<<"f">>},
%                   <<"tab2">>,
%                   {{as,<<"tab3">>,<<"b">>},
%                    [{join_inner,<<"tab2">>,{on,{'=',<<"a1">>,<<"b4">>}}}]},
%                   {<<"tab5">>,
%                    [{join_inner,<<"tab2">>,{on,{'=',<<"a1">>,<<"b4">>}}}]}]},
%            {where,{}},
%            {'group by',[]},
%            {having,{}},
%            {'order by',[]}]}

    case sqlparse:parsetree(Qry) of
        {ok, [{{select,QOpts},_}|_]} ->
            case lists:keyfind(from, 1, QOpts) of
                {from, Tables} ->
                    lists:foldl(fun(T,_) ->
                             Tab = case T of
                                 {{_,T1,_}, _} when is_binary(T1) -> T1;
                                 {T1, _} when is_binary(T1) -> T1;
                                 {_,T1,_} when is_binary(T1) -> T1;
                                 T when is_binary(T) -> T;
                                 _ -> T
                             end,
                             lists:member(Tab, SysTabs)
                         end,
                         true,
                         Tables);
                _ -> false
            end;
        {lex_error, Error} ->
            ?Error("SQL lexer error ~p", [Error]),
            false;
        {parse_error, Error} ->
            ?Error("SQL parser error ~p", [Error]),
            false;
        _ ->
            false
    end.

-spec can_connect_locally({atom(), pid()}) -> boolean().
can_connect_locally(Sess) ->
    Sess:run_cmd(have_permission, [[?USE_LOCAL_CONN]]) == true.

-spec conn_permission({atom(), pid()}, ddEntityId(), #ddConn{}) -> boolean().
conn_permission(_Sess, UserId, #ddConn{owner=UserId}) -> true; %% If it is the owner always allow usage.
conn_permission(Sess, _UserId, #ddConn{id=ConnId, owner=system}) ->
    Sess:run_cmd(have_permission, [?USE_SYS_CONNS]) orelse  %% If it can use system connections.
        Sess:run_cmd(have_permission, [?USE_CONN(ConnId)]); %% or maybe only this particular connection.
conn_permission(Sess, _UserId, #ddConn{id=ConnId}) ->
    Sess:run_cmd(have_permission, [?USE_CONN(ConnId)]). %% Access to a connection by id.

-spec add_connect_internal({atom(), pid()}, {atom(), pid()}, #ddConn{}) -> #ddConn{} | {error, binary()}.
add_connect_internal(UserSess, DalSess, #ddConn{schm = undefined} = Conn) ->
    {ok, SchemaName} = application:get_env(imem, mnesia_schema_name),
    add_connect_internal(UserSess, DalSess, Conn#ddConn{schm = atom_to_binary(SchemaName, utf8)});
add_connect_internal(UserSess, DalSess, #ddConn{id = null, owner = Owner} = Conn) ->
    case UserSess:run_cmd(select, [ddConn, [{#ddConn{name='$1', owner='$2', id='$3', _='_'}
                                         , [{'=:=','$1',Conn#ddConn.name},{'=:=','$2',Owner}]
                                         , ['$_']}]]) of
        {[#ddConn{id = Id} = OldCon | _], true} ->
            %% User is updating is own connection no need for privileges
            NewCon = Conn#ddConn{id=Id},
            if OldCon == NewCon -> %% Connection not changed
                   Conn#ddConn{owner = get_name(DalSess, Conn#ddConn.owner)};
               true ->
                   ?Info("replacing connection ~p, owner ~p", [NewCon, Owner]),
                   check_save_conn(UserSess, DalSess, update, {OldCon, NewCon})
            end;
        _ ->
            HavePermission = (UserSess =:= DalSess) orelse
                UserSess:run_cmd(have_permission, [[manage_system, ?MANAGE_CONNS, ?CREATE_CONNS]]),
            case HavePermission of
                true ->
                    Id = erlang:phash2(crypto:strong_rand_bytes(16)),
                    NewCon = Conn#ddConn{id=Id},
                    ?Info("adding new connection ~p", [NewCon]),
                    check_save_conn(UserSess, DalSess, insert, NewCon);
                _ -> {error, <<"Create connections unauthorized">>}
            end
    end;
add_connect_internal(UserSess, DalSess, #ddConn{id = OldId, owner = Owner} = Conn)
  when is_integer(OldId) ->
    case UserSess:run_cmd(select, [ddConn, [{#ddConn{id=OldId, _='_'}, [], ['$_']}]]) of
        {[#ddConn{owner = OldOwner} = OldCon], true} ->
            %% Save the conn only if there is some difference.
            case is_same_conn(OldCon, Conn) of
                true ->
                    %% If the connection is not changed then do not save a copy.
                    OldCon#ddConn{owner = get_name(DalSess, OldCon#ddConn.owner)};
                false ->
                    if
                        Owner =:= OldOwner -> %% Same owner update the connection.
                           check_save_conn(UserSess, DalSess, update, {OldCon, Conn});
                       true -> %% Different owner, create a copy.
                           add_connect_internal(UserSess, DalSess, Conn#ddConn{id = null})
                   end
            end;
        {[], true} ->
            %% Connection with id not found, adding a new one.
            add_connect_internal(UserSess, DalSess, Conn#ddConn{id = null});
        Result ->
            ?Error("Error getting connection with id ~p, Result:~n~p", [OldId, Result]),
            {error, <<"Error saving the connection">>}
    end.

is_same_conn(#ddConn{access = Access1} = Conn1, Conn2) when not is_map(Access1) ->
	is_same_conn(Conn1#ddConn{access = maps:from_list(Access1)}, Conn2);
is_same_conn(Conn1, #ddConn{access = Access2} = Conn2) when not is_map(Access2) ->
	is_same_conn(Conn1, Conn2#ddConn{access = maps:from_list(Access2)});
is_same_conn(Conn1, Conn2) ->
    Conn1#ddConn{access = maps:remove(<<"user">>,maps:remove(user,Conn1#ddConn.access))} ==
    Conn2#ddConn{owner = Conn1#ddConn.owner,
                 access = maps:remove(<<"user">>,maps:remove(user, Conn2#ddConn.access))}.

-spec check_save_conn({atom(), pid()}, {atom(), pid()}, atom(), #ddConn{}) -> #ddConn{} | {error, binary()}.
check_save_conn(UserSess, DalSess, Op, Conn0) ->
    Conn = case Conn0 of
		   Conn0 when is_record(Conn0, ddConn) ->
			   Conn0#ddConn{access = maps:remove(password, pl2m(Conn0#ddConn.access))};
		   {C1, C2} when is_record(C1, ddConn), is_record(C2, ddConn) ->
			   {C1, C2#ddConn{access = maps:remove(password, pl2m(C2#ddConn.access))}}
	   end,
    case UserSess:run_cmd(Op, [ddConn, Conn]) of
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("failed to ~p connection with ~p : ~n~p", [Op, Conn, Error]),
            Msg = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            {error, Msg};
        {error, Error} ->
            ?Error("~p connection failed: ~p", [Op, Error]),
            Msg = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {error, Msg};
        #ddConn{} = SavedConn ->
            SavedConn#ddConn{owner = get_name(DalSess, SavedConn#ddConn.owner)};
        InvalidReturn ->
            ?Error("Invalid return on ~p connection ~p: The result:~n~p", [Op, Conn, InvalidReturn]),
            {error, <<"Error saving the connection (Invalid value returned from DB)">>}
    end.

-spec pl2m([tuple()] | map()) -> map().
pl2m(Map) when is_map(Map) -> Map;
pl2m([{_,_}|_] = PL) -> maps:from_list(PL).

-spec check_cmd_select({atom(), pid()}, list()) -> {error, binary()} | list().
check_cmd_select(UserSess, Args) ->
    case UserSess:run_cmd(select, Args) of
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("select failed : ~n~p", [Error]),
            Msg = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            {error, Msg};
        {error, Error} ->
            ?Error("select failed: ~p", [Error]),
            Msg = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {error, Msg};
        {Result, true} ->
            Result;
        InvalidReturn ->
            ?Error("Invalid return on select, args ~p: The result:~n~p", [Args, InvalidReturn]),
            {error, <<"Error on select (Invalid value returned from DB)">>}
    end.

-spec check_dependencies([atom()]) -> boolean().
check_dependencies([]) -> true;
check_dependencies([Dep | Rest]) ->
    case code:priv_dir(Dep) of
        {error, bad_name} -> false;
        _ -> check_dependencies(Rest)
    end.

-spec filter_view_result([#ddView{}], {atom(), pid()}, atom()) -> #ddView{} | {error, binary()}.
filter_view_result([], _, _) -> undefined;
filter_view_result([V | Views], Sess, Adapter) ->
    case check_cmd_select(Sess, [ddCmd, [{#ddCmd{id=V#ddView.cmd, adapters='$1', _='_'}, [], ['$1']}]]) of
        {error, _} = Error ->
            Error;
        [C] ->
            case lists:member(Adapter, C) of
                true -> V;
                false -> filter_view_result(Views, Sess, Adapter)
            end;
        _ ->
            ?Error("Command id ~p not found for view ~p, with id ~p", [V#ddView.cmd, V#ddView.name, V#ddView.id]),
            {error, iolist_to_binary(["Command not found for view ", V#ddView.name])}
    end.

-spec get_restartable_apps() -> [atom()].
get_restartable_apps() ->
    {ok, CurrApp} = application:get_application(?MODULE),
    ?RESTARTAPPS(CurrApp).

-spec process_login(map(), any(),
                    #{auth => fun((any()) -> ok | {any(), list()}),
                      connInfo => map(),
                      stateUpdateUsr =>  fun((any(), any()) -> any()),
                      stateUpdateSKey => fun((any(), any()) -> any()),
                      relayState => fun((any(), any()) -> any()),
                      urlPrefix => list()}) -> ok | map().
process_login(#{<<"smsott">> := Token} = Body, State,
              #{auth := AuthFun} = Ctx) ->
    process_login_reply(AuthFun({smsott,Token}), Body, Ctx, State);
process_login(#{<<"user">>:=User, <<"password">>:=Password} = Body, State,
              #{stateUpdateUsr := StateUpdateFun, auth := AuthFun} = Ctx) ->
    process_login_reply(AuthFun({pwdmd5,
                                 {User, list_to_binary(Password)}}),
                        Body, Ctx, StateUpdateFun(State, User));
process_login(#{<<"samluser">>:=User} = Body, State,
              #{stateUpdateUsr := StateUpdateFun, auth := AuthFun} = Ctx)
  when is_function(StateUpdateFun, 2), is_function(AuthFun, 1) ->
    process_login_reply(AuthFun({saml, User}), Body, Ctx,
                        StateUpdateFun(State, User));
process_login(Body, State, #{connInfo := ConnInfo, auth := AuthFun} = Ctx)
  when is_map(ConnInfo), is_function(AuthFun, 1) ->
    process_login_reply(AuthFun({access, ConnInfo}), Body, Ctx, State).

process_login_reply(ok, _Body, _Ctx, State) -> {ok, State};

process_login_reply({ok, []}, _Body, _Ctx, State) -> {ok, State};
process_login_reply({SKey, []}, _Body, #{stateUpdateSKey := StateUpdateFun},
                    State) when is_function(StateUpdateFun, 2) ->
    {ok, StateUpdateFun(State, SKey)};

process_login_reply({ok, [{pwdmd5, Data}|_]}, _Body, _Ctx, State) ->
    {#{pwdmd5=>fix_login_data(Data)}, State};
process_login_reply({SKey, [{pwdmd5, Data}|_]}, _Body,
                    #{stateUpdateSKey := StateUpdateFun}, State)
  when is_function(StateUpdateFun, 2) ->
    {#{pwdmd5=>fix_login_data(Data)}, StateUpdateFun(State, SKey)};

process_login_reply({ok, [{smsott, Data}|_]}, _Body, _Ctx, State) ->
    {#{smsott=>fix_login_data(Data)}, State};
process_login_reply({SKey, [{smsott, Data}|_]}, _Body,
                    #{stateUpdateSKey := StateUpdateFun}, State)
  when is_function(StateUpdateFun, 2) ->
    {#{smsott=>fix_login_data(Data)}, StateUpdateFun(State, SKey)};

process_login_reply({SKey, [{saml, _Data}|_]}, Body,
                    #{urlPrefix := UrlPrefix,
                      stateUpdateSKey := StateUpdateFun,
                      relayState := RelayStateFun}, State)
  when is_function(RelayStateFun, 2), is_function(StateUpdateFun, 2) ->
    #{<<"host_url">> := HostUrlBin} = Body,
    HostUrl = binary_to_list(HostUrlBin),
    {#{saml =>
       fix_login_data(
         #{form =>
           dderl_saml_handler:fwdUrl(
             HostUrl, HostUrl ++ UrlPrefix ++ dderl:get_sp_url_suffix(),
             RelayStateFun)})},
     if SKey == ok  -> State;
        true -> StateUpdateFun(State, SKey)
     end}.

fix_login_data(#{accountName:=undefined}=Data) ->
    fix_login_data(Data#{accountName=><<"">>});
fix_login_data(Data) -> Data.

%% Functions used to extract rows from fsm using ets directly.
%% Used by data_sender and csv_export_buffer.
-spec rows_from(ets:tab(), term(), pos_integer()) -> [term()].
rows_from(TableId, Key, Limit) ->
    ets:select(TableId, [{'$1', [{'>=',{element,1,'$1'}, {const, Key}}],['$_']}], Limit).

-spec expand_rows([term()], ets:tab(), fun(), [integer()]) -> [[binary()]].
expand_rows([], _TableId, _RowFun, _ColumnPos) -> [];
expand_rows([{_, Id} | RestRows], TableId, RowFun, ColumnPos) ->
    Row = lists:nth(1, ets:lookup(TableId, Id)),
    expand_rows([Row | RestRows], TableId, RowFun, ColumnPos);
expand_rows([{_I,_Op, RK} | RestRows], TableId, RowFun, ColumnPos) ->
    ExpandedRow = list_to_tuple(RowFun(RK)), %% As tuple for faster access.
    SelectedColumns = [element(Col, ExpandedRow) || Col <- ColumnPos],
    [SelectedColumns | expand_rows(RestRows, TableId, RowFun, ColumnPos)];
expand_rows([FullRowTuple | RestRows], TableId, RowFun, ColumnPos) ->
    SelectedColumns = [element(3+Col, FullRowTuple) || Col <- ColumnPos],
    [SelectedColumns | expand_rows(RestRows, TableId, RowFun, ColumnPos)].

-spec is_proxy(list(), map()) -> boolean().
is_proxy(AppId, NetCtx) ->
    ProxyCheckFun = ?GET_CONFIG(isProxyCheckFun, [AppId], <<"fun(NetCtx) -> false end">>, "Function checks if user is coming through a proxy or not"),
    CacheKey = {?MODULE, isProxyCheckFun, ProxyCheckFun},
    case imem_cache:read(CacheKey) of
        [] ->
            case imem_datatype:io_to_fun(ProxyCheckFun) of
                PF when is_function(PF, 1) ->
                    imem_cache:write(CacheKey, PF),
                    exec_is_proxy_fun(PF, NetCtx);
                _ ->
                    ?Error("Not a valid is proxy fun configured"),
                    false
            end;
        [PF] when is_function(PF, 1) -> exec_is_proxy_fun(PF, NetCtx);
        _ -> false
    end.

-spec exec_is_proxy_fun(reference(), map()) -> boolean().
exec_is_proxy_fun(Fun, NetCtx) ->
    case catch Fun(NetCtx) of
        false -> false;
        true -> true;
        Error ->
            ?Error("proxy check fail : ~p", [Error]),
            false
    end.
