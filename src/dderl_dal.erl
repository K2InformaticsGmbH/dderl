-module(dderl_dal).
-include("dderl.hrl").

-behavior(gen_server).

-export([init/1
        ,start_link/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,format_status/2
        ]).

-export([get_adapters/1
        ,login/3
        ,change_password/4
        ,add_adapter/2
        ,add_command/7
        ,update_command/6
        ,add_view/5
        ,update_view/4
        ,rename_view/3
        ,delete_view/2
        ,add_connect/2
        ,get_connects/2
        ,del_conn/2
        ,get_command/2
        ,get_view/4
        ,get_view/2
        ,is_local_query/1
        ,is_admin/1
        ,save_dashboard/5
        ,get_dashboards/2
        ,log_to_db/7
        ,get_maxrowcount/0
        ,add_adapter_to_cmd/3
        ]).

-record(state, { schema :: term()
               , sess :: {atom(), pid()}
    }).

%% Privileges
-define(MANAGE_CONNS, {dderl, conn, manage}).
-define(CREATE_CONNS, {dderl, conn, create}).
-define(USE_SYS_CONNS, {dderl, conn, {owner, system}, use}).
-define(USE_CONN(__ConnId), {dderl, conn, {conn, __ConnId}, use}).

%% Validate this permission.
-define(USE_ADAPTER, {dderl, adapter, {id, __AdaptId}, use}).

-spec login(binary(), binary(), binary()) -> {error, term()} | {true, {atom(), pid()}, ddEntityId()}.
login(User, Password, SessionId) -> gen_server:call(?MODULE, {login, User, Password, SessionId}).

-spec change_password(binary(), binary(), binary(), binary())-> {error, term()} | {true, {atom(), pid()}, ddEntityId()}.
change_password(User, Password, NewPassword, SessionId) -> gen_server:call(?MODULE, {change_password, User, Password, NewPassword, SessionId}).

-spec add_adapter(atom(), binary()) -> ok.
add_adapter(Id, FullName) -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).

-spec add_connect({atom(), pid()} | undefined, #ddConn{}) -> integer() | {error, binary()}.
add_connect(undefined, #ddConn{} = Conn) -> gen_server:call(?MODULE, {add_connect, Conn});
add_connect(Sess, #ddConn{} = Conn) -> gen_server:call(?MODULE, {add_connect, Sess, Conn}).

-spec get_connects({atom(), pid()}, ddEntityId()) -> [#ddConn{}].
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

-spec get_adapters({atom(), pid()}) -> [#ddAdapter{}].
get_adapters(Sess) ->
    ?Debug("get_adapters"),
    {Adapters, true} = Sess:run_cmd(select, [ddAdapter, [{'$1', [], ['$_']}]]),
    Adapters.

-spec del_conn({atom(), pid()}, ddEntityId()) -> ok | no_permission.
del_conn(Sess, ConId) ->
    HasAll = (Sess:run_cmd(have_permission, [[manage_system, manage_connections]]) == true),
    if HasAll ->
        ok = Sess:run_cmd(delete, [ddConn, ConId]),
        ?Info("del_conn connection ~p deleted", [ConId]),
        ok;
    true ->
        case Sess:run_cmd(have_permission, [{ConId, use}]) of
        true ->
            ok = Sess:run_cmd(delete, [ddConn, ConId]),
            ?Info("del_conn connection ~p deleted", [ConId]),
            ok;
        _ ->
            ?Error("del_conn no permission to delete connection ~p", [ConId]),
            no_permission
        end
    end.

-spec get_command({atom(), pid()}, ddEntityId() | binary()) -> #ddCmd{}.
get_command(Sess, IdOrName) ->
  ?Debug("get_command for id ~p", [IdOrName]),
  {Cmds, true} =
      case IdOrName of
          Id when is_integer(Id) -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=Id, _='_'}, [], ['$_']}]]);
          Name -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{name=Name, _='_'}, [], ['$_']}]])
      end,
  Cmd = if length(Cmds) > 0 -> lists:nth(1, Cmds); true -> #ddCmd{opts=[]} end,
  Cmd.

-spec get_view({atom(), pid()}, ddEntityId()) -> #ddView{} | undefined.
get_view(undefined, ViewId) -> gen_server:call(?MODULE, {get_view, ViewId});
get_view(Sess, ViewId) ->
    ?Debug("get view by id ~p", [ViewId]),
    case Sess:run_cmd(select, [ddView, [{#ddView{id = ViewId, _ = '_'}, [], ['$_']}]]) of
        {[View], true} ->
            View;
        Result ->
            ?Error("View with the id ~p was not found, select result: ~n~p", [ViewId, Result]),
            View = undefined
    end,
    View.

-spec get_view({atom(), pid()} | undefined, binary(), atom(), ddEntityId()) -> #ddView{} | undefined.
get_view(undefined, Name, Adapter, Owner) -> gen_server:call(?MODULE, {get_view, Name, Adapter, Owner});
get_view(Sess, Name, Adapter, Owner) -> 
    ?Debug("get_view ~p", [Name]),
    {Views, true} = Sess:run_cmd(select, [ddView,[{#ddView{name=Name, owner=Owner, _='_'}, [], ['$_']}]]),
    ListResult = [{V, Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=V#ddView.cmd, adapters='$1', _='_'}, [], ['$1']}]])} || V <- Views],
    Result = [V || {V, {[C], true}} <- ListResult, lists:member(Adapter, C)],
    if
        length(Result) > 0 ->
            %% TODO: How can we discriminate to ignore the correct one?
            [View | _Ignored] = Result;
        true ->
            View = undefined
    end,
    View.

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

-spec get_dashboards({atom(), pid()}, ddEntityId()) -> [#ddDash{}].
get_dashboards(Sess, Owner) ->
    {Dashboards, true} = Sess:run_cmd(select, [ddDash, [{#ddDash{owner = Owner, _='_'}, [], ['$_']}]]),
    Dashboards.

-spec get_id({atom(), pid()}, ddIdentity()) -> ddEntityId().
get_id(Sess, Username) ->
    {[UserId], true} = Sess:run_cmd(select, [ddAccount, [{#ddAccount{id='$1', name=Username, _='_'}, [], ['$1']}]]),
    UserId.

-spec get_name({atom(), pid()}, ddEntityId()) -> ddIdentity().
get_name(Sess, UserId) ->
    {[Username], true} = Sess:run_cmd(select, [ddAccount, [{#ddAccount{id=UserId, name='$1', _='_'}, [], ['$1']}]]),
    Username.

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

-spec get_maxrowcount() -> integer().
get_maxrowcount() ->
    imem_meta:get_config_hlk(ddConfig, {imem, imem_sql_expr, rownumDefaultLimit}, imem_sql_expr, [node()], 10000).

-spec start_link(term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SchemaName) ->
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [SchemaName], []),
    ?Info("Started! ~n~p", [Result]),
    Result.

init([SchemaName]) ->
    case erlimem:open(local, {SchemaName}, {<<>>, <<>>, dderl_dal}) of
        {ok, Sess} ->
            %lager:set_loglevel(lager_console_backend, debug),
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
            {ok, #state{sess=Sess, schema=SchemaName}};
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

handle_call({login, User, Password, SessionId}, _From, #state{sess=DalSess, schema=SchemaName} = State) ->
    ?Debug("login for user ~p", [User]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, erlang:md5(Password), SessionId}) of
        {error, Error} ->
            ?Error("login exception ~n~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, UserSess} ->
            UserId = get_id(DalSess, User),
            ?Debug("login accepted user ~p with id = ~p", [User, UserId]),
            {reply, {true, UserSess, UserId}, State}
    end;

handle_call({change_password, User, Password, NewPassword, SessionId}, _From, #state{sess=DalSess, schema=SchemaName} = State) ->
    ?Debug("changing password for user ~p", [User]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, erlang:md5(Password), erlang:md5(NewPassword), SessionId}) of
        {error, Error} ->
            ?Error("change password exception ~n~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, UserSess} ->
            UserId = get_id(DalSess, User),
            ?Info("login with new password user ~p with id = ~p", [User, UserId]),
            {reply, {true, UserSess, UserId}, State}
    end;

handle_call({add_connect, Conn}, _From, #state{sess=Sess} = State) ->
    {reply, add_connect_internal(Sess, Sess, Conn), State};
handle_call({add_connect, UserSess, Conn}, _From, #state{sess=DalSess} = State) ->
    {reply, add_connect_internal(UserSess, DalSess, Conn), State};

handle_call({get_connects, UserSess, UserId}, _From, #state{sess = DalSess} = State) ->
    {AllCons, true} = UserSess:run_cmd(select, [ddConn, [{'$1', [], ['$_']}]]),
    case UserSess:run_cmd(have_permission, [[manage_system, ?MANAGE_CONNS]]) of
        true -> Cons = [C#ddConn{owner = get_name(DalSess, C#ddConn.owner)} || C <- AllCons];
        _ ->
            Cons = [C#ddConn{owner = get_name(DalSess, C#ddConn.owner)}
                    || C <- AllCons, conn_permission(UserSess, UserId, C)]
    end,
    {reply, Cons, State};

handle_call({update_command, Id, Owner, Name, Sql, Opts}, _From, #state{sess=Sess} = State) ->
    {reply, update_command(Sess, Id, Owner, Name, Sql, Opts), State};

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
        Adapter:init(),
        ?Debug("init_adapter ~p", [Adapter])
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
                                 T when is_binary(T) -> T
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

-spec is_admin({atom(), pid()}) -> boolean().
is_admin(Sess) ->
    Sess:run_cmd(have_permission, [[manage_system]]) == true.

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
    add_connect_internal(UserSess, DalSess, Conn#ddConn{schm = SchemaName});
add_connect_internal(UserSess, DalSess, #ddConn{id = undefined, owner = Owner} = Conn) ->
    case UserSess:run_cmd(select, [ddConn, [{#ddConn{name='$1', owner='$2', id='$3', _='_'}
                                         , [{'=:=','$1',Conn#ddConn.name},{'=:=','$2',Owner}]
                                         , ['$3']}]]) of
        {[Id|_], true} ->
            %% User is updating is own connection no need for privileges
            NewCon = Conn#ddConn{id=Id},
            ?Info("replacing connection ~p, owner ~p", [NewCon, Owner]),
            check_save_conn(UserSess, DalSess, update, NewCon);
        _ ->
            case UserSess:run_cmd(have_permission, [[manage_system, ?MANAGE_CONNS, ?CREATE_CONNS]]) of
                true ->
                    Id = erlang:phash2(crypto:rand_bytes(16)),
                    NewCon = Conn#ddConn{id=Id},
                    ?Info("adding new connection ~p", [NewCon]),
                    check_save_conn(UserSess, DalSess, insert, NewCon);
                _ ->
                    {error, <<"Create connections unauthorized">>}
            end
    end;
add_connect_internal(UserSess, DalSess, #ddConn{id = OldId, owner = Owner} = Conn) ->
    case UserSess:run_cmd(select, [ddConn, [{#ddConn{id='$1', _='_'}
                                         , [{'=:=', '$1', OldId}]
                                         , ['$_']}]]) of
        {[#ddConn{owner = OldOwner} = OldCon], true} ->
            %% Save the conn only if there is some difference.
            case compare_connections(OldCon, Conn#ddConn{owner = OldOwner}) of
                true ->
                    %% If the connection is not changed then do not save a copy.
                    OldCon#ddConn{owner = get_name(DalSess, OldCon#ddConn.owner)};
                false ->
                    if
                        Owner =:= OldOwner -> %% Same owner update the connection.
                            check_save_conn(UserSess, DalSess, update, Conn);
                        true -> %% Different owner, create a copy.
                            add_connect_internal(UserSess, DalSess, Conn#ddConn{id = undefined})
                    end
            end;
        {[], true} ->
            %% Connection with id not found, adding a new one.
            add_connect_internal(UserSess, DalSess, Conn#ddConn{id = undefined});
        Result ->
            ?Error("Error getting connection with id ~p, Result:~n~p", [OldId, Result]),
            {error, <<"Error saving the connection">>}
    end.

-spec check_save_conn({atom(), pid()}, {atom(), pid()}, atom(), #ddConn{}) -> #ddConn{} | {error, term()}.
check_save_conn(UserSess, DalSess, Op, Conn) ->
    case UserSess:run_cmd(Op, [ddConn, Conn]) of
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("~p connection failed : ~n~p", [Op, Error]),
            Msg = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            {error, Msg};
        {error, Error} ->
            ?Error("~p connection failed: ~p", [Op, Error]),
            Msg = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {error, Msg};
        Conn ->
            Conn#ddConn{owner = get_name(DalSess, Conn#ddConn.owner)};
        InvalidReturn ->
            ?Error("Invalid return on ~p connection ~p: The result:~n~p", [Op, Conn, InvalidReturn]),
            {error, <<"Invalid return saving the connection">>}
    end.

-spec compare_connections(#ddConn{}, #ddConn{}) -> boolean().
compare_connections(Connection, Connection) -> true;
compare_connections(#ddConn{access = []}, _) -> false;
compare_connections(_, #ddConn{access = []}) -> false;
compare_connections(#ddConn{access = A1} = Con1, #ddConn{access = A2} = Con2) ->
    lists:sort(A1) =:= lists:sort(A2)
        andalso compare_connections(Con1#ddConn{access = []}, Con2#ddConn{access = []}).

-spec log_to_db(atom(), atom(), atom(), integer(), list(), binary(), list()) -> ok.
log_to_db(Level,Module,Function,Line,Fields,Message,StackTrace)
when is_atom(Level)
    , is_atom(Module)
    , is_atom(Function)
    , is_integer(Line)
    , is_list(Fields)
    , is_binary(Message)
    , is_list(StackTrace) ->
    spawn(fun() ->
        imem_meta:log_to_db(Level,Module,Function,Line,Fields,Message,StackTrace)
    end),
    ok.
