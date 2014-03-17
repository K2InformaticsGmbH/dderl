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
        ,login/2
        ,change_password/3
        ,add_adapter/2
        ,add_command/7
        ,update_command/8
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
        ,save_dashboard/5
        ,get_dashboards/2
        ,log_to_db/7
        ,get_maxrowcount/0
        ]).

-record(state, { schema :: term()
               , sess :: {atom(), pid()}
    }).

-spec login(binary(), binary()) -> {error, term()} | {true, {atom(), pid()}, ddEntityId()}.
login(User, Password) -> gen_server:call(?MODULE, {login, User, Password}).

-spec change_password(binary(), binary(), binary())-> {error, term()} | {true, {atom(), pid()}, ddEntityId()}.
change_password(User, Password, NewPassword) -> gen_server:call(?MODULE, {change_password, User, Password, NewPassword}).

-spec add_adapter(atom(), binary()) -> ok.
add_adapter(Id, FullName)           -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).

-spec add_connect({atom(), pid()} | undefined, #ddConn{}) -> ok.
add_connect(Sess, #ddConn{} = Conn) -> gen_server:cast(?MODULE, {add_connect, Sess, Conn}).

-spec add_command({atom(), pid()} | undefined, ddEntityId(), atom(), binary(), binary(), atom(), term()) -> ddEntityId().
add_command(Sess, Owner, Adapter, Name, Cmd, Conn, Opts) -> gen_server:call(?MODULE, {add_command, Sess, Owner, Adapter, Name, Cmd, Conn, Opts}).

-spec update_command({atom(), pid()} | undefined, ddEntityId(), ddEntityId(), atom(), binary(), binary(), atom(), term()) -> ddEntityId().
update_command(Sess, Id, Owner, Adapter, Name, Cmd, Conn, Opts) -> gen_server:call(?MODULE, {update_command, Sess, Id, Owner, Adapter, Name, Cmd, Conn, Opts}).

-spec add_view({atom(), pid()} | undefined, ddEntityId(), binary(), ddEntityId(), #viewstate{}) -> ddEntityId().
add_view(Sess, Owner, Name, CmdId, ViewsState) -> gen_server:call(?MODULE, {add_view, Sess, Owner, Name, CmdId, ViewsState}).

-spec update_view({atom(), pid()}, integer(), #viewstate{}, binary()) -> integer() | {error, binary()}.
update_view(Sess, ViewId, ViewsState, Qry) when is_integer(ViewId) -> gen_server:call(?MODULE, {update_view, Sess, ViewId, ViewsState, Qry}).

-spec rename_view({atom(), pid()}, integer(), binary()) -> ok | {error, term()}.
rename_view(Sess, ViewId, ViewName) -> gen_server:call(?MODULE, {rename_view, Sess, ViewId, ViewName}).

-spec delete_view({atom(), pid()}, integer()) -> integer() | {error, binary()}.
delete_view(Sess, ViewId) -> gen_server:call(?MODULE, {delete_view, Sess, ViewId}).

-spec get_adapters({atom(), pid()}) -> [#ddAdapter{}].
get_adapters(Sess) -> gen_server:call(?MODULE, {get_adapters, Sess}).

-spec get_connects({atom(), pid()}, binary()) -> [#ddConn{}].
get_connects(Sess, User) -> gen_server:call(?MODULE, {get_connects, Sess, User}).

-spec del_conn({atom(), pid()}, ddEntityId()) -> ok | no_permission.
del_conn(Sess, ConId) -> gen_server:call(?MODULE, {del_conn, Sess, ConId}).

-spec get_command({atom(), pid()}, ddEntityId() | binary()) -> #ddCmd{}.
get_command(Sess, IdOrName) -> gen_server:call(?MODULE, {get_command, Sess, IdOrName}).

-spec get_view({atom(), pid()}, ddEntityId()) -> #ddView{} | undefined.
get_view(Sess, ViewId) -> gen_server:call(?MODULE, {get_view, Sess, ViewId}).

-spec get_view({atom(), pid()} | undefined, binary(), atom(), ddEntityId()) -> #ddView{} | undefined.
get_view(Sess, Name, Adapter, Owner) -> gen_server:call(?MODULE, {get_view, Sess, Name, Adapter, Owner}).

-spec save_dashboard({atom(), pid()}, ddEntityId(), integer(), binary(), list()) -> integer() | {error, binary()}.
save_dashboard(Sess, Owner, DashId, Name, Views) -> gen_server:call(?MODULE, {save_dashboard, Sess, Owner, DashId, Name, Views}).

-spec get_dashboards({atom(), pid()}, ddEntityId()) -> [#ddDash{}].
get_dashboards(Sess, Owner) -> gen_server:call(?MODULE, {get_dashboards, Sess, Owner}).

-spec get_maxrowcount() -> integer().
get_maxrowcount() ->
    imem_meta:get_config_hlk(ddConfig, {imem,imem_sql,rownumDefaultLimit}, ?MODULE, [node()], 10000).

-spec start_link(term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SchemaName) ->
    ?Info("starting...~n"),
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [SchemaName], []),
    ?Debug("started!~n~p", [Result]),
    Result.

init([SchemaName]) ->
    case erlimem:open(local, {SchemaName}, {<<>>, <<>>}) of
        {ok, Sess} ->
            %lager:set_loglevel(lager_console_backend, debug),
            build_tables_on_boot(Sess, [
                  {ddAdapter, record_info(fields, ddAdapter), ?ddAdapter, #ddAdapter{}}
                , {ddInterface, record_info(fields, ddInterface), ?ddInterface, #ddInterface{}}
                , {ddConn, record_info(fields, ddConn), ?ddConn, #ddConn{}}
                , {ddCmd, record_info(fields, ddCmd), ?ddCmd, #ddCmd{}}
                , {ddView, record_info(fields, ddView), ?ddView, #ddView{}}
                , {ddDash, record_info(fields, ddDash), ?ddDash, #ddDash{}}
            ]),
            ?Info("tables ~p created", [[ddAdapter, ddInterface, ddConn, ddCmd, ddView, ddDash]]),
            Sess:run_cmd(write, [ddInterface, #ddInterface{id = ddjson, fullName = <<"DDerl">>}]),
            % Initializing adapters (all the *_adapter modules compiled with dderl)
            %  doesn't include dynamically built adapters
            {ok, AdaptMods} = application:get_key(dderl, modules),
            Adapters = [A || A <- AdaptMods, re:run(erlang:atom_to_binary(A, utf8), ".*_adapter$") =/= nomatch],
            [gen_server:cast(?MODULE, {init_adapter, Adapter}) || Adapter <- Adapters],
            ?Info("adapters ~p", [Adapters]),
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
    ?Info("creating table ~p", [N]),
    Sess:run_cmd(create_check_table, [N, {Cols, Types, Default}, []]),
    build_tables_on_boot(Sess, R).

handle_call({update_command, undefined, Id, Owner, Adapter, Name, Cmd, Conn, Opts}, From, #state{sess=Sess} = State) ->
    handle_call({update_command, Sess, Id, Owner, Adapter, Name, Cmd, Conn, Opts}, From, State);
handle_call({update_command, Sess, Id, Owner, Adapter, Name, Cmd, undefined, Opts}, From, State) ->
    case is_local_query(Cmd) of
        true -> Conn = local;
        false -> Conn = remote
    end,
    handle_call({update_command, Sess, Id, Owner, Adapter, Name, Cmd, Conn, Opts}, From, State);
handle_call({update_command, Sess, Id, Owner, Adapter, Name, Cmd, Conn, Opts}, _From, State) ->
    ?Debug("update command ~p replacing id ~p", [Name, Id]),
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = Owner
                 , adapters  = [Adapter]
                 , command   = Cmd
                 , conns     = Conn
                 , opts      = Opts},
    Sess:run_cmd(write, [ddCmd, NewCmd]),
    {reply, Id, State};

handle_call({add_command, undefined, Owner, Adapter, Name, Cmd, Conn, Opts}, From, #state{sess=Sess} = State) ->
    handle_call({add_command, Sess, Owner, Adapter, Name, Cmd, Conn, Opts}, From, State);
handle_call({add_command, Sess, Owner, Adapter, Name, Cmd, undefined, Opts}, From, State) ->
    case is_local_query(Cmd) of
        true -> Conn = local;
        false -> Conn = remote
    end,
    handle_call({add_command, Sess, Owner, Adapter, Name, Cmd, Conn, Opts}, From, State);
handle_call({add_command, Sess, Owner, Adapter, Name, Cmd, Conn, Opts}, _From, State) ->
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
    {reply, Id, State};

handle_call({add_view, undefined, Owner, Name, CmdId, ViewsState}, From, #state{sess=Sess} = State) ->
    handle_call({add_view, Sess, Owner, Name, CmdId, ViewsState}, From, State);
handle_call({add_view, Sess, Owner, Name, CmdId, ViewsState}, _From, State) ->
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
    Sess:run_cmd(write, [ddView, NewView]),
    ?Debug("add_view written ~p", [NewView]),
    {reply, Id, State};

handle_call({update_view, Sess, ViewId, ViewsState, Qry}, From, State) ->
    %% TODO: At the moment the command and the view always have the same owner.
    %%       Check for authorization.
    case Sess:run_cmd(select, [ddView, [{#ddView{id=ViewId, _='_'}, [], ['$_']}]]) of
        {[OldView], true} ->
            ?Debug("The oldView ~p and the session ~p", [OldView, Sess]),
            Cmd = internal_get_command(Sess, OldView#ddView.cmd),
            ?Debug("The old cmd ~p", [Cmd]),
            %% TODO: Handle multiple adapters.
            [Adapter] = Cmd#ddCmd.adapters,
            UpdateCmdParams = {update_command,
                               Sess,
                               Cmd#ddCmd.id,
                               Cmd#ddCmd.owner,
                               Adapter,
                               Cmd#ddCmd.name,
                               Qry,
                               Cmd#ddCmd.conns,
                               Cmd#ddCmd.opts},
            handle_call(UpdateCmdParams, From, State),
            NewView = OldView#ddView{state=ViewsState},
            Sess:run_cmd(write, [ddView, NewView]),
            ?Debug("update_view written ~p", [NewView]),
            {reply, ViewId, State};
        _ ->
            ?Error("Unable to get the view to update ~p", [ViewId]),
            {reply, {error, <<"Unable to get the view to update">>}, State}
    end;

handle_call({rename_view, Sess, ViewId, ViewName}, _From, State) ->
    %% TODO: At the moment the command and the view always have the same owner.
    %%       Check for authorization.
    case Sess:run_cmd(select, [ddView, [{#ddView{id=ViewId, _='_'}, [], ['$_']}]]) of
        {[OldView], true} ->
            case Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=OldView#ddView.cmd, _='_'}, [], ['$_']}]]) of
                {[OldCmd], true} ->
                    Sess:run_cmd(write, [ddCmd, OldCmd#ddCmd{name = ViewName}]),
                    Sess:run_cmd(write, [ddView, OldView#ddView{name = ViewName}]),
                    {reply, ok, State};
                _ ->
                    ?Error("Unable to get the command to rename ~p", [OldView#ddView.cmd]),
                    {reply, {error, <<"Unable to find the command to rename">>}, State}
            end;
        _ ->
            ?Error("Unable to get the view to rename ~p", [ViewId]),
            {reply, {error, <<"Unable to find the view to rename">>}, State}
    end;

handle_call({delete_view, Sess, ViewId}, _From, State) ->
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
            {reply, ok, State};
        _ ->
            ?Error("Unable to get the view to delete ~p", [ViewId]),
            {reply, {error, <<"Unable to find the view to delete">>}, State}
    end;

handle_call({get_view, Sess, ViewId}, _From, #state{} = State) ->
    ?Debug("get view by id ~p", [ViewId]),
    case Sess:run_cmd(select, [ddView, [{#ddView{id = ViewId, _ = '_'}, [], ['$_']}]]) of
        {[View], true} ->
            View;
        Result ->
            ?Error("View with the id ~p was not found, select result: ~n~p", [ViewId, Result]),
            View = undefined
    end,
    {reply, View, State};

handle_call({get_view, undefined, Name, Adapter, Owner}, From, #state{sess=Sess} = State) ->
    handle_call({get_view, Sess, Name, Adapter, Owner}, From, State);
handle_call({get_view, Sess, Name, Adapter, Owner}, _From, State) ->
    ?Debug("get_view ~p", [Name]),
    {Views, true} = Sess:run_cmd(select, [ddView,[{#ddView{name=Name, owner=Owner, _='_'}, [], ['$_']}]]),
    %% TODO: Add support to multiples adapters.
    ListResult = [{V, Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=V#ddView.cmd, adapters=[Adapter], _='_'}, [], ['$_']}]])} || V <- Views],
    Result = [V || {V, {C, true}} <- ListResult, C =/= []],
    if
        length(Result) > 0 ->
            %% TODO: How can we discriminate to ignore the correct one?
            [View | _Ignored] = Result;
        true ->
            View = undefined
    end,
    {reply, View, State};

handle_call({get_command, Sess, IdOrName}, _From, State) ->
    Cmd = internal_get_command(Sess, IdOrName),
    {reply, Cmd, State};

handle_call({save_dashboard, Sess, Owner, -1, Name, Views}, From, State) ->
    NewId = erlang:phash2(make_ref()),
    case Sess:run_cmd(select, [ddDash, [{#ddDash{id=NewId, name='$1', _='_'}, [], ['$1']}]]) of
        {[DashName], true} ->
            ?Debug("Save dashboard colision saving the dashboard ~p with id ~p", [DashName, NewId]),
            handle_call({save_dashboard, Sess, Owner, -1, Name, Views}, From, State);
        _ ->
            handle_call({save_dashboard, Sess, Owner, NewId, Name, Views}, From, State)
    end;
handle_call({save_dashboard, Sess, Owner, DashId, Name, Views}, _From, State) ->
    NewDash = #ddDash{id = DashId, name = Name, owner = Owner, views = Views},
    Sess:run_cmd(write, [ddDash, NewDash]),
    ?Debug("dashboard saved ~p", [NewDash]),
    {reply, DashId, State};

handle_call({get_dashboards, Sess, Owner}, _From, State) ->
    {Dashboards, true} = Sess:run_cmd(select, [ddDash, [{#ddDash{owner = Owner, _='_'}, [], ['$_']}]]),
    {reply, Dashboards, State};

handle_call({get_connects, Sess, User}, _From, State) ->
    {Cons, true} = Sess:run_cmd(select, [ddConn, [{'$1', [], ['$_']}]]),
    HasAll = (Sess:run_cmd(have_permission, [[manage_system, manage_connections]]) == true),
    NewCons =
        if HasAll ->
            [if
                 is_integer(C#ddConn.owner) ->
                     C#ddConn{owner = Sess:run_cmd(admin_exec, [imem_account, get_name, [C#ddConn.owner]])};
                 true -> C
             end
            || C <- Cons];
        true ->
            lists:foldl(fun(C,Acc) ->        
                HavePerm = Sess:run_cmd(have_permission, [{C#ddConn.id, use}]),
                if (HavePerm == true)   ->
                        [if
                             is_integer(C#ddConn.owner) ->
                                 C#ddConn{owner = Sess:run_cmd(admin_exec, [imem_account, get_name, [C#ddConn.owner]])};
                             true -> C
                         end
                        | Acc];
                   true -> Acc
                end
            end,
            [],
            Cons)
    end,
    ?Debug("get_connects for ~p user -- ~p", [User, NewCons]),
    {reply, NewCons, State};

handle_call({del_conn, Sess, ConId}, _From, State) ->
    HasAll = (Sess:run_cmd(have_permission, [[manage_system, manage_connections]]) == true),
    if HasAll ->
        ok = Sess:run_cmd(delete, [ddConn, ConId]),
        ?Info("del_conn connection ~p deleted", [ConId]),
        {reply, ok, State};
    true ->
        case Sess:run_cmd(have_permission, [{ConId, use}]) of
        true ->
            ok = Sess:run_cmd(delete, [ddConn, ConId]),
            ?Info("del_conn connection ~p deleted", [ConId]),
            {reply, ok, State};
        _ ->
             ?Error("del_conn no permission to delete connection ~p", [ConId]),
             {reply, no_permission, State}
        end
    end;

handle_call({get_adapters, Sess}, _From, State) ->
    ?Debug("get_adapters"),
    {Adapters, true} = Sess:run_cmd(select, [ddAdapter, [{'$1', [], ['$_']}]]),
    {reply, Adapters, State};

handle_call({login, User, Password}, _From, #state{schema=SchemaName} = State) ->
    ?Debug("login for user ~p", [User]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, erlang:md5(Password)}) of
        {error, Error} ->
            ?Error("login exception ~n~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, Sess} ->
            UserId = Sess:run_cmd(admin_exec, [imem_account, get_id_by_name, [User]]),
            ?Debug("login accepted user ~p with id = ~p", [User, UserId]),
            {reply, {true, Sess, UserId}, State}
    end;

handle_call({change_password, User, Password, NewPassword}, _From, #state{schema=SchemaName} = State) ->
    ?Debug("changing password for user ~p", [User]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, erlang:md5(Password), erlang:md5(NewPassword)}) of
        {error, Error} ->
            ?Error("change password exception ~n~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, Sess} ->
            UserId = Sess:run_cmd(admin_exec, [imem_account, get_id_by_name, [User]]),
            ?Info("login with new password user ~p with id = ~p", [User, UserId]),
            {reply, {true, Sess, UserId}, State}
    end;

handle_call(Req,From,State) ->
    ?Info("unknown call req ~p from ~p~n", [Req, From]),
    {reply, ok, State}.

handle_cast({add_connect, undefined, Con}, #state{sess=Sess} = State) ->
    handle_cast({add_connect, Sess, Con}, State);
handle_cast({add_connect, Sess, #ddConn{schm = undefined} = Con}, #state{schema = SchemaName} = State) ->
    handle_cast({add_connect, Sess, Con#ddConn{schm = SchemaName}}, State);
handle_cast({add_connect, Sess, #ddConn{id = undefined, owner = Owner} = Con}, State) ->
    case Sess:run_cmd(select, [ddConn, [{#ddConn{name='$1', owner='$2', id='$3', _='_'}
                                         , [{'=:=','$1',Con#ddConn.name},{'=:=','$2',Owner}]
                                         , ['$3']}]]) of
        {[Id|_], true} ->
            ?Info("add_connect replacing id ~p", [Id]),
            NewCon = Con#ddConn{id=Id};
        _ ->
            NewId = erlang:phash2(make_ref()),
            ?Info("add_connect adding new id ~p", [NewId]),
            NewCon = Con#ddConn{id=NewId}
    end,
    Sess:run_cmd(write, [ddConn, NewCon]),
    ?Debug("add_connect written ~p", [NewCon]),
    {noreply, State};
handle_cast({add_connect, Sess, #ddConn{id = OldId, owner = Owner} = Con}, State) ->
    case Sess:run_cmd(select, [ddConn, [{#ddConn{id='$1', _='_'}
                                         , [{'=:=', '$1', OldId}]
                                         , ['$_']}]]) of
        {[#ddConn{owner = Owner}], true} ->
            %% The same owner, save old view as it is.
            Sess:run_cmd(write, [ddConn, Con]);
        {[#ddConn{owner = OldOwner} = OldCon], true} ->
            %% It is not the same owner, save a copy if there is some difference.
            %% TODO: Validate authorization before saving.
            case compare_connections(OldCon, Con#ddConn{owner = OldOwner}) of
                true ->
                    %% If the connection is not changed then do not save a copy.
                    ok;
                false ->
                    handle_cast({add_connect, Sess, Con#ddConn{id = undefined}}, State)
            end;
        {[], true} ->
            %% Connection with id not found, adding a new one.
            Sess:run_cmd(insert, [ddConn, Con]);
        Result ->
            ?Error("Error getting connection with id ~p, Result:~n~p", [OldId, Result])
    end,
    {noreply, State};

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
        {ok, { [{{select, QOpts},_}|_ ], _Tokens}} ->
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

-spec compare_connections(#ddConn{}, #ddConn{}) -> boolean().
compare_connections(Connection, Connection) -> true;
compare_connections(#ddConn{access = []}, _) -> false;
compare_connections(_, #ddConn{access = []}) -> false;
compare_connections(#ddConn{access = A1} = Con1, #ddConn{access = A2} = Con2) ->
    lists:sort(A1) =:= lists:sort(A2)
        andalso compare_connections(Con1#ddConn{access = []}, Con2#ddConn{access = []}).

-spec internal_get_command({atom(), pid()}, ddEntityId() | binary()) -> #ddCmd{}.
internal_get_command(Sess, IdOrName) ->
  ?Debug("get_command for id ~p", [IdOrName]),
  {Cmds, true} =
      case IdOrName of
          Id when is_integer(Id) -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=Id, _='_'}, [], ['$_']}]]);
          Name -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{name=Name, _='_'}, [], ['$_']}]])
      end,
  Cmd = if length(Cmds) > 0 -> lists:nth(1, Cmds); true -> #ddCmd{opts=[]} end,
  Cmd.

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
