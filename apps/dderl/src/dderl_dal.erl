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

-export([get_adapters/0
        ,login/2
        ,change_password/3
        ,add_adapter/2
        ,add_command/5
        ,add_view/3
        ,add_connect/1
        ,get_connects/1
        ,del_conn/1
        ,get_commands/2
        ,get_command/1
        ,get_view/1
        ,get_view/2
        ,get_session/0
        ,is_local_query/1
        ]).

-record(state, { schema
               , sess
               , owner = system
    }).

login(User, Password)                   -> gen_server:call(?MODULE, {login, User, Password}).
change_password(User, Password, NewPassword) -> gen_server:call(?MODULE, {change_password, User, Password, NewPassword}).

add_adapter(Id, FullName)               -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).
add_connect(#ddConn{} = Connection)     -> gen_server:cast(?MODULE, {add_connect, Connection}).

add_command(Adapter, Name, Cmd, Conn, Opts) -> gen_server:call(?MODULE, {add_command, Adapter, Name, Cmd, Conn, Opts}).
add_view(Name, CmdId, ViewsState)           -> gen_server:call(?MODULE, {add_view, Name, CmdId, ViewsState}).

get_adapters()                  -> gen_server:call(?MODULE, {get_adapters}).
get_connects(User)              -> gen_server:call(?MODULE, {get_connects, User}).
del_conn(ConId)                 -> gen_server:call(?MODULE, {del_conn, ConId}).
get_commands(User, Adapter)     -> gen_server:call(?MODULE, {get_commands, User, Adapter}).
get_command(IdOrName)           -> gen_server:call(?MODULE, {get_command, IdOrName}).
get_view(Name)                  -> gen_server:call(?MODULE, {get_view, Name}).
get_view(Name, Owner)           -> gen_server:call(?MODULE, {get_view, Name, Owner}).
get_session()                   -> gen_server:call(?MODULE, {get_session}).
is_local_query(Qry)             -> gen_server:call(?MODULE, {is_local_query, Qry}).
            
hexstr_to_bin(S)        -> hexstr_to_bin(S, []).
hexstr_to_bin([], Acc)  -> list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).

start_link(SchemaName) ->
    ?Debug("starting...~n"),
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [SchemaName], []),
    ?Debug("started!~n~p", [Result]),
    Result.

init([SchemaName]) ->
    erlimem:start(),
    Cred = {<<>>, <<>>},
    case erlimem:open(local, {SchemaName}, Cred) of
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
        Sess:run_cmd(insert, [ddInterface, #ddInterface{id=ddjson,fullName="DDerl"}]),
        DDerlEbinPath = case lists:flatten([P || P <- code:get_path(), re:run(P, ".*/dderl/.*/dderl*") =/= nomatch]) of
            "" ->
                {ok, Cwd} = file:get_cwd(),
                filename:join([Cwd, "ebin"]);
            Other -> Other
        end,
        case filelib:is_dir(DDerlEbinPath) of
        true ->
            Adapters = [list_to_existing_atom(lists:nth(1, re:split(Fl, "[.]", [{return,list}]))) || Fl <- filelib:wildcard("*_adapter.beam", DDerlEbinPath)],
            ?Info("initializing ~p", [Adapters]),
            [gen_server:cast(?MODULE, {init_adapter, Adapter}) || Adapter <- Adapters],
            {ok, #state{sess=Sess, schema=SchemaName}};
        _ ->
            {stop, {"no adapters found or directoty doesn't exists", DDerlEbinPath}}
        end;
    {error, Reason} ->
        {stop, Reason}
    end.

build_tables_on_boot(_, []) -> ok;
build_tables_on_boot(Sess, [{N, Cols, Types, Default}|R]) ->
    ?Info("creating table ~p", [N]),
    Sess:run_cmd(create_check_table, [N, {Cols, Types, Default}, []]),
    build_tables_on_boot(Sess, R).

handle_call({is_local_query, Qry}, _From, State) ->
    SysTabs = [erlang:atom_to_binary(Dt, utf8) || Dt <- [ddAdapter,ddInterface,ddConn,ddCmd,ddView,ddDash]],
    case sql_parse:parsetree(Qry) of
        {ok, {[{select, QOpts}|_], _Tokens}} ->
            case lists:keyfind(from, 1, QOpts) of
                {from, Tables} ->
                    {reply, lists:foldl(fun(T,_) ->
                             Tab = case T of
                                 {_,T1,_} when is_binary(T1) -> T1;
                                 T when is_binary(T) -> T
                             end,
                             HasSysTab = lists:member(Tab, SysTabs),
                             if HasSysTab -> true; true -> false end
                         end,
                         true,
                         Tables)
                    , State};
                _ -> {reply, false, State}
            end;
        {lex_error, Error} ->
            ?Error({"SQL lexer error", Error}),
            {reply, false, State};
        {parse_error, Error} ->
            ?Error({"SQL parser error", Error}),
            {reply, false, State};
        _ ->
            ?Info("non select query ~p", [Qry]),
            {reply, false, State}
    end;

handle_call({add_command, Adapter, Name, Cmd, Conn, Opts}, _From, #state{sess=Sess, owner=Owner} = State) ->
    SysTabs = [erlang:atom_to_binary(Dt, utf8) || Dt <- [ddAdapter,ddInterface,ddConn,ddCmd,ddView,ddDash]],
    NewConn =
        if Conn =:= undefined ->
            case  sql_parse:parsetree(Cmd) of
                {ok, {[{select, QOpts}|_], _Tokens}} ->
                    case lists:keyfind(from, 1, QOpts) of
                        {from, Tables} ->
                            ?Info("Query tables ~p", [Tables]),
                            case 
                                lists:foldl(fun(T,A) ->
                                                Tab = case T of
                                                    {_,T1,_} when is_binary(T1) -> T1;
                                                    T when is_binary(T) -> T
                                                end,
                                                HasSysTab = lists:member(Tab, SysTabs),
                                                if HasSysTab -> found; true -> A end
                                            end,
                                            [],
                                            Tables) of
                            [] ->
                                ?Info("No system table in query ~p tables ~p", [Cmd, Tables]),
                                remote;
                            _ -> local
                            end;
                        false ->
                            ?Info("no tables in ~p opts ~p", [Cmd, Opts]),
                            remote
                    end;
                {lex_error, Error} ->
                    ?Error({"SQL lexer error", Error}),
                    {reply, false, State};
                {parse_error, Error} ->
                    ?Error({"SQL parser error", Error}),
                    {reply, false, State};
                _ ->
                    ?Info("non select query ~p", [Cmd]),
                    remote
            end;
        true -> Conn
    end,
    Id = case Sess:run_cmd(select, [ddCmd, [{#ddCmd{name=Name, id='$1', adapters='$2', owner=Owner, _='_'}
                                           , [{'=:=', '$2', [Adapter]}]
                                           , ['$1']}]]) of
        {[Id0|_], true} ->
            ?Debug("add_command ~p replacing id ~p", [Name, Id0]),
            Id0;
        _ ->
            Id1 = erlang:phash2(make_ref()),
            ?Debug("add_command ~p new id ~p", [Name, Id1]),
            Id1
    end,
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = Owner
                 , adapters  = [Adapter]
                 , command   = Cmd
                 , conns     = NewConn
                 , opts      = Opts},
    Sess:run_cmd(insert, [ddCmd, NewCmd]),
    ?Debug("add_command inserted ~p", [NewCmd]),
    {reply, Id, State};

handle_call({add_view, Name, CmdId, ViewsState}, _From, #state{sess=Sess, owner=Owner} = State) ->
    Id = case Sess:run_cmd(select, [ddView, [{#ddView{name=Name, id='$1', owner=Owner, _='_'}
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
    Sess:run_cmd(insert, [ddView, NewView]),
    ?Debug("add_view inserted ~p", [NewView]),
    {reply, Id, State};
handle_call({get_view, Name, Owner}, _From, #state{sess=Sess} = State) ->
    ?Debug("get_view ~p", [Name]),
    {[View], true} = Sess:run_cmd(select, [ddView, [{#ddView{name=Name, owner=Owner, _='_'}, [], ['$_']}]]),
    ?Debug("view ~p", [View]),
    {reply, View, State};
handle_call({get_view, Name}, _From, #state{sess=Sess} = State) ->
    ?Debug("get_view ~p", [Name]),
    {Views, true} = Sess:run_cmd(select, [ddView, [{#ddView{name=Name, _='_'}, [], ['$_']}]]),
    ?Debug("view ~p", [Views]),
    {reply, Views, State};
handle_call({get_session}, _From, #state{sess=Sess} = State) ->
    ?Info("get_session ~p", [Sess]),
    {reply, Sess, State};

handle_call({get_command, IdOrName}, _From, #state{sess=Sess} = State) ->
    ?Debug("get_command for id ~p", [IdOrName]),
    {Cmds, true} = case IdOrName of
        Id when is_integer(Id) -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=Id, _='_'}, [], ['$_']}]]);
        Name -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{name=Name, _='_'}, [], ['$_']}]])
    end,
    Cmd = if length(Cmds) > 0 -> lists:nth(1, Cmds); true -> #ddCmd{opts=[]} end,
    {reply, Cmd, State};
handle_call({get_commands, User, Adapter}, _From, #state{sess=Sess} = State) ->
    ?Debug("get_commands user ~p adapter ~p", [User, Adapter]),
    {Cmds, true} = Sess:run_cmd(select, [ddCmd, [{#ddCmd{owner='$1', _='_'}
                                                , [{'or', {'=:=', '$1', system}
                                                , {'=:=','$1',User}}], ['$_']}]]),
    NewCmds = [C || C <- Cmds, lists:any(fun(E) -> E =:= Adapter end, C#ddCmd.adapters)],
    ?Debug("get_commands user ~p adapter ~p cmds ~p", [User, Adapter, NewCmds]),
    {reply, NewCmds, State};

handle_call({get_connects, User}, _From, #state{sess=Sess} = State) ->
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

handle_call({del_conn, ConId}, _From, #state{sess=Sess} = State) ->
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

handle_call({get_adapters}, _From, #state{sess=Sess} = State) ->
    ?Debug("get_adapters"),
    {Adapters, true} = Sess:run_cmd(select, [ddAdapter, [{'$1', [], ['$_']}]]),
    {reply, Adapters, State};

handle_call({login, User, Password}, _From, #state{schema=SchemaName} = State) ->
    BinPswd = hexstr_to_bin(Password),
    ?Debug("login for user ~p", [User]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, BinPswd}) of
        {error, Error} ->
            ?Error("login exception ~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, Sess} ->
            UserId = Sess:run_cmd(admin_exec, [imem_account, get_id_by_name, [User]]),
            ?Info("login accepted user ~p with id = ~p", [User, UserId]),
            {reply, true, State#state{sess=Sess, owner=UserId}}
    end;

handle_call({change_password, User, Password, NewPassword}, _From, #state{schema=SchemaName} = State) ->
    BinPswd = hexstr_to_bin(Password),
    BinNewPswd = hexstr_to_bin(NewPassword),
    ?Debug("changing password for user ~p", [User]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, BinPswd, BinNewPswd}) of
        {error, Error} ->
            ?Error("change password exception ~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, Sess} ->
            UserId = Sess:run_cmd(admin_exec, [imem_account, get_id_by_name, [User]]),
            ?Info("login with new password user ~p with id = ~p", [User, UserId]),
            {reply, true, State#state{sess=Sess, owner=UserId}}
    end;

handle_call(Req,From,State) ->
    ?Info("unknown call req ~p from ~p~n", [Req, From]),
    {reply, ok, State}.

handle_cast({add_connect, #ddConn{} = Con}, #state{sess=Sess, schema=SchemaName, owner=UserId} = State) ->
    NewCon0 = Con#ddConn{owner = UserId},
    NewCon1 = case NewCon0#ddConn.schema of
        undefined -> NewCon0#ddConn{schema = SchemaName};
        _ -> NewCon0
    end,
    NewCon = case Sess:run_cmd(select, [ddConn, [{#ddConn{name='$1', owner='$2', id='$3', _='_'}
                                                , [{'=:=','$1',Con#ddConn.name},{'=:=','$2',UserId}]
                                                , ['$3']}]]) of
        {[Id|_], true} ->
            ?Info("add_connect replacing id ~p", [Id]),
            NewCon1#ddConn{id=Id};
        _ ->
            ?Info("add_connect adding new ~p", [NewCon1#ddConn.id]),
            NewCon1
    end,
    Sess:run_cmd(insert, [ddConn, NewCon]),
    ?Debug("add_connect inserted ~p", [NewCon]),
    {noreply, State};
handle_cast({add_adapter, Id, FullName}, #state{sess=Sess} = State) ->
    Adp = #ddAdapter{id=Id,fullName=FullName},
    Sess:run_cmd(insert, [ddAdapter, Adp]),
    ?Debug("add_adapter inserted ~p", [Adp]),
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
terminate(Reason, _State)              ->
    ?Debug("terminating, reason ~p", [Reason]),
    ok.
code_change(_OldVsn, State, _Extra)     -> {ok, State}.
format_status(_Opt, [_PDict, _State])   -> ok.
