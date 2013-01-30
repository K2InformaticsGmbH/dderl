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
        ,add_adapter/2
        ,add_command/4
        ,add_view/3
        ,add_connect/1
        ,get_connects/1
        ,get_commands/2
        ,get_command/1
        ,get_view/1
        ,get_session/0
        ]).

-record(state, { schema
               , sess
               , owner = system
    }).

login(User, Password)                   -> gen_server:call(?MODULE, {login, User, Password}).

add_adapter(Id, FullName)               -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).
add_connect(#ddConn{} = Connection)     -> gen_server:cast(?MODULE, {add_connect, Connection}).

add_command(Adapter, Name, Cmd, Opts)   -> gen_server:call(?MODULE, {add_command, Adapter, Name, Cmd, Opts}).
add_view(Name, CmdId, ViewsState)       -> gen_server:call(?MODULE, {add_view, Name, CmdId, ViewsState}).

get_adapters()                  -> gen_server:call(?MODULE, {get_adapters}).
get_connects(User)              -> gen_server:call(?MODULE, {get_connects, User}).
get_commands(User, Adapter)     -> gen_server:call(?MODULE, {get_commands, User, Adapter}).
get_command(IdOrName)           -> gen_server:call(?MODULE, {get_command, IdOrName}).
get_view(Name)                  -> gen_server:call(?MODULE, {get_view, Name}).
get_session()                   -> gen_server:call(?MODULE, {get_session}).
            
hexstr_to_bin(S)        -> hexstr_to_bin(S, []).
hexstr_to_bin([], Acc)  -> list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).

start_link(SchemaName) ->
    lager:debug("~p starting...~n", [?MODULE]),
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [SchemaName], []),
    lager:debug("~p started!~n~p", [?MODULE, Result]),
    Result.

init([SchemaName]) ->
    erlimem:start(),
    Cred = {<<"admin">>, <<"change_on_install">>},
    case erlimem:open(local_sec, {SchemaName}, Cred) of
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
        lager:info("~p tables ~p created", [?MODULE, [ddAdapter, ddInterface, ddConn, ddCmd, ddView, ddDash]]),
        Sess:run_cmd(insert, [ddInterface, #ddInterface{id=ddjson,fullName="DDerl"}]),
        Adapters = [list_to_existing_atom(lists:nth(1, re:split(Fl, "[.]", [{return,list}]))) || Fl <- filelib:wildcard("*_adapter.beam", "ebin")],
        lager:info("~p initializing ~p", [?MODULE, Adapters]),
        [gen_server:cast(?MODULE, {init_adapter, Adapter}) || Adapter <- Adapters],
        {ok, #state{sess=Sess, schema=SchemaName}};
    {error, Reason} ->
        {stop, Reason}
    end.

build_tables_on_boot(_, []) -> ok;
build_tables_on_boot(Sess, [{N, Cols, Types, Default}|R]) ->
    lager:info("~p creating table ~p", [?MODULE, [N]]),
    Sess:run_cmd(create_table, [N, {Cols, Types, Default}, []]),
    build_tables_on_boot(Sess, R).

handle_call({add_command, Adapter, Name, Cmd, Opts}, _From, #state{sess=Sess, owner=Owner} = State) ->
    Id = case Sess:run_cmd(select, [ddCmd, [{#ddCmd{name=Name, id='$1', adapters='$2', owner=Owner, _='_'}
                                           , [{'=:=', '$2', [Adapter]}]
                                           , ['$1']}]]) of
        {[Id0|_], true} ->
            lager:debug("~p add_command ~p replacing id ~p", [?MODULE, Name, Id0]),
            Id0;
        _ ->
            Id1 = erlang:phash2(make_ref()),
            lager:debug("~p add_command ~p new id ~p", [?MODULE, Name, Id1]),
            Id1
    end,
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = Owner
                 , adapters  = [Adapter]
                 , command   = Cmd
                 , opts      = Opts},
    Sess:run_cmd(insert, [ddCmd, NewCmd]),
    lager:debug("~p add_command inserted ~p", [?MODULE, NewCmd]),
    {reply, Id, State};

handle_call({add_view, Name, CmdId, ViewsState}, _From, #state{sess=Sess, owner=Owner} = State) ->
    Id = case Sess:run_cmd(select, [ddView, [{#ddView{name=Name, id='$1', owner=Owner, _='_'}
                                            , []
                                            , ['$1']}]]) of
        {[Id0|_], true} ->
            lager:debug("~p add_view ~p replacing id ~p ~p~n", [?MODULE, Name, Id0, Owner]),
            Id0;
        _ ->
            Id1 = erlang:phash2(make_ref()),
            lager:debug("~p add_view ~p new id ~p", [?MODULE, Name, Id1]),
            Id1
    end,
    NewView = #ddView { id      = Id
                     , name     = Name
                     , owner    = Owner
                     , cmd      = CmdId
                     , state    = ViewsState},
    Sess:run_cmd(insert, [ddView, NewView]),
    lager:debug("~p add_view inserted ~p", [?MODULE, NewView]),
    {reply, Id, State};
handle_call({get_view, Name}, _From, #state{sess=Sess} = State) ->
    lager:info("~p get_view ~p", [?MODULE, Name]),
    {Views, true} = Sess:run_cmd(select, [ddView, [{#ddView{name=Name, _='_'}, [], ['$_']}]]),
    lager:info("~p view ~p", [?MODULE, Views]),
    {reply, Views, State};
handle_call({get_session}, _From, #state{sess=Sess} = State) ->
    lager:info("~p get_session ~p", [?MODULE, Sess]),
    {reply, Sess, State};

handle_call({get_command, IdOrName}, _From, #state{sess=Sess} = State) ->
    lager:debug("~p get_command for id ~p", [?MODULE, IdOrName]),
    {Cmds, true} = case IdOrName of
        Id when is_integer(Id) -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{id=Id, _='_'}, [], ['$_']}]]);
        Name -> Sess:run_cmd(select, [ddCmd, [{#ddCmd{name=Name, _='_'}, [], ['$_']}]])
    end,
    Cmd = if length(Cmds) > 0 -> lists:nth(1, Cmds); true -> #ddCmd{opts=[]} end,
    {reply, Cmd, State};
handle_call({get_commands, User, Adapter}, _From, #state{sess=Sess} = State) ->
    lager:debug("~p get_commands user ~p adapter ~p", [?MODULE, User, Adapter]),
    {Cmds, true} = Sess:run_cmd(select, [ddCmd, [{#ddCmd{owner='$1', _='_'}
                                                , [{'or', {'=:=', '$1', system}
                                                , {'=:=','$1',User}}], ['$_']}]]),
    NewCmds = [C || C <- Cmds, lists:any(fun(E) -> E =:= Adapter end, C#ddCmd.adapters)],
    lager:debug("~p get_commands user ~p adapter ~p cmds ~p", [?MODULE, User, Adapter, NewCmds]),
    {reply, NewCmds, State};

handle_call({get_connects, User}, _From, #state{sess=Sess} = State) ->
    {Cons, true} = Sess:run_cmd(select, [ddConn, [{'$1', [], ['$_']}]]),
    HasAll = (Sess:run_cmd(have_permission, [[manage_system, manage_connections]]) == true),
    NewCons =
        if HasAll -> Cons;
        true ->
            lists:foldl(fun(C,Acc) ->        
                HavePerm = Sess:run_cmd(have_permission, [{C#ddConn.id, use}]),
                if (HavePerm == true)   -> [C|Acc];
                   true                 -> Acc
                end
            end,
            [],
            Cons)
    end,
    lager:debug("~p get_connects for ~p user -- ~p", [?MODULE, User, NewCons]),
    {reply, NewCons, State};

handle_call({get_adapters}, _From, #state{sess=Sess} = State) ->
    lager:debug("~p get_adapters", [?MODULE]),
    {Adapters, true} = Sess:run_cmd(select, [ddAdapter, [{'$1', [], ['$_']}]]),
    {reply, Adapters, State};

handle_call({login, User, Password}, _From, #state{schema=SchemaName} = State) ->
    BinPswd = hexstr_to_bin(Password),
    lager:debug("~p login for user ~p pass ~p", [?MODULE, User, BinPswd]),
    case erlimem:open(rpc, {node(), SchemaName}, {User, BinPswd}) of
        {error, Error} ->
            lager:error("login exception ~p~n", [Error]),
            {reply, {error, Error}, State};
        {ok, Sess} ->
            UserId = element(2, Sess:run_cmd(admin_exec, [imem_account, get_by_name, [User]])),
            lager:info("~p login accepted user ~p with id = ~p", [?MODULE, User, UserId]),
            {reply, true, State#state{sess=Sess, owner=UserId}}
    end;

handle_call(Req,From,State) ->
    lager:info("unknown call req ~p from ~p~n", [Req, From]),
    {reply, ok, State}.

handle_cast({add_connect, #ddConn{} = Con}, #state{sess=Sess, schema=SchemaName, owner=UserId} = State) ->
    NewCon0 = Con#ddConn{owner = UserId},
    NewCon1 = case NewCon0#ddConn.schema of
        undefined -> NewCon0#ddConn{schema = SchemaName};
        _ -> NewCon0
    end,
    NewCon = case Sess:run_cmd(select, [ddConn, [{#ddConn{name='$1', id='$2', _='_'}
                                                , [{'=:=','$1',Con#ddConn.name}]
                                                , ['$2']}]]) of
        {[Id|_], true} ->
            lager:debug("~p add_connect replacing id ~p", [?MODULE, Id]),
            NewCon1#ddConn{id=Id};
        _ ->
            lager:debug("~p add_connect adding new ~p", [?MODULE, NewCon1#ddConn.id]),
            NewCon1
    end,
    Sess:run_cmd(insert, [ddConn, NewCon]),
    lager:debug("~p add_connect inserted ~p", [?MODULE, NewCon]),
    {noreply, State};
handle_cast({add_adapter, Id, FullName}, #state{sess=Sess} = State) ->
    Adp = #ddAdapter{id=Id,fullName=FullName},
    Sess:run_cmd(insert, [ddAdapter, Adp]),
    lager:debug("~p add_adapter inserted ~p", [?MODULE, Adp]),
    {noreply, State};
handle_cast({init_adapter, Adapter}, State) ->
    spawn(fun() ->
        Adapter:init(),
        lager:debug("~p init_adapter ~p", [?MODULE, Adapter])
    end),
    {noreply, State};
handle_cast(Req,State) ->
    lager:debug("~p unknown cast ~p", [?MODULE, Req]),
    {noreply, State}.

handle_info(Req,State) ->
    lager:debug("~p unknown info ~p", [?MODULE, Req]),
    {noreply, State}.
terminate(Reason, _State)              ->
    lager:debug("~p terminating, reason ~p", [?MODULE, Reason]),
    ok.
code_change(_OldVsn, State, _Extra)     -> {ok, State}.
format_status(_Opt, [_PDict, _State])   -> ok.
