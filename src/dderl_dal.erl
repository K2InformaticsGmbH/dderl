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
        ,add_connect/1
        ,get_connects/1
        ,get_commands/2
        ,get_command/1
        ]).

login(User, Password)                   -> gen_server:call(?MODULE, {login, User, Password}).

add_adapter(Id, FullName)               -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).
add_command(Adapter, Name, Cmd, Opts)   -> gen_server:cast(?MODULE, {add_command, Adapter, Name, Cmd, Opts}).
add_connect(#ddConn{} = Connection)     -> gen_server:cast(?MODULE, {add_connect, Connection}).

get_adapters()                  -> gen_server:call(?MODULE, {get_adapters}).
get_connects(User)              -> gen_server:call(?MODULE, {get_connects, User}).
get_commands(User, Adapter)     -> gen_server:call(?MODULE, {get_commands, User, Adapter}).
get_command(Id)                 -> gen_server:call(?MODULE, {get_command, Id}).

-record(state, { schema
               , sess
    }).

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
    Cred = {<<>>, <<>>},
    Sess = erlimem_session:open(local, {SchemaName}, Cred),
    Sess:run_cmd(create_table, [ddAdapter, record_info(fields, ddAdapter), []]),
    Sess:run_cmd(create_table, [ddInterface, record_info(fields, ddInterface), []]),
    Sess:run_cmd(create_table, [ddConn, record_info(fields, ddConn), []]),
    Sess:run_cmd(create_table, [ddCmd, record_info(fields, ddCmd), []]),
    Sess:run_cmd(create_table, [ddView, record_info(fields, ddView), []]),
    Sess:run_cmd(create_table, [ddDash, record_info(fields, ddDash), []]),
    Sess:run_cmd(insert, [ddInterface, #ddInterface{id=ddjson,fullName="DDerl"}]),
    Adapters = [list_to_existing_atom(lists:nth(1, re:split(Fl, "[.]", [{return,list}]))) || Fl <- filelib:wildcard("*_adapter.beam", "ebin")],
    lager:info("~p initializing ~p", [?MODULE, Adapters]),
    [gen_server:cast(?MODULE, {init_adapter, Adapter}) || Adapter <- Adapters],
    {ok, #state{sess=Sess, schema=SchemaName}}.

handle_call({get_command, Id}, _From, #state{sess=Sess} = State) ->
    lager:debug("~p get_command for id ~p", [?MODULE, Id]),
    {Cmds, true} = Sess:run_cmd(select, [ddCmd
                                           , [{{ddCmd,'$1','_','_','_','_','_','_'}
                                           , [{'=:=','$1',Id}]
                                          , ['$_']}]]),
    Cmd = if length(Cmds) > 0 -> lists:nth(1, Cmds); true -> #ddCmd{opts=[]} end,
    {reply, Cmd, State};
handle_call({get_commands, User, Adapter}, _From, #state{sess=Sess} = State) ->
    lager:debug("~p get_commands user ~p adapter ~p", [?MODULE, User, Adapter]),
    {Cmds, true} = Sess:run_cmd(select, [ddCmd
                                        , [{{ddCmd,'$1','$2','$3','$4','_','$5','_'}
                                        , [{'or', {'=:=', '$3', system}, {'=:=','$3',User}}]
                                          %, [['$1','$2', '$5']]}]]),
                                        , ['$_']}]]),
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
    case erlimem_session:open(rpc, {node(), SchemaName}, {User, BinPswd}) of
        {error, Error} ->
            lager:error("login exception ~p~n", [Error]),
            {reply, {error, Error}, State};
        Sess ->
            lager:debug("~p login accepted user ~p", [?MODULE, User]),
            {reply, true, State#state{sess=Sess}}
    end;
handle_call(Req,From,State) ->
    lager:info("unknown call req ~p from ~p~n", [Req, From]),
    {reply, ok, State}.

handle_cast({add_connect, #ddConn{} = Con}, #state{sess=Sess, schema=SchemaName} = State) ->
    NewCon0 = Con#ddConn{owner = <<"admin">>},
    NewCon1 = case NewCon0#ddConn.schema of
        undefined -> NewCon0#ddConn{schema = SchemaName};
        _ -> NewCon0
    end,
    lager:debug("~p add_connect ~p", [?MODULE, NewCon1]),
    NewCon = case Sess:run_cmd(select, [ddConn
                                       , [{{ddConn,'_','$1','_','_','_','_'}
                                         , [{'=:=','$1',Con#ddConn.name}]
                                         , ['$_']}]]) of
        {[#ddConn{id=Id}|_], true} ->
            lager:debug("~p add_connect replacing id ~p", [?MODULE, Id]),
            NewCon1#ddConn{id=Id};
        _ -> NewCon1
    end,
    Sess:run_cmd(insert, [ddConn, NewCon]),
    lager:debug("~p add_connect inserted ~p", [?MODULE, NewCon]),
    {noreply, State};
handle_cast({add_command, Adapter, Name, Cmd, Opts}, #state{sess=Sess} = State) ->
    lager:debug("~p add_command ~p", [?MODULE, {Adapter, Name, Cmd, Opts}]),
    Id = case Sess:run_cmd(select, [ddCmd
                                       , [{{ddCmd,'$1','$2','$3','$4','_','$5','_'}
                                         , [{'and', {'=:=','$2',Name}, {'=:=', '$4', [Adapter]}}]
                                         , ['$_']}]]) of
        {[#ddCmd{id=Id0}|_], true} -> Id0;
        _ -> erlang:phash2(make_ref())
    end,
    lager:debug("~p add_command inserting id ~p", [?MODULE, Id]),
    NewCmd = #ddCmd { id     = Id
                 , name      = Name
                 , owner     = system
                 , adapters  = [Adapter]
                 , command   = Cmd
                 , opts      = Opts},
    Sess:run_cmd(insert, [ddCmd, NewCmd]),
    lager:debug("~p add_connect inserted ~p", [?MODULE, NewCmd]),
    {noreply, State};
handle_cast({add_adapter, Id, FullName}, #state{sess=Sess} = State) ->
    Adp = #ddAdapter{id=Id,fullName=FullName},
    Sess:run_cmd(insert, [ddAdapter, Adp]),
    lager:debug("~p add_adapter inserted ~p", [?MODULE, Adp]),
    {noreply, State};
handle_cast({init_adapter, Adapter}, State) ->
    Adapter:init(),
    lager:debug("~p init_adapter ~p", [?MODULE, Adapter]),
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
