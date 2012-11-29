-module(dderl_dal).
-include("dderl.hrl").

-compile(export_all).

-behavior(gen_server).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,format_status/2
        ]).

-define(USER, <<"admin">>).
-define(PASSWORD, erlang:md5(<<"change_on_install">>)).

verify_password(User, Password) -> gen_server:call(?MODULE, {verify_password, User, Password}).

add_adapter(Id, FullName)               -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).
add_command(Adapter, Name, Cmd, Opts)   -> gen_server:cast(?MODULE, {add_command, Adapter, Name, Cmd, Opts}).
add_connect(#ddConn{} = Connection)     -> gen_server:cast(?MODULE, {add_connect, Connection}).

get_adapters(User)              -> gen_server:call(?MODULE, {get_adapters, User}).
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
    io:format(user, "~p starting...~n", [?MODULE]),
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [SchemaName], []),
    io:format(user, "~p started!~n~p", [?MODULE, Result]),
    Result.

init([SchemaName]) ->
    erlimem:start(),
    Cred = {?USER, ?PASSWORD},
    Sess = erlimem_session:open(rpc, {node(), SchemaName}, Cred),
    Sess:run_cmd(create_table, [ddAdapter, record_info(fields, ddAdapter), []]),
    Sess:run_cmd(create_table, [ddInterface, record_info(fields, ddInterface), []]),
    Sess:run_cmd(create_table, [ddConn, record_info(fields, ddConn), []]),
    Sess:run_cmd(create_table, [ddCmd, record_info(fields, ddCmd), []]),
    Sess:run_cmd(create_table, [ddView, record_info(fields, ddView), []]),
    Sess:run_cmd(create_table, [ddDash, record_info(fields, ddDash), []]),
    Sess:run_cmd(insert, [ddInterface, #ddInterface{id=ddjson,fullName="DDerl"}]),
    Adapters = [list_to_existing_atom(lists:nth(1, re:split(Fl, "[.]", [{return,list}]))) || Fl <- filelib:wildcard("*_adapter.beam", "ebin")],
    io:format(user, "~p initializing ~p~n", [?MODULE, Adapters]),
    [gen_server:cast(?MODULE, {init_adapter, Adapter}) || Adapter <- Adapters],
    {ok, #state{sess=Sess, schema=SchemaName}}.

handle_call({get_command, Id}, _From, #state{sess=Sess} = State) ->
    {Cmds, true} = Sess:run_cmd(select, [ddCmd
                                           , [{{ddCmd,'$1','_','_','_','_','_','_'}
                                           , [{'=:=','$1',Id}]
                                          , ['$_']}]]),
    Cmd = if length(Cmds) > 0 -> lists:nth(1, Cmds); true -> #ddCmd{opts=[]} end,
    {reply, Cmd, State};
handle_call({get_commands, User, Adapter}, _From, #state{sess=Sess} = State) ->
    {Cmds, true} = Sess:run_cmd(select, [ddCmd
                                        , [{{ddCmd,'$1','$2','$3','$4','_','$5','_'}
                                        , [{'and', {'=:=','$3',User}, {'=:=', '$4', [Adapter]}}]
                                          %, [['$1','$2', '$5']]}]]),
                                          , ['$_']}]]),
    {reply, Cmds, State};

handle_call({get_connects, User}, _From, #state{sess=Sess} = State) ->
    {Cons, true} = Sess:run_cmd(select, [ddConn
                                        , [{{ddConn,'_','_','$1','_','_','_'}
                                          , [{'=:=','$1',User}]
                                          , ['$_']}]]),
    {reply, Cons, State};

handle_call({verify_password, User, Password}, _From, #state{sess=Sess} = State) ->
    BinPswd = hexstr_to_bin(Password),
    case Sess:run_cmd(authenticate, [adminSessionId, User, {pwdmd5, BinPswd}]) of
        {error, {_Exception, {Msg, _Extra}}} ->
            io:format(user, "authenticate exception ~p~n", [Msg]),
            {reply, {error, Msg}, State};
        _SeCo ->
            case Sess:run_cmd(login, []) of
                {error, {_Exception, {Msg, _Extra}}} ->
                    io:format(user, "login exception ~p~n", [Msg]),
                    {reply, {error, Msg}, State};
                _NewSeCo -> {reply, true, State}
            end
    end;
handle_call(_Req,_From,State) -> {reply, ok, State}.

handle_cast({add_connect, #ddConn{} = Con}, #state{sess=Sess, schema=SchemaName} = State) ->
    NewCon0 = case Con#ddConn.owner of
        undefined -> Con#ddConn{owner = ?USER};
        _ -> Con
    end,
    NewCon1 = case NewCon0#ddConn.schema of
        undefined -> NewCon0#ddConn{schema = SchemaName};
        _ -> NewCon0
    end,
    NewCon = case Sess:run_cmd(select, [ddConn
                                       , [{{ddConn,'_','$1','_','_','_','_'}
                                         , [{'=:=','$1',Con#ddConn.name}]
                                         , ['$_']}]]) of
        {[#ddConn{id=Id}|_], true} -> NewCon1#ddConn{id=Id};
        _ -> NewCon1
    end,
    Sess:run_cmd(insert, [ddConn, NewCon]),
    {noreply, State};
handle_cast({add_command, Adapter, Name, Cmd, Opts}, #state{sess=Sess} = State) ->
    Id = case Sess:run_cmd(select, [ddCmd
                                       , [{{ddCmd,'$1','$2','$3','$4','_','$5','_'}
                                         , [{'and', {'=:=','$2',Name}, {'=:=', '$4', [Adapter]}}]
                                         , ['$_']}]]) of
        {[#ddCmd{id=Id0}|_], true} -> Id0;
        _ -> erlang:phash2(make_ref())
    end,
    Sess:run_cmd(insert, [ddCmd, #ddCmd { id        = Id
                                        , name      = Name
                                        , owner     = ?USER
                                        , adapters  = [Adapter]
                                        , command   = Cmd
                                        , opts      = Opts}]),
    {noreply, State};
handle_cast({add_adapter, Id, FullName}, #state{sess=Sess} = State) ->
    Sess:run_cmd(insert, [ddAdapter, #ddAdapter{id=Id,fullName=FullName}]),
    {noreply, State};
handle_cast({init_adapter, Adapter}, State) ->
    Adapter:init(),
    {noreply, State};
handle_cast(_Req,State)                 -> {noreply, State}.

handle_info(_Req,State)                 -> {noreply, State}.
terminate(_Reason, _State)              -> ok.
code_change(_OldVsn, State, _Extra)     -> {ok, State}.
format_status(_Opt, [_PDict, _State])   -> ok.

%% -     imem_if:insert_into_table(common, {?MODULE, [
%% -                 #file{name="Nodes.sql",
%% -                       content="imem_nodes",
%% -                       posX=0, posY=25, width=200, height=500}
%% -               , #file{name="Tables.sql",
%% -                       content="tables",
%% -                       posX=0, posY=25, width=200, height=500}
%% -               , #file{name="Views.sql",
%% -                       content="views",
%% -                       posX=0, posY=25, width=200, height=500}
%% -             ]}).


%% -    erlimem:stop().
%% - 
%% - setup() -> 
%% -     Schema = "Imem",
%% -     User = <<"admin">>,
%% -     Password = erlang:md5(<<"change_on_install">>),
%% -     Cred = {User, Password},
%% -     erlimem:start(),
%% -     erlimem_session:open(tcp, {localhost, 8124, Schema}, Cred).
%% - 
%% - db_test_() ->
%% -     {timeout, 1000000, {
%% -         setup,
%% -         fun setup/0,
%% -         fun teardown/1,
%% -         {with, [
%% -             fun tcp_table_test/1
%% -         ]}
%% -         }
%% -     }.
%% - 
%% - tcp_table_test(Sess) ->
%% -     Res = Sess:exec("create table def (col1 int, col2 char);"),
%% -     io:format(user, "Create ~p~n", [Res]),
%% -     Res0 = insert_range(Sess, 210, "def"),
%% -     io:format(user, "insert ~p~n", [Res0]),
%% -     {ok, Clms, Statement} = Sess:exec("select * from def;", 100),
%% -     io:format(user, "select ~p~n", [{Clms, Statement}]),
%% -     Statement:start_async_read(),
%% -     timer:sleep(1000),
%% -     io:format(user, "receiving...~n", []),
%% -     Rows = Statement:get_next(100, [{},{}]),
%% -     io:format(user, "received ~p~n", [length(Rows)]),
%% -     ok = Sess:exec("drop table def;"),
%% -     Statement:close(),
%% -     io:format(user, "drop table~n", []).
%% - 
%% - insert_range(_Sess, 0, _TableName) -> ok;
%% - insert_range(Sess, N, TableName) when is_integer(N), N > 0 ->
%% -     Sess:exec("insert into " ++ TableName ++ " values (" ++ integer_to_list(N) ++ ", '" ++ integer_to_list(N) ++ "');"),
%% -     insert_range(Sess, N-1, TableName).
