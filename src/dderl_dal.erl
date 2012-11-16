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

-define(SCHEMA, "Imem").
-define(USER, <<"admin">>).
-define(PASSWORD, erlang:md5(<<"change_on_install">>)).

verify_password(User, Password) -> gen_server:call(?MODULE, {verify_password, User, Password}).

add_adapter(Id, FullName)               -> gen_server:cast(?MODULE, {add_adapter, Id, FullName}).
add_command(Adapter, Name, Cmd, Opts)   -> gen_server:cast(?MODULE, {add_command, Adapter, Name, Cmd, Opts}).

get_adapters(User)              -> gen_server:call(?MODULE, {get_adapters, User}).

-record(state, {sess}).

start_link() ->
    io:format(user, "~p starting...~n", [?MODULE]),
    Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format(user, "~p started!~n~p", [?MODULE, Result]),
    Result.

init(_Args) ->
    erlimem:start(),
    Cred = {?USER, ?PASSWORD},
    Sess = erlimem_session:open(rpc, {node(), ?SCHEMA}, Cred),
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
    {ok, #state{sess=Sess}}.

handle_call({verify_password, _User, _Password},_From,State) -> {reply, true, State};
handle_call(_Req,_From,State)           -> {reply, ok, State}.

handle_cast({add_command, Adapter, Name, Cmd, Opts}, #state{sess=Sess} = State) ->
    Sess:run_cmd(insert, [ddCmd, #ddCmd { id=erlang:make_ref()
                                        , name = Name
                                        , owner = ?USER
                                        , adapters = [Adapter]
                                        , command = Cmd
                                        , opts = Opts}]),
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
