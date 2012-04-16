-module(dderl_session).

-behavior(gen_server).

-export([start/0
        , process_request/3
        , get_state/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).

-define(SESSION_IDLE_TIMEOUT, 3600000). % 1 hour

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Key = erlang:phash2({dderl_session, Pid}),
    {Key, {dderl_session, Pid}}.

get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

process_request(SessKey, WReq, {?MODULE, Pid}) ->
    Type = wrq:disp_path(WReq),
    gen_server:call(Pid, {SessKey, Type, WReq}, infinity).

-record(state, {
        key
        , session
        , statements = []
        , tref
    }).

init(_Args) ->
    io:format(user, "dderl_session ~p started...~n", [self()]),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {ok, #state{key=erlang:phash2({dderl_session, self()}),tref=TRef}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({SessKey, Typ, WReq}, From, #state{tref=TRef, key=Key} = State) ->
    timer:cancel(TRef),
    NewKey = if SessKey =/= Key -> SessKey; true -> Key end,
    io:format(user, "[~p] process_request ~p~n", [NewKey, Typ]),
    {Rep, Resp, NewState} = process_call({Typ, WReq}, From, State#state{key=NewKey}),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {Rep, Resp, NewState#state{tref=NewTRef,key=NewKey}}.

process_call({"login", ReqData}, _From, #state{key=Key} = State) ->
    {struct, [{<<"login">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    IpAddr   = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    Service  = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Type      = binary_to_list(proplists:get_value(<<"type">>, BodyJson, <<>>)),
    User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    io:format(user, "[~p] Params ~p~n", [Key, {IpAddr, Port, Service, Type, User, Password}]),
    {ok, Pool} =
        if Service =/= "MOCK" ->
            oci_session_pool:start_link(IpAddr, Port, {list_to_atom(Type), Service}, User, Password, []);
        true ->
            oci_session_pool:start_link(IpAddr, Port,  {list_to_atom(Type), "db.local"},  User, Password, [{port_options, [{mock_port, oci_port_mock}]}])
    end,
    %%oci_session_pool:enable_log(Pool),
    Session = oci_session_pool:get_session(Pool),
    io:format(user, "[~p] Session ~p~n", [Key, {Session,Pool}]),
    Resp = "{\"session\":" ++ integer_to_list(State#state.key) ++ "}",
    {reply, Resp, State#state{session={Session,Pool}}};
process_call({"users", _ReqData}, _From, #state{session={Session, _Pool}, key=Key} = State) ->
    Query = "select distinct owner from all_tables",
    io:format(user, "[~p] Users for ~p~n", [Key, {Session, Query}]),
    {statement, Statement} = Session:execute_sql(Query, [], 10001),
    io:format(user, "[~p] Rows ~p~n", [Key, Statement:next_rows()]),
    Resp = prepare_json_rows(Statement, Key),
    Statement:close(),
    {reply, Resp, State};
process_call({"tables", ReqData}, _From, #state{session={Session, _Pool}, key=Key} = State) ->
    {struct, [{<<"tables">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    Owner = binary_to_list(proplists:get_value(<<"owner">>, BodyJson, <<>>)),
    Query = "select table_name from all_tables where owner='" ++ Owner ++ "' order by table_name desc",
    io:format(user, "[~p] Tables for ~p~n", [Key, {Session, Query}]),
    {statement, Statement} = Session:execute_sql(Query, [], 10001),
    Resp = prepare_json_rows(Statement, Key),
    Statement:close(),
    {reply, Resp, State};
process_call({"views", ReqData}, _From, #state{session={Session, _Pool}, key=Key} = State) ->
    {struct, [{<<"views">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    Owner = binary_to_list(proplists:get_value(<<"owner">>, BodyJson, <<>>)),
    Query = "select view_name from all_views where owner='" ++ Owner ++ "' order by view_name desc",
    io:format(user, "[~p] Views for ~p~n", [Key, {Session, Query}]),
    {statement, Statement} = Session:execute_sql(Query, [], 10001),
    Resp = prepare_json_rows(Statement, Key),
    Statement:close(),
    {reply, Resp, State};
process_call({"columns", ReqData}, _From, #state{session={Session, _Pool}, key=Key} = State) ->
    {struct, [{<<"cols">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    TableNames = string:join(["'" ++ binary_to_list(X) ++ "'" || X <- proplists:get_value(<<"tables">>, BodyJson, <<>>)], ","),
    Owner = string:join(["'" ++ binary_to_list(X) ++ "'" || X <- proplists:get_value(<<"owners">>, BodyJson, <<>>)], ","),
    Query = "select column_name from all_tab_cols where table_name in (" ++ TableNames ++ ") and owner in (" ++ Owner ++ ")",
    io:format(user, "[~p] Columns for ~p~n", [Key, {Session, Query, TableNames}]),
    {statement, Statement} = Session:execute_sql(Query, [], 150),
    Resp = prepare_json_rows(Statement, Key),
    Statement:close(),
    {reply, Resp, State};
process_call({"query", ReqData}, _From, #state{session={Session, _Pool}, statements=Statements, key=Key} = State) ->
    {struct, [{<<"query">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    {ok, Tokens, _} = sql_lex:string(Query++";"),
    {ok, [ParseTree|_]} = sql_parse:parse(Tokens),
    TableName = binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>)),
    io:format(user, "[~p] Query ~p~n", [Key, {Session, Query, TableName}]),
    {statement, Statement} = Session:execute_sql(Query, [], 150, true),
    {ok, Clms} = Statement:get_columns(),
    StmtHndl = erlang:phash2(Statement),
    Columns = lists:reverse(lists:map(fun({N,_,_})->N end, Clms)),
    Resp = "{\"session\":"++integer_to_list(Key)++", \"table\":\""++TableName++"\",\"headers\":"++string_list_to_json(Columns, [])++",\"statement\":"++integer_to_list(StmtHndl)++"}",
    {reply, Resp, State#state{statements=[{StmtHndl, {Statement, Query, ParseTree}}|Statements]}};
process_call({"row", ReqData}, _From, #state{statements=Statements, key=Key} = State) ->
    {struct, [{<<"row">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            io:format("[~p, ~p] Statements ~p~n", [Key, StmtKey, Statements]),
            {reply, "{\"session\":"++integer_to_list(Key)++"}", State};
        {Statement, _, _} -> {reply, prepare_json_rows(Statement, Key, StmtKey), State}
    end;
process_call({"build_qry", ReqData}, _From, #state{key=Key} = State) ->
    {struct, [{<<"build_qry">>, BodyJson}]} = mochijson2:decode(wrq:req_body(ReqData)),
    {struct, QObj} = mochijson2:decode(BodyJson),
    Tables      = proplists:get_value(<<"tables">>, QObj, <<>>),
    Fields      = proplists:get_value(<<"fields">>, QObj, <<>>),
    Sorts       = proplists:get_value(<<"sorts">>, QObj, <<>>),
    Conditions  = proplists:get_value(<<"conds">>, QObj, <<>>),
    Joins       = proplists:get_value(<<"joins">>, QObj, <<>>),
    io:format(user, "[~p] Sorts: ~p~n", [Key, Sorts]),
    SqlStr = create_select_string(Tables, Fields, Sorts, Conditions, Joins),
    io:format(user, "[~p] SQL: ~p~n", [Key, SqlStr]),
    {reply, "{\"session\":"++integer_to_list(Key)++", \"sql\":\""++SqlStr++"\"}", State};
process_call({"parse_stmt", ReqData}, _From, #state{key=Key} = State) ->
    {struct, [{<<"parse_stmt">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    {ok, Tokens, _} = sql_lex:string(Query++";"),
    {ok, [ParseTree|_]} = sql_parse:parse(Tokens),
    io:format(user, "[~p] parsed sql ~p~n", [Key, ParseTree]),
    {reply, sql_parse_to_json(Key, ParseTree), State};
process_call({"stmt_close", ReqData}, _From, #state{statements=Statements, key=Key} = State) ->
    {struct, [{<<"stmt_close">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            io:format("[~p] Statement ~p not found. Statements ~p~n", [Key, StmtKey, proplists:get_keys(Statements)]),
            {reply, "{\"session\":"++integer_to_list(Key)++"}", State};
        {Statement, _, _} ->
            io:format("[~p, ~p] Remove statement ~p~n", [Key, StmtKey, Statement]),
            Statement:close(),
            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
            {reply, "{\"session\":"++integer_to_list(Key)++"}", State#state{statements = NewStatements}}
    end;
process_call(Request, _From, {key=Key}=State) ->
    io:format(user, "[~p] Unknown request ~p~n", [Key, Request]),
    {reply, "{\"dderl-version\":1.0}", State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(die, State) -> {stop, timeout, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{key=Key}) ->
    io:format(user, "[~p] ~p terminating for ~p~n", [Key, self(), Reason]),
    ets:delete(dderl_req_sessions, Key).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_json_rows(Statement, Key) -> prepare_json_rows(Statement, Key, undefined).
prepare_json_rows(Statement, Key, StmtKey) ->
    case Statement:next_rows() of
        [] -> "{\"session\":"++integer_to_list(Key)++", \"rows\":[]}";
        Rows ->
            io:format(user, "[~p, ~p] row produced _______ ~p _______~n", [Key, StmtKey, length(Rows)]),
            J = convert_rows_to_json(Rows, "[\n"),
            "{\"session\":"++integer_to_list(Key)++", \"rows\":"++string:substr(J,1,length(J)-1)++"]}"
    end.

convert_rows_to_json([], Acc) -> Acc;
convert_rows_to_json([Row|Rows], Acc) ->
    convert_rows_to_json(Rows, Acc ++ string_list_to_json(lists:reverse(Row), []) ++ ",").

string_list_to_json([], Json) -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
string_list_to_json([S|Strings], Json) ->
    string_list_to_json(Strings, Json ++ "\"" ++ lists:flatten([if X > 127 -> "&#" ++ integer_to_list(X) ++ ";";
                                                                   (X == 10) or (X == 13) -> "";
                                                                   true -> X
                                                               end || X <- S]) ++ "\",").

create_select_string(Tables, Fields, Sorts, Conditions, Joins) ->
    "select " ++ string:join([binary_to_list(X)||X<-Fields],", ") ++
    " from " ++ string:join([binary_to_list(X)||X<-Tables],", ") ++
    cond_str(Conditions, Joins) ++
    order_str(Sorts).

cond_str(_Conditions, _Joins) -> "".

order_str(Sorts) ->
    case string:join(order_str(Sorts, []), ", ") of
        Str when length(Str) > 0 ->  " order by " ++ Str;
        _ -> ""
    end.
order_str([], Acc) -> Acc;
order_str([{struct, S} | Sorts], Acc) ->
    Acc1 = order_str(Sorts, Acc),
    [binary_to_list(proplists:get_value(<<"txt">>, S, <<>>)) ++
        case proplists:get_value(<<"dir">>, S, 0) of
            0 -> " ASC";
            1 -> " DESC"
        end | Acc1].

cond_to_json({Op, A, B}, Json) when Op =:= 'and'; Op =:= 'or' ->
%%    {S, C} = if erlang:element(1, A) == Op -> {[cond_to_json(A, [])], ""};
%%        true -> {[], cond_to_json(A, [])} end,
%%    {S1, C1} = if erlang:element(1, B) == Op -> {S ++ [cond_to_json(B, [])], C};
%%        true ->
%%           {S, string:join(
%%                   lists:foldl(fun(E,Acc) ->
%%                                   if length(E) > 0 -> [E|Acc];
%%                                   true -> Acc end
%%                          end
%%                          , []
%%                          , [C, cond_to_json(B, [])]
%%                          )
%%                   , ",")}
%%        end,
%%    string:join(lists:foldl(fun(E,Acc) -> if length(E) > 0 -> [E|Acc]; true -> Acc end end, [], [Json,
%%         "{\"title\": \""++string:to_upper(atom_to_list(Op))++"\",
%%         \"icon\":false,
%%         \"isFolder\": true,
%%         \"expand\": true,
%%         \"children\": ["++C1++"]}"]++S1), ",");
    Json ++ "{\"title\": \""++string:to_upper(atom_to_list(Op))++"\",
    \"icon\":false,
    \"isFolder\": true,
    \"expand\": true,
    \"children\": ["++cond_to_json(A, [])++","++cond_to_json(B, [])++"]}";
cond_to_json({Op, A}, _) ->
         "{\"title\": \""++string:to_upper(atom_to_list(Op))++"\",
         \"icon\":false,
         \"isFolder\": true,
         \"expand\": true,
         \"children\": ["++cond_to_json(A, [])++"]}";
cond_to_json({Op, A, B}, _) ->
         "{\"title\": \""++A++" "++string:to_upper(atom_to_list(Op))++" "++B++"\",
         \"icon\":false,
         \"isFolder\": false,
         \"expand\": true}";
cond_to_json([], _) -> "".

sql_parse_to_json(Key,
                  {select, {opt, _Opt},
                           {fields, Fields},
                           {into, _Into},
                           {from, Tables},
                           {where, Cond},
                           {group_by, _GroupBy},
                           {having, _Having},
                           {order_by, Orders}}) ->
    JsonFields = string_list_to_json(Fields, []),
    JsonTables = string_list_to_json(Tables, []),
    OrdStr =
    case lists:foldl(fun({N,O},L) ->
                        "{\"txt\":\""++N++"\", \"dir\":"++(if O =:= 'ASC' -> "0"; true -> "1" end)++"},"++L
                     end,
                     "",
                     Orders)
    of
        Str when length(Str) > 0 -> string:substr(Str,1,length(Str)-1);
        _ -> ""
    end,
    "{\"session\":"++integer_to_list(Key)++
    ", \"fields\":"++JsonFields++
    ", \"tables\":"++JsonTables++
    ", \"sorts\":["++OrdStr++"]"++
    ", \"conds\":["++cond_to_json(Cond, [])++"]"++
    "}".
