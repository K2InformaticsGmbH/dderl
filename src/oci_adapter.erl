-module(oci_adapter).

-export([process_cmd/2]).

process_cmd({"connect", BodyJson}, srv) ->
    IpAddr   = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    Service  = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Type     = binary_to_list(proplists:get_value(<<"type">>, BodyJson, <<>>)),
    User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
        dderl_session:log(srv, "Params ~p~n", [{IpAddr, Port, Service, Type, User, Password}]),
    {ok, Pool} =
        if Service =/= "MOCK" ->
            oci_session_pool:start_link(IpAddr, Port, {list_to_atom(Type), Service}, User, Password, []);
        true ->
            oci_session_pool:start_link(IpAddr, Port,  {list_to_atom(Type), "db.local"},  User, Password, [{port_options, [{mock_port, oci_port_mock}]}])
    end,
    %%oci_session_pool:enable_log(Pool),
    Session = oci_session_pool:get_session(Pool),
    {Session,Pool}.

%process_call({"users", _ReqData}, _From, #state{session={Session, _Pool}, key=Key,file=File} = State) ->
%    Query = "SELECT DISTINCT OWNER FROM \"ALL_TABLES\"",
%    logi(File, "[~p] Users for ~p~n", [Key, {Session, Query}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 10001),
%    Resp = prepare_json_rows(Statement, Key, erlang:phash2(Statement), File),
%    logi(File, "[~p] Users Resp ~p~n", [Key, Resp]),
%    Statement:close(),
%    {reply, Resp, State};
%process_call({"tables", ReqData}, _From, #state{session={Session, _Pool}, key=Key,file=File} = State) ->
%    {struct, [{<<"tables">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    Owner = binary_to_list(proplists:get_value(<<"owner">>, BodyJson, <<>>)),
%    Query = "SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER='" ++ Owner ++ "' ORDER BY TABLE_NAME DESC",
%    logi(File, "[~p] Tables for ~p~n", [Key, {Session, Query}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 10001),
%    Resp = prepare_json_rows(Statement, Key, erlang:phash2(Statement), File),
%    Statement:close(),
%    {reply, Resp, State};
%process_call({"views", ReqData}, _From, #state{session={Session, _Pool}, key=Key,file=File} = State) ->
%    {struct, [{<<"views">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    Owner = binary_to_list(proplists:get_value(<<"owner">>, BodyJson, <<>>)),
%    Query = "SELECT VIEW_NAME FROM ALL_VIEWS WHERE OWNER='" ++ Owner ++ "' ORDER BY VIEW_NAME DESC",
%    logi(File, "[~p] Views for ~p~n", [Key, {Session, Query}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 10001),
%    Resp = prepare_json_rows(Statement, Key, erlang:phash2(Statement), File),
%    Statement:close(),
%    {reply, Resp, State};
%process_call({"columns", ReqData}, _From, #state{session={Session, _Pool}, key=Key,file=File} = State) ->
%    {struct, [{<<"cols">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    TableNames = string:join(["'" ++ binary_to_list(X) ++ "'" || X <- proplists:get_value(<<"tables">>, BodyJson, <<>>)], ","),
%    Owner = string:join(["'" ++ binary_to_list(X) ++ "'" || X <- proplists:get_value(<<"owners">>, BodyJson, <<>>)], ","),
%    Query = "SELECT COLUMN_NAME FROM ALL_TAB_COLS WHERE TABLE_NAME IN (" ++ TableNames ++ ") AND OWNER IN (" ++ Owner ++ ")",
%    logi(File, "[~p] Columns for ~p~n", [Key, {Session, Query, TableNames}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 150),
%    Resp = prepare_json_rows(Statement, Key, erlang:phash2(Statement), File),
%    Statement:close(),
%    {reply, Resp, State};
%process_call({"query", ReqData}, _From, #state{session={Session, _Pool}, statements=Statements, key=Key,file=File} = State) ->
%    {struct, [{<<"query">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
%    %{ok, Tokens, _} = sql_lex:string(Query++";"),
%    %{ok, [ParseTree|_]} = sql_parse:parse(Tokens),
%    ParseTree = [],
%    TableName = binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>)),
%    logi(File, "[~p] Query ~p~n", [Key, {Session, Query, TableName}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 150, true),
%    {ok, Clms} = Statement:get_columns(),
%    StmtHndl = erlang:phash2(Statement),
%    Columns = lists:reverse(lists:map(fun({N,_,_})->N end, Clms)),
%    Resp = "{\"session\":"++integer_to_list(Key)++", \"table\":\""++TableName++"\",\"headers\":"++string_list_to_json(Columns, [])++",\"statement\":"++integer_to_list(StmtHndl)++"}",
%    {reply, Resp, State#state{statements=[{StmtHndl, {Statement, Query, ParseTree}}|Statements]}};
%process_call({"row", ReqData}, _From, #state{statements=Statements, key=Key,file=File} = State) ->
%    {struct, [{<<"row">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
%    case proplists:get_value(StmtKey, Statements) of
%        undefined ->
%            logi(File, "[~p, ~p] Statements ~p~n", [Key, StmtKey, Statements]),
%            {reply, "{\"session\":"++integer_to_list(Key)++"}", State};
%        {Statement, _, _} -> {reply, prepare_json_rows(Statement, Key, StmtKey, File), State}
%    end;
%process_call({"build_qry", ReqData}, _From, #state{key=Key,file=File} = State) ->
%    {struct, [{<<"build_qry">>, BodyJson}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    {struct, QObj} = mochijson2:decode(BodyJson),
%    Tables      = proplists:get_value(<<"tables">>, QObj, <<>>),
%    Fields      = proplists:get_value(<<"fields">>, QObj, <<>>),
%    Sorts       = proplists:get_value(<<"sorts">>, QObj, <<>>),
%    Conditions  = proplists:get_value(<<"conds">>, QObj, <<>>),
%    Joins       = proplists:get_value(<<"joins">>, QObj, <<>>),
%    logi(File, "[~p] Sorts: ~p~n", [Key, Sorts]),
%    SqlStr = create_select_string(Tables, Fields, Sorts, Conditions, Joins),
%    logi(File, "[~p] SQL: ~p~n", [Key, SqlStr]),
%    {reply, "{\"session\":"++integer_to_list(Key)++", \"sql\":\""++SqlStr++"\"}", State};
%process_call({"parse_stmt", ReqData}, _From, #state{key=Key,file=File} = State) ->
%    {struct, [{<<"parse_stmt">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
%    {ok, Tokens, _} = sql_lex:string(Query++";"),
%    {ok, [ParseTree|_]} = sql_parse:parse(Tokens),
%    logi(File, "[~p] parsed sql ~p~n", [Key, ParseTree]),
%    {reply, sql_parse_to_json(Key, ParseTree), State};
%process_call({"stmt_close", ReqData}, _From, #state{statements=Statements, key=Key,file=File} = State) ->
%    {struct, [{<<"stmt_close">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
%    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
%    case proplists:get_value(StmtKey, Statements) of
%        undefined ->
%            logi(File, "[~p] Statement ~p not found. Statements ~p~n", [Key, StmtKey, proplists:get_keys(Statements)]),
%            {reply, "{\"session\":"++integer_to_list(Key)++"}", State};
%        {Statement, _, _} ->
%            logi(File, "[~p, ~p] Remove statement ~p~n", [Key, StmtKey, Statement]),
%            Statement:close(),
%            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
%            {reply, "{\"session\":"++integer_to_list(Key)++"}", State#state{statements = NewStatements}}
%    end.
%
%prepare_json_rows(Statement, Key, StmtKey, File) ->
%    case Statement:next_rows() of
%        [] -> "{\"session\":"++integer_to_list(Key)++", \"rows\":[]}";
%        Rows ->
%            logi(File, "[~p, ~p] row produced _______ ~p _______~n", [Key, StmtKey, length(Rows)]),
%            J = convert_rows_to_json(Rows, "[\n"),
%            "{\"session\":"++integer_to_list(Key)++", \"rows\":"++string:substr(J,1,length(J)-1)++"]}"
%    end.
%
%create_select_string(Tables, Fields, Sorts, Conditions, Joins) ->
%    "select " ++ string:join([binary_to_list(X)||X<-Fields],", ") ++
%    " from " ++ string:join([binary_to_list(X)||X<-Tables],", ") ++
%    cond_str(Conditions, Joins) ++
%    order_str(Sorts).
%
%cond_str(_Conditions, _Joins) -> "".
%
%order_str(Sorts) ->
%    case string:join(order_str(Sorts, []), ", ") of
%        Str when length(Str) > 0 ->  " order by " ++ Str;
%        _ -> ""
%    end.
%order_str([], Acc) -> Acc;
%order_str([{struct, S} | Sorts], Acc) ->
%    Acc1 = order_str(Sorts, Acc),
%    [binary_to_list(proplists:get_value(<<"txt">>, S, <<>>)) ++
%        case proplists:get_value(<<"dir">>, S, 0) of
%            0 -> " ASC";
%            1 -> " DESC"
%        end | Acc1].
%
%cond_to_json({Op, A, B}, Json) when Op =:= 'and'; Op =:= 'or' ->
%%%    {S, C} = if erlang:element(1, A) == Op -> {[cond_to_json(A, [])], ""};
%%%        true -> {[], cond_to_json(A, [])} end,
%%%    {S1, C1} = if erlang:element(1, B) == Op -> {S ++ [cond_to_json(B, [])], C};
%%%        true ->
%%%           {S, string:join(
%%%                   lists:foldl(fun(E,Acc) ->
%%%                                   if length(E) > 0 -> [E|Acc];
%%%                                   true -> Acc end
%%%                          end
%%%                          , []
%%%                          , [C, cond_to_json(B, [])]
%%%                          )
%%%                   , ",")}
%%%        end,
%%%    string:join(lists:foldl(fun(E,Acc) -> if length(E) > 0 -> [E|Acc]; true -> Acc end end, [], [Json,
%%%         "{\"title\": \""++string:to_upper(atom_to_list(Op))++"\",
%%%         \"icon\":false,
%%%         \"isFolder\": true,
%%%         \"expand\": true,
%%%         \"children\": ["++C1++"]}"]++S1), ",");
%    Json ++ "{\"title\": \""++string:to_upper(atom_to_list(Op))++"\",
%    \"icon\":false,
%    \"isFolder\": true,
%    \"expand\": true,
%    \"children\": ["++cond_to_json(A, [])++","++cond_to_json(B, [])++"]}";
%cond_to_json({Op, A}, _) ->
%         "{\"title\": \""++string:to_upper(atom_to_list(Op))++"\",
%         \"icon\":false,
%         \"isFolder\": true,
%         \"expand\": true,
%         \"children\": ["++cond_to_json(A, [])++"]}";
%cond_to_json({Op, A, {B1,[]}}, _) ->
%         "{\"title\": \""++A++" "++string:to_upper(atom_to_list(Op))++" "++B1++"\",
%         \"icon\":false,
%         \"isFolder\": false,
%         \"expand\": true}";
%cond_to_json({Op, A, {B1,{escape, B2}}}, _) ->
%         "{\"title\": \""++A++" "++string:to_upper(atom_to_list(Op))++" "++B1++" ESCAPE "++B2++"\",
%         \"icon\":false,
%         \"isFolder\": false,
%         \"expand\": true}";
%cond_to_json({Op, A, {B1,B2}}, _) ->
%         "{\"title\": \""++A++" "++string:to_upper(atom_to_list(Op))++" "++B1++" AND "++B2++"\",
%         \"icon\":false,
%         \"isFolder\": false,
%         \"expand\": true}";
%cond_to_json({Op, A, B}, _) ->
%         "{\"title\": \""++A++" "++string:to_upper(atom_to_list(Op))++" "++B++"\",
%         \"icon\":false,
%         \"isFolder\": false,
%         \"expand\": true}";
%cond_to_json([], _) -> "".
%
%sql_parse_to_json(Key,
%                  {select, {opt, _Opt},
%                           {fields, Fields},
%                           {into, _Into},
%                           {from, Tables},
%                           {where, Cond},
%                           {group_by, _GroupBy},
%                           {having, _Having},
%                           {order_by, Orders}}) ->
%    JsonFields = string_list_to_json(Fields, []),
%    JsonTables = string_list_to_json(Tables, []),
%    OrdStr =
%    case lists:foldl(fun({N,O},L) ->
%                        "{\"txt\":\""++N++"\", \"dir\":"++(if O =:= 'ASC' -> "0"; true -> "1" end)++"},"++L
%                     end,
%                     "",
%                     Orders)
%    of
%        Str when length(Str) > 0 -> string:substr(Str,1,length(Str)-1);
%        _ -> ""
%    end,
%    "{\"session\":"++integer_to_list(Key)++
%    ", \"fields\":"++JsonFields++
%    ", \"tables\":"++JsonTables++
%    ", \"sorts\":["++OrdStr++"]"++
%    ", \"conds\":["++cond_to_json(Cond, [])++"]"++
%    "}".
%
%%where: {op:"and", argList:[]},
%
%conds_to_json(Cond) -> conds_to_json(Cond,[]).
%conds_to_json(Cond,Json) when is_tuple(Cond) -> conds_to_json(tuple_to_list(Cond),Json);
%conds_to_json([],Json) -> Json;
%conds_to_json(A,_) when is_list(A) -> A;
%conds_to_json([Op,A,B],Json) ->
%    io:format(user, "1. ~p~n", [Json]),
%    Json ++
%    "{\"op\":"++string:to_upper(atom_to_list(Op))++"\",
%    \"argList\":["++conds_to_json(A,[])++","++conds_to_json(B,[])++"]}";
%conds_to_json([select|_]=Q,Json) ->
%    io:format(user, "2. ~p~n", [Json]),
%    Json ++ select_to_json(list_to_tuple(Q)).
%
%%add_session_to_json(Key, Json) ->
%%    "{ \"session\":"++integer_to_list(Key)++
%%    ", \"data\":" ++ Json ++ "}".
%
%sql_to_json(Sql) ->
%    {ok, T, _} = sql_lex:string(Sql),
%    {ok, [S|_]} = sql_parse:parse(T),
%    io:format(user, "Parsed ~p~n", [S]),
%    select_to_json(S).
%
%select_to_json({select, {opt, Opt},
%                        {fields, Fields},
%                        {into, Into},
%                        {from, Tables},
%                        {where, Cond},
%                        {group_by, GroupBy},
%                        {having, Having},
%                        {order_by, Orders}}) ->
%    "{  \"select\":"++string_list_to_json(Fields, [])++
%    ", \"options\":"++string_list_to_json(Opt, [])++
%    ",    \"into\":"++string_list_to_json(Into, [])++
%    ",    \"from\":"++ string_list_to_json(Tables, [])++
%    ",   \"where\":"++ conds_to_json(Cond)++
%    ", \"groupby\":"++ string_list_to_json(GroupBy, [])++
%    ",  \"having\":"++ string_list_to_json(Having, [])++
%    ",\"order_by\":"++ string_list_to_json(Orders, [])++
%    "}".
