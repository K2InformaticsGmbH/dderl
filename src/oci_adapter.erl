-module(oci_adapter).

-include("dderl.hrl").

-export([ process_cmd/3
        , init/0
        , import_sql/2
        ]).

init() ->
    imem_if:insert_into_table(common, {?MODULE, [
                #file{name="Users.sql",  content="SELECT DISTINCT OWNER FROM ALL_TABLES"}
              , #file{name="Tables.sql", content="SELECT TABLE_NAME FROM ALL_TABLES ORDER BY TABLE_NAME DESC"}
              , #file{name="Views.sql",  content="SELECT VIEW_NAME FROM ALL_VIEWS ORDER BY VIEW_NAME DESC"}
            ]}).

process_cmd({"connect", BodyJson}, SrvPid, _) ->
    IpAddr   = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    Service  = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Type     = binary_to_list(proplists:get_value(<<"type">>, BodyJson, <<>>)),
    User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    dderl_session:log(SrvPid, "Params ~p~n", [{IpAddr, Port, Service, Type, User, Password}]),
    {ok, Pool} =
        if Service =/= "MOCK" ->
            oci_session_pool:start_link(IpAddr, Port, {list_to_atom(Type), Service}, User, Password, []);
        true ->
            oci_session_pool:start_link(IpAddr, Port,  {list_to_atom(Type), "db.local"},  User, Password, [{port_options, [{mock_port, oci_port_mock}]}])
    end,
    %%oci_session_pool:enable_log(Pool),
    Session = oci_session_pool:get_session(Pool),
    {{Session,Pool,[]}, "{\"connect\":\"ok\"}"};
%process_cmd({"users", _BodyJson}, SrvPid, {Session,_,_} = MPort) ->
%    Query = "SELECT DISTINCT OWNER FROM \"ALL_TABLES\"",
%    dderl_session:log(SrvPid, "[~p] Users for ~p~n", [SrvPid, {Session, Query}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 10001),
%    Resp = prepare_json_rows(Statement, erlang:phash2(Statement), SrvPid),
%    dderl_session:log(SrvPid, "[~p] Users Resp ~p~n", [SrvPid, Resp]),
%    Statement:close(),
%    {MPort, Resp};
%process_cmd({"tables", BodyJson}, SrvPid, {Session,_,_} = MPort) ->
%    Owner = binary_to_list(proplists:get_value(<<"owner">>, BodyJson, <<>>)),
%    Query = "SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER='" ++ Owner ++ "' ORDER BY TABLE_NAME DESC",
%    dderl_session:log(SrvPid, "[~p] Tables for ~p~n", [SrvPid, {Session, Query}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 10001),
%    Resp = prepare_json_rows(Statement, erlang:phash2(Statement), SrvPid),
%    Statement:close(),
%    {MPort, Resp};
%process_cmd({"views", BodyJson}, SrvPid, {Session,_,_} = MPort) ->
%    Owner = binary_to_list(proplists:get_value(<<"owner">>, BodyJson, <<>>)),
%    Query = "SELECT VIEW_NAME FROM ALL_VIEWS WHERE OWNER='" ++ Owner ++ "' ORDER BY VIEW_NAME DESC",
%    dderl_session:log(SrvPid, "[~p] Views for ~p~n", [SrvPid, {Session, Query}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 10001),
%    Resp = prepare_json_rows(Statement, erlang:phash2(Statement), SrvPid),
%    Statement:close(),
%    {MPort, Resp};
%process_cmd({"columns", BodyJson}, SrvPid, {Session,_,_} = MPort) ->
%    TableNames = string:join(["'" ++ binary_to_list(X) ++ "'" || X <- proplists:get_value(<<"tables">>, BodyJson, <<>>)], ","),
%    Owner = string:join(["'" ++ binary_to_list(X) ++ "'" || X <- proplists:get_value(<<"owners">>, BodyJson, <<>>)], ","),
%    Query = "SELECT COLUMN_NAME FROM ALL_TAB_COLS WHERE TABLE_NAME IN (" ++ TableNames ++ ") AND OWNER IN (" ++ Owner ++ ")",
%    dderl_session:log(SrvPid, "[~p] Columns for ~p~n", [SrvPid, {Session, Query, TableNames}]),
%    {statement, Statement} = Session:execute_sql(Query, [], 150),
%    Resp = prepare_json_rows(Statement, erlang:phash2(Statement), SrvPid),
%    Statement:close(),
%    {MPort, Resp};
process_cmd({"get_query", BodyJson}, SrvPid, {Session,Pool,Statements}) ->
    Table = binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>)),
    Query = "SELECT * FROM " ++ Table,
    dderl_session:log(SrvPid, "[~p] get query ~p~n", [SrvPid, Query]),
    {{Session,Pool,Statements}, "{\"qry\":\""++Query++"\"}"};
process_cmd({"query", BodyJson}, SrvPid, {Session,Pool,Statements}) ->
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    %{ok, Tokens, _} = sql_lex:string(Query++";"),
    %{ok, [ParseTree|_]} = sql_parse:parse(Tokens),
    ParseTree = [],
    dderl_session:log(SrvPid, "[~p] Query ~p~n", [SrvPid, {Session, Query}]),
    
    case Session:execute_sql(Query, [], ?DEFAULT_ROW_SIZE, true) of
        {statement, Statement} ->
            {ok, Clms} = Statement:get_columns(),
            StmtHndl = erlang:phash2(Statement),
            Columns = lists:reverse(lists:map(fun({N,_,_})->N end, Clms)),
            Resp = "{\"headers\":"++dderl_session:string_list_to_json(Columns, [])++
            ",\"statement\":"++integer_to_list(StmtHndl)++"}",
            {{Session,Pool,[{StmtHndl, {Statement, Query, ParseTree}}|Statements]}, Resp};
        {error, Error} ->
            dderl_session:log(SrvPid, "[~p] Query Error ~p~n", [SrvPid, Error]),
            Resp = "{\"headers\":[],\"statement\":0,\"error\":\""++Error++"\"}",
            {{Session,Pool,Statements}, Resp}
    end;
process_cmd({"row_prev", BodyJson}, SrvPid, {_,_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p, ~p] Statements ~p~n", [SrvPid, StmtKey, Statements]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} -> {MPort, prepare_json_rows(prev, -1, Statement, StmtKey, SrvPid)}
    end;
process_cmd({"row_next", BodyJson}, SrvPid, {_,_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowNum = proplists:get_value(<<"row_num">>, BodyJson, -1),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p, ~p] Statements ~p~n", [SrvPid, StmtKey, Statements]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} -> {MPort, prepare_json_rows(next, RowNum, Statement, StmtKey, SrvPid)}
    end;
process_cmd({"stmt_close", BodyJson}, SrvPid, {Session,Pool,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p] Statement ~p not found. Statements ~p~n", [SrvPid, StmtKey, proplists:get_keys(Statements)]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} ->
            dderl_session:log(SrvPid, "[~p, ~p] Remove statement ~p~n", [SrvPid, StmtKey, Statement]),
            Statement:close(),
            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
            {{Session,Pool,NewStatements}, "{\"rows\":[]}"}
    end;
process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort);
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.

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

%prepare_json_rows(Statement, StmtKey, SrvPid) -> prepare_json_rows(Statement, next_rows, -1, StmtKey, SrvPid).

prepare_json_rows(prev, RowNum, Statement, StmtKey, SrvPid) ->
    prepare_json_rows(Statement, RowNum, prev_rows, StmtKey, SrvPid);
prepare_json_rows(next, RowNum, Statement, StmtKey, SrvPid) ->
    prepare_json_rows(Statement, RowNum, next_rows, StmtKey, SrvPid);
prepare_json_rows(Statement, RowNum, Fun, StmtKey, SrvPid) ->
    Rows = fetch_rows(Statement, RowNum, Fun, StmtKey, SrvPid, []),
    NewRows = if length(Rows) > ?DEFAULT_ROW_SIZE ->
            {First, _} = lists:split(?DEFAULT_ROW_SIZE, Rows),
            First;
        true -> Rows
    end,
    dderl_session:log(SrvPid, "[~p] fetched ~p rows from row ~p~n", [StmtKey, length(NewRows), RowNum]),
    case NewRows of
        [] ->
            "{\"done\":true, \"rows\":[]}";
        NewRows ->
            J = dderl_session:convert_rows_to_json(NewRows),
            "{\"done\":false, \"rows\":"++string:substr(J,1,length(J)-1)++"]}"
    end.

fetch_rows(Statement, RowNum, Fun, StmtKey, SrvPid, RowsAcc) ->
    case {apply(Statement, Fun, []), length(RowsAcc)} of
        {[], 0} -> [];
        {[], L} when L > 0 -> RowsAcc;
        {Rows, _} ->
            NewRows = [R || R <- case Fun of
                next_rows -> RowsAcc ++ Rows;
                prev_rows -> Rows ++ RowsAcc
            end, list_to_integer(lists:nth(1, R)) >= RowNum],
            RowNum1 = if length(NewRows) > 0 ->
                        list_to_integer(lists:nth(1, lists:nth(1, NewRows)));
                      true -> 0
            end,
            if (RowNum1 =< RowNum) or (RowNum < 1) ->
                dderl_session:log(SrvPid, "[~p] rows ~p starting ~p~n", [StmtKey, length(NewRows), RowNum]),
                if (length(NewRows) =< 0) or (length(NewRows) < ?DEFAULT_ROW_SIZE) ->
                        fetch_rows(Statement, RowNum, next_rows, StmtKey, SrvPid, NewRows);
                    true ->
                        NewRows
                end;
            (RowNum >= 1) and (RowNum1 > RowNum)->
                fetch_rows(Statement, RowNum, prev_rows, StmtKey, SrvPid, NewRows)
            end
    end.

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

%% Import sqls
import_sql(User, Path) ->
    [Acc|_]=imem_if:read(accounts, User),
    Files = [#file{name=Fn, content=binary_to_list(Fc)}
            ||{Fn,{ok,Fc}}<-[{filename:basename(F),file:read_file(F)}
                            ||F<-filelib:wildcard(Path++"/*.sql")]],
    NewAcc = Acc#accounts{db_files = Files},
    imem_if:insert_into_table(accounts, NewAcc).
