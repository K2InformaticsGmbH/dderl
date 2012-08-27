-module(oci_adapter).

-include("dderl.hrl").

-export([ process_cmd/3
        , init/0
        , import_sql/2
        ]).

init() ->
    imem_if:insert_into_table(common, {?MODULE, [
                #file{name="Users.sql",
                      content="SELECT USERNAME FROM ALL_USERS",
                      posX=0, posY=20, width=200, height=500}
              , #file{name="Tables.sql",
                      %content="SELECT OWNER, TABLE_NAME FROM ALL_TABLES WHERE OWNER=user ORDER BY TABLE_NAME",
                      content="SELECT OWNER, TABLE_NAME FROM ALL_TABLES ORDER BY TABLE_NAME",
                      posX=0, posY=20, width=200, height=500}
              , #file{name="Views.sql",
                      %content="SELECT OWNER, VIEW_NAME FROM ALL_VIEWS WHERE OWNER=user ORDER BY VIEW_NAME",
                      content="SELECT DISTINCT OWNER, VIEW_NAME  FROM ALL_VIEWS ORDER BY VIEW_NAME",
                      posX=0, posY=20, width=200, height=500}
            ]}).

process_cmd({"connect", BodyJson}, SrvPid, _) ->
    IpAddr   = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    Service  = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Type     = binary_to_list(proplists:get_value(<<"type">>, BodyJson, <<>>)),
    User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    Tnsstr   = binary_to_list(proplists:get_value(<<"tnsstring">>, BodyJson, <<>>)),
    dderl_session:log(SrvPid, "Params ~p~n", [{IpAddr, Port, Service, Type, User, Password, Tnsstr}]),
    {ok, Pool} =
        if length(Tnsstr) > 0 ->
            oci_session_pool:start_link(Tnsstr, User, Password, []);
        Service =/= "MOCK" ->
            oci_session_pool:start_link(IpAddr, Port, {list_to_atom(Type), Service}, User, Password, []);
        true ->
            oci_session_pool:start_link(IpAddr, Port,  {list_to_atom(Type), "db.local"},  User, Password, [{port_options, [{mock_port, oci_port_mock}]}])
    end,
    %%oci_session_pool:enable_log(Pool),
    case oci_session_pool:get_session(Pool) of
        {error, Error} ->
            {{undefined,Pool,[]}, "{\"connect\":false, \"msg\":\""++re:replace(Error, "(\\n)", "", [global, {return, list}])++"\"}"};
        Session -> {{Session,Pool,[]}, "{\"connect\":true}"}
    end;
process_cmd({"get_query", BodyJson}, SrvPid, {Session,Pool,Statements}) ->
    Table = binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>)),
    Query = "SELECT * FROM " ++ Table,
    dderl_session:log(SrvPid, "[~p] get query ~p~n", [SrvPid, Query]),
    {{Session,Pool,Statements}, "{\"qry\":"++dderl_session:create_files_json([#file{name=Table, content=Query}])++"}"};
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
process_cmd({"get_buffer_max", BodyJson}, SrvPid, {_,_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p] Statement ~p not found. Statements ~p~n", [SrvPid, StmtKey, proplists:get_keys(Statements)]),
            {MPort, "-1"};
        {Statement, _, _} ->
            {ok, Finished, CacheSize} = Statement:get_buffer_max(),
            dderl_session:log(SrvPid, "[~p, ~p] get_buffer_max ~p~n", [SrvPid, StmtKey, CacheSize]),
            {MPort, "{\"count\":"++integer_to_list(CacheSize)++", \"finished\":"++atom_to_list(Finished)++"}"}
    end;
process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort);
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.

prepare_json_rows(C, RowNum, Statement, StmtKey, SrvPid) when RowNum >= 0, is_atom(C) ->
    {Rows, Status, CacheSize} = apply(Statement, rows_from, [RowNum]),
    if length(Rows) > 0 -> dderl_session:log(SrvPid, "[~p] rows_from rows ~p starting ~p~n", [StmtKey, length(Rows), RowNum]); true -> ok end,
    process_data(Rows, Status, CacheSize);
prepare_json_rows(prev, RowNum, Statement, StmtKey, SrvPid) ->
    prepare_json_rows(Statement, RowNum, prev_rows, StmtKey, SrvPid);
prepare_json_rows(next, RowNum, Statement, StmtKey, SrvPid) ->
    prepare_json_rows(Statement, RowNum, next_rows, StmtKey, SrvPid);
prepare_json_rows(Statement, RowNum, Fun, StmtKey, SrvPid) ->
    {Rows, Status, CacheSize} = apply(Statement, Fun, []),
    if length(Rows) > 0 -> dderl_session:log(SrvPid, "[~p] ~p rows ~p starting ~p~n", [StmtKey, Fun, length(Rows), RowNum]); true -> ok end,
    process_data(Rows, Status, CacheSize).

process_data(Rows, more, CacheSize) -> "{\"done\":false, \"rows\":"++dderl_session:convert_rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}";
process_data(Rows, _, CacheSize)    -> "{\"done\":true,  \"rows\":"++dderl_session:convert_rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}".

%% Import sqls
import_sql(User, Path) ->
    [Acc|_]=imem_if:read(accounts, User),
    Files = [#file{name=Fn, content=binary_to_list(Fc)}
            ||{Fn,{ok,Fc}}<-[{filename:basename(F),file:read_file(F)}
                            ||F<-filelib:wildcard(Path++"/*.sql")]],
    NewAcc = Acc#accounts{db_files = Files},
    imem_if:insert_into_table(accounts, NewAcc).
