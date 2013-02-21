-module(gen_adapter).

-include("dderl.hrl").
-include_lib("sqlparse/src/sql_box.hrl").

-export([ process_cmd/2
        , prepare_json_rows/4
        , init/0
        , strs2bins/1
        , add_cmds_views/2
        ]).

init() -> ok.

add_cmds_views(_, []) -> ok;
add_cmds_views(A, [{N,C}|Rest]) ->
    Id = dderl_dal:add_command(A, N, C, []),
    dderl_dal:add_view(N, Id, #viewstate{}),
    add_cmds_views(A, Rest);
add_cmds_views(A, [{N,C,#viewstate{}=V}|Rest]) ->
    Id = dderl_dal:add_command(A, N, C, []),
    dderl_dal:add_view(N, Id, V),
    add_cmds_views(A, Rest).

box_to_json(Box) ->
[
    {<<"box">>, [
        {<<"ind">>, Box#box.ind}
        , {<<"name">>, any_to_bin(Box#box.name)}
        , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
        %, {<<"collapsed">>, Box#box.collapsed}
        , {<<"collapsed">>, false}
        , {<<"error">>, Box#box.error}
        , {<<"color">>, Box#box.color}
        , {<<"pick">>, Box#box.pick}
    ]}
].

any_to_bin(C) when is_list(C) -> list_to_binary(C);
any_to_bin(C) when is_binary(C) -> C;
any_to_bin(C) -> list_to_binary(lists:nth(1, io_lib:format("~p", [C]))).
    
process_cmd({"parse_stmt", ReqBody}, Priv) ->
    [{<<"parse_stmt">>,BodyJson}] = ReqBody,
    Sql = string:strip(binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>))),
    ?Info("parsing ~p", [Sql]),
    %% Sql = case string:substr(Query, length(Query), 1) of
    %% ";" -> Query;
    %% _ -> Query ++ ";"
    %% end,
    case (catch jsx:encode([{<<"parse_stmt">>, [
        case (catch sql_box:boxed(Sql)) of
            {'EXIT', ErrorBox} -> {<<"boxerror">>, ErrorBox};
            Box ->
                BoxJson = box_to_json(Box),
                ?Debug("--- Box --- ~n~p~n--- Json ---~n~p~n", [Box, BoxJson]),
                {<<"box">>, BoxJson}
        end,
        case (catch sql_box:pretty(Sql)) of
            {'EXIT', ErrorPretty} -> {<<"prettyerror">>, ErrorPretty};
            Pretty -> {<<"pretty">>, list_to_binary(Pretty)}
        end,
        case (catch sql_box:flat(Sql)) of
            {'EXIT', ErrorFlat} -> {<<"flaterror">>, ErrorFlat};
            Flat -> {<<"flat">>, list_to_binary(Flat)}
        end
    ]}])) of
        ParseStmt when is_binary(ParseStmt) -> {Priv, binary_to_list(ParseStmt)};
        Error ->
            ?Error("parse_stmt error ~p~n", [Error]),
            ReasonBin = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {Priv, binary_to_list(jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}]))}
    end;
    %% case sql_lex:string(Sql) of
    %% {ok, Tokens, _} ->
    %%     case sql_parse:parse(Tokens) of
    %%     {ok, [ParseTree|_]} -> 
    %%         case sql_box:box_tree(ParseTree) of
    %%         {error, Error} ->
    %%             ?Error("box generator ~p~n", [{Sql, ParseTree, Error}]),
    %%             {Priv, binary_to_list(jsx:encode([{<<"parse_stmt">>, [{<<"error">>, <<"ERROR: check log for details">>}]}]))};
    %%         Box ->
    %%             BoxJson = jsx:encode(box_to_json(Box)),
    %%             ?Debug("box ~p~n", [BoxJson]),
    %%             {Priv, binary_to_list(BoxJson)}
    %%         end;
    %%     Error -> 
    %%         ?Error("parser ~p~n", [{Sql, Tokens, Error}]),
    %%         {Priv, binary_to_list(jsx:encode([{<<"parse_stmt">>, [{<<"error">>, <<"ERROR: check log for details">>}]}]))}
    %%     end;
    %% Error ->
    %%     ?Error("lexer ~p~n", [{Sql, Error}]),
    %%     {Priv, binary_to_list(jsx:encode([{<<"parse_stmt">>, [{<<"error">>, <<"ERROR: check log for details">>}]}]))}
    %% end;
process_cmd({"get_query", ReqBody}, Priv) ->
    [{<<"get_query">>,BodyJson}] = ReqBody,
    Table = proplists:get_value(<<"table">>, BodyJson, <<>>),
    Query = "SELECT * FROM " ++ binary_to_list(Table),
    ?Debug("get query ~p~n", [Query]),
    Res = jsx:encode([{<<"qry">>,[{<<"name">>,Table},{<<"content">>,list_to_binary(Query)}]}]),
    {Priv, binary_to_list(Res)};
process_cmd({"save_view", ReqBody}, Priv) ->
    [{<<"save_view">>,BodyJson}] = ReqBody,
    Name = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>)),
    Query = binary_to_list(proplists:get_value(<<"content">>, BodyJson, <<>>)),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, <<>>),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, <<>>),
    ?Info("save_view for ~p layout ~p", [Name, TableLay]),
    gen_adapter:add_cmds_views(imem, [{Name, Query, #viewstate{table_layout=TableLay, column_layout=ColumLay}}]),
    Res = jsx:encode([{<<"save_view">>,<<"ok">>}]),
    {Priv, binary_to_list(Res)};
process_cmd({Cmd, _BodyJson}, Priv) ->
    io:format(user, "Unknown cmd ~p ~p~n", [Cmd, _BodyJson]),
    {Priv, binary_to_list(jsx:encode([{<<"error">>, <<"unknown command">>}]))}.

%prepare_json_rows(Cmd, -2, Statement, StmtKey, SrvPid, Fun) ->
%    {Rows, Status, CacheSize} = apply(Statement, next_rows, []),
%    case Status of
%        more -> prepare_json_rows(Cmd, -2, Statement, StmtKey, SrvPid, Fun);
%        _ ->
%            if length(Rows) > 0 -> ?Debug("[~p] next_rows end table ~p~n", [StmtKey, length(Rows)]); true -> ok end,
%            process_data(Rows, Status, CacheSize)
%    end;
prepare_json_rows(C, RowNum, Statement, StmtKey) when RowNum >= 0, is_atom(C) ->
    {Rows, Status, CacheSize} = apply(Statement, rows_from, [RowNum]),
    if length(Rows) > 0 -> ?Debug("[~p] rows_from rows ~p starting ~p~n", [StmtKey, length(Rows), RowNum]);
                   true -> ok end,
    process_data(Rows, Status, CacheSize);
prepare_json_rows(prev, RowNum, Statement, StmtKey) ->
    prepare_json_rows(Statement, RowNum, prev_rows, StmtKey);
prepare_json_rows(next, RowNum, Statement, StmtKey) ->
    prepare_json_rows(Statement, RowNum, next_rows, StmtKey);
prepare_json_rows(Statement, RowNum, Fun, StmtKey) ->
    {Rows, Status, CacheSize} = apply(Statement, Fun, []),
    if length(Rows) > 0 -> ?Debug("[~p] ~p rows ~p starting ~p~n", [StmtKey, Fun, length(Rows), RowNum]); true -> ok end,
    process_data(lists:reverse(Rows), Status, CacheSize).

process_data(Rows, Status, CacheSize) ->
    [{<<"done">>, Status}, {<<"rows">>, rows_to_json1(Rows)}, {<<"cache_max">>, CacheSize}].
%% ;
%% process_data(Rows, _, CacheSize) ->
%%     [{<<"done">>, true}, {<<"rows">>, rows_to_json1(Rows)}, {<<"cache_max">>, CacheSize}].

strs2bins(Strings) ->
    lists:foldl(fun
        (S, Acc) when is_atom(S)  -> [list_to_binary(atom_to_list(S))|Acc];
        (S, Acc) when is_tuple(S) -> [list_to_binary(lists:nth(1, io_lib:format("~p", [S])))|Acc];
        (S, Acc)                  -> [list_to_binary(S)|Acc]
    end,
    [],
    Strings).

rows_to_json1(Rows) -> rows_to_json1(Rows, []).
rows_to_json1([], NewRows) -> lists:reverse(NewRows);
rows_to_json1([Row|Rows], NewRows) -> rows_to_json1(Rows, [strs2bins(lists:reverse(Row))|NewRows]).
