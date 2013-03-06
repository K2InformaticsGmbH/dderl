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
add_cmds_views(A, [{N,C,Con}|Rest]) ->
    Id = dderl_dal:add_command(A, N, C, Con, []),
    dderl_dal:add_view(N, Id, #viewstate{}),
    add_cmds_views(A, Rest);
add_cmds_views(A, [{N,C,Con,#viewstate{}=V}|Rest]) ->
    Id = dderl_dal:add_command(A, N, C, Con, []),
    dderl_dal:add_view(N, Id, V),
    add_cmds_views(A, Rest).

box_to_json(Box) ->
[
    {<<"box">>, [
        {<<"ind">>, Box#box.ind}
        , {<<"name">>, any_to_bin(Box#box.name)}
        , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
        , {<<"collapsed">>, Box#box.collapsed}
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
    case (catch jsx:encode([{<<"parse_stmt">>, [
        try
            Box = sql_box:boxed(Sql),
            ?Info("--- Box --- ~n~p", [Box]),
            {<<"sqlbox">>, box_to_json(Box)}
        catch
            _:ErrorBox -> {<<"boxerror">>, list_to_binary(lists:flatten(io_lib:format("~p", [ErrorBox])))}
        end,
        try
            Pretty = sql_box:pretty(Sql),
            {<<"pretty">>, list_to_binary(Pretty)}
        catch
            _:ErrorPretty -> {<<"prettyerror">>, list_to_binary(lists:flatten(io_lib:format("~p", [ErrorPretty])))}
        end,
        try
            Flat = sql_box:flat(Sql),
            {<<"flat">>, list_to_binary(Flat)}
        catch
            _:ErrorFlat -> {<<"flaterror">>, list_to_binary(lists:flatten(io_lib:format("~p", [ErrorFlat])))}
        end
    ]}])) of
        ParseStmt when is_binary(ParseStmt) ->
            ?Info("Json -- "++binary_to_list(jsx:prettify(ParseStmt))),
            {Priv, binary_to_list(ParseStmt)};
        Error ->
            ?Error("parse_stmt error ~p~n", [Error]),
            ReasonBin = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {Priv, binary_to_list(jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}]))}
    end;
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
    V = widest_cell_per_clm(Rows),
    ?Debug("the maxes ~p", [V]),
    [ {<<"done">>, Status}
    , {<<"max_width_vec">>, V}
    , {<<"cache_max">>, CacheSize}
    , {<<"rows">>, rows_to_json1(Rows)}
    ].

widest_cell_per_clm([]) -> [];
widest_cell_per_clm(Rows) -> widest_cell_per_clm(Rows, lists:duplicate(length(lists:nth(1,Rows)), "")).

widest_cell_per_clm([],V) -> [list_to_binary(Ve) || Ve <- V];
widest_cell_per_clm([R|Rows],V) ->
    NewV = 
    [case {Re, Ve} of
        {Re, Ve} ->
            ReL = length(Re),
            VeL = length(Ve),
            if ReL > VeL -> Re; true -> Ve end
     end
    || {Re, Ve} <- lists:zip(R,V)],
    widest_cell_per_clm(Rows,NewV).

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
