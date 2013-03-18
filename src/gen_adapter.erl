-module(gen_adapter).

-include("dderl.hrl").
-include_lib("sqlparse/src/sql_box.hrl").
-include_lib("erlimem/src/gres.hrl").

-export([ process_cmd/2
        , init/0
        , strs2bins/1
        , add_cmds_views/2
        , gui_resp/1
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
    [{<<"ind">>, Box#box.ind}
    , {<<"name">>, any_to_bin(Box#box.name)}
    , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
    , {<<"collapsed">>, Box#box.collapsed}
    , {<<"error">>, Box#box.error}
    , {<<"color">>, Box#box.color}
    , {<<"pick">>, Box#box.pick}].

any_to_bin(C) when is_list(C) -> list_to_binary(C);
any_to_bin(C) when is_binary(C) -> C;
any_to_bin(C) -> list_to_binary(lists:nth(1, io_lib:format("~p", [C]))).
    
process_cmd({[<<"parse_stmt">>], ReqBody}, Priv) ->
    [{<<"parse_stmt">>,BodyJson}] = ReqBody,
    Sql = string:strip(binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>))),
    ?Info("parsing ~p", [Sql]),
    case (catch jsx:encode([{<<"parse_stmt">>, [
        try
            Box = sql_box:boxed(Sql),
            ?Debug("--- Box --- ~n~p", [Box]),
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
            ?Debug("Json -- "++binary_to_list(jsx:prettify(ParseStmt))),
            {Priv, binary_to_list(ParseStmt)};
        Error ->
            ?Error("parse_stmt error ~p~n", [Error]),
            ReasonBin = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {Priv, binary_to_list(jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}]))}
    end;
process_cmd({[<<"get_query">>], ReqBody}, Priv) ->
    [{<<"get_query">>,BodyJson}] = ReqBody,
    Table = proplists:get_value(<<"table">>, BodyJson, <<>>),
    Query = "SELECT * FROM " ++ binary_to_list(Table),
    ?Debug("get query ~p~n", [Query]),
    Res = jsx:encode([{<<"qry">>,[{<<"name">>,Table},{<<"content">>,list_to_binary(Query)}]}]),
    {Priv, binary_to_list(Res)};
process_cmd({[<<"save_view">>], ReqBody}, Priv) ->
    [{<<"save_view">>,BodyJson}] = ReqBody,
    Name = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>)),
    Query = binary_to_list(proplists:get_value(<<"content">>, BodyJson, <<>>)),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, <<>>),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, <<>>),
    ?Info("save_view for ~p layout ~p", [Name, TableLay]),
    add_cmds_views(imem, [{Name, Query, undefined, #viewstate{table_layout=TableLay, column_layout=ColumLay}}]),
    Res = jsx:encode([{<<"save_view">>,<<"ok">>}]),
    {Priv, binary_to_list(Res)};
process_cmd({Cmd, _BodyJson}, Priv) ->
    io:format(user, "Unknown cmd ~p ~p~n", [Cmd, _BodyJson]),
    {Priv, binary_to_list(jsx:encode([{<<"error">>, <<"unknown command">>}]))}.

gui_resp(#gres{} = Gres) ->
    ?Info("processing resp ~p", [Gres]),
    [{<<"op">>,         Gres#gres.operation} %% rep (replace) | app (append) | prp (prepend) | nop | close
    ,{<<"cnt">>,        Gres#gres.cnt}         %% current buffer size (raw table or index table size)
    ,{<<"toolTip">>,    Gres#gres.toolTip}     %% current buffer sizes RawCnt/IndCnt plus status information
    ,{<<"message">>,    Gres#gres.message}     %% error message
    ,{<<"beep">>,       Gres#gres.beep}        %% alert with a beep if true
    ,{<<"state">>,      Gres#gres.state}       %% determines color of buffer size indicator
    ,{<<"loop">>,       Gres#gres.loop}        %% gui should come back with this command
    ,{<<"rows">>,       r2jsn(Gres#gres.rows)} %% rows .. show (append / prepend / merge)
    ,{<<"keep">>,       Gres#gres.keep}].      %% row count .. be kept

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

r2jsn(Rows) -> r2jsn(Rows, []).
r2jsn([], NewRows) -> lists:reverse(NewRows);
r2jsn([Row|Rows], NewRows) -> r2jsn(Rows, [strs2bins(lists:reverse(Row))|NewRows]).
