-module(gen_adapter).

-include("dderl.hrl").

-export([ process_cmd/2
        , prepare_json_rows/4
        , init/0
        , string_list_to_json/1
        , rows_to_json/1
        , add_cmds_views/2
        ]).

init() -> ok.

add_cmds_views(_, []) -> ok;
add_cmds_views(A, [{N,C}|Rest]) ->
    Id = dderl_dal:add_command(A, N, C, []),
    dderl_dal:add_view(N, Id, #viewstate{}),
    add_cmds_views(A, Rest).

process_cmd({"parse_stmt", BodyJson}, Priv) ->
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    case sql_walk:to_json(Query) of
        {error, Error} ->
            lager:debug("parsing Error ~p~n", [{Query, Error}]),
            {Priv, "{\"error\":\"ERROR: check log for details\"}"};
        Json -> {Priv, Json}
    end;
process_cmd({"get_query", BodyJson}, Priv) ->
    Table = binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>)),
    Query = "SELECT * FROM " ++ Table,
    lager:debug("get query ~p~n", [Query]),
    {Priv, "{\"qry\":{\"name\":\""++Table++"\",\"content\":\""++Query++"\"}}"};
process_cmd({Cmd, _BodyJson}, Priv) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {Priv, "{\"rows\":[]}"}.

process_data(Rows, more, CacheSize) ->
    "{\"done\":false, \"rows\":"++rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}";
process_data(Rows, _, CacheSize) ->
    JsonRows = rows_to_json(Rows),
    "{\"done\":true,  \"rows\":"++JsonRows++", \"cache_max\":"++integer_to_list(CacheSize)++"}".

%prepare_json_rows(Cmd, -2, Statement, StmtKey, SrvPid, Fun) ->
%    {Rows, Status, CacheSize} = apply(Statement, next_rows, []),
%    case Status of
%        more -> prepare_json_rows(Cmd, -2, Statement, StmtKey, SrvPid, Fun);
%        _ ->
%            if length(Rows) > 0 -> lager:debug("[~p] next_rows end table ~p~n", [StmtKey, length(Rows)]); true -> ok end,
%            process_data(Rows, Status, CacheSize)
%    end;
prepare_json_rows(C, RowNum, Statement, StmtKey) when RowNum >= 0, is_atom(C) ->
    {Rows, Status, CacheSize} = apply(Statement, rows_from, [RowNum]),
    if length(Rows) > 0 -> lager:debug("[~p] rows_from rows ~p starting ~p~n", [StmtKey, length(Rows), RowNum]); true -> ok end,
    process_data(Rows, Status, CacheSize);
prepare_json_rows(prev, RowNum, Statement, StmtKey) ->
    prepare_json_rows(Statement, RowNum, prev_rows, StmtKey);
prepare_json_rows(next, RowNum, Statement, StmtKey) ->
    prepare_json_rows(Statement, RowNum, next_rows, StmtKey);
prepare_json_rows(Statement, RowNum, Fun, StmtKey) ->
    {Rows, Status, CacheSize} = apply(Statement, Fun, []),
    if length(Rows) > 0 -> lager:debug("[~p] ~p rows ~p starting ~p~n", [StmtKey, Fun, length(Rows), RowNum]); true -> ok end,
    process_data(lists:reverse(Rows), Status, CacheSize).

string_list_to_json(Strings) ->
    NewStrings =
        lists:foldl(fun
           (S, Acc) when is_atom(S)  -> [atom_to_list(S)|Acc];
           (S, Acc) when is_tuple(S) -> [lists:nth(1, io_lib:format("~p", [S]))|Acc];
           (S, Acc)                  -> [S|Acc]
           
        end,
        [],
        Strings),
    string_list_to_json(NewStrings, "").
string_list_to_json([], []) -> "[]";
string_list_to_json([], Json) -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
string_list_to_json([S|Strings], Json) ->
    string_list_to_json(Strings, Json ++ "\"" ++ lists:flatten([if X > 127 -> "&#" ++ integer_to_list(X) ++ ";";
                                                                   (X == 10) or (X == 13) -> "";
                                                                   (X == $") -> "";
                                                                   true -> X
                                                               end || X <- S]) ++ "\",").

rows_to_json(Rows) -> rows_to_json(Rows, "").
rows_to_json([], Json) when length(Json) > 0 -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
rows_to_json([], _)                          -> "[]";
rows_to_json([Row|Rows], Json)               ->
    rows_to_json(Rows, Json ++ string_list_to_json(row_to_json_str(Row)) ++ ",").

row_to_json_str(Row)                -> row_to_json_str(Row, []).
row_to_json_str([], NewRow)         -> NewRow;
row_to_json_str([R|Row], NewRow)    -> row_to_json_str(Row, [lists:flatten(io_lib:format("~p", [R]))|NewRow]).

% - convert_row_to_string([]) -> [];
% - convert_row_to_string(Row) -> [lists:flatten(io_lib:format("~p", [R])) || R <- Row].
% - 
% - convert_rows_to_string([]) -> [];
% - convert_rows_to_string(Rows) -> [lists:reverse([lists:flatten(io_lib:format("~p", [R])) || R <- Row]) || Row <- Rows].
