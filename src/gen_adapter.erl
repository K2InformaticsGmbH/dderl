-module(gen_adapter).

-export([ process_cmd/3
        , prepare_json_rows/5
        , init/0
        , string_list_to_json/2
%        , convert_rows_to_json/1
%        , convert_row_to_string/1
%        , convert_rows_to_string/1
        ]).

init() -> ok.

process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort) ->
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    case sql_walk:to_json(Query) of
        {error, Error} ->
            dderl_session:log(SrvPid, "[~p] Parsing Error ~p~n", [SrvPid, {Query, Error}]),
            {MPort, "{\"error\":\"ERROR: check log for details\"}"};
        Json -> {MPort, Json}
    end;
process_cmd({"get_query", BodyJson}, SrvPid, MPort) ->
    Table = binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>)),
    Query = "SELECT * FROM " ++ Table,
    dderl_session:log(SrvPid, "[~p] get query ~p~n", [SrvPid, Query]),
    {MPort, "{\"qry\":{\"name\":\""++Table++"\",\"content\":\""++Query++"\"}}"};
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.

process_data(Rows, more, CacheSize) ->
    io:format(user, "got rows ~p~n", [Rows]),
    "{\"done\":false, \"rows\":"++convert_rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}";
process_data(Rows, _, CacheSize) ->
    io:format(user, "got rows ~p~n", [Rows]),
    "{\"done\":true,  \"rows\":"++convert_rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}".

%prepare_json_rows(Cmd, -2, Statement, StmtKey, SrvPid) ->
%    {Rows, Status, CacheSize} = apply(Statement, next_rows, []),
%    case Status of
%        more -> prepare_json_rows(Cmd, -2, Statement, StmtKey, SrvPid);
%        _ ->
%            if length(Rows) > 0 -> dderl_session:log(SrvPid, "[~p] next_rows end table ~p~n", [StmtKey, length(Rows)]); true -> ok end,
%            process_data(Rows, Status, CacheSize)
%    end;
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

convert_rows_to_json(Rows) -> convert_rows_to_json(Rows, "").
convert_rows_to_json([], Json) when length(Json) > 0 -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
convert_rows_to_json([], _)                          -> "[]";
convert_rows_to_json([Row|Rows], Json)               -> convert_rows_to_json(Rows, Json ++ string_list_to_json(lists:reverse(Row)) ++ ",").


% - convert_row_to_string([]) -> [];
% - convert_row_to_string(Row) -> [lists:flatten(io_lib:format("~p", [R])) || R <- Row].
% - 
% - convert_rows_to_string([]) -> [];
% - convert_rows_to_string(Rows) -> [lists:reverse([lists:flatten(io_lib:format("~p", [R])) || R <- Row]) || Row <- Rows].
