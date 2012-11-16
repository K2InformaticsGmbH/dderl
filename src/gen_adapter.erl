-module(gen_adapter).

-export([ process_cmd/3
        , prepare_json_rows/5
        , init/0
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
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.

process_data(Rows, more, CacheSize) -> "{\"done\":false, \"rows\":"++dderl_session:convert_rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}";
process_data(Rows, _, CacheSize)    -> "{\"done\":true,  \"rows\":"++dderl_session:convert_rows_to_json(Rows)++", \"cache_max\":"++integer_to_list(CacheSize)++"}".

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
    process_data(Rows, Status, CacheSize).
