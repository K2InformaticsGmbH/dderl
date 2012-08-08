-module(gen_adapter).

-export([process_cmd/3]).

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
