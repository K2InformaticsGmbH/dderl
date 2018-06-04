-module(dderl_cow_mw).
-include("dderl_request.hrl").
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    %% WARNING: changing x-frame-options from SAMEORIGIN to DENY will break the all file downloads through browsers (IE, Chrome).
    RespHeaders = #{<<"x-frame-options">> => <<"SAMEORIGIN">>,
                    <<"x-xss-protection">> => <<"1">>,
                    <<"x-content-type-options">> => <<"nosniff">>},
    case re:run(Path, [$^ | dderl:get_url_suffix()]) of
        nomatch ->
            {ok, cowboy_req:set_resp_headers(RespHeaders, Req), Env};
        _ ->
            Req1 = cowboy_req:set_resp_headers(
                             RespHeaders#{<<"server">> => <<"dderl">>}, Req),
            {ok, ?COW_REQ_SET_META(dderl_request, true, Req1), Env}
    end.
