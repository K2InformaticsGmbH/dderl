-module(dderl_cow_mw).
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
            Resp1 = cowboy_req:set_resp_headers(
                             RespHeaders#{<<"server">> => <<"dderl">>}, Req),
            {ok, Resp1#{'_dderl' => true}, Env}
    end.
