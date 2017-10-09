-module(dderl_cow_mw).
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    %% WARNING: changing x-frame-options from SAMEORIGIN to DENY will break the all file downloads through browsers (IE, Chrome).
    Req1 = cowboy_req:set_resp_headers(#{<<"x-frame-options">> => <<"SAMEORIGIN">>,
                                        <<"x-xss-protection">> => <<"1">>,
                                        <<"x-content-type-options">> => <<"nosniff">>
                                       }, Req),
    case re:run(Path, [$^ | dderl:get_url_suffix()]) of
        nomatch ->
            {ok, Req1, Env};
        _ ->
            Req2 = cowboy_req:set_resp_headers(#{<<"server">> => <<"dderl">>}, Req1),
            {ok, Req2#{'_dderl' => true}, Env}
    end.
