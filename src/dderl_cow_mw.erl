-module(dderl_cow_mw).
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
    {Path, Req0} = cowboy_req:path(Req0),
    Req1 = cowboy_req:set_resp_header(<<"x-frame-options">>, "DENY", Req0),
    Req2 = cowboy_req:set_resp_header(<<"x-xss-protection">>, "1", Req1),
    Req3 = cowboy_req:set_resp_header(<<"x-content-type-options">>, "nosniff", Req2),
    case re:run(Path, "^" ++ dderl:get_url_suffix()) of
        nomatch ->
            {ok, Req3, Env};
        _ ->
            Req = cowboy_req:set_resp_header(<<"server">>, "dderl", Req3),
            {ok, cowboy_req:set_meta(dderl, true, Req), Env}
    end.
