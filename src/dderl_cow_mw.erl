-module(dderl_cow_mw).
-behavior(cowboy_middleware).

-include("dderl.hrl").

-export([execute/2]).

execute(Req, Env) ->
    {Path, Req} = cowboy_req:path(Req),
    case re:run(Path, "^"++?URLSUFFIX) of
        nomatch ->
            %?Info("NOT DDErl ~p", [Path]),
            {ok, Req, Env};
        _ ->
            %?Info("Env ~p", [Env]),
            %?Info("DDErl ~p", [Path]),
            {ok, cowboy_req:set_meta(dderl, true, Req), Env}
    end.
