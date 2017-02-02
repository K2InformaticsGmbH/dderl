-ifndef(DDERL_REST_HRL).
-define(DDERL_REST_HRL, true).

-define(REPLY_HEADERS,
        [{<<"access-control-allow-origin">>,<<"*">>},
         {<<"cache-control">>,<<"no-cache, no-store, must-revalidate">>},
         {<<"server">>, <<?SERVER>>}]).
-define(REPLY_JSON_HEADERS,
        [{<<"content-encoding">>, <<"utf-8">>},
         {<<"content-type">>, <<"application/json">>}
         | ?REPLY_HEADERS]).
-define(REPLY_JSON_SPEC_HEADERS,
        [{<<"connection">>, <<"close">>},
         {<<"content-type">>,
          <<"application/json; charset=UTF-8">>},
         {<<"content-disposition">>,
          <<"attachment; filename=\"",?SPEC_FILE,"\"">>}
         | ?REPLY_HEADERS]).
-define(REPLY_OPT_HEADERS,
        [{<<"connection">>, <<"close">>} | ?REPLY_HEADERS]).

-endif. % DDERL_REST_HRL
