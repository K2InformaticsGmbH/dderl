-ifndef(DDERL_REST_HRL).
-define(DDERL_REST_HRL, true).

-define(REPLY_HEADERS,
        #{<<"access-control-allow-origin">> => <<"*">>,
          <<"cache-control">> => <<"no-cache, no-store, must-revalidate">>,
          <<"server">> => <<?SERVER>>}).

-define(REPLY_JSON_HEADERS,
        maps:merge(#{<<"content-type">> => <<"application/json; charset=utf-8">>},
                   ?REPLY_HEADERS)).

-define(REPLY_JSON_SPEC_HEADERS, ?REPLY_JSON_SPEC_HEADERS(?SPEC_FILE)).

-define(REPLY_OPT_HEADERS,
        maps:merge(#{<<"connection">> => <<"close">>}, ?REPLY_HEADERS)).

-define(REPLY_JSON_SPEC_HEADERS(__SPEC_FILE__), 
        maps:merge(#{<<"connection">> => <<"close">>,
                <<"content-type">> => <<"application/json; charset=UTF-8">>,
                <<"content-disposition">> => list_to_binary(["attachment; filename=\"",__SPEC_FILE__,"\""])},
         ?REPLY_HEADERS)).

-endif. % DDERL_REST_HRL
