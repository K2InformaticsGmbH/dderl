-ifndef(DDERL_REQUEST_HRL).
-define(DDERL_REQUEST_HRL, true).

% Cowboy meta data set and get macros
-define(COW_REQ_GET_META(Key, Req, Default),
    dderl:cow_req_get_meta(application:get_application(?MODULE), Key, Req, Default)
).
-define(COW_REQ_SET_META(Key, Value, Req),
    dderl:cow_req_set_meta(application:get_application(?MODULE), Key, Value, Req)
).

-endif.