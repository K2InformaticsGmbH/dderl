%% @author Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>
%% @copyright 2012-2050 K2 Informatics GmbH.  All Rights Reserved.
%% @doc dderl Resource.

-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-export([init/1,
        content_types_provided/2,
        is_authorized/2,
        generate_etag/2,
        expires/2,
        process_post/2,
        allowed_methods/2,
        malformed_request/2]).

-include_lib("dderl.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    lager:debug("~p starting...", [?MODULE]),
    {{trace, "./priv/log"}, undefined}.

allowed_methods(ReqData, Context) ->
    lager:debug("allowed_methods ~p", [['HEAD', 'POST']]),
    {['HEAD', 'POST'], ReqData, Context}.

process_post(ReqData, Context) ->
    {DDerlSessPid, Body} = to_html(ReqData),
    lager:debug("process_post - POST Response ~p~n", [Body]),
    ReqData1 = wrq:set_resp_body(Body, ReqData),
    ReqData2 = wrq:set_resp_header("dderl_sess", ?EncryptPid(DDerlSessPid), ReqData1),
    lager:debug("received request ~p", [ReqData]),
    {true, ReqData2, Context}.

content_types_provided(ReqData, Context) ->
    lager:debug("content_types_provided ...~n"),
    {[{"text/html", to_html},
      {"application/json", to_html}
     ], ReqData, Context}.

malformed_request(ReqData, Context) ->
    case wrq:req_body(ReqData) of
        undefined ->
            lager:error("received malformed request ~p", [wrq:disp_path(ReqData)]),
            lager:debug("malformed request ~p", [ReqData]),
            {halt, 401};
        _ -> {false, ReqData, Context}
    end.

to_html(ReqData) ->
    Session = wrq:get_req_header("dderl_sess",ReqData),
    {_,DDerlSessPid} = DderlSess = create_new_session(Session),
    case wrq:get_req_header("adapter",ReqData) of
        undefined -> ok;
        Adapter   -> DderlSess:set_adapter(Adapter)
    end,
    {DDerlSessPid, DderlSess:process_request(ReqData)}.

create_new_session([]) -> create_new_session(undefined);
create_new_session(undefined) ->
    DderlSess = dderl_session:start(),
    lager:info("new dderl session ~p", [{DderlSess}]),
    DderlSess;
create_new_session(DDerlSessPid) -> {dderl_session, ?DecryptPid(DDerlSessPid)}.

is_authorized(ReqData, Context) ->
    case wrq:disp_path(ReqData) of
        "authdemo" ->
            case wrq:get_req_header("authorization", ReqData) of
                "Basic "++Base64 ->
                    Str = base64:mime_decode_to_string(Base64),
                    case string:tokens(Str, ":") of
                        ["authdemo", "demo1"] ->
                            {true, ReqData, Context};
                        _ ->
                            {"Basic realm=k2informatics", ReqData, Context}
                    end;
                _ ->
                    {"Basic realm=k2informatics", ReqData, Context}
            end;
        _ -> {true, ReqData, Context}
    end.

expires(ReqData, Context) -> {{{2021,1,1},{0,0,0}}, ReqData, Context}.

generate_etag(ReqData, Context) -> {wrq:raw_path(ReqData), ReqData, Context}.
