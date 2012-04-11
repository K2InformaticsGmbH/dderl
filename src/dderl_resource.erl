%% @author Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>
%% @copyright 2012-2050 K2 Informatics GmbH.  All Rights Reserved.
%% @doc dderl Resource.

-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').
-export([init/1,
        %content_types_provided/2,
        is_authorized/2,
        generate_etag/2,
        expires/2,
        process_post/2,
        allowed_methods/2,
        malformed_request/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(VERSION, "1.0").

init([]) ->
    {{trace, "./priv/log"}, undefined}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'POST'], ReqData, Context}.

process_post(ReqData, Context) ->
    Body = to_html(ReqData),
    %io:format(user, "POST Response ~p~n", [Body]),
    ReqData1 = wrq:set_resp_body(Body, ReqData),
    {true, ReqData1, Context}.

%content_types_provided(ReqData, Context) ->
%    io:format(user, "Got so far...~n", []),
%    {[{"text/html", to_html},
%      {"application/json", do_req}
%     ], ReqData, Context}.

malformed_request(ReqData, Context) ->
    case wrq:req_body(ReqData) of
        undefined -> {halt, 401};
        _ -> {false, ReqData, Context}
    end.

to_html(ReqData) ->
    Session = wrq:get_req_header("dderl_sess",ReqData),
    {SessKey, DderlSess} = create_new_session(Session),
    DderlSess:process_request(SessKey, ReqData).

create_new_session([]) -> create_new_session(undefined);
create_new_session(undefined) ->
    {Key, DderlSess} = dderl_session:start(),
    io:format(user, "new dderl_sess ~p~n", [{Key, DderlSess}]),
    R = ets:insert(dderl_req_sessions, {Key, DderlSess}),
    io:format(user, "session inserted to ETS ~p~n", [R]),
    {Key, DderlSess};
create_new_session(Session) ->
    case ets:lookup(dderl_req_sessions, list_to_integer(Session)) of
        [] -> create_new_session(undefined);
        [DderlSessTup|_] -> DderlSessTup
    end.

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
