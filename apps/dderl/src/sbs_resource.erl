-module(sbs_resource).

-behaviour(cowboy_loop_handler).

-include("dderl.hrl").

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init({ssl, http}, Req, []) ->
    case cowboy_req:has_body(Req) of
    true ->
        {Session, Req1} = cowboy_req:header(<<"sbs-sess">>, Req),
        process_request(Session, Req1);
    _Else ->
        ?Error("Sbs request ~p, error ~p", [Req, _Else]),
        self() ! {reply, <<"{}">>},
        {loop, Req, <<>>, 5000, hibernate}
    end.

process_request(Session, Req) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {AuthorizationHeader, Req2} = cowboy_req:header(<<"authorization">>, Req1),
    {Typ, Req3} = cowboy_req:path_info(Req2),
    process_request_low(create_new_session(Session, AuthorizationHeader), Req3, Body, Typ).

process_request_low({ok, {_,SessPid} = Session}, Req, Body, Typ) ->
    Session:process_request(Typ, Body, self()),
    {loop, Req, SessPid, 3600000, hibernate};
process_request_low({error, unauthorized}, Req, _Body, _Typ) ->
    self() ! reply_unauthorized,
    {loop, Req, undefined, 5000, hibernate};
process_request_low({error, Reason, Session}, Req, _Body, _Typ) ->
    {{Ip, Port}, Req2} = cowboy_req:peer(Req),
    ?Error("Error processing request from ~s:~p, reason ~p", [imem_datatype:ipaddr_to_io(Ip), Port, Reason]),
    self() ! {reply, jsx:encode([{<<"error">>, <<"Unable to process the request">>}])},
    {loop, Req2, Session, 5000, hibernate}.

info(reply_unauthorized, Req, undefined) ->
    Req1 = cowboy_req:set_resp_header(<<"www-authenticate">>, <<"Basic realm=\"SBS Authentication\"">>, Req),
    Req2 = cowboy_req:set_resp_body(unauthorized_body(), Req1),
    {ok, Req3} = cowboy_req:reply(401, Req2),
    {ok, Req3, undefined};
info({reply, Body}, Req, DDerlSessPid) ->
    ?NoDbLog(debug, [], "reply ~n~s", [jsx:prettify(Body)]),
    {ok, Req2} = reply_200_json(Body, DDerlSessPid, Req),
    {ok, Req2, DDerlSessPid};
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.

%% Helper functions
-spec create_new_session(binary() | list(), binary() | undefined) -> {ok, {atom(), pid()}} | {error, term()}.
create_new_session(<<>>, undefined) -> {error, unauthorized};
create_new_session(<<>>, AuthHeader) ->
    case binary:split(AuthHeader, <<$ >>) of
        [<<"Basic">>, EncodedCredentials] ->
            case decode_credentials(EncodedCredentials) of
                {undefined, undefined} -> {error, unauthorized};
                {Username, Password} -> sbs_session:start(Username, Password)
            end;
        _ ->
            {error, unauthorized}
    end;
create_new_session(SessStr, _AppId) when is_list(SessStr) ->
    try
        {SessionMod, Pid} = ?DecryptPid(SessStr),
        case erlang:process_info(Pid) of
            undefined -> {error, unauthorized};
            _ -> {ok, {SessionMod, Pid}}
        end
    catch
        Error:Reason ->  {error, {Error, Reason}, list_to_binary(SessStr)}
    end;
create_new_session(S, AuthHeader) when is_binary(S) -> create_new_session(binary_to_list(S), AuthHeader);
create_new_session(_, AuthHeader) -> create_new_session(<<>>, AuthHeader).

% Reply templates
% cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req),
% cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req),
% {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
% Echo = proplists:get_value(<<"echo">>, PostVals),
% cowboy_req:reply(400, [], <<"Missing body.">>, Req)
reply_200_json(Body, DDerlSessPid, Req) when is_pid(DDerlSessPid) ->
    reply_200_json(Body, list_to_binary(?EncryptPid({dderl_session, DDerlSessPid})), Req);
reply_200_json(Body, EncryptedPid, Req) ->
	cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        , {<<"dderl_sess">>, EncryptedPid}
        ], Body, Req).

unauthorized_body() ->
<<"
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dt\">
<HTML>
<HEAD>
<TITLE>Error</TITLE>
<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">
</HEAD>
<BODY><H1>401 Unauthorized.</H1></BODY>
</HTML>
">>.

decode_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {undefined, undefined}
    end.
