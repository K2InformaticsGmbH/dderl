%% @doc POST ajax handler.
-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(cowboy_loop_handler).
 
-include("dderl.hrl").

-export([init/3]).
-export([info/3]).
-export([terminate/3]).


%-define(DISP_REQ, 1).

init({ssl, http}, Req, []) ->
    display_req(Req),
    {_Method, Req0} = cowboy_req:method(Req),
    case cowboy_req:has_body(Req0) of
    true ->
        {Session, Req1} = cowboy_req:header(<<"dderl_sess">>,Req0),
        {Adapter, Req2} = cowboy_req:header(<<"adapter">>,Req1),
        ?Debug("DDerl {session, adapter} from header ~p", [{Session,Adapter}]),
        case create_new_session(Session) of
            {ok, {_,DDerlSessPid} = DderlSess} ->
                if
                    is_binary(Adapter) ->
                        AdaptMod = list_to_existing_atom(binary_to_list(Adapter) ++ "_adapter");
                    true ->
                        AdaptMod = undefined
                end,
                {Typ, Req3} = cowboy_req:path_info(Req2),
                {ok, Body, Req4} = cowboy_req:body(Req3),
                DderlSess:process_request(AdaptMod, Typ, Body, self()),
                {loop, Req4, DDerlSessPid, 60000, hibernate};
            {error, Reason} ->
                ?Error("session ~p doesn't exists (~p)", [Session, Reason]),
                self() ! {reply, jsx:encode([{<<"error">>, <<"Session is not valid">>}])},
                {loop, Req2, Session, 5000, hibernate}
        end;
    _ ->
        self() ! {reply, <<"{}">>},
        {loop, Req, <<>>, 5000, undefined}
    end.

info({reply, Body}, Req, DDerlSessPid) ->
    ?Debug("reply ~n" ++ binary_to_list(jsx:prettify(Body))),
    {ok, Req2} = reply_200_json(Body, DDerlSessPid, Req),
    {ok, Req2, DDerlSessPid};
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.

create_new_session(<<>>) ->
    DderlSess = dderl_session:start(),
    ?Info("new dderl session ~p from ~p", [DderlSess, self()]),
    {ok, DderlSess};
create_new_session([_,_|_] = DDerlSessPid) ->
    ?Debug("existing session ~p", [DDerlSessPid]),
    try ?DecryptPid(DDerlSessPid) of
        Pid ->
            case erlang:process_info(Pid) of
                undefined -> {error, "process not found"};
                _ -> {ok, {dderl_session, Pid}}
            end
    catch
        Error:Reason ->  {error, {Error, Reason}}
    end;
create_new_session(S) when is_binary(S) -> create_new_session(binary_to_list(S));
create_new_session(_) -> create_new_session(<<>>).

% Reply templates
% cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req),
% cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req),
% {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
% Echo = proplists:get_value(<<"echo">>, PostVals),
% cowboy_req:reply(400, [], <<"Missing body.">>, Req)
reply_200_json(Body, DDerlSessPid, Req) when is_pid(DDerlSessPid) ->
    reply_200_json(Body, list_to_binary(?EncryptPid(DDerlSessPid)), Req);
reply_200_json(Body, EncryptedPid, Req) ->
	cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        , {<<"dderl_sess">>, EncryptedPid}
        ], Body, Req).

-ifdef(DISP_REQ).
display_req(Req) ->
    ?Info("-------------------------------------------------------"),
    ?Info("method     ~p~n", [element(1,cowboy_req:method(Req))]),
    ?Info("version    ~p~n", [element(1,cowboy_req:version(Req))]),
    ?Info("peer       ~p~n", [element(1,cowboy_req:peer(Req))]),
    ?Info("peer_addr  ~p~n", [element(1,cowboy_req:peer_addr(Req))]),
    ?Info("host       ~p~n", [element(1,cowboy_req:host(Req))]),
    ?Info("host_info  ~p~n", [element(1,cowboy_req:host_info(Req))]),
    ?Info("port       ~p~n", [element(1,cowboy_req:port(Req))]),
    ?Info("path       ~p~n", [element(1,cowboy_req:path(Req))]),
    ?Info("path_info  ~p~n", [element(1,cowboy_req:path_info(Req))]),
    ?Info("qs         ~p~n", [element(1,cowboy_req:qs(Req))]),
    %?Info("qs_val     ~p~n", [element(1,cowboy_req:qs_val(Req))]),
    ?Info("qs_vals    ~p~n", [element(1,cowboy_req:qs_vals(Req))]),
    ?Info("fragment   ~p~n", [element(1,cowboy_req:fragment(Req))]),
    ?Info("host_url   ~p~n", [element(1,cowboy_req:host_url(Req))]),
    ?Info("url        ~p~n", [element(1,cowboy_req:url(Req))]),
    %?Info("binding    ~p~n", [element(1,cowboy_req:binding(Req))]),
    ?Info("bindings   ~p~n", [element(1,cowboy_req:bindings(Req))]),
    ?Info("hdr(ddls)  ~p~n", [element(1,cowboy_req:header(<<"dderl_sess">>,Req))]),
    ?Info("hdr(host)  ~p~n", [element(1,cowboy_req:header(<<"host">>,Req))]),
    %?Info("headers    ~p~n", [element(1,cowboy_req:headers(Req))]),
    %?Info("cookie     ~p~n", [element(1,cowboy_req:cookie(Req))]),
    ?Info("cookies    ~p~n", [element(1,cowboy_req:cookies(Req))]),
    %?Info("meta       ~p~n", [element(1,cowboy_req:meta(Req))]),
    ?Info("has_body   ~p~n", [cowboy_req:has_body(Req)]),
    ?Info("body_len   ~p~n", [element(1,cowboy_req:body_length(Req))]),
    ?Info("body_qs    ~p~n", [element(2,cowboy_req:body_qs(Req))]),
    ?Info("body       ~p~n", [element(2,cowboy_req:body(Req))]),
    ?Info("-------------------------------------------------------").
-else.
display_req(_) -> ok.
-endif.
