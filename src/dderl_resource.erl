%% @doc POST ajax handler.
-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%-define(DISP_REQ, 1).

init(_Transport, Req, []) ->    
	{ok, Req, undefined}.

handle(Req, State) ->
    ?Info("---- handler ~p", [self()]),
	{Method, Req2} = cowboy_req:method(Req),
	process(Method, Req2, State).

process(<<"POST">>, Req, State) ->
    display_req(Req),
    {DDerlSessPid, Body, Req1} = case cowboy_req:has_body(Req) of
        true -> process_request(Req);
       _     -> <<>>
    end,
    ?Debug("Resp ~p", [Body]),
    {ok, Req2} = reply_200_json(Body, DDerlSessPid, Req1),
    {ok, Req2, State};
process(_Method, Req, State) ->
    ?Info("not allowed method ~p with ~p~n", [_Method,Req]),
	%% Method not allowed.
	{ok, Req1} = cowboy_req:reply(405, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
	ok.

process_request(Req) ->
    {Session, Req1} = cowboy_req:header(<<"dderl_sess">>,Req),
    {Adapter, Req2} = cowboy_req:header(<<"adapter">>,Req1),
    ?Info("DDerl {session, adapter} from header ~p", [{Session,Adapter}]),
    case create_new_session(Session) of
        {ok, {_,DDerlSessPid} = DderlSess} ->
            case Adapter of
                undefined                       -> ok;
                Adapter when is_binary(Adapter) -> DderlSess:set_adapter(binary_to_list(Adapter))
            end,
            {Typ, Req3} = cowboy_req:path_info(Req2),
            {ok, Body, Req4} = cowboy_req:body(Req3),
            {DDerlSessPid, DderlSess:process_request(Typ, Body), Req4};
        {error, Reason} ->
            ?Error("session ~p doesn't exists (~p)", [Session, Reason]),
            {error, session_timeout, Req2}
    end.

create_new_session(<<>>) ->
    DderlSess = dderl_session:start(),
    ?Info("new dderl session ~p", [DderlSess]),
    {ok, DderlSess};
create_new_session([_,_|_] = DDerlSessPid) ->
    ?Info("existing session ~p", [DDerlSessPid]),
    Pid = ?DecryptPid(DDerlSessPid),
    case erlang:process_info(Pid) of
        undefined -> {error, "process not found"};
        _ -> {ok, {dderl_session, Pid}}
    end;
create_new_session(S) when is_binary(S) -> create_new_session(binary_to_list(S));
create_new_session(_) -> create_new_session(<<>>).

% Reply templates
% cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req),
% cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req),
% {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
% Echo = proplists:get_value(<<"echo">>, PostVals),
% cowboy_req:reply(400, [], <<"Missing body.">>, Req)
reply_200_json(Content, DDerlSessPid, Req) ->
	cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        , {<<"dderl_sess">>, list_to_binary(?EncryptPid(DDerlSessPid))}
        ], list_to_binary(Content), Req).

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
