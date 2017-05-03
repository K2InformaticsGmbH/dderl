%% @doc POST ajax handler.
-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(cowboy_loop_handler).
 
-include("dderl.hrl").

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-export([samlRelayStateHandle/2, conn_info/1]).

%-define(DISP_REQ, 1).

-record(state, {sessionToken}).

init({ssl, http}, Req0, []) ->
    Req1 = cowboy_req:set_meta(reqTime, os:timestamp(), Req0),
    Req2 = cowboy_req:set_meta(accessLog, #{}, Req1),
    display_req(Req2),
    case cowboy_req:has_body(Req2) of
        true ->
            {Typ, Req3} = cowboy_req:path_info(Req2),
            {SessionToken, Req4} = cowboy_req:cookie(cookie_name(?SESSION_COOKIE, Req1), Req3, <<>>),
            {XSRFToken, Req4} = cowboy_req:header(?XSRF_HEADER, Req4, <<>>),
            {Adapter, Req4} = cowboy_req:header(<<"dderl-adapter">>,Req4),
            process_request(SessionToken, XSRFToken, Adapter, Req4, Typ);
        Else ->
            ?Error("DDerl request ~p, error ~p", [Req2, Else]),
            self() ! {reply, <<"{}">>},
            {loop, Req2, #state{}, 5000, hibernate}
    end.

process_request(SessionToken, _, _Adapter, Req, [<<"download_query">>] = Typ) ->
    {ok, ReqDataList, Req1} = cowboy_req:body_qs(Req),
    Adapter = proplists:get_value(<<"dderl-adapter">>, ReqDataList, <<>>),
    FileToDownload = proplists:get_value(<<"fileToDownload">>, ReqDataList, <<>>),
    XSRFToken = proplists:get_value(<<"xsrfToken">>, ReqDataList, <<>>),
    case proplists:get_value(<<"exportAll">>, ReqDataList, <<"false">>) of
        <<"true">> ->
            QueryToDownload = proplists:get_value(<<"queryToDownload">>, ReqDataList, <<>>),
            BindVals = imem_json:decode(proplists:get_value(<<"binds">>, ReqDataList, <<>>)),
            Connection = proplists:get_value(<<"connection">>, ReqDataList, <<>>),
            process_request_low(SessionToken, XSRFToken, Adapter, Req1,
                jsx:encode([{<<"download_query">>,
                                [{<<"connection">>, Connection},
                                {<<"fileToDownload">>, FileToDownload},
                                {<<"queryToDownload">>, QueryToDownload},
                                {<<"binds">>,BindVals}]
                            }]), Typ);
        _ ->
            FsmStmt = proplists:get_value(<<"statement">>, ReqDataList, <<>>),
            ColumnPositions = jsx:decode(proplists:get_value(<<"column_positions">>, ReqDataList, <<"[]">>)),
            process_request_low(SessionToken, XSRFToken, Adapter, Req1,
                jsx:encode([{<<"download_buffer_csv">>,
                                [{<<"statement">>, FsmStmt},
                                {<<"filename">>, FileToDownload},
                                {<<"column_positions">>, ColumnPositions}]
                            }]), [<<"download_buffer_csv">>])
    end;
process_request(SessionToken, XSRFToken, Adapter, Req, [<<"close_tab">>]) ->
    {Connection, Req1} = cowboy_req:header(<<"dderl-connection">>, Req),
    process_request_low(SessionToken, XSRFToken, Adapter, Req1, 
        imem_json:encode(#{disconnect => #{connection => Connection}}), [<<"disconnect">>]);
process_request(SessionToken, XSRFToken, Adapter, Req, Typ) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    process_request_low(SessionToken, XSRFToken, Adapter, Req1, Body, Typ).

samlRelayStateHandle(Req, SamlAttrs) ->
    {Adapter, Req} = cowboy_req:header(<<"dderl-adapter">>,Req),
    {SessionToken, Req1} = cowboy_req:cookie(cookie_name(?SESSION_COOKIE, Req), Req, <<>>),
    {XSRFToken, Req} = cowboy_req:header(?XSRF_HEADER, Req, <<>>),
    AccName = list_to_binary(proplists:get_value(windowsaccountname, SamlAttrs)),
    process_request_low(SessionToken, XSRFToken, Adapter, Req1, imem_json:encode(#{samluser => AccName}), [<<"login">>]).

process_request_low(SessionToken, XSRFToken, Adapter, Req, Body, Typ) ->
    AdaptMod = if
        is_binary(Adapter) -> list_to_existing_atom(binary_to_list(Adapter) ++ "_adapter");
        true -> undefined
    end,
    {{Ip, Port}, Req} = cowboy_req:peer(Req),
    NewBody =
    if Typ == [<<"login">>] -> 
            {HostUrl, Req} = cowboy_req:host_url(Req),
            BodyMap = imem_json:decode(Body, [return_maps]),
            jsx:encode(BodyMap#{host_url => HostUrl});
       true -> Body
    end,
    CheckXSRF = not lists:member(Typ, [[<<"login">>], [<<"logout">>]]),
    case dderl_session:get_session(SessionToken, XSRFToken, CheckXSRF, fun() -> conn_info(Req) end) of
        {ok, SessionToken1, XSRFToken1} ->
            dderl_session:process_request(AdaptMod, Typ, NewBody, self(), {Ip, Port}, SessionToken1),
            {loop, set_xsrf_cookie(Req, XSRFToken, XSRFToken1),
             #state{sessionToken = SessionToken1}, 3600000, hibernate};
        {error, Reason} ->
            case Typ of
                [<<"login">>] -> 
                    {ok, NewToken, NewXSRFToken} =
                        dderl_session:get_session(<<>>, <<>>, CheckXSRF, fun() -> conn_info(Req) end),
                    dderl_session:process_request(AdaptMod, Typ, NewBody, self(), {Ip, Port}, NewToken),
                    {loop, set_xsrf_cookie(Req, XSRFToken, NewXSRFToken),
                     #state{sessionToken = NewToken}, 3600000, hibernate};
                [<<"logout">>] ->
                    self() ! {reply, imem_json:encode([{<<"logout">>, <<"ok">>}])},
                    {loop, Req, #state{sessionToken = SessionToken},
                     5000, hibernate};
                _ ->
                    ?Info("[~p] session ~p doesn't exist (~p), from ~s:~p",
                          [Typ, SessionToken, Reason, imem_datatype:ipaddr_to_io(Ip), Port]),
                    Node = atom_to_binary(node(), utf8),
                    self() ! {reply, imem_json:encode([{<<"error">>, <<"Session is not valid ", Node/binary>>}])},
                    {loop, Req, #state{sessionToken = SessionToken}, 5000, hibernate}
            end
    end.

conn_info(Req) ->
    {{PeerIp, PeerPort}, Req} = cowboy_req:peer(Req),
    Sock = cowboy_req:get(socket, Req),
    ConnInfo = case Sock of
                {sslsocket, _, _} ->
                       {ok, {LocalIp, LocalPort}} = ssl:sockname(Sock),
                       Info = #{tcp => #{localip => LocalIp, localport => LocalPort}},
                       case ssl:peercert(Sock) of
                           {ok, Cert} ->
                               Info#{ssl => #{cert => public_key:pkix_decode_cert(Cert, otp)}};
                           _ -> Info#{ssl => #{}}
                       end;
                   _ ->
                       {ok, {LocalIp, LocalPort}} = inet:sockname(Sock),
                       #{tcp => #{localip => LocalIp, localport => LocalPort}}
               end,
    #{tcp := ConnTcpInfo} = ConnInfo,
    {Headers, Req} = cowboy_req:headers(Req),
    ConnInfo#{tcp => ConnTcpInfo#{peerip => PeerIp, peerport => PeerPort},
              http => #{headers => Headers}}.
info({access, Log}, Req, State) ->
    {OldLog, Req} = cowboy_req:meta(accessLog, Req, 0),
    {loop, cowboy_req:set_meta(accessLog, maps:merge(OldLog, Log), Req), State, hibernate};
info({spawn, SpawnFun}, Req, State) when is_function(SpawnFun) ->
    ?Debug("spawn fun~n to ~p", [State#state.sessionToken]),
    spawn(SpawnFun),
    {loop, Req, State, hibernate};
info({reply, Body}, Req, #state{sessionToken = SessionToken} = State) ->
    ?Debug("reply ~n~p to ~p", [Body, SessionToken]),
    BodyEnc = if is_binary(Body) -> Body;
                 true -> imem_json:encode(Body)
              end,
    {ok, Req2} = reply_200_json(BodyEnc, SessionToken, Req),
    {ok, Req2, State};
info({reply_csv, FileName, Chunk, ChunkIdx}, Req, State) ->
    ?Debug("reply csv FileName ~p, Chunk ~p, ChunkIdx ~p", [FileName, Chunk, ChunkIdx]),
    {ok, Req1} = reply_csv(FileName, Chunk, ChunkIdx, Req),
    case ChunkIdx of
        last -> {ok, Req1, State};
        single -> {ok, Req1, State};
        _ -> {loop, Req1, State, hibernate}
    end;
info({newToken, NewSessionToken}, Req, #state{sessionToken = SessionToken} = State) ->
    ?Debug("cookie chnaged ~p -> ~p", [SessionToken, NewSessionToken]),
    {loop, Req, State#state{sessionToken = NewSessionToken}, hibernate};
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {loop, Req, State, hibernate}.

terminate(_Reason, Req, _State) ->
    {Log, Req} = cowboy_req:meta(accessLog, Req, 0),
    {ReqTime, Req} = cowboy_req:meta(reqTime, Req, 0),
    {RespSize, Req} = cowboy_req:meta(respSize, Req, 0),
    {ReqSize, _Req} = cowboy_req:body_length(Req),
    Size = ReqSize + RespSize,
    ProcessingTimeMicroS = timer:now_diff(os:timestamp(), ReqTime),
    catch dderl_access_logger:log(Log#{bytes => Size,
                                       time => ProcessingTimeMicroS}).

% Reply templates
% cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req),
% cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req),
% {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
% Echo = proplists:get_value(<<"echo">>, PostVals),
% cowboy_req:reply(400, [], <<"Missing body.">>, Req)
reply_200_json(Body, SessionToken, Req) when is_binary(SessionToken) ->
    CookieName = cookie_name(?SESSION_COOKIE, Req),
    Req2 = case cowboy_req:cookie(CookieName, Req, <<>>) of
        {SessionToken, Req1} -> Req1;
        {_, Req1} ->
            {Host, Req} = cowboy_req:host(Req),
            Path = dderl:format_path(dderl:get_url_suffix()),
            cowboy_req:set_resp_cookie(CookieName, SessionToken,
                                        ?HTTP_ONLY_COOKIE_OPTS(Host, Path), Req1)
    end,
    cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        ], Body, cowboy_req:set_meta(respSize, byte_size(Body), Req2)).

reply_csv(FileName, Chunk, ChunkIdx, Req) ->
    {Size, Req} = cowboy_req:meta(respSize, Req, 0),
    Req2 = case ChunkIdx of
               first ->
                   {ok, Req1} = cowboy_req:chunked_reply(
                                  200, [{<<"content-encoding">>, <<"utf-8">>},
                                        {<<"content-type">>, <<"text/csv">>},
                                        {<<"Content-disposition">>,
                                         list_to_binary(["attachment;filename=",
                                                         FileName])}], Req),
                   Req1;
               single ->
                   {ok, Req1} = cowboy_req:chunked_reply(
                                  200, [{<<"content-encoding">>, <<"utf-8">>},
                                        {<<"content-type">>, <<"text/csv">>},
                                        {<<"Content-disposition">>,
                                         list_to_binary(["attachment;filename=",
                                                         FileName])}], Req),
                   Req1;
               _ -> Req
           end,
    Size1 = Size + byte_size(Chunk),
    Req3 = cowboy_req:set_meta(respSize, Size1, Req2),
    ok = cowboy_req:chunk(Chunk, Req3),
    {ok, Req3}.

set_xsrf_cookie(Req, XSRFToken, XSRFToken) -> Req;
set_xsrf_cookie(Req, _, XSRFToken) ->
    XSRFCookie = cookie_name(?XSRF_COOKIE, Req),
    {Host, Req} = cowboy_req:host(Req),
    Path = dderl:format_path(dderl:get_url_suffix()),
    cowboy_req:set_resp_cookie(XSRFCookie, XSRFToken, ?COOKIE_OPTS(Host, Path), Req).

cookie_name(Name, Req) ->
    {Port, Req} = cowboy_req:port(Req),
    list_to_binary([Name, integer_to_list(Port)]).

%-define(DISP_REQ, true).
-ifdef(DISP_REQ).
display_req(Req) ->
    ?Info("-------------------------------------------------------"),
    ?Info("method     ~p~n", [element(1,cowboy_req:method(Req))]),
    ?Info("version    ~p~n", [element(1,cowboy_req:version(Req))]),
    ?Info("peer       ~p~n", [element(1,cowboy_req:peer(Req))]),
    %?Info("peer_addr  ~p~n", [element(1,cowboy_req:peer_addr(Req))]),
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
    ?Info("hdr(ddls)  ~p~n", [element(1,cowboy_req:header(<<"dderl-session">>,Req))]),
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
