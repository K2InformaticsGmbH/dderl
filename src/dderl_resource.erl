%% @doc POST ajax handler.
-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(cowboy_loop).
 
-include("dderl.hrl").

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

-export([samlRelayStateHandle/2, conn_info/1, cookie_name/1]).

% -define(DISP_REQ, 1).

-record(state, {sessionToken}).

init(Req0, []) ->
    Req1 = Req0#{reqTime => os:timestamp(),
                 accessLog => #{}},
    ?Debug("Request : ~p", [Req1]),
    case cowboy_req:has_body(Req1) of
        true ->
            Typ = cowboy_req:path_info(Req1),
            SessionToken = dderl:get_cookie(cookie_name(?SESSION_COOKIE), Req1, <<>>),
            XSRFToken = cowboy_req:header(?XSRF_HEADER, Req1, <<>>),
            Adapter = cowboy_req:header(<<"dderl-adapter">>,Req1),
            process_request(SessionToken, XSRFToken, Adapter, Req1, Typ);
        Else ->
            ?Error("DDerl request ~p, error ~p", [Req1, Else]),
            Req2 = reply_200_json(<<"{}">>, <<>>, Req1),
            {ok, Req2, #state{}}
    end.

process_request(SessionToken, _, _Adapter, Req, [<<"download_query">>] = Typ) ->
    {ok, ReqDataList, Req1} = cowboy_req:read_urlencoded_body(Req),
    Adapter = dderl:keyfetch(<<"dderl-adapter">>, ReqDataList, <<>>),
    FileToDownload = dderl:keyfetch(<<"fileToDownload">>, ReqDataList, <<>>),
    XSRFToken = dderl:keyfetch(<<"xsrfToken">>, ReqDataList, <<>>),
    case dderl:keyfetch(<<"exportAll">>, ReqDataList, <<"false">>) of
        <<"true">> ->
            QueryToDownload = dderl:keyfetch(<<"queryToDownload">>, ReqDataList, <<>>),
            BindVals = imem_json:decode(dderl:keyfetch(<<"binds">>, ReqDataList, <<>>)),
            Connection = dderl:keyfetch(<<"connection">>, ReqDataList, <<>>),
            process_request_low(SessionToken, XSRFToken, Adapter, Req1,
                jsx:encode([{<<"download_query">>,
                                [{<<"connection">>, Connection},
                                {<<"fileToDownload">>, FileToDownload},
                                {<<"queryToDownload">>, QueryToDownload},
                                {<<"binds">>,BindVals}]
                            }]), Typ);
        _ ->
            FsmStmt = dderl:keyfetch(<<"statement">>, ReqDataList, <<>>),
            ColumnPositions = jsx:decode( dderl:keyfetch(<<"column_positions">>, ReqDataList, <<"[]">>)),
            process_request_low(SessionToken, XSRFToken, Adapter, Req1,
                jsx:encode([{<<"download_buffer_csv">>,
                                [{<<"statement">>, FsmStmt},
                                {<<"filename">>, FileToDownload},
                                {<<"column_positions">>, ColumnPositions}]
                            }]), [<<"download_buffer_csv">>])
    end;
process_request(SessionToken, XSRFToken, Adapter, Req, [<<"close_tab">>]) ->
    Connection = cowboy_req:header(<<"dderl-connection">>, Req),
    process_request_low(SessionToken, XSRFToken, Adapter, Req, 
        imem_json:encode(#{disconnect => #{connection => Connection}}), [<<"disconnect">>]);
process_request(SessionToken, XSRFToken, Adapter, Req, Typ) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    process_request_low(SessionToken, XSRFToken, Adapter, Req1, Body, Typ).

samlRelayStateHandle(Req, SamlAttrs) ->
    Adapter = cowboy_req:header(<<"dderl-adapter">>,Req),
    SessionToken = dderl:get_cookie(cookie_name(?SESSION_COOKIE), Req, <<>>),
    XSRFToken = cowboy_req:header(?XSRF_HEADER, Req, <<>>),
    AccName = list_to_binary(dderl:keyfetch(windowsaccountname, SamlAttrs, "")),
    self() ! {terminateCallback, fun ?MODULE:terminate/3},
    process_request_low(SessionToken, XSRFToken, Adapter, Req, imem_json:encode(#{samluser => AccName}), [<<"login">>]).

process_request_low(SessionToken, XSRFToken, Adapter, Req, Body, Typ) ->
    AdaptMod = if
        is_binary(Adapter) -> list_to_existing_atom(binary_to_list(Adapter) ++ "_adapter");
        true -> undefined
    end,
    {Ip, Port} = cowboy_req:peer(Req),
    NewBody =
    if Typ == [<<"login">>] -> 
            HostUrl = iolist_to_binary(cowboy_req:uri(Req, #{path => undefined, qs => undefined})),
            BodyMap = imem_json:decode(Body, [return_maps]),
            jsx:encode(BodyMap#{host_url => HostUrl});
       true -> Body
    end,
    CheckXSRF = not lists:member(Typ, [[<<"login">>], [<<"logout">>]]),
    case dderl_session:get_session(SessionToken, XSRFToken, CheckXSRF, fun() -> conn_info(Req) end) of
        {ok, SessionToken1, XSRFToken1} ->
            dderl_session:process_request(AdaptMod, Typ, NewBody, self(), {Ip, Port}, SessionToken1),
            {cowboy_loop, set_xsrf_cookie(Req, XSRFToken, XSRFToken1),
             #state{sessionToken = SessionToken1}, hibernate};
        {error, Reason} ->
            case Typ of
                [<<"login">>] -> 
                    {ok, NewToken, NewXSRFToken} =
                        dderl_session:get_session(<<>>, <<>>, CheckXSRF, fun() -> conn_info(Req) end),
                    dderl_session:process_request(AdaptMod, Typ, NewBody, self(), {Ip, Port}, NewToken),
                    {cowboy_loop, set_xsrf_cookie(Req, XSRFToken, NewXSRFToken),
                     #state{sessionToken = NewToken}, hibernate};
                [<<"logout">>] ->
                    reply_200_json(imem_json:ecode([{<<"logout">>, <<"ok">>}]), SessionToken, Req),
                    {ok, Req, #state{sessionToken = SessionToken}};
                _ ->
                    ?Info("[~p] session ~p doesn't exist (~p), from ~s:~p",
                          [Typ, SessionToken, Reason, imem_datatype:ipaddr_to_io(Ip), Port]),
                    Node = atom_to_binary(node(), utf8),
                    reply_200_json(imem_json:encode([{<<"error">>, <<"Session is not valid ", Node/binary>>}]), <<>>, Req),
                    {ok, Req, #state{sessionToken = SessionToken}}
            end
    end.

conn_info(Req) ->
    {PeerIp, PeerPort} = cowboy_req:peer(Req),
    {ok, LocalIp}   = application:get_env(dderl, interface),
    {ok, LocalPort} = application:get_env(dderl, port),
    ConnTcpInfo = #{localip => LocalIp, localport => LocalPort},
    ConnInfo = #{tcp => ConnTcpInfo},
    Headers = cowboy_req:headers(Req),
    ConnInfo#{tcp => ConnTcpInfo#{peerip => PeerIp, peerport => PeerPort},
              http => #{headers => Headers}}.
info({access, Log}, Req, State) ->
    OldLog = maps:get(accessLog, Req, 0),
    {ok, Req#{accessLog => maps:merge(OldLog, Log)}, State, hibernate};
info({spawn, SpawnFun}, Req, State) when is_function(SpawnFun) ->
    ?Debug("spawn fun~n to ~p", [State#state.sessionToken]),
    spawn(SpawnFun),
    {ok, Req, State, hibernate};
info({reply, Body}, Req, #state{sessionToken = SessionToken} = State) ->
    ?Debug("reply ~n~p to ~p", [Body, SessionToken]),
    BodyEnc = if is_binary(Body) -> Body;
                 true -> imem_json:encode(Body)
              end,
    Req2 = reply_200_json(BodyEnc, SessionToken, Req),
    {stop, Req2, State};
info({reply_csv, FileName, Chunk, ChunkIdx}, Req, State) ->
    ?Debug("reply csv FileName ~p, Chunk ~p, ChunkIdx ~p", [FileName, Chunk, ChunkIdx]),
    Req1 = reply_csv(FileName, Chunk, ChunkIdx, Req),
    case ChunkIdx of
        last -> {ok, Req1, State};
        single -> {ok, Req1, State};
        _ -> {ok, Req1, State, hibernate}  %% first/continue
    end;
info({newToken, NewSessionToken}, Req, #state{sessionToken = SessionToken} = State) ->
    ?Debug("cookie chnaged ~p -> ~p", [SessionToken, NewSessionToken]),
    {ok, Req, State#state{sessionToken = NewSessionToken}, hibernate};
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {ok, Req, State, hibernate}.

terminate(_Reason, Req, _State) ->
    Log = maps:get(accessLog, Req, 0),
    ReqTime = maps:get(reqTime, Req, 0),
    RespSize = maps:get(respSize, Req, 0),
    ReqSize = cowboy_req:body_length(Req),
    Size = ReqSize + RespSize,
    ProcessingTimeMicroS = timer:now_diff(os:timestamp(), ReqTime),
    catch dderl_access_logger:log(Log#{bytes => Size,
                                       time => ProcessingTimeMicroS}).

reply_200_json(Body, SessionToken, Req) when is_binary(SessionToken) ->
    CookieName = cookie_name(?SESSION_COOKIE),
    Req1 = case dderl:get_cookie(CookieName, Req, <<>>) of
        SessionToken -> Req;
        _ ->
            Host = cowboy_req:host(Req),
            Path = dderl:format_path(dderl:get_url_suffix()),
            cowboy_req:set_resp_cookie(CookieName, SessionToken, Req, ?HTTP_ONLY_COOKIE_OPTS(Host, Path))
    end,
    cowboy_req:reply(200, 
          #{<<"content-encoding">> => <<"utf-8">>,
            <<"content-type">> => <<"application/json">>}
        , Body, Req1#{respSize => byte_size(Body)}).

reply_csv(FileName, Chunk, ChunkIdx, Req) ->
    Size = maps:get(respSize, Req, 0),
    %% Status is fin or nofin
    {Req1, Status} =
    case ChunkIdx of
        Type when Type == first; Type == single ->
            {cowboy_req:stream_reply(200, #{
                <<"content-encoding">> => <<"utf-8">>,
                <<"content-type">> => <<"text/csv">>,
                <<"content-disposition">> => list_to_binary(["attachment;filename=", FileName])}, Req),
             if Type == first -> nofin;
                true -> fin
            end};
        continue -> {Req, nofin};
        last -> {Req, fin}
    end,
    Size1 = Size + byte_size(Chunk),
    ok = cowboy_req:stream_body(Chunk, Status, Req1),
    Req1#{respSize => Size1}.

set_xsrf_cookie(Req, XSRFToken, XSRFToken) -> Req;
set_xsrf_cookie(Req, _, XSRFToken) ->
    XSRFCookie = cookie_name(?XSRF_COOKIE),
    Host = cowboy_req:host(Req),
    Path = dderl:format_path(dderl:get_url_suffix()),
    cowboy_req:set_resp_cookie(XSRFCookie, XSRFToken, Req, ?COOKIE_OPTS(Host, Path)).

cookie_name(Name) ->
    HostApp = dderl_dal:get_host_app(),
    list_to_binary([HostApp, Name]).
