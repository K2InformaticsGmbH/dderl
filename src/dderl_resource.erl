%% @doc POST ajax handler.
-module(dderl_resource).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(cowboy_loop).
 
-include("dderl.hrl").
-include("dderl_request.hrl").

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

-export([samlRelayStateHandle/2, conn_info/1]).

-record(state, {sessionToken}).

init(Req0, []) ->
    Req = ?COW_REQ_SET_META(reqTime, os:timestamp(), Req0),
    Req1 = ?COW_REQ_SET_META(accessLog, #{}, Req),
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
    #{<<"dderl-adapter">> := Adapter, <<"fileToDownload">> := FileToDownload,
      <<"xsrfToken">> := XSRFToken} =
        maps:merge(
            #{<<"dderl-adapter">> => <<>>, <<"fileToDownload">> => <<>>,
              <<"xsrfToken">> => <<>>},
            maps:from_list(ReqDataList)
        ),
    case dderl:keyfetch(<<"exportAll">>, ReqDataList, <<"false">>) of
        <<"true">> ->
            QueryToDownload = dderl:keyfetch(<<"queryToDownload">>, ReqDataList, <<>>),
            BindVals = imem_json:decode(dderl:keyfetch(<<"binds">>, ReqDataList, <<>>)),
            Connection = dderl:keyfetch(<<"connection">>, ReqDataList, <<>>),
            Id = dderl:keyfetch(<<"id">>, ReqDataList, <<>>),
            process_request_low(SessionToken, XSRFToken, Adapter, Req1,
                jsx:encode([{<<"download_query">>,
                                [{<<"connection">>, Connection},
                                {<<"fileToDownload">>, FileToDownload},
                                {<<"queryToDownload">>, QueryToDownload},
                                {<<"binds">>, BindVals},
                                {<<"id">>, Id}]
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
                    self() ! {reply, [{<<"logout">>, <<"ok">>}]},
                    {cowboy_loop, Req, #state{sessionToken = SessionToken}, hibernate};
                _ ->
                    ?Info("[~p] session ~p doesn't exist (~p), from ~s:~p",
                          [Typ, SessionToken, Reason, imem_datatype:ipaddr_to_io(Ip), Port]),
                    Node = atom_to_binary(node(), utf8),
                    reply_200_json(imem_json:encode([{<<"error">>, <<"Session is not valid ", Node/binary>>}]), <<>>, Req),
                    {ok, Req, #state{sessionToken = SessionToken}}
            end
    end.

conn_info(Req) ->
    #{sock := {LocalIp, LocalPort}, peer := {PeerIp, PeerPort}} = Req,
    ConnTcpInfo = #{localip => LocalIp, localport => LocalPort,
                    peerip => PeerIp, peerport => PeerPort},
    Headers = cowboy_req:headers(Req),
    #{tcp => ConnTcpInfo, http => #{headers => Headers}}.
info({access, Log}, Req, State) ->
    OldLog = ?COW_REQ_GET_META(accessLog, Req, 0),
    Req1 = ?COW_REQ_SET_META(accessLog, maps:merge(OldLog, Log), Req),
    {ok, Req1, State, hibernate};
info({spawn, SpawnFun}, Req, State) when is_function(SpawnFun) ->
    ?Debug("spawn fun~n to ~p", [State#state.sessionToken]),
    spawn(SpawnFun),
    {ok, Req, State, hibernate};
info({reply, [{<<"logout">>, _} | _] = Body}, Req, State) ->
    %% delete session cookie
    Req1 = set_xsrf_cookie(Req, none, delete_cookie),
    info({reply, imem_json:encode(Body)}, Req1, State#state{sessionToken = delete_cookie});
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
    Log = ?COW_REQ_GET_META(accessLog, Req, 0),
    ReqTime = ?COW_REQ_GET_META(reqTime, Req, 0),
    RespSize = ?COW_REQ_GET_META(respSize, Req, 0),
    ReqSize = cowboy_req:body_length(Req),
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
reply_200_json(Body, SessionToken, Req) ->
    CookieName = cookie_name(?SESSION_COOKIE),
    Req1 = case dderl:get_cookie(CookieName, Req, <<>>) of
        SessionToken -> Req;
        _ ->
            Host = cowboy_req:host(Req),
            Path = dderl:format_path(dderl:get_url_suffix()),
            {SessToken, Opts} = get_cookie_opts(SessionToken, ?HTTP_ONLY_COOKIE_OPTS(Host, Path)),
            cowboy_req:set_resp_cookie(CookieName, SessToken, Req, Opts)
    end,
    cowboy_req:reply(200, 
          #{<<"content-type">> => <<"application/json; charset=utf-8">>}
        , Body, ?COW_REQ_SET_META(respSize, byte_size(Body), Req1)).

reply_csv(FileName, Chunk, ChunkIdx, Req) ->
    Size = ?COW_REQ_GET_META(respSize, Req, 0),
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
    ?COW_REQ_SET_META(respSize, Size1, Req1).

set_xsrf_cookie(Req, XSRFToken, XSRFToken) -> Req;
set_xsrf_cookie(Req, _, XSRFToken) ->
    XSRFCookie = cookie_name(?XSRF_COOKIE),
    Host = cowboy_req:host(Req),
    Path = dderl:format_path(dderl:get_url_suffix()),
    {XSRFToken1, Opts} = get_cookie_opts(XSRFToken, ?COOKIE_OPTS(Host, Path)),
    cowboy_req:set_resp_cookie(XSRFCookie, XSRFToken1, Req, Opts).

cookie_name(Name) ->
    [Node, _] = binary:split(atom_to_binary(node(), utf8), <<"@">>),
    list_to_binary([Node, Name]).

get_cookie_opts(delete_cookie, Opts) -> {<<>>, Opts#{max_age => 0}};
get_cookie_opts(Cookie, Opts) -> {Cookie, Opts}.
