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
        {Session, Req1} = cowboy_req:cookie(?DDERL_COOKIE_NAME, Req, <<>>),
        {Adapter, Req2} = cowboy_req:header(<<"dderl-adapter">>,Req1),
        {Typ, Req3} = cowboy_req:path_info(Req2),
        % ?Info("DDerl {session, adapter} from header ~p", [{Session,Adapter,Typ}]),
        process_request(Session, Adapter, Req3, Typ);
    _Else ->
        ?Error("DDerl request ~p, error ~p", [Req0, _Else]),
        self() ! {reply, <<"{}">>},
        {loop, Req, <<>>, 5000, hibernate}
    end.

multipart(Req) -> multipart(Req, []).
multipart(Req, Files) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {file, FieldName, Filename, CType, CTransferEncoding} ->                    
                    {Data, Req2} = stream_file(Req1),
                    multipart(
                      Req2,
                      [#{fieldName => FieldName, fileName => Filename,
                         contentType => CType, data => Data,
                         contentTransferEncoding => CTransferEncoding} | Files])
            end;
        {done, Req2} ->
            {Files, Req2}
    end.
 
stream_file(Req) -> stream_file(Req, <<>>).
stream_file(Req, Buffer) ->
    case cowboy_req:part_body(Req) of
        {ok, Body, Req2} ->
            {list_to_binary([Buffer, Body]), Req2};
        {more, Body, Req2} ->
            stream_file(Req2, list_to_binary([Buffer, Body]))
    end.

process_request(_, _, Req, [<<"upload">>]) ->
    {Files, Req1} = multipart(Req),
    ?Debug("Files ~p", [[F#{data := byte_size(maps:get(data, F))}|| F <- Files]]),
    {{SrcIp, _}, Req} = cowboy_req:peer(Req),
    catch dderl:access(?CMD_WITHARGS, SrcIp, "", "upload", io_lib:format("~p", [Files]), "", "", "", "", ""),
    self() ! {reply, jsx:encode(
                       #{upload =>
                         [case string:to_lower(binary_to_list(maps:get(contentType,F))) of
                              "text/plain" -> F;
                              _ ->
                                  F#{data:=base64:encode(maps:get(data,F))}
                          end || F <- Files]}
                      )},
    {loop, Req1, <<>>, 5000, hibernate};
process_request(Session, _Adapter, Req, [<<"download_query">>] = Typ) ->
    {ok, ReqDataList, Req1} = cowboy_req:body_qs(Req),
    Adapter = proplists:get_value(<<"dderl-adapter">>, ReqDataList, <<>>),
    FileToDownload = proplists:get_value(<<"fileToDownload">>, ReqDataList, <<>>),
    QueryToDownload = proplists:get_value(<<"queryToDownload">>, ReqDataList, <<>>),
    BindVals = imem_json:decode(proplists:get_value(<<"binds">>, ReqDataList, <<>>)),
    Connection = proplists:get_value(<<"connection">>, ReqDataList, <<>>),
    process_request_low(Session, Adapter, Req1,
                        jsx:encode([{<<"download_query">>,
                                     [{<<"connection">>, Connection},
                                      {<<"fileToDownload">>, FileToDownload},
                                      {<<"queryToDownload">>, QueryToDownload},
                                      {<<"binds">>,BindVals}]
                                    }]), Typ);
process_request(Session, Adapter, Req, Typ) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    process_request_low(Session, Adapter, Req1, Body, Typ).

process_request_low(Session, Adapter, Req, Body, Typ) ->
    AdaptMod = if
        is_binary(Adapter) -> list_to_existing_atom(binary_to_list(Adapter) ++ "_adapter");
        true -> undefined
    end,
    {{Ip, Port}, Req} = cowboy_req:peer(Req),
    case dderl_session:get_session(Session, fun() -> conn_info(Req) end) of
        {ok, DderlSess} ->
            dderl_session:process_request(AdaptMod, Typ, Body, self(), {Ip, Port}, DderlSess),
            {loop, Req, DderlSess, 3600000, hibernate};
        {error, Reason} ->
            case Typ of
                [<<"login">>] -> 
                    {ok, DderlSess2} = dderl_session:get_session(<<>>, fun() -> conn_info(Req) end),
                    dderl_session:process_request(AdaptMod, Typ, Body, self(), {Ip, Port}, DderlSess2),
                    {loop, Req, DderlSess2, 3600000, hibernate};
                _ ->
                    ?Info("session ~p doesn't exist (~p), from ~s:~p",
                          [Session, Reason, imem_datatype:ipaddr_to_io(Ip), Port]),
                    Node = atom_to_binary(node(), utf8),
                    self() ! {reply, jsx:encode([{<<"error">>, <<"Session is not valid ", Node/binary>>}])},
                    {loop, Req, Session, 5000, hibernate}
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

info({spawn, SpawnFun}, Req, DDerlSessPid) when is_function(SpawnFun) ->
    ?Debug("spawn fun~n to ~p", [DDerlSessPid]),
    spawn(SpawnFun),
    {loop, Req, DDerlSessPid, hibernate};
info({reply, Body}, Req, DDerlSessPid) ->
    ?Debug("reply ~n~s to ~p", [jsx:prettify(Body), DDerlSessPid]),
    {ok, Req2} = reply_200_json(Body, DDerlSessPid, Req),
    {ok, Req2, DDerlSessPid};
info({reply_csv, FileName, Chunk, ChunkIdx}, Req, DDerlSessPid) ->
    ?Debug("reply csv FileName ~p, Chunk ~p, ChunkIdx ~p", [FileName, Chunk, ChunkIdx]),
    {ok, Req1} = reply_csv(FileName, Chunk, ChunkIdx, Req),
    case ChunkIdx of
        last -> {ok, Req1, DDerlSessPid};
        single -> {ok, Req1, DDerlSessPid};
        _ -> {loop, Req1, DDerlSessPid, hibernate}
    end;
info(Message, Req, State) ->
    ?Error("~p unknown message in loop ~p", [self(), Message]),
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.

% Reply templates
% cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req),
% cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req),
% {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
% Echo = proplists:get_value(<<"echo">>, PostVals),
% cowboy_req:reply(400, [], <<"Missing body.">>, Req)
reply_200_json(Body, EncryptedPid, Req) when is_list(EncryptedPid) ->
    reply_200_json(Body, list_to_binary(EncryptedPid), Req);
reply_200_json(Body, EncryptedPid, Req) when is_binary(EncryptedPid) ->
    Req1 = set_cookies(Req, EncryptedPid),
    cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        ], Body, Req1).

reply_csv(FileName, Chunk, ChunkIdx, Req) ->
    case ChunkIdx of
        first ->
            {ok, Req1} = cowboy_req:chunked_reply(200, [
                  {<<"content-encoding">>, <<"utf-8">>}
                , {<<"content-type">>, <<"text/csv">>}
                , {<<"Content-disposition">>
                   , list_to_binary(["attachment;filename=", FileName])}
                ], Req),
            ok = cowboy_req:chunk(Chunk, Req1),
            {ok, Req1};
        single ->
            {ok, Req1} = cowboy_req:chunked_reply(200, [
                  {<<"content-encoding">>, <<"utf-8">>}
                , {<<"content-type">>, <<"text/csv">>}
                , {<<"Content-disposition">>
                   , list_to_binary(["attachment;filename=", FileName])}
                ], Req),
            ok = cowboy_req:chunk(Chunk, Req1),
            {ok, Req1};
        _ ->
            ok = cowboy_req:chunk(Chunk, Req),
            {ok, Req}
    end.

set_cookies(Req, Session) ->
    {Host,Req} = cowboy_req:host(Req),
    cowboy_req:set_resp_cookie(?DDERL_COOKIE_NAME, Session,
                                      ?HTTP_ONLY_COOKIE_OPTS(Host), Req).

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
