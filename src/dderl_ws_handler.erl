-module(dderl_ws_handler).

-include("dderl.hrl").

-export([init/3, websocket_init/3, websocket_info/3, websocket_handle/3]).

-record(state, {dderl_session}).

init({ssl, http}, Req, Opts) ->
    ?Info("got init req"),
    {SessionToken, Req4} = cowboy_req:cookie(cookie_name(?SESSION_COOKIE, Req1), Req3, <<>>),
    {XSRFToken, Req4} = cowboy_req:header(?XSRF_HEADER, Req4, <<>>),
    {upgrade, protocol, cowboy_websocket, Req, Opts}.

websocket_init(_Type, Req, _Opts) ->
    ?Info("Webscoket opened"),
    {ok, Req, #state{}}.

websocket_handle(Frame = {text, <<"ping">>}, Req, State) ->
    ?Info("Handled Frame : ~p", [Frame]),
    {reply, {text, imem_json:encode(#{ping => pong})}, Req, State};
websocket_handle(Frame = {text, _}, Req, State) ->
    ?Info("Handled Frame : ~p", [Frame]),
    {reply, Frame, Req, State};
websocket_handle(_Frame, Req, State) ->
    ?Info("Unhandled Frame : ~p", [_Frame]),
    {ok, Req, State}.

websocket_info(hello_world, Req, State) ->
    {reply, [
        {text, "Hello"},
        {text, <<"world!">>},
        {binary, <<0:8000>>}
    ], Req, State};
websocket_info({log, Text}, Req, State) ->
    {reply, {text, Text}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.