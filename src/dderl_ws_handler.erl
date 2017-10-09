-module(dderl_ws_handler).

-include("dderl.hrl").

-export([init/2, websocket_init/1, websocket_info/2, websocket_handle/2]).

-record(state, {dderl_session}).

init(Req, Opts) ->
    ?Info("got init req"),
    Port = cowboy_req:port(Req),
    SessCookie = list_to_binary([?SESSION_COOKIE, integer_to_list(Port)]),
    SessionToken = dderl:get_cookie(SessCookie, Req, <<>>),
    case global:whereis_name(SessionToken) of
        undefined -> {ok, Req, Opts};
        _Pid -> {cowboy_websocket, Req, #state{dderl_session = SessionToken}}
    end.

websocket_init(State) -> {ok, State}.

websocket_handle(Frame = {text, <<"ping">>}, State) ->
    ?Info("Handled Frame : ~p", [Frame]),
    {reply, {text, imem_json:encode(#{ping => pong})}, State};
websocket_handle(Frame = {text, _}, State) ->
    ?Info("Handled Frame : ~p", [Frame]),
    {reply, Frame, State};
websocket_handle(_Frame, State) ->
    ?Info("Unhandled Frame : ~p", [_Frame]),
    {ok, State}.

websocket_info({log, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.