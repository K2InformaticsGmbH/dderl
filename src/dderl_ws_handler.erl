-module(dderl_ws_handler).

-include("dderl.hrl").

-export([init/2, websocket_init/1, websocket_info/2, websocket_handle/2]).

-record(state, {session_pid}).

init(Req, Opts) ->
    SessionToken = dderl:get_cookie(dderl_resource:cookie_name(?SESSION_COOKIE), Req, <<>>),
    case global:whereis_name(SessionToken) of
        undefined -> {ok, Req, Opts};
        Pid -> {cowboy_websocket, Req, #state{session_pid = Pid}}
    end.

websocket_init(State) -> {ok, State}.

websocket_handle({text, <<"ping">>}, #state{session_pid = Pid} = State) ->
    Pid ! rearm_session_idle_timer,
    {reply, {text, imem_json:encode(#{ping => node()})}, State};
websocket_handle(_Frame, State) ->
    ?Info("Unhandled Frame : ~p", [_Frame]),
    {ok, State}.

websocket_info({log, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.