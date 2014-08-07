-module(dderl_ws).
-behaviour(cowboy_websocket_handler).

-include("dderl.hrl").

%% cowboy websocket interface
-export([
    init/3
    , websocket_init/3
    , websocket_handle/3
    , websocket_info/3
    , websocket_terminate/3
]).

init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined}.

websocket_handle({text, Msg}, Req, State) ->
    case jsx:decode(Msg) of
        [{<<"time">>, _Args}] ->
            {_,_,Micros} = Now = erlang:now(),
            {{Year,Month,Day},{Hour,Minute,Sec}} = calendar:now_to_local_time(Now),
            Time = lists:flatten(io_lib:format("~2..0B.~2..0B.~4..0B ~2..0B:~2..0B:~2..0B.~6..0B", [Day,Month,Year,Hour,Minute,Sec,Micros rem 1000000])),
            erlang:send_after(1000, self(), timeout),
            {reply, {text, list_to_binary(Time)}, Req, State};
        Unsupported ->
            ?Info("port not running cmd can't be served : ~p~n", [Unsupported]),
            {reply, {text, <<>>}, Req, State}
    end;
    
websocket_handle(_Data, Req, State) ->
    ?Info("~p:~p unknown websocket_handle ~p!~n", [?MODULE, ?LINE, _Data]),
    {ok, Req, State}.

websocket_info(timeout, Req, State) ->
    {_,_,Micros} = Now = erlang:now(),
    {{Year,Month,Day},{Hour,Minute,Sec}} = calendar:now_to_local_time(Now),
    Time = lists:flatten(io_lib:format("~2..0B.~2..0B.~4..0B ~2..0B:~2..0B:~2..0B.~6..0B", [Day,Month,Year,Hour,Minute,Sec,Micros rem 1000000])),
    erlang:send_after(1000, self(), timeout),
    {reply, {text, list_to_binary(Time)}, Req, State};
websocket_info(_Info, Req, State) ->
    ?Info("~p:~p unknown websocket_info ~p~n", [?MODULE, ?LINE, _Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ?Info("~p:~p terminating ~p~n", [?MODULE, ?LINE, _Reason]),
    ok.
