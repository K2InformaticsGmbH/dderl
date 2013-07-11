%% Feel free to use, reuse and abuse the code in this file.

%% @doc Stream handler for clock synchronizing.
-module(dderl_stream).

-include("dderl.hrl").

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(Transport, Req, Opts, Active) ->
    ?Debug("init~n Transport: ~p~nRequest: ~p~nOpts: ~p~nActive: ~p~n",
           [Transport, Req, Opts, Active]),
    TRef = erlang:send_after(?PERIOD, self(), refresh),
    {ok, Req, TRef}.

stream(<<"ping">>, Req, State) ->
    {reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
    ?Debug("unknown stream data received ~s~n", [Data]),
    {ok, Req, State}.

info(refresh, Req, _) ->
    TRef = erlang:send_after(?PERIOD, self(), refresh),
    Time = get_time(),
    {reply, Time, Req, TRef};
info(Info, Req, State) ->
    ?Debug("message received ~p~n", [Info]),
    {ok, Req, State}.

terminate(_Req, TRef) ->
    ?Debug("terminate~n"),
    is_reference(TRef) andalso erlang:cancel_timer(TRef),
    ok.

get_time() ->
    {_,_,Micros} = Now = erlang:now(),
    {{Year,Month,Day},{Hour,Minute,Sec}} = calendar:now_to_local_time(Now),
    Time = iolist_to_binary(io_lib:format("~2..0B.~2..0B.~4..0B ~2..0B:~2..0B:~2..0B.~6..0B", [Day,Month,Year,Hour,Minute,Sec,Micros rem 1000000])),
    Time.
