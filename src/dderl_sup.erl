-module(dderl_sup).
-behaviour(supervisor).

-include("dderl.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), #{id => I, start => {I, start_link, Args},
                                restart => permanent, shutdown => 5000,
                                type => Type, modules => [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok,_} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10},
          [?CHILD(dderloci_sup, supervisor, []),
           ?CHILD(dderl_dal, worker, []),
           ?CHILD(dderl_rest, worker, []),
           ?CHILD(dderl_metrics, worker, []),
           ?CHILD(dderl_session_sup, supervisor, []),
           ?CHILD(dderl_data_sender_sup, supervisor, []),
           ?CHILD(dderl_data_receiver_sup, supervisor, [])]}}.
