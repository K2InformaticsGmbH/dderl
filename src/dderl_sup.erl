-module(dderl_sup).
-behaviour(supervisor).

-include("dderl.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

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
    {ok, SchemaName} = application:get_env(imem, mnesia_schema_name),
    {ok, { {one_for_one, 5, 10}, [?CHILD(dderl_dal, worker, [SchemaName])
                                 ,?CHILD(dderl_session_sup, supervisor, [])
                                 ,?CHILD(dderl_data_sender_sup, supervisor, [])
                                 ,?CHILD(dderl_data_receiver_sup, supervisor, [])
                                 ]} }.

