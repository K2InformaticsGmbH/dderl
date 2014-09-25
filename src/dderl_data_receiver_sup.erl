-module(dderl_data_receiver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_receiver/3
        ,terminate_receiver/1
        ,list_receivers/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, temporary, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_receiver({atom(), pid()}, [integer()], pid()) -> {error, term()} | {ok, pid()}.
start_receiver(Statement, ColumnPositions, SenderPid) ->
	supervisor:start_child(?MODULE, [Statement, ColumnPositions, SenderPid]).

-spec terminate_receiver(pid()) -> ok | {error, not_found | simple_one_for_one}.
terminate_receiver(ReceiverPid) ->
    supervisor:terminate_child(?MODULE, ReceiverPid).

-spec list_receivers() -> list().
list_receivers() ->
    supervisor:which_children(?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(dderl_data_receiver)]}}.
