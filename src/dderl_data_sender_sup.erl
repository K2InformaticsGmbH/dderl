-module(dderl_data_sender_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_sender/3
        ,terminate_sender/1
        ,list_senders/0]).

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

-spec start_sender({atom(), pid()}, list(), table | stats) -> {error, term()} | {ok, pid()}.
start_sender(Statement, Data, Type) ->
    supervisor:start_child(?MODULE, [Statement, Data, Type]).

-spec terminate_sender(pid()) -> ok | {error, not_found | simple_one_for_one}.
terminate_sender(SenderPid) ->
    supervisor:terminate_child(?MODULE, SenderPid).

-spec list_senders() -> list().
list_senders() ->
    supervisor:which_children(?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(dderl_data_sender)]}}.
