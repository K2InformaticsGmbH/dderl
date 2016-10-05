-module(dderl_session_sup).
-behaviour(supervisor).

-include("dderl.hrl").

%% API
-export([start_link/0
        ,start_session/2
        ,close_session/1
        ,list_sessions/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, temporary, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
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

-spec start_session(binary(), fun(() -> map())) -> {error, term()} | {ok, pid()}.
start_session(Token, ConnInfoFun) when is_function(ConnInfoFun, 0) ->
	supervisor:start_child(?MODULE, [Token, ConnInfoFun]).

-spec close_session(binary()) -> ok | {error, not_found | simple_one_for_one}.
close_session(Token) ->
    supervisor:terminate_child(?MODULE, Token).

-spec list_sessions() -> list().
list_sessions() ->
    %%TODO: We should return more information, maybe ip and username.
    supervisor:which_children(?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(dderl_session)]}}.
