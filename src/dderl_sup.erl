-module(dderl_sup).

-behaviour(supervisor).

-include("dderl.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, upgrade/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

-define(TBL_SPEC(T), {T, record_info(fields, T)}).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
%    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    Processes =
    case application:get_env(webconfigs) of
        undefined -> [
                {webmachine_mochiweb,
                    {webmachine_mochiweb, start, [
                        [{name, webmachine_mochiweb},
                        {ip, "127.0.0.1"},
                        {port, 443},
                        {ssl, true},
                        {ssl_opts, [{certfile, "certs/host.cert"},
                                   %{cacertfile,"tmp/api_server.ca.crt"},
                                    {keyfile, "certs/host.key"}
                        ]},
                        {log_dir, "priv/log"},
                        {dispatch, Dispatch}]
                    ]},
                permanent, 5000, worker, dynamic}
        ];
        {ok, WebConfigs} ->  [
            {proplists:get_value(name, Wc, default),
            {webmachine_mochiweb, start, [Wc++[{dispatch, Dispatch}]]},
             permanent, 5000, worker, dynamic}
        || Wc <- WebConfigs
        ]
    end,

    ets:new(dderl_req_sessions, [set, public, named_table]),
    
    init_tables([
              ?TBL_SPEC(accounts)
            , ?TBL_SPEC(common)
        ]),
    %case imem_if:build_table(accounts, [record_info(fields, accounts)]) of
    %    {atomic, ok} -> ok;
    %    {aborted, R1} -> io:format(user, "mnesia:create_table aborted ~p~n", [R1])
    %end,
    %case imem_if:build_table(common, [record_info(fields, common)]) of
    %    {atomic, ok} -> ok;
    %    {aborted, R2} -> io:format(user, "mnesia:create_table aborted ~p~n", [R2])
    %end,
    {ok, { {one_for_one, 10, 10}, Processes} }.

init_tables([]) -> ok;
init_tables([{T,C}|Tables]) ->
    case imem_if:build_table(T, C) of
        {atomic, ok} -> ok;
        {aborted, R} -> io:format(user, "mnesia:create_table aborted ~p~n", [R])
    end,    
    init_tables(Tables).
