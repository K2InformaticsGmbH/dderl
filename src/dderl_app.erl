-module(dderl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    imem:start(),
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, info},
                                          {lager_imem, [{db, "DDerlLog"},
                                                        {table, dderlLogs},
                                                        {level, info},
                                                        {user, <<"admin">>},
                                                        {password, <<"change_on_install">>}]},
                                          {lager_file_backend,
                                           [{"error.log", error, 10485760, "$D0", 5},
                                            {"console.log", info, 10485760, "$D0", 5}]}]),
    application:set_env(lager, error_logger_redirect, false),
    lager:start(),
    %lager:set_loglevel(lager_console_backend, debug),
    dderl_sup:start_link().

stop(_State) ->
    ok.
