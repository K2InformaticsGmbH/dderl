-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-export([ start/0
        , start_link/0
        , stop/0
        , ensure_started/1
        , encrypt_pid/1
        , decrypt_pid/1
        ]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    inets:start(),
    ensure_started(erloci),
    ensure_started(crypto),
    ensure_started(mochiweb),
%    application:set_env(webmachine, webmachine_logger_module, 
%                        webmachine_log),
    ensure_started(webmachine),
    webmachine_demo_sup:start_link().

%% @spec start() -> ok
%% @doc Start the webmachine_demo server.
start() ->
    inets:start(),
    ensure_started(erloci),
    ensure_started(crypto),
    ensure_started(mochiweb),
%    application:set_env(webmachine, webmachine_logger_module, 
%                        webmachine_log),
    ensure_started(webmachine),
    application:start(dderl).

%% @spec stop() -> ok
%% @doc Stop the webmachine_demo server.
stop() ->
    Res = application:stop(dderl),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(erloci),
    application:stop(imem),
    Res.

% encrypt_pid(Pid)    when is_pid(Pid)        -> base64:encode_to_string(pid_to_list(Pid)).
% decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(base64:decode_to_string(PidStr)).
encrypt_pid(Pid)    when is_pid(Pid)        -> pid_to_list(Pid).
decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(PidStr).
