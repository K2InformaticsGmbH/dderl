-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

%% API.
-export([ start/0
        , init/3
        , handle/2
        , terminate/3
        , encrypt_pid/1
        , decrypt_pid/1
        ]).

%% API.

-ifdef(LAGER).
start() ->
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [{lager_console_backend, info},
                                               {lager_file_backend, [{"error.log", error, 10485760, "$D0", 5},
                                                                     {"console.log", info, 10485760, "$D0", 5}]}]),
    ok = application:set_env(lager, error_logger_redirect, false),
    ok = lager:start(),
    ok = imem:start(),
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(dderl).
-else.
start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
    ok = imem:start(),
	ok = application:start(dderl).
-endif.


init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Html = get_html(),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_html() ->
	{ok, Cwd} = file:get_cwd(),
	Filename =filename:join([Cwd, "priv", "login.html"]),
	{ok, Binary} = file:read_file(Filename),
	Binary.

% encrypt_pid(Pid)    when is_pid(Pid)        -> base64:encode_to_string(pid_to_list(Pid)).
% decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(base64:decode_to_string(PidStr)).
encrypt_pid(Pid)    when is_pid(Pid)        -> pid_to_list(Pid).
decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(PidStr).
