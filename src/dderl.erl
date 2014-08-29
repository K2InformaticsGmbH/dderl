-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").

%% API.
-export([ start/0
        , init/3
        , handle/2
        , terminate/3
        , encrypt_id/1
        , decrypt_id/1
        ]).

%% API.
start() ->
    ssl:start(),
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [{lager_console_backend, info},
                                               {lager_file_backend, [{file, "log/error.log"},
                                                                     {level, error},
                                                                     {size, 10485760},
                                                                     {date, "$D0"},
                                                                     {count, 5}]},
                                               {lager_file_backend, [{file, "log/console.log"},
                                                                     {level, info},
                                                                     {size, 10485760},
                                                                     {date, "$D0"},
                                                                     {count, 5}]}]),
    ok = application:set_env(lager, error_logger_redirect, false),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:load(sasl),
    ok = application:set_env(sasl, sasl_error_logger, false),
    ok = application:start(sasl),
    ok = application:start(os_mon),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    erlimem:start(),
    application:start(sqlparse),% maybe already started by imem 
    imem:start(),
	ok = application:start(?MODULE).


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

-spec get_html() -> binary().
get_html() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end,
	Filename = filename:join([PrivDir, "login.html"]),
	{ok, Binary} = file:read_file(Filename),
	Binary.

-spec encrypt_id(binary()) -> base64:ascii_binary().
encrypt_id(Bin) when is_binary(Bin) -> base64:encode(Bin).

-spec decrypt_id(base64:ascii_binary()|base64:ascii_string()) -> binary().
decrypt_id(BinOrStr) when is_binary(BinOrStr); is_list(BinOrStr) -> base64:decode(BinOrStr).

%%encrypt_pid(Pid)    when is_pid(Pid)        -> pid_to_list(Pid).
%%decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(PidStr).
