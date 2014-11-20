-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").

%% API.
-export([ start/0
        , stop/0
        , init/3
        , handle/2
        , terminate/3
        , encrypt/1
        , decrypt/1
        ]).

%% API.
start() ->
    erlimem:start(),
    imem:start(),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    catch dderloci:start(),
	ok = application:start(?MODULE).

stop() ->
    ok = application:stop(?MODULE),
    catch dderloci:stop(),
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    imem:stop(),
    erlimem:stop().

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

-spec encrypt(binary()) -> base64:ascii_binary().
encrypt(Bin) when is_binary(Bin) ->
    base64:encode(Bin).

-spec decrypt(base64:ascii_binary()|base64:ascii_string()) -> binary().
decrypt(BinOrStr) when is_binary(BinOrStr); is_list(BinOrStr) ->
    base64:decode(BinOrStr).

%%encrypt_pid(Pid)    when is_pid(Pid)        -> pid_to_list(Pid).
%%decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(PidStr).
