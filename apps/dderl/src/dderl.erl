-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").

%% API.
-export([ start/0
        , init/3
        , handle/2
        , terminate/3
        , encrypt_pid/1
        , decrypt_pid/1
        ]).

%% API.
start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
    ok = application:start(erlimem),
    application:start(sqlparse),% maybe already started by imem 
    ok = application:start(imem),
	ok = application:start(dderl).


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
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end,
	Filename = filename:join([PrivDir, "login.html"]),
	{ok, Binary} = file:read_file(Filename),
	Binary.

% encrypt_pid(Pid)    when is_pid(Pid)        -> base64:encode_to_string(pid_to_list(Pid)).
% decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(base64:decode_to_string(PidStr)).
encrypt_pid(Pid)    when is_pid(Pid)        -> pid_to_list(Pid).
decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(PidStr).
