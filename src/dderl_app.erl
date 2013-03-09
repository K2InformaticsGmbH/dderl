%% @private
-module(dderl_app).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
		{'_', [
            {"/", dderl, []},
            {"/ws", weberl_ws, []},
            {"/app/[...]", dderl_resource, []},
            {"/[...]", cowboy_static, [
                {directory, {priv_dir, dderl, []}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
		]}
	]),

    {ok, Interface}     = application:get_env(dderl, interface),
    {ok, Port}          = application:get_env(dderl, port),
    {ok, CaCertFile}    = application:get_env(dderl, ssl_cacertfile),
    {ok, CertFile}      = application:get_env(dderl, ssl_certfile),
    {ok, KeyFile}       = application:get_env(dderl, ssl_keyfile),

    {ok, _} = cowboy:start_https(https, 100, [
        {ip, Interface},
		{port, Port},
		{cacertfile, CaCertFile},
		{certfile, CertFile},
		{keyfile, KeyFile}
	], [{env, [{dispatch, Dispatch}]}]),

    case application:get_env(dderl, flash_fallback) of
        {ok, true} ->
            %% we serve the flash policy file which MUST be on port 843
            %% you need root privileges to run this
            ranch:start_listener(flash_fallback, 100,
                                 ranch_tcp, [{port, 843}],
                                 flash_policy, []);
        _ ->
            ok
    end,
	dderl_sup:start_link().

stop(_State) ->
	ok.
