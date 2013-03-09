%% @private
-module(dderl_app).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(application).

-include("dderl.hrl").

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

	%{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
	%	{env, [{dispatch, Dispatch}]}
	%]),

    {ok, Ip}            = application:get_env(dderl, interface),
    {ok, Port}          = application:get_env(dderl, port),
    {ok, CaCertFile}    = application:get_env(dderl, ssl_cacertfile),
    {ok, CertFile}      = application:get_env(dderl, ssl_certfile),
    {ok, KeyFile}       = application:get_env(dderl, ssl_keyfile),
    
    ?Info(lists:flatten([ "starting dderl at https://"
                        , if is_list(Ip) -> Ip; true -> io_lib:format("~p",[Ip]) end
                        , ":~p"]), [Port]),

    {ok, Interface} = inet:getaddr(Ip, inet),
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
