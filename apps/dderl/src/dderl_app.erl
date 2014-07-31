% @private
-module(dderl_app).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(application).

-include("dderl.hrl").

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

get_priv_dir() ->
    case code:priv_dir(dderl) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end.

check_file(F) ->
    PrivDir = get_priv_dir(),
    File = filename:join([PrivDir, F]),
    IsFile = filelib:is_file(File),
    if IsFile =:= true -> ok;
        true ->
            throw("File "++File++" doesn't exists")
    end,
    File.

start(_Type, _Args) ->
    PrivDir = get_priv_dir(),
    SbsBaseDir = PrivDir ++ "/../../../deps/sbs_gui",
    SbsBower = SbsBaseDir ++ "/bower_components",
    SbsAppDir = SbsBaseDir ++ "/app",
    Dispatch = cowboy_router:compile([
		{'_', [
            {"/", dderl, []},
            {"/ws", bullet_handler, [{handler, dderl_stream}]},
            {"/app/[...]", dderl_resource, []},
            {"/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}},
            {"/sbs/", cowboy_static, {file, SbsAppDir ++ "/index.html"}},
            {"/sbs/bower_components/[...]", cowboy_static, {dir, SbsBower}},
            {"/sbs/[...]", cowboy_static, {dir, SbsAppDir}},
            {"/[...]", cowboy_static, {dir, PrivDir}}
		]}
	]),

    {ok, Ip}         = application:get_env(dderl, interface),
    {ok, Port}       = application:get_env(dderl, port),
    {ok, CaCertF}    = application:get_env(dderl, ssl_cacertfile),
    {ok, CertF}      = application:get_env(dderl, ssl_certfile),
    {ok, KeyF}       = application:get_env(dderl, ssl_keyfile),

    CaCertFile = check_file(CaCertF),
    CertFile = check_file(CertF),
    KeyFile = check_file(KeyF),
    
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
	dderl_sup:start_link().

stop(_State) ->
	ok.
