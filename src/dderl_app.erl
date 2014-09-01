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
    {ok, MasterUrlRoutePaths} = application:get_env(dderl, master_paths),
    ?Info("MasterPaths ~p", [MasterUrlRoutePaths]),
    Dispatch = cowboy_router:compile([
		{'_',
            % sbs routes
            MasterUrlRoutePaths ++
            % default routes
            [{"/", dderl, []},
            {"/ws", bullet_handler, [{handler, dderl_stream}]},
            {"/app/[...]", dderl_resource, []},
            {"/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}},
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

    {ok, PrivateFile}   = application:get_env(dderl, crypt_private),
    {ok, PublicFile}    = application:get_env(dderl, crypt_public),
    {ok, RsaPassword}   = application:get_env(dderl, crypt_password),    
    MaybePrivateFile = filename:join(PrivDir, PrivateFile),
    MaybePublicFile = filename:join(PrivDir, PublicFile),
    RsaPrivateFile = case filelib:is_file(MaybePrivateFile) of
                         false -> PrivateFile;
                         true -> MaybePrivateFile
                     end,
    RsaPublicFile = case filelib:is_file(MaybePublicFile) of
                         false -> PublicFile;
                         true -> MaybePublicFile
                     end,
    {ok, PrivateBin} = file:read_file(RsaPrivateFile),
    {ok, PublicBin} = file:read_file(RsaPublicFile),
    [PrivateRSAEntry] = public_key:pem_decode(PrivateBin),
    [PublicRSAEntry] = public_key:pem_decode(PublicBin),
    PrivateKey = public_key:pem_entry_decode(PrivateRSAEntry, RsaPassword),
    PublicKey = public_key:pem_entry_decode(PublicRSAEntry),

    ok = application:set_env(dderl, crypt_private_key, PrivateKey),
    ok = application:set_env(dderl, crypt_public_key, PublicKey),

	dderl_sup:start_link().

stop(_State) ->
	ok.
