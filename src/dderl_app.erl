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
    % external routes
    {ok, MasterUrlRoutePaths} = application:get_env(dderl, master_paths),
    % default routes
    NewRoutePaths = MasterUrlRoutePaths ++
        [{"/", dderl, []},
         {"/ws", bullet_handler, [{handler, dderl_stream}]},
         {"/app/[...]", dderl_resource, []},
         {"/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}},
         {"/[...]", cowboy_static, {dir, PrivDir}}],
    ok = application:set_env(dderl, master_paths, NewRoutePaths),
    ?Info("DDerl started with route paths ~p", [NewRoutePaths]),
    Dispatch = cowboy_router:compile([{'_', NewRoutePaths}]),

    {ok, Ip}         = application:get_env(dderl, interface),
    {ok, Port}       = application:get_env(dderl, port),
    case application:get_env(dderl, ssl_opts) of
        {ok, []} ->
            SslOpts = [{certfile, check_file("certs/server.crt")}
                      ,{cacertfile, check_file("certs/cowboy-ca.crt")}
                      ,{keyfile, check_file("certs/server.key")}
                      ,{versions, ['tlsv1.2','tlsv1.1',tlsv1]}];
        {ok, SslOpts} -> SslOpts
    end,
                
    {ok, Interface} = inet:getaddr(Ip, inet),
    {ok, _} = cowboy:start_https(https, 100,
        [
            {ip, Interface},
            {port, Port}
            | SslOpts],
        [{env, [{dispatch, Dispatch}]}]),
    ?Info("---------------------------------------------------"),
    ?Info("STARTING DDERL"),
    ?Info(lists:flatten(["URL https://", if is_list(Ip) -> Ip;
                                            true -> io_lib:format("~p",[Ip])
                                         end, ":~p"]), [Port]),
    SupRef = dderl_sup:start_link(),
    ?Info("DDERL STARTED"),
    ?Info("---------------------------------------------------"),
    SupRef.

stop(_State) ->
    ?Info("SHUTDOWN DDERL"),
    ok.
