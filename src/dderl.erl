-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(application).

-include("dderl.hrl").

%% Script interface for OTP application control
-export([start/0, stop/0]).

%% Cowboy callbacks
-export([init/3, handle/2, terminate/3]).

%% Private interfaces
-export([encrypt/1, decrypt/1, insert_mw/2, insert_routes/2, remove_mw/2,
         reset_routes/1]).

%% OTP Application API
-export([start/2, stop/1]).

%%-----------------------------------------------------------------------------
%% Console Interface
%%-----------------------------------------------------------------------------
start() ->
    imem:start(),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    erlimem:start(),
    catch dderloci:start(),
	ok = application:start(?MODULE).

stop() ->
    ok = application:stop(?MODULE),
    catch dderloci:stop(),
    erlimem:stop(),
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    imem:stop().

%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Application Interface
%%-----------------------------------------------------------------------------
start(_Type, _Args) ->
    {ok, Ip}   = application:get_env(dderl, interface),
    {ok, Port} = application:get_env(dderl, port),
    {ok, PemCrt} = file:read_file(check_file("certs/server.crt")),
    [{'Certificate',Cert,not_encrypted}] = public_key:pem_decode(PemCrt),
    {ok, PemKey} = file:read_file(check_file("certs/server.key")),
    [{KeyType,Key, not_encrypted}] = public_key:pem_decode(PemKey),
    SslOptions = case application:get_env(dderl, ssl_opts) of
                     {ok, []} ->
                         ?GET_CONFIG(dderlSslOpts,[],
                                     [{cert, Cert}, {key, {KeyType,Key}},
                                      {versions, ['tlsv1.2','tlsv1.1',tlsv1]}]);
                     {ok, SslOpts} -> SslOpts
                 end,
    {ok, Interface} = inet:getaddr(Ip, inet),
    DDerlRoutes = get_routes(),
    Dispatch = cowboy_router:compile([{'_', DDerlRoutes}]),
    {ok, _} = cowboy:start_https(
                https, 100, [{ip, Interface}, {port, Port} | SslOptions],
                [{env, [{dispatch, Dispatch}]},
                 {middlewares, [cowboy_router, dderl_cow_mw, cowboy_handler]}]),
    % adding lager imem handler (after IMEM start)
    LogTableNameFun =
        fun() ->
            ?GET_CONFIG(dderlLogTable,[],'dderlLog_86400@')
        end,
    ok = gen_event:add_handler(
           lager_event, {imem_lager_backend, dderl},
           [{level,info},{tablefun,LogTableNameFun},{application,dderl},
            {tn_event,[{dderl,?MODULE,dderlLogTable}]}]
          ),
    ?Info("---------------------------------------------------"),
    ?Info("STARTING DDERL"),
    ?Info(lists:flatten(["URL https://",
                         if is_list(Ip) -> Ip;
                            true -> io_lib:format("~p",[Ip])
                         end, ":~p~s"]), [Port,?URLSUFFIX]),
    ?Info("Routes:~n~s~n---", [string:join([lists:flatten(
                                              io_lib:format("~p",[NRP]))
                                            ||NRP<-DDerlRoutes], "\n")]),
    SupRef = dderl_sup:start_link(),
    ?Info("DDERL STARTED"),
    ?Info("---------------------------------------------------"),
    SupRef.

stop(_State) ->
    ok = gen_event:delete_handler(lager_event, {imem_lager_backend, dderl}, []),
    ok = cowboy:stop_listener(https),
    ?Info("SHUTDOWN DDERL"),
    ?Info("---------------------------------------------------").
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Cowboy Interface
%%-----------------------------------------------------------------------------
init(_Transport, Req, _Opts) -> {ok, Req, undefined}.

handle(Req, State) ->
    {Url, Req} = cowboy_req:url(Req),
    {ok, Req1} = case binary:last(Url) of
                     $/ ->
                         Filename = filename:join([get_priv_dir(),
                                                   "login.html"]),
                         {ok, Html} = file:read_file(Filename),
                         cowboy_req:reply(
                           200, [{<<"content-type">>, <<"text/html">>}],
                           Html, Req);
                     _ ->
                         cowboy_req:reply(
                           301, [{<<"Location">>, <<Url/binary,"/">>}],
                           <<>>, Req)
                 end,
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) -> ok.
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Public APIs
%%-----------------------------------------------------------------------------
-spec encrypt(binary()) -> base64:ascii_binary().
encrypt(Bin) when is_binary(Bin) ->
    base64:encode(Bin).

-spec decrypt(base64:ascii_binary()|base64:ascii_string()) -> binary().
decrypt(BinOrStr) when is_binary(BinOrStr); is_list(BinOrStr) ->
    base64:decode(BinOrStr).

%%encrypt_pid(Pid)    when is_pid(Pid)        -> pid_to_list(Pid).
%%decrypt_pid(PidStr) when is_list(PidStr)    -> list_to_pid(PidStr).

-spec insert_mw(atom(), atom()) -> atom().
insert_mw(Intf, MwMod) when is_atom(Intf), is_atom(MwMod) ->
    Opts = ranch:get_protocol_options(Intf),
    {value, {middlewares, Middlewares}, Opts1} = lists:keytake(middlewares,
                                                               1, Opts),
    [LastMod|RestMods] = lists:reverse(Middlewares),    
    ok = ranch:set_protocol_options(
           https, [{middlewares, lists:reverse([LastMod, MwMod | RestMods])}
                   | Opts1]).

-spec remove_mw(atom(), atom()) -> atom().
remove_mw(Intf, MwMod) when is_atom(Intf), is_atom(MwMod) ->
    Opts = ranch:get_protocol_options(Intf),
    {value, {middlewares, Middlewares}, Opts1} = lists:keytake(middlewares,
                                                               1, Opts),
    ok = ranch:set_protocol_options(
           https, [{middlewares, Middlewares -- [MwMod]} | Opts1]).

-spec insert_routes(atom(), list()) -> ok.
insert_routes(Intf, [{'_',[],Dispatch}]) ->
    Opts = ranch:get_protocol_options(Intf),
    {value, {env, [{dispatch,[{'_',[],OldDispatches}]}]}, Opts1}
    = lists:keytake(env, 1, Opts),
    ok = ranch:set_protocol_options(
           https, [{env, [{dispatch,[{'_',[],OldDispatches++Dispatch}]}]}
                   | Opts1]).


-spec reset_routes(atom()) -> ok.
reset_routes(Intf) ->
    Opts = ranch:get_protocol_options(Intf),
    {value, {env, [{dispatch,_}]}, Opts1} = lists:keytake(env, 1, Opts),
    [{'_',[],DefaultDispatches}] = cowboy_router:compile([{'_', get_routes()}]),
    ok = ranch:set_protocol_options(
           https, [{env, [{dispatch,[{'_',[],DefaultDispatches}]}]}
                   | Opts1]).

%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Local Functions
%%-----------------------------------------------------------------------------
get_routes() ->
    PrivDir = get_priv_dir(),
    UrlPathPrefix = ?URLSUFFIX,
    [{UrlPathPrefix++"/", dderl, []},
     {UrlPathPrefix++"/ws", bullet_handler, [{handler, dderl_stream}]},
     {UrlPathPrefix++"/app/[...]", dderl_resource, []},
     {UrlPathPrefix++"/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}},
     {UrlPathPrefix++"/[...]", cowboy_static, {dir, PrivDir}}].

get_priv_dir() ->
    case code:priv_dir(?MODULE) of
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
%%-----------------------------------------------------------------------------
