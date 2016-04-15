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

-export([access/10]).

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
    ?Info("---------------------------------------------------"),
    ?Info("STARTING DDERL"),
    {ok, Ip}   = application:get_env(dderl, interface),
    {ok, Port} = application:get_env(dderl, port),
    {ok, Interface} = inet:getaddr(Ip, inet),
    DDerlRoutes = get_routes(),
    Dispatch = cowboy_router:compile([{'_', DDerlRoutes}]),
    SslOptions = get_ssl_options(),
    {ok, _} = cowboy:start_https(
                https, ?MAXACCEPTORS,
                [{ip, Interface}, {port, Port},
                 {max_connections, ?MAXCONNS} | SslOptions],
                [{env, [{dispatch, Dispatch}]},
                 {middlewares, [cowboy_router, dderl_cow_mw, cowboy_handler]}]),
    % adding lager imem handler (after IMEM start)
    ok = gen_event:add_handler(
           lager_event, {imem_lager_backend, dderl},
           [{level,info},{tablefun, fun() -> ?LOGTABLE end},{application,dderl},
            {tn_event,[{dderl,?MODULE,dderlLogTable}]}]
          ),
    ok = gen_event:add_handler(
           lager_event, dderl_access_lager_file_backend,
           [{file, "log/dderl_access.log"}, {level, debug}, {size, 10485760},
            {date, "$D0"}, {count, 5}]),
    ok = lager:set_loglevel(dderl_access_lager_file_backend, debug),
    ?Info(lists:flatten(["URL https://",
                         if is_list(Ip) -> Ip;
                            true -> io_lib:format("~p",[Ip])
                         end, ":~p~s"]), [Port,?URLSUFFIX]),
    ?Info("Routes:~n~s~n---", [string:join([lists:flatten(
                                              io_lib:format("~p",[NRP]))
                                            ||NRP<-DDerlRoutes], "\n")]),
    SupRef = dderl_sup:start_link(),
    ?Info("restartable apps ~p", [dderl_dal:get_restartable_apps()]),
    ?Info("DDERL STARTED"),
    ?Info("---------------------------------------------------"),
    SupRef.

stop(_State) ->
    ok = gen_event:delete_handler(lager_event, {imem_lager_backend, dderl}, []),
    ok = gen_event:delete_handler(lager_event, dderl_access_lager_file_backend, []),
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
                                                   "static",
                                                   "index.html"]),
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
     {UrlPathPrefix++"/app/[...]", dderl_resource, []},
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

get_ssl_options() ->
    get_ssl_options(application:get_env(dderl, ssl_opts)).
get_ssl_options({ok, []}) ->
    case ?SSLOPTS of
        '$no_ssl_conf' ->
            {ok, PemCrt} = file:read_file(check_file("certs/server.crt")),
            [{'Certificate',Cert,not_encrypted} | Certs]
            = AllCerts = public_key:pem_decode(PemCrt),
            CACerts = [C || {'Certificate',C,not_encrypted} <- Certs],
            {ok, PemKey} = file:read_file(check_file("certs/server.key")),
            [{KeyType,Key,not_encrypted}|_] = AllKeys = public_key:pem_decode(PemKey),
            DDErlSslDefault =
            [{cert, Cert},
             {key, {KeyType,Key}},
             {versions, ['tlsv1.2','tlsv1.1',tlsv1]}
             | if length(CACerts) > 0 ->
                      [{cacerts,CACerts}];
                  true -> []
               end],
            ?Info("Installing SSL certificates ~p~nKeys ~p",
                  [[public_key:pem_entry_decode(C)||C<-AllCerts],
                   [public_key:pem_entry_decode(K)||K<-AllKeys]]),
            ?PUT_CONFIG(dderlSslOpts, [], DDErlSslDefault,
                        list_to_binary(
                          io_lib:format("Installed at ~p on ~s",
                                        [node(), imem_datatype:timestamp_to_io(os:timestamp())]
                                       ))),
            DDErlSslDefault;
        DDErlSslOpts -> DDErlSslOpts
    end;
get_ssl_options({ok, SslOpts}) ->
    SslOpts.

% dderl:access(1, "", "", "", "", "", "", "", "", "").
access(LogLevel, SrcIp, User, SessId, Cmd, CmdArgs, ConnUser, ConnTarget, 
       ConnDBType, ConnStr) when is_binary(CmdArgs) ->
    access(LogLevel, SrcIp, User, SessId, Cmd, binary_to_list(CmdArgs), ConnUser,
           ConnTarget, ConnDBType, ConnStr);
access(LogLevel, SrcIp, User, SessId, Cmd, CmdArgs, ConnUser, ConnTarget, ConnDBType,
       ConnStr) when is_tuple(SrcIp) ->
    access(LogLevel, inet:ntoa(SrcIp), User, SessId, Cmd, CmdArgs, ConnUser,
           ConnTarget, ConnDBType, ConnStr);
access(LogLevel, SrcIp, User, SessId, Cmd, CmdArgs, ConnUser, ConnTarget, ConnDBType,
    ConnStr) ->
    log(?ACTLOGLEVEL, LogLevel, SrcIp, User, SessId, Cmd, CmdArgs, ConnUser,
        ConnTarget, ConnDBType, ConnStr).

log(MinLogLevel, LogLevel, _, _, _, _, _, _, _, _, _)
  when MinLogLevel < LogLevel -> ok;
log(_, LogLevel, SrcIp, User, SessId, Cmd, CmdArgs, ConnUser, ConnTarget, ConnDBType,
    ConnStr) ->
    Proxy = case ?PROXY of
                SrcIp -> "yes";
                _ -> "no"
            end,
    Version = case proplists:get_value(
                     vsn, element(2, application:get_all_key(?MODULE))) of
                  undefined -> "";
                  Vsn -> Vsn
              end,
    LL = if is_integer(LogLevel) -> integer_to_list(LogLevel);
                  true -> LogLevel end,
    ?Access(#{proxy => Proxy, version => Version, loglevel => LL,
              src => SrcIp, dderlUser => User, dderlSessId => SessId,
              dderlCmd => Cmd, dderlCmdArgs => CmdArgs, connUser => ConnUser,
              connTarget => ConnTarget, connDbType => ConnDBType,
              connStr => ConnStr}).
%%-----------------------------------------------------------------------------
