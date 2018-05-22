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
         reset_routes/1, get_ssl_options/0, add_d3_templates_path/2]).

%% OTP Application API
-export([start/2, stop/1]).

-export([get_url_suffix/0, get_sp_url_suffix/0, format_path/1, priv_dir/0, priv_dir/1]).

%%-----------------------------------------------------------------------------
%% Console Interface
%%-----------------------------------------------------------------------------
start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
    ok = application:stop(?MODULE).

%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Application Interface
%%-----------------------------------------------------------------------------
start(_Type, _Args) ->
    % adding lager imem handler (after IMEM start)
    ok = gen_event:add_handler(
           lager_event, {imem_lager_backend, ?MODULE},
           [{level,info},{tablefun, fun() -> ?LOGTABLE end},
            {application, ?MODULE},
            {tn_event,[{dderl,?MODULE,dderlLogTable}]}]
          ),
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
                [{compress, true},
                 {env, [{dispatch, Dispatch}]},
                 {middlewares, [cowboy_router, dderl_cow_mw, cowboy_handler]}]),
    ?Info(lists:flatten(["URL https://",
                         if is_list(Ip) -> Ip;
                            true -> io_lib:format("~p",[Ip])
                         end, ":~p~s"]), [Port,get_url_suffix()]),
    ?Info("Routes:~n~s~n---", [string:join([lists:flatten(
                                              io_lib:format("~p",[NRP]))
                                            ||NRP<-DDerlRoutes], "\n")]),
    SupRef = dderl_sup:start_link(),
    ?Info("restartable apps ~p", [dderl_dal:get_restartable_apps()]),
    ?Info("DDERL STARTED"),
    ?Info("---------------------------------------------------"),
    SupRef.

stop(_State) ->
    ok = cowboy:stop_listener(https),
    ?Info("SHUTDOWN DDERL"),
    ?Info("---------------------------------------------------"),
        ok = gen_event:delete_handler(
           lager_event, {imem_lager_backend, ?MODULE}, []).
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Cowboy Interface
%%-----------------------------------------------------------------------------

-define(PROBE_RESP,
        ?GET_CONFIG(probeResp,[],
                    {200,
                     <<"<html>"
                       "<body>Service is alive</body>"
                       "</html>">>},
                    "Response given to the load balancer when the probeUrl is requested")).

init(_Transport, Req, '$path_probe') ->
    {Code, Resp} = ?PROBE_RESP,
    {ok, Req1} = cowboy_req:reply(Code, [], Resp, Req),
    {shutdown, Req1, undefined};
init(_Transport, Req, _Opts) -> {ok, Req, undefined}.

handle(Req, State) ->
    {Url, Req} = cowboy_req:url(Req),
    {ok, Req1} = case binary:last(Url) of
                     $/ ->
                         Filename = filename:join([priv_dir(),
                                                   "public", "dist",
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

-spec add_d3_templates_path(atom(), string()) -> ok.
add_d3_templates_path(Application, Path) ->
    dderl_dal:add_d3_templates_path(Application, Path).

%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Local Functions
%%-----------------------------------------------------------------------------
-define(PROBE_URL,
        ?GET_CONFIG(probeUrl,[], "/probe.html",
                    "Defines the url of the probe for the load balancer")).

get_routes() ->
    PrivDir = priv_dir(),
    UrlPathPrefix = get_url_suffix(),
    [{?PROBE_URL, dderl, '$path_probe'},
     {UrlPathPrefix++"/", dderl, []},
     {UrlPathPrefix++"/app/[...]", dderl_resource, []},
     {UrlPathPrefix++ get_sp_url_suffix(), dderl_saml_handler, []},
     {UrlPathPrefix++"/[...]", cowboy_static, {dir, PrivDir}}].


-spec priv_dir() -> list().
priv_dir() -> priv_dir(?MODULE).

-spec priv_dir(atom()) -> list().
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end.

check_file(F) ->
    PrivDir = priv_dir(),
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
            {ok, CertBin} = file:read_file(check_file("certs/server.crt")),
            {ok, KeyBin} = file:read_file(check_file("certs/server.key")),
            Cert = imem_server:get_cert_key(CertBin),
            Key = imem_server:get_cert_key(KeyBin),
            DDErlSslDefault = [{versions, ['tlsv1.2','tlsv1.1',tlsv1]}
                               | Cert ++ Key],
            ?Info("Installing SSL ~p", [DDErlSslDefault]),
            ?PUT_CONFIG(dderlSslOpts, [], #{cert => CertBin, key => KeyBin},
                        list_to_binary(
                          io_lib:format(
                            "Installed at ~p on ~s",
                            [node(), imem_datatype:timestamp_to_io(
                                       imem_meta:time())]))),
            DDErlSslDefault;
        #{cert := CertBin, key := KeyBin} ->
            CertFile = filename:join([priv_dir(), "certs/server.crt"]),
            case file:read_file(CertFile) of
                {ok, CertBin} -> nop;
                _ -> ok = file:write_file(CertFile, CertBin)
            end,
            KeyFile = filename:join([priv_dir(), "certs/server.key"]),
            case file:read_file(KeyFile) of
                {ok, KeyBin} -> nop;
                _ -> ok = file:write_file(KeyFile, KeyBin)
            end,
            Cert = imem_server:get_cert_key(CertBin),
            Key = imem_server:get_cert_key(KeyBin),
            [{versions, ['tlsv1.2','tlsv1.1',tlsv1]} | Cert ++ Key]
    end;
get_ssl_options({ok, SslOpts}) ->
    SslOpts.

get_url_suffix() -> ?URLSUFFIX.

get_sp_url_suffix() -> ?SPURLPREFIX.

format_path([]) -> <<"/">>;
format_path(Path) when is_list(Path) -> list_to_binary(Path).

%%-----------------------------------------------------------------------------
