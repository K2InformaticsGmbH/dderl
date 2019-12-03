-module(dderl).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behaviour(application).

-include("dderl.hrl").
-include("dderl_request.hrl").

%% Script interface for OTP application control
-export([start/0, stop/0]).

%% Cowboy callbacks
-export([init/2, terminate/3]).

%% Private interfaces
-export([encrypt/1, decrypt/1, insert_mw/2, insert_routes/2, remove_mw/2,
         reset_routes/1, get_ssl_options/0, add_d3_templates_path/2]).

%% OTP Application API
-export([start/2, stop/1]).

-export([get_url_suffix/0, get_sp_url_suffix/0, format_path/1, priv_dir/0,
         priv_dir/1, log_table/0]).

%% Helper functions
-export([get_cookie/3, keyfetch/3, cow_req_set_meta/4, cow_req_get_meta/4,
         can_handle_request/1, exec_coldstart_cb/2, local_ipv4s/0,
         unauthorized/2]).

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
    ?Info("---------------------------------------------------"),
    ?Info("STARTING DDERL"),
    ?COLDSTART_CB(<<"fun() ->  end">>),
    {ok, Ip}   = application:get_env(dderl, interface),
    {ok, Port} = application:get_env(dderl, port),
    {ok, Interface} = inet:getaddr(Ip, inet),
    DDerlRoutes = get_routes(),
    Dispatch = cowboy_router:compile([{'_', DDerlRoutes}]),
    SslOptions = get_ssl_options(),
    {ok, _} = cowboy:start_tls(
                https,
                [{ip, Interface}, {port, Port},
                 {num_acceptors, ?MAXACCEPTORS},
                 {max_connections, ?MAXCONNS} | SslOptions],
                #{env => #{dispatch => Dispatch},
                  stream_handlers => [cowboy_compress_h, cowboy_stream_h],
                  middlewares => [cowboy_router, dderl_cow_mw, cowboy_handler]}),
    IpStr = case Ip of
                "0.0.0.0"            -> "127.0.0.1";
                {0,0,0,0}            -> "127.0.0.1";
                Ip when is_list(Ip)  -> Ip;
                Ip when is_tuple(Ip) -> inet:ntoa(Ip)
            end,
    ?Info("URL https://~s:~p~s", [IpStr , Port, get_url_suffix()]),
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
    ?Info("---------------------------------------------------").

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

init(Req, '$path_probe') ->
    {Code, Resp} = ?PROBE_RESP,
    {ok, cowboy_req:reply(Code, #{}, Resp, Req), undefined};
init(Req, State) ->
    Url = iolist_to_binary(cowboy_req:uri(Req)),
    Req1 =
    case binary:last(Url) of
        $/ ->
            Priv = priv_dir(),
            Filename = filename:join([Priv, "public", "dist", "index.html"]),
            case file:read_file(Filename) of
                {ok, Html} ->
                    cowboy_req:reply(
                        200, #{<<"content-type">> => <<"text/html">>}, Html,
                        Req
                    );
                {error, Error} ->
                    ?Error("unable to read ~s : ~p", [Filename, Error]),
                    cowboy_req:reply(
                        500, #{<<"content-type">> => <<"text/html">>},
                        <<
                            "<p><b style='color:red'>ERROR</b> ",
                            (list_to_binary(Filename))/binary, " not found</p>"
                        >>,
                        Req
                    )
            end;
        _ ->
            cowboy_req:reply(301, #{<<"location">> => <<Url/binary,"/">>}, Req)
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
    #{middlewares := Middlewares} = Opts = ranch:get_protocol_options(Intf),
    [LastMod|RestMods] = lists:reverse(Middlewares),
    ok = ranch:set_protocol_options(
           https, Opts#{middlewares => lists:reverse([LastMod, MwMod | RestMods])}).

-spec remove_mw(atom(), atom()) -> atom().
remove_mw(Intf, MwMod) when is_atom(Intf), is_atom(MwMod) ->
    #{middlewares := Middlewares} = Opts = ranch:get_protocol_options(Intf),
    ok = ranch:set_protocol_options(
           https, Opts#{middlewares => Middlewares -- [MwMod]}).

-spec insert_routes(atom(), list()) -> ok.
insert_routes(Intf, [{'_',[],Dispatch}]) ->
    #{env := #{dispatch := [{'_',[],OldDispatches}]}} = Opts = ranch:get_protocol_options(Intf),
    ok = ranch:set_protocol_options(
           https, Opts#{env => #{dispatch => [{'_',[],OldDispatches++Dispatch}]}}).

-spec reset_routes(atom()) -> ok.
reset_routes(Intf) ->
    Opts = ranch:get_protocol_options(Intf),
    [{'_',[],DefaultDispatches}] = cowboy_router:compile([{'_', get_routes()}]),
    ok = ranch:set_protocol_options(
           https, Opts#{env => #{dispatch => [{'_',[],DefaultDispatches}]}}).

-spec get_cookie(binary(), map(), term()) -> term().
get_cookie(CookieName, Req, Default) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case maps:from_list(Cookies) of
        #{CookieName := Value} -> Value;
        _ -> Default
    end.

-spec keyfetch(term(), term(), list()) -> term().
keyfetch(Key, List, Default) ->
    keyfetch(Key, 1, List, Default).

-spec keyfetch(term(), integer(), term(), list()) -> term().
keyfetch(Key, Pos, List, Default) ->
    case lists:keyfind(Key, Pos, List) of
        false -> Default;
        {Key, Val} -> Val
    end.

-spec add_d3_templates_path(atom(), string()) -> ok.
add_d3_templates_path(Application, Path) ->
    dderl_dal:add_d3_templates_path(Application, Path).

-spec cow_req_get_meta({ok, atom()}, term(), map(), term()) -> term() | undefined.
cow_req_get_meta({ok, Application}, Key, Req, Default) ->
    case Req of
        #{Application := #{Key := Value}} -> Value;
        _ -> Default
    end.

-spec cow_req_set_meta({ok, atom()}, term(), term(), map()) -> map().
cow_req_set_meta({ok, Application}, Key, Value, Req) ->
    case Req of
        #{Application := Meta} -> Req#{Application => Meta#{Key => Value}};
        _ -> Req#{Application => #{Key => Value}}
    end.

-spec can_handle_request(map()) -> boolean().
can_handle_request(Req) ->
    ?COW_REQ_GET_META(dderl_request, Req, false).

-spec exec_coldstart_cb(atom(), atom() | binary()) -> any().
exec_coldstart_cb(App, disabled) ->
    ?Warn("'~p' cold start : cold_start hook disabled", [App]);
exec_coldstart_cb(App, not_cold_start) ->
    ?Info("'~p' not cold start", [App]);
exec_coldstart_cb(App, Fun) when is_binary(Fun); is_function(Fun, 0) ->
    try
        Ret =
            if is_function(Fun, 0) -> Fun();
                true -> (imem_compiler:compile(Fun))()
            end,
        ?Info("'~p' cold start : cold_start_fun hook : ~p", [App, Ret])
    catch Class:Exception ->
        ?Error("'~p' cold start : ~p:~p~nHook cold_start_fun : ~p~nStack ~p",
               [App, Class, Exception, Fun, erlang:get_stacktrace()])
    end;
exec_coldstart_cb(App, FunStr) ->
    ?Error("'~p' cold start : bad cold_start_fun hook '~p'", [App, FunStr]).

-spec log_table() -> atom().
log_table() ->
    ?GET_CONFIG(dderlLogTable,[],'dderlLog_86400@',"Rolling log table name").

local_ipv4s() ->
    {ok, PhyIntfs} = inet:getifaddrs(),
    lists:usort([{127,0,0,1} | local_ipv4s(PhyIntfs, [])]).

local_ipv4s([], IPv4s) -> IPv4s;
local_ipv4s([{_Nm, Props}|Rest], IPv4s) ->
    case get_ipv4(Props) of
        {_,_,_,_} = IPv4 -> local_ipv4s(Rest, [IPv4 | IPv4s]);
        _ -> local_ipv4s(Rest, IPv4s)
    end.

get_ipv4([]) -> undefined;
get_ipv4([{addr, {_,_,_,_} = IPv4}|_]) -> IPv4;
get_ipv4([_|Rest]) -> get_ipv4(Rest).

unauthorized(Req, Module) ->
    cowboy_req:reply(200, 
            #{<<"cache-control">> => <<"no-cache">>,
              <<"pragma">> => <<"no-cache">>,
              <<"content-type">> => <<"text/html">>}
        , ?UNAUTHORIZEDPAGE(Module), Req).

%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Local Functions
%%-----------------------------------------------------------------------------
-define(PROBE_URL,
        ?GET_CONFIG(probeUrl,[], "/probe.html",
                    "Defines the url of the probe for the load balancer")).

-define(PROMETHEUS_URL,
        ?GET_CONFIG(prometheusMetricsUrl,[], "/metrics",
                    "Prometheus metrics fetch url")).

get_routes() ->
    PrivDir = priv_dir(),
    UrlPathPrefix = get_url_suffix(),
    [{?PROBE_URL, dderl, '$path_probe'},
     {?PROMETHEUS_URL, dderl_prometheus, metrics},
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
