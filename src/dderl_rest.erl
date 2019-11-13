-module(dderl_rest).
-behaviour(cowboy_loop).

-include("dderl.hrl").
-include_lib("imem/include/imem_sql.hrl").

% gen_server exports
-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, format_status/2]).

-record(state, {stmts = #{}}).

% cowboy rest exports
-export([init/2, info/3, terminate/3]).

% cowboy constrains
-export([cmd_constraint/2, view_constraint/2]).

% imem statement callbacks
-export([rows/2, delete/2, stop/1]).

% service export
-export([stop/0, start/0]).

-define(API_VERSION, "0.0.1").

-ifdef(TESTwwe).
f().
{ok, S} = erlimem:open(local_sec, imem_meta:schema()).
S:auth(dderl_rest, undefined, {pwdmd5, {<<"system">>, erlang:md5("change_on_install")}}).
S:run_cmd(login,[]).
S:run_cmd(admin_exec, [imem_seco, account_id, []]).

dderl_dal:get_view(S, "Remote Tables", imem, system).

Sql = "select ddView.id id, ddView.name name, command from ddView, ddCmd where adapters = to_list('[imem]') and ddView.cmd = ddCmd.id".
S:exec(Sql, 1000, []).

S:run_cmd(logout, []).
S:close().

-endif.


-define(E400(__E,__M,__D),
        #{errorCode => (__E * 1000 + 400),
          errorMessage => list_to_binary(__M),
          errorDetails => list_to_binary(__D)}).

% imem statement callbacks
rows(Rows, {?MODULE, StmtRef, Pid}) -> Pid ! {rows, StmtRef, Rows}.
delete(Rows, {?MODULE, StmtRef, Pid}) -> Pid ! {dels, StmtRef, Rows}.
stop({?MODULE, StmtRef, Pid}) -> Pid ! {stop, StmtRef}.

stop() -> supervisor:terminate_child(dderl_sup, ?MODULE).
start() -> supervisor:restart_child(dderl_sup, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

init([]) ->
    process_flag(trap_exit, true),
    try
        init_interface(),
        {ok, #state{}}
    catch
        _:Error ->
            {stop, {Error, erlang:get_stacktrace()}}
    end.

handle_call({login, User, Password}, _From, State) ->
    {reply,
     try
         {ok, ErlimemSession} = erlimem:open(local_sec, imem_meta:schema()),
         ErlimemSession:auth(?MODULE, undefined, {pwdmd5, {User, Password}}),
         ErlimemSession:run_cmd(login,[]),
         {ok, ErlimemSession}
     catch
         _:Error -> {error, Error}
     end, State};
handle_call(Request, _From, State) ->
    ?Warn("Unsupported handle_call ~p", [Request]),
    {reply, ok, State}.

handle_cast(#{reply := RespPid, cmd := views,
              params := #{view := View} = Params,
              opts := #{session := Connection}} = Req, State) ->
    case if is_integer(View) ->
                dderl_dal:get_view(Connection, View);
            true ->
                UserId = Connection:run_cmd(
                           admin_exec, [imem_seco, account_id, []]),
                dderl_dal:get_view(Connection, View, imem, UserId)
         end of
        ViewRec when is_record(ViewRec, ddView) ->
            Binds = maps:get(binds, Params, []),
            Cmd = dderl_dal:get_command(Connection, ViewRec#ddView.cmd),
            handle_cast(Req#{cmd => sql,
                             params => Params#{sql => Cmd#ddCmd.command,
                                               binds => Binds}},
                        State);
        _ ->
            RespPid ! {reply,
                       {400,
                        #{<<"x-irest-conn">> => Connection},
                        ?E400(4, "Bad View", "View not found")}},
            {noreply, State}
    end;
handle_cast(#{cmd := views, params := #{stmt := _}} = Req, State) ->
    handle_cast(Req#{cmd => sql}, State);
handle_cast(#{reply := RespPid, cmd := sql, params := #{stmt := StmtRef},
              opts := #{session := Connection}},
            #state{stmts = Stmts} = State) ->
    ok = Connection:run_cmd(fetch_recs_async, [[], StmtRef]),
    #{stmtResult := StmtRslt} = maps:get(StmtRef, Stmts),
    {noreply,
     State#state{
       stmts =
       (State#state.stmts)#{StmtRef =>
                            #{respPid => {more, RespPid}, stmtResult => StmtRslt,
                              connection => Connection}}
      }};
handle_cast(#{reply := RespPid, cmd := sql,
              params := #{sql := Sql, row_count := RowCount} = Params,
              opts := #{session := Connection}}, State) ->
    case Connection:exec(Sql, RowCount, maps:get(binds, Params, [])) of
        {error, Exception} ->
            RespPid ! {reply, {400, #{<<"x-irest-conn">> => Connection},
                               list_to_binary(io_lib:format("~p", [Exception]))}},
            {noreply, State};
        {ok, #stmtResults{rowCols=RowCols, stmtRefs=StmtRefs} = StmtRslt} ->
            Connection:add_stmt_fsm(StmtRefs, {?MODULE, StmtRefs, self()}),
            [Connection:run_cmd(fetch_recs_async, [[], SR]) || SR <- StmtRefs],
            ClmsJson = [maps:from_list(C)
                        || C <- gen_adapter:build_column_json(lists:reverse(RowCols))],
            {noreply,
             State#state{
               stmts =
               (State#state.stmts)#{StmtRefs =>
                                    #{stmtResults => StmtRslt#stmtResults{rowCols = ClmsJson},
                                      connection => Connection,
                                      respPid => {first, RespPid}}}
              }}
    end;
handle_cast(#{reply := RespPid}, State) ->
    RespPid ! {reply, bad_req},
    {noreply, State};
handle_cast(Request, State) ->
    ?Warn("Unsupported handle_cast ~p", [Request]),
    {noreply, State}.

handle_info({rows, StmtRef, {Rows, EOT}}, #state{stmts = Stmts} = State) when is_list(Rows) ->
    case maps:get(StmtRef, Stmts) of
        #{respPid := {first, RespPid}, connection := Connection,
          stmtResult := #stmtResults{rowFun=RowFun, rowCols=RowCols}} ->
            RowsJson = [RowFun(R) || R <- Rows],
            RespPid ! {reply,
                       {200,
                        #{<<"x-irest-conn">> => Connection},
                        #{rows => RowsJson,
                          clms => RowCols,
                          more => not EOT,
                          stmt => base64:encode(term_to_binary(StmtRef))}}};
        #{respPid := {more, RespPid}, connection := Connection,
          stmtResult := #stmtResults{rowFun = RowFun}} ->
            if length(Rows) == 0 andalso EOT == false ->
                   gen_server:cast(
                     ?MODULE,
                     #{reply => RespPid, cmd => sql,
                       params => #{stmt => StmtRef},
                       opts => #{session => Connection}});
               true ->
                   RowsJson = [RowFun(R) || R <- Rows],
                   RespPid ! {reply,
                              {200,
                               #{<<"x-irest-conn">> => Connection},
                               #{rows => RowsJson, more => not EOT}}}
            end
    end,
    {noreply, State};
handle_info({rows, StmtRef, {error, {Exception, Error}}}, #state{stmts = Stmts} = State) ->
    #{respPid := {_, RespPid}, connection := Connection}
    = maps:get(StmtRef, Stmts),
    RespPid ! {reply,
               {400,
                #{<<"x-irest-conn">> => Connection},
                ?E400(3, atom_to_list(Exception), Error)}},
    {noreply, State};
handle_info(Request, State) ->
    ?Warn("Unsupported handle_info ~p", [Request]),
    {noreply, State}.

terminate(Reason, _State) ->
    try
        stop_interface(Reason),
        ?Info("terminate ~p", [Reason])
    catch
        _:Error ->
        ?Error("terminate ~p:~p", [Reason, Error])
    end.

code_change(OldVsn, State, Extra) ->
    ?Info("code_change ~p: ~p", [OldVsn, Extra]),
    {ok, State}.

format_status(Opt, [PDict, State]) ->
    ?Info("format_status ~p: ~p", [Opt, PDict]),
    State.

init_interface() ->
    MaxAcceptors = ?MAXACCEPTORS,
    MaxConnections = ?MAXCONNS,
    IpWhitelist = ?IMEMREST_IPWHITELIST,
    Opts = #{resource => self(), whitelist => IpWhitelist},
    Dispatch =
    cowboy_router:compile(
      [{'_',
        [{"/swagger/", ?MODULE, swagger},
         {"/swagger/[...]", cowboy_static,
          {dir, filename:join([dderl:priv_dir(), "public", "swagger"])}},
         {"/dderlrest/"?API_VERSION"/[dderlrest.json]", ?MODULE, spec},
         {"/dderlrest/"?API_VERSION"/:cmd/[:view]",
          [{cmd, [fun ?MODULE:cmd_constraint/2]},
           {view, [fun ?MODULE:view_constraint/2]}], ?MODULE, Opts}]
       }]),
    ProtoOpts = #{env => #{dispatch => Dispatch}},
    lists:foreach(
      fun({Ip, Port}) ->
              IpStr = inet:ntoa(Ip),
              TransOpts = [{ip, Ip}, {port, Port},
                           {num_acceptors, MaxAcceptors},
                           {max_connections, MaxConnections}],
              case ?IMEMREST_SSLOPTS of
                  #{cert := Cert, key := Key} ->
                      SslTransOpts = TransOpts
                        ++ [{versions, ['tlsv1.2','tlsv1.1',tlsv1]}
                            | imem_server:get_cert_key(Cert)]
                        ++ imem_server:get_cert_key(Key),
                      {ok, P} = cowboy:start_tls({?MODULE, Ip, Port},
                                                   SslTransOpts, ProtoOpts),
                      ?Info("[~p] Activated https://~s:~p", [P, IpStr, Port]);
                  _ ->
                      {ok, P} = cowboy:start_clear({?MODULE, Ip, Port},
                                                  TransOpts, ProtoOpts),
                      ?Info("[~p] Activated http://~s:~p", [P, IpStr, Port])
              end
      end, local_ips(?IMEMREST_IPS)).

local_ips(Listeners) ->
    local_ips(Listeners, dderl:local_ipv4s(), []).
local_ips([], _LocalIps, Acc) -> Acc;
local_ips([{Ip, _} = Ep | Rest], LocalIps, Acc) ->
    local_ips(
      Rest, LocalIps,
      case lists:member(Ip, LocalIps) of
          true -> [Ep|Acc];
          _ -> Acc
      end).

cmd_constraint(format_error, Value) -> io_lib:format("The cmd ~p is not an valid.", [Value]);
cmd_constraint(_Type, <<"sql">>) -> {ok, sql};
cmd_constraint(_Type, <<"views">>) -> {ok, views};
cmd_constraint(_, _) -> {error, not_valid}.

view_constraint(format_error, Value) -> io_lib:format("The view ~p is not an valid.", [Value]);
view_constraint(_Type, View) ->
    case catch binary_to_integer(View) of
        ViewId when is_integer(ViewId) -> {ok, ViewId};
        _ when is_binary(View) -> {ok, View};
        _ -> {error, not_valid}
    end.

stop_interface(Reason) ->
    lists:foreach(
      fun({Ip, Port}) ->
              IpStr = inet:ntoa(Ip),
              case catch cowboy:stop_listener({?MODULE, Ip, Port}) of
                  ok ->
                      ?Info("De-Activated http(s)://~s:~p", [IpStr, Port]);
                  Error ->
                      ?Error("[~p] Deactivating http(s)://~s:~p : ~p",
                             [Reason, IpStr, Port, Error])
              end
      end, local_ips(?IMEMREST_IPS)).

%%
%% Cowboy REST resource
%%

-define(SERVER,     "DDErl IMEM-REST").
-define(SPEC_FILE,  "dderlrest.json").
-include("dderl_rest.hrl").

-define(E2400,
        #{errorCode => 2400,
          errorMessage => <<"Missing body">>,
          errorDetails => <<"Missing request payload">>}).
-define(E9400,
        #{errorCode => 9400,
          errorMessage => <<"Bad Request">>,
          errorDetails => <<"Operation payload missmatch">>}).

-define(E1401,
        #{errorCode => 1401,
          errorMessage => <<"Invalid credentials">>,
          errorDetails => <<"Incorrect Username/Password">>}).

-define(E1403,
        #{errorCode => 1403,
          errorMessage => <<"Not Whitelisted">>,
          errorDetails => <<"Requesting IP address is not in whitelist">>}).

-define(E1405,
        #{errorCode => 1405,
          errorMessage => <<"Method Not Allowed">>,
          errorDetails => <<"HTTP method isn't allowed on this resource">>}).

-define(JSON(__BODY), imem_json:encode(__BODY)).

init(Req, swagger) ->
    Url = iolist_to_binary(cowboy_req:uri(Req)),
    LastAt = byte_size(Url) - 1,
    Req1 =
    cowboy_req:reply(
      302, #{<<"cache-control">> => <<"no-cache">>,
             <<"pragma">> => <<"no-cache">>,
             <<"location">> =>
             list_to_binary([Url, case Url of
                                      <<_:LastAt/binary, "/">> -> "";
                                      _ -> "/"
                                  end, "index.html"])},
      <<"Redirecting...">>, Req),
    {ok, Req1, #state{}};
init(Req, spec) ->
    Req1 =
    case cowboy_req:method(Req) of
        <<"GET">> ->
            {ok, Content} = file:read_file(
                              filename:join(dderl:priv_dir(),
                                            ?SPEC_FILE)),
            cowboy_req:reply(200, ?REPLY_JSON_SPEC_HEADERS, Content, Req);
        <<"OPTIONS">> ->
            ACRHS = cowboy_req:header(<<"access-control-request-headers">>, Req),
            cowboy_req:reply(200, maps:merge(#{<<"allow">> => <<"GET,OPTIONS">>,
                                               <<"access-control-allow-headers">> => ACRHS},
                                   ?REPLY_OPT_HEADERS), <<>>, Req);
        Method ->
            ?Error("~p not supported", [Method]),
            cowboy_req:reply(405, ?REPLY_JSON_HEADERS, ?JSON(?E1405), Req)
    end,
    {ok, Req1, #state{}};
init(Req, #{whitelist := WhiteList} = Opts) ->
    % whitelist check
    {Ip, _Port} = cowboy_req:peer(Req),
    case lists:member(Ip, WhiteList) of
        true ->
            Cmd = cowboy_req:binding(cmd, Req),
            Op = cowboy_req:method(Req),
            case cowboy_req:header(<<"x-irest-conn">>, Req, '$notfound') of
                '$notfound' ->
                    case cowboy_req:parse_header(<<"authorization">>, Req) of
                        {basic, Username, Password} ->
                            case gen_server:call(
                                   ?MODULE, {login, Username,
                                             erlang:md5(Password)}) of
                                {ok, Session} ->
                                    push_request(Cmd, Op, Req,
                                                 Opts#{session => Session});
                                {error, Error} ->
                                    Req1 =
                                    cowboy_req:reply(
                                      400, ?REPLY_JSON_HEADERS,
                                      ?JSON(?E400(1, "DB login error",
                                                  io_lib:format("~p", [Error]))),
                                      Req),
                                    {ok, Req1, undefined}
                            end;
                        _ ->
                            Req1 = cowboy_req:reply(
                                           401, ?REPLY_JSON_HEADERS,
                                           ?JSON(?E1401), Req),
                            {ok, Req1, undefined}
                    end;
                SessionBin when is_binary(SessionBin) ->
                    push_request(
                      Cmd, Op, Req,
                      Opts#{session => binary_to_term(
                                         base64:decode(SessionBin))})
            end;
        _ ->
            Req1 = cowboy_req:reply(403, ?REPLY_JSON_HEADERS,
                                          ?JSON(?E1403), Req),
            {ok, Req1, undefined}
    end.

push_request(Cmd, Op, Req, Opts) ->
    case cowboy_req:has_body(Req) of
        HB when (HB == false andalso Op == <<"GET">>) orelse
                (HB == true andalso Op == <<"POST">>) ->
            case get_params(Cmd, Req) of
                {{error, Error}, Req1} ->
                    ?Error("~p", [Error]),
                    Req2 = cowboy_req:reply(400, ?REPLY_JSON_HEADERS,
                                                  ?JSON(?E2400), Req1),
                    {ok, Req2, undefined};
                {Params, Req1} when is_map(Params) ->
                    ok = gen_server:cast(
                           ?MODULE, #{cmd => Cmd, params => Params,
                                      reply => self(), opts => Opts}),
                    {cowboy_loop, Req1, Opts, hibernate}
            end;
        false when Op == <<"POST">> ->
            Req1 = cowboy_req:reply(400, ?REPLY_JSON_HEADERS, ?JSON(?E2400),
                                          Req),
            {ok, Req1, undefined};
        HB ->
            ?Error("~s has body = ~p", [Op, HB]),
            Req1 = cowboy_req:reply(400, ?REPLY_JSON_HEADERS, ?JSON(?E9400),
                                          Req),
            {ok, Req1, undefined}
    end.

get_params(sql, Req) ->
    Params = cowboy_req:parse_qs(Req),
    case maps:from_list(Params) of
        #{<<"q">> := Sql} = P ->
            RC = maps:get(<<"r">>, P, integer_to_binary(?DEFAULT_ROW_SIZE)),
            case catch binary_to_integer(RC) of
                Rows when is_integer(Rows) ->
                    {#{sql => Sql, row_count => Rows}, Req};
                _ -> {{error, non_numeric_row_count}, Req}
            end;
        #{<<"s">> := StmtRef} ->
            {#{stmt => binary_to_term(base64:decode(StmtRef))}, Req};
        _ -> {{error, invalid_parameters}, Req}
    end;
get_params(views, Req) ->
    Params = cowboy_req:parse_qs(Req),
    case maps:from_list(Params) of
        #{<<"s">> := StmtRef} ->
            {#{stmt => binary_to_term(base64:decode(StmtRef))}, Req};
        P when is_map(P) ->
            RC = maps:get(<<"r">>, P, integer_to_binary(?DEFAULT_ROW_SIZE)),
            case catch binary_to_integer(RC) of
                Rows when is_integer(Rows) ->
                    View = cowboy_req:binding(view, Req),
                    Prms = #{view => View, row_count => Rows},
                    case cowboy_req:has_body(Req) of
                        true ->
                            case cowboy_req:has_body(Req) of
                                true ->
                                    case cowboy_req:read_body(Req) of
                                        {ok, <<"{}">>, Req2} ->
                                            {{error, "No params"}, Req2};
                                        {ok, Data, Req2} ->
                                            Binds =
                                            [{maps:get(<<"name">>, B),
                                              binary_to_existing_atom(maps:get(<<"typ">>, B, <<>>), utf8),
                                              0, [maps:get(<<"value">>, B, <<>>)]}
                                             || B <- imem_json:decode(Data, [return_maps])],
                                            ?Info("Binds : ~p", [Binds]),
                                            {Prms#{binds => Binds}, Req2};
                                        {more, Data, Req2} ->
                                            {{error, {to_many_params, Data}}, Req2};
                                        {error, Error} ->
                                            {{error, Error}, Req}
                                    end;
                                false -> {Prms, Req}
                            end;
                        false -> {Prms, Req}
                    end;
                _ -> {{error, non_numeric_row_count}, Req}
            end;
        _ -> {{error, invalid_parameters}, Req}
    end.

info({reply, bad_req}, Req, State) ->
    Req1 = cowboy_req:reply(400, ?REPLY_HEADERS, "", Req),
    {stop, Req1, State};
info({reply, {Code, Headers, Body}}, Req, State) when is_integer(Code), is_map(Body) ->
    info({reply, {Code, Headers, imem_json:encode(Body)}}, Req, State);
info({reply, {Code, Headers, Body}}, Req, State) when is_integer(Code), is_binary(Body) ->
    RespHeaders =
    maps:merge(?REPLY_JSON_HEADERS,
    maps:map(
      fun(_H, V) when is_binary(V) -> base64:encode(V);
         (_H, V) -> base64:encode(term_to_binary(V))
      end, Headers)),
    Req1 = cowboy_req:reply(Code, RespHeaders, Body, Req),
    {stop, Req1, State}.

terminate(_Reason, _Req, _State) -> ok.
