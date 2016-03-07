-module(dderl_session).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behavior(gen_server).

-type ipport() :: {inet:ip_address(), inet:port_number()}.

-include("dderl.hrl").

-export([start_link/3
        , get_session/2
        , process_request/6
        , get_state/1
        , get_apps_version/2
        ]).

-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
        ]).

% via exports
-export([ register_name/2
          , register_name/3
          , unregister_name/1
          , whereis_name/1
          , send/2
        ]).

-define(SESSION_IDLE_TIMEOUT, 90000). % 90 secs
%%-define(SESSION_IDLE_TIMEOUT, 5000). % 5 sec (for testing)

-record(state, {
          id            = <<>>          :: binary()
          , adapt_priv  = []            :: list()
          , tref                        :: timer:tref()
          , user        = <<>>          :: binary()
          , user_id                     :: ddEntityId()
          , sess                        :: {atom, pid()}
          , active_sender               :: pid()
          , registered_name             :: reference()
          , conn_info                   :: map()
         }).

%% Helper functions
-spec get_session(binary() | list(), fun(() -> map())) -> {ok, {atom(), pid()}} | {error, term()}.
get_session(<<>>, ConnInfoFun) when is_function(ConnInfoFun, 0) ->
    Ref = erlang:make_ref(),
    Bytes = crypto:rand_bytes(64),
    {ok, _Pid} = dderl_session_sup:start_session(Ref, Bytes, ConnInfoFun),
    DderlSess = {?MODULE, Ref, Bytes},
    ?Debug("new dderl session ~p from ~p", [DderlSess, self()]),
    {ok, DderlSess};
get_session(DDerlSessStr, _ConnInfoFun) when is_list(DDerlSessStr) ->
    try
        DDerlSessBin = ?Decrypt(DDerlSessStr),
        RefSize = byte_size(DDerlSessBin) - 64,
        << First:32/binary
           , RefBin:RefSize/binary
           , Last:32/binary >> = DDerlSessBin,        
        Ref = binary_to_term(RefBin),
        Bytes = << First/binary, Last/binary >>,
        DDerlSession = {?MODULE, Ref, Bytes},
        case whereis_name(Ref) of
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true -> {ok, DDerlSession};
                    _ -> {error, <<"process not found">>}
                end;
            _ -> {error, <<"process not found">>}
        end
    catch
        Error:Reason ->
            ?Warn("Request attempted on a dead session"),
            {error, {Error, Reason}}
    end;
get_session(S, ConnInfoFun) when is_binary(S) ->
    get_session(binary_to_list(S), ConnInfoFun);
get_session(_, ConnInfoFun) ->
    get_session(<<>>, ConnInfoFun).

-spec start_link(reference(), binary(), fun(() -> map())) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ref, Bytes, ConnInfoFun) when is_function(ConnInfoFun, 0) ->
    gen_server:start_link({via, ?MODULE, Ref}, ?MODULE, [Bytes, Ref, ConnInfoFun()], []).

-spec get_state({atom(), pid()}) -> #state{}.
get_state({?MODULE, Ref, Bytes}) ->
    gen_server:call({via, ?MODULE, Ref}, {get_state, Bytes}, infinity).

-spec process_request(atom(), [binary()], term(), pid(), {atom(), pid()},
                      ipport()) -> term().
process_request(undefined, Type, Body, ReplyPid, Ref, RemoteEp) ->
    process_request(gen_adapter, Type, Body, ReplyPid, Ref, RemoteEp);
process_request(Adapter, Type, Body, ReplyPid, RemoteEp, {?MODULE, Ref, Bytes}) ->
    ?NoDbLog(debug, [], "request received, type ~p body~n~s", [Type, jsx:prettify(Body)]),
    true = gen_server:call({via, ?MODULE, Ref}, {verify, Bytes}),
    gen_server:cast({via, ?MODULE, Ref}, {process, Adapter, Type, Body, ReplyPid, RemoteEp}).

init([Bytes, RegisteredName, ConnInfo]) when is_binary(Bytes) ->
    process_flag(trap_exit, true),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {ok, #state{tref=TRef, id=Bytes, registered_name=RegisteredName,
                conn_info = ConnInfo}}.

handle_call({verify, InBytes}, _From, State) ->
    {reply, InBytes =:= State#state.id, State};
handle_call({get_state, Bytes}, _From, #state{id = Bytes} = State) ->
    ?Debug("get_state, result: ~p~n", [State]),
    {reply, State, State};
handle_call({get_state, _}, _From, State) ->
    ?Debug("get_state, unauthorized access attempt result: ~p~n", [State]),
    {reply, unauthorized, State};
handle_call(Unknown, _From, #state{user=_User}=State) ->
    ?Error("unknown call ~p", [Unknown]),
    {reply, {not_supported, Unknown} , State}.

handle_cast({process, Adapter, Typ, WReq, From, RemoteEp}, #state{tref=TRef} = State) ->
    timer:cancel(TRef),
    State0 = try process_call({Typ, WReq}, Adapter, From, RemoteEp, State)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            reply(From, [{<<"error">>, <<"Unable to process the request">>}], self()),
            State
    end,
    {noreply, State0#state{tref=undefined}};
handle_cast(_Unknown, #state{user=_User}=State) ->
    ?Error("~p received unknown cast ~p for ~p", [self(), _Unknown, _User]),
    {noreply, State}.

handle_info(rearm_session_idle_timer, State) ->
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {noreply, State#state{tref=NewTRef}};
handle_info(die, #state{user=User}=State) ->
    ?Info([{user, User}], "session ~p idle for ~p ms", [{self(), User}, ?SESSION_IDLE_TIMEOUT]),
    {stop, normal, State};
handle_info(logout, #state{user = User} = State) ->
    ?Debug("terminating session of logged out user ~p", [User]),
    {stop, normal, State};
handle_info(invalid_credentials, #state{} = State) -> %% TODO : perhaps monitor erlimemsession
    ?Debug("terminating session ~p due to invalid credentials", [self()]),
    {stop, normal, State};
handle_info({'EXIT', _Pid, normal}, #state{user = _User} = State) ->
    %?Debug("Received normal exit from ~p for ~p", [Pid, User]),
    {noreply, State};
handle_info(Info, #state{user = User} = State) ->
    ?Error("~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{user=User} = State) ->
    ?Info([{user, User}], "~p ~p terminating, reason ~p", [?MODULE, {self(), User}, Reason]),
    logout(State).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_login(SessionId,#{<<"SMS Token">>:=Token}, #state{sess=ErlImemSess}=State) ->
    {process_login_reply(ErlImemSess:auth(dderl,SessionId,{smsott,Token})), State};
process_login(SessionId,#{<<"User">>:=User,<<"Password">>:=Password}, #state{sess = ErlImemSess} = State) ->
    {process_login_reply(
       ErlImemSess:auth(dderl,SessionId,{pwdmd5,{User,list_to_binary(Password)}})
      ), State#state{user=User}};
process_login(SessionId,#{}, #state{conn_info=ConnInfo}=State) ->
    case erlimem:open({rpc, node()}, imem_meta:schema()) of
        {error, Error} ->
            {{error, Error}, State};
        {ok, ErlImemSess} ->
            {process_login_reply(ErlImemSess:auth(dderl,SessionId,{access,ConnInfo})), 
                State#state{sess=ErlImemSess}}
    end.

process_login_reply(ok)                         -> ok;
process_login_reply({ok, []})                   -> ok;
process_login_reply({ok, [{pwdmd5,Data}|_]})    -> #{pwdmd5=>process_data(Data)};
process_login_reply({ok, [{smsott,Data}|_]})    -> #{smsott=>process_data(Data)}.

process_data(#{accountName:=undefined}=Data) -> process_data(Data#{accountName=><<"">>});
process_data(Data) -> Data.

-spec process_call({[binary()], term()}, atom(), pid(), #state{}, ipport()) -> #state{}.
process_call({[<<"restart">>], _ReqData}, _Adapter, From, {SrcIp,_},
             #state{sess = ErlImemSess} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, State#state.user_id, "restart", "", "", "", "", "", ""),
    case ErlImemSess:run_cmd(have_permission, [{dderl,restart}]) of
        true ->
            From ! {spawn,
                    fun() ->
                            [App|_] = dderl_dal:get_restartable_apps(),
                            StopApps = find_deps_app_seq(App) ++ [App],
                            ?Info("Stopping... ~p", [StopApps]),
                            _ = [application:stop(A) || A <- StopApps],
                            StartApps = lists:reverse(StopApps),
                            ?Info("Starting... ~p", [StartApps]),
                            _ = [application:start(A) || A <- StartApps]
                    end},
            reply(From, #{restart => <<"ok">>}, self());
        _ ->
            reply(From, #{restart => #{error => <<"insufficient privilege">>}}, self())
    end,
    State;
process_call({[<<"login">>], ReqData}, _Adapter, From, {SrcIp,_}, #state{} = State) ->
    #state{id = << First:32/binary, Last:32/binary >>,
           registered_name = RegisteredName, sess = ErlImemSess} = State,
    SessionId = ?Hash(list_to_binary([First, term_to_binary(RegisteredName), Last])),
    catch dderl:access(?CMD_WITHARGS, SrcIp, "", "login", ReqData, "", "", "", "", ""),
    case catch process_login(SessionId,jsx:decode(ReqData,[return_maps]),State) of
        {{E,M},St} when is_atom(E) ->
            ?Error("Error(~p) ~p~n~p", [E,M,St]),
            reply(From, #{login=>
                               #{error=>
                                 if is_binary(M) -> M;
                                    is_list(M) -> list_to_binary(M);
                                    true -> list_to_binary(io_lib:format("~p", [M]))
                               end}}, self()),
            State;
        {Reply, State1} ->
            {Reply1, State2}
            = case Reply of
                  ok ->
                      case ErlImemSess:run_cmd(login,[]) of
                          {error,{{'SecurityException',{?PasswordChangeNeeded,_}},ST}} ->
                              ?Warn("Password expired ~s~n~p", [State1#state.user, ST]),
                              {#{changePass=>State1#state.user}, State1};
                          _ ->
                              {[UserId],true} = imem_meta:select(
                                                  ddAccount,
                                                  [{#ddAccount{name=State1#state.user,
                                                               id='$1',_='_'},
                                                    [], ['$1']}]),
                              {#{accountName=>State1#state.user},
                               State1#state{user_id = UserId}}
                      end;
                  _ -> {Reply, State1}
              end,
            {ok,Vsn} = application:get_key(dderl, vsn),            
            reply(From, #{login => Reply1#{vsn => list_to_binary(Vsn),
                                           node => list_to_binary(imem_meta:node_shard())}}, self()),
            State2
    end;

%% IMPORTANT:
% This function clause is placed right after login to be able to catch all
% request (other than login above) which are NOT to be allowed without a login
%
process_call(Req, _Adapter, From, {SrcIp,_}, #state{user = <<>>} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, "", "invalid", io_lib:format("~p", [Req]), "", "", "", "", ""),
    ?Debug("Request from a not logged in user: ~n~p", [Req]),
    reply(From, [{<<"error">>, <<"user not logged in">>}], self()),
    State;

process_call({[<<"login_change_pswd">>], ReqData}, _Adapter, From, {SrcIp,_},
             #state{sess = ErlImemSess} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "login_change_pswd", ReqData, "", "", "", "", ""),
    [{<<"change_pswd">>, BodyJson}] = jsx:decode(ReqData),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    OldPassword = list_to_binary(proplists:get_value(<<"password">>, BodyJson, [])),
    NewPassword = list_to_binary(proplists:get_value(<<"new_password">>, BodyJson, [])),
    case ErlImemSess:run_cmd(
           change_credentials,
           [{pwdmd5, OldPassword}, {pwdmd5, NewPassword}]
          ) of
        SeKey when is_integer(SeKey)  ->
            ?Debug("change password successful for ~p", [User]),
            reply(From, [{<<"login_change_pswd">>,<<"ok">>}], self());
        {error, {error, {Exception, M}}} ->
            ?Error("change password failed for ~p, result ~n~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            reply(From, [{<<"login_change_pswd">>, Err}], self());
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("change password failed for ~p, result ~n~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            reply(From, [{<<"login_change_pswd">>, Err}], self())
    end,
    State;

process_call({[<<"logout">>], _ReqData}, _Adapter, From, {SrcIp,_}, #state{} = State) ->
    catch dderl:access(?LOGIN_CONNECT, SrcIp, State#state.user_id, "logout", "", "", "", "", "", ""),
    NewState = logout(State),
    reply(From, [{<<"logout">>, <<"ok">>}], self()),
    self() ! logout,
    NewState;

process_call({[<<"format_erlang_term">>], ReqData}, _Adapter, From, {SrcIp,_}, #state{} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "format_erlang_term", ReqData, "", "", "", "", ""),
    [{<<"format_erlang_term">>, BodyJson}] = jsx:decode(ReqData),
    StringToFormat = proplists:get_value(<<"erlang_term">>, BodyJson, <<>>),
    ?Debug("The string to format: ~p", [StringToFormat]),
    case proplists:get_value(<<"expansion_level">>, BodyJson, 1) of
        <<"auto">> -> ExpandLevel = auto;
        ExpandLevel -> ok
    end,
    Force = proplists:get_value(<<"force">>, BodyJson, false),
    ?Debug("Forced value: ~p", [Force]),
    case erlformat:format(StringToFormat, ExpandLevel, Force) of
        {error, ErrorInfo} ->
            ?Debug("Error trying to format the erlang term ~p~n~p", [StringToFormat, ErrorInfo]),
            reply(From, [{<<"format_erlang_term">>,
                          [{<<"error">>, <<"Invalid erlang term">>},
                           {<<"originalText">>, StringToFormat}]}], self());
        Formatted ->
            ?Debug("The formatted text: ~p", [Formatted]),
            reply(From, [{<<"format_erlang_term">>, Formatted}], self())
    end,
    State;

process_call({[<<"format_json_to_save">>], ReqData}, _Adapter, From, {SrcIp,_}, #state{} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "format_json_to_save", ReqData, "", "", "", "", ""),
    [{<<"format_json_to_save">>, BodyJson}] = jsx:decode(ReqData),
    StringToFormat = proplists:get_value(<<"json_string">>, BodyJson, <<>>),
    case jsx:is_json(StringToFormat) of
        true ->
            Formatted = jsx:encode(jsx:decode(StringToFormat)),
            ?Debug("The formatted text: ~p", [Formatted]),
            reply(From, [{<<"format_json_to_save">>, Formatted}], self());
        _ ->
            ?Error("Error trying to format the json string ~p~n", [StringToFormat]),
            reply(From, [{<<"format_json_to_save">>,
                          [{<<"error">>, <<"Invalid json string">>},
                           {<<"originalText">>, StringToFormat}]}], self())
    end,
    State;

process_call({[<<"about">>], _ReqData}, _Adapter, From, {SrcIp,_}, #state{} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, State#state.user_id, "about", "", "", "", "", "", ""),
    case application:get_key(dderl, applications) of
        undefined -> Deps = [];
        {ok, Deps} -> Deps
    end,
    Apps = application:which_applications(),
    Versions = get_apps_version(Apps, [dderl|Deps]),
    reply(From, [{<<"about">>, Versions}], self()),
    State;

process_call({[<<"ping">>], _ReqData}, _Adapter, From, {SrcIp,_}, #state{} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, State#state.user_id, "ping", "", "", "", "", "", ""),
    reply(From, [{<<"ping">>, atom_to_binary(node(), utf8)}], self()),
    State;

process_call({[<<"connect_info">>], _ReqData}, _Adapter, From, {SrcIp,_},
             #state{sess=Sess, user_id=UserId, user=User} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, State#state.user_id, "connect_info", "", "", "", "", "", ""),
    ConnInfo
    = case dderl_dal:get_adapters(Sess) of
          {error, Reason} when is_binary(Reason) -> #{error => Reason};
          Adapters when is_list(Adapters) ->
              case dderl_dal:get_connects(Sess, UserId) of
                  {error, Reason} when is_binary(Reason) -> #{error => Reason};
                  UnsortedConns when is_list(UnsortedConns) ->
                      Connections
                      = lists:foldl(
                          fun(C,Cl) ->
                                  Adapter = list_to_existing_atom(
                                              atom_to_list(C#ddConn.adapter)++"_adapter"),
                                  [Adapter:connect_map(C)|Cl]
                          end, [],
                          lists:sort(fun(#ddConn{name = Name},
                                         #ddConn{name = Name2}) ->
                                             Name > Name2
                                     end, UnsortedConns)
                         ),
                      CInfo = #{adapters =>
                                  [#{id => jsq(A#ddAdapter.id),
                                     fullName => A#ddAdapter.fullName}
                                   || A <- Adapters],
                                  connections => Connections},
                      #{connect_info =>
                        CInfo#{connections =>
                               Connections ++
                               case [A || #{adapter := A} <- Connections, A == <<"oci">>] of
                                   [] -> [#{adapter => <<"oci">>,
                                            id => null,
                                            name => <<"template oracle">>,
                                            owner => User,
                                            method => <<"tns">>}];
                                   _ -> []
                               end ++
                               case [A || #{adapter := A} <- Connections, A == <<"imem">>] of
                                   [] -> [#{adapter => <<"imem">>,
                                            id => null,
                                            name => <<"template imem">>,
                                            schema => atom_to_binary(imem_meta:schema(),utf8),
                                            owner => User,
                                            method => <<"tcp">>}];
                                   _ -> []
                               end
                              }
                       }
              end
      end,
    reply(From, ConnInfo, self()),
    State;

process_call({[<<"del_con">>], ReqData}, _Adapter, From, {SrcIp,_}, #state{sess=Sess, user_id=UserId} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "del_con", ReqData, "", "", "", "", ""),
    [{<<"del_con">>, BodyJson}] = jsx:decode(ReqData),
    ConId = proplists:get_value(<<"conid">>, BodyJson, 0),
    ?Info([{user, State#state.user}], "connection to delete ~p", [ConId]),
    Resp = case dderl_dal:del_conn(Sess, UserId, ConId) of
        ok -> <<"success">>;
        Error -> [{<<"error">>, list_to_binary(lists:flatten(io_lib:format("~p", [Error])))}]
    end,
    reply(From, [{<<"del_con">>, Resp}], self()),
    State;

process_call({[<<"activate_sender">>], ReqData}, _Adapter, From, {SrcIp,_}, #state{active_sender = undefined} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "activate_sender", ReqData, "", "", "", "", ""),
    [{<<"activate_sender">>, BodyJson}] = jsx:decode(ReqData),
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnPositions = proplists:get_value(<<"column_positions">>, BodyJson, []),
    %% TODO: Add options to override default parameters
    case dderl_data_sender_sup:start_sender(Statement, ColumnPositions) of
        {ok, Pid} ->
            reply(From, [{<<"activate_sender">>, <<"ok">>}], self()),
            State#state{active_sender = Pid};
        {error, Reason} ->
            Error = [{<<"error">>, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}],
            reply(From, [{<<"activate_sender">>, Error}], self()),
            State
    end;
process_call({[<<"activate_sender">>], ReqData}, Adapter, From, {SrcIp,_} = RemoteEp, #state{active_sender = PidSender} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "activate_sender", ReqData, "", "", "", "", ""),
    case erlang:is_process_alive(PidSender) of
        true ->
            ?Error("Sender ~p already waiting for connection", [PidSender]), %% Log more details user, active sender etc...
            %% TODO: Add information about the active sender. ( if possible block this from browser & show status of sender)
            reply(From, [{<<"activate_sender">>, [{<<"error">>, <<"Sender already waiting for connection">>}]}], self()),
            State;
        false ->
            process_call({[<<"activate_sender">>], ReqData}, Adapter, From, RemoteEp, State#state{active_sender = undefined})
    end;

process_call({[<<"activate_receiver">>], _ReqData}, _Adapter, From, {SrcIp,_}, #state{active_sender = undefined} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, State#state.user_id, "activate_receiver", "", "", "", "", "", ""),
    ?Error("No active data sender found"), %% TODO: Log more 
    reply(From, [{<<"activate_receiver">>, [{<<"error">>, <<"No table sending data">>}]}], self()),
    State;
process_call({[<<"activate_receiver">>], ReqData}, _Adapter, From, {SrcIp,_}, #state{active_sender = PidSender} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, "activate_receiver", ReqData, "", "", "", "", ""),
    case erlang:is_process_alive(PidSender) of
        true ->
            [{<<"activate_receiver">>, BodyJson}] = jsx:decode(ReqData),
            Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
            ColumnPositions = proplists:get_value(<<"column_positions">>, BodyJson, []),
            %% TODO: Add options to override default parameters
            case dderl_data_receiver_sup:start_receiver(Statement, ColumnPositions, PidSender, From) of
                {ok, PidReceiver} ->
                    ?Info("Data receiver ~p started and connected with ~p", [PidReceiver, PidSender]),
                    State#state{active_sender = undefined};
                {error, Reason} ->
                    Error = [{<<"error">>, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}],
                    reply(From, [{<<"activate_receiver">>, Error}], self()),
                    State
            end;
        false ->
            ?Error("No active data sender found"), %% Log more details...
            reply(From, [{<<"activate_receiver">>, [{<<"error">>, <<"No table sending data">>}]}], self()),
            State#state{active_sender = undefined}
    end;

% commands handled generically
process_call({[C], ReqData}, Adapter, From, {SrcIp,_}, #state{sess=Sess, user_id=UserId} = State) when
      C =:= <<"parse_stmt">>;
      C =:= <<"get_query">>;
      C =:= <<"save_view">>;
      C =:= <<"view_op">>;
      C =:= <<"update_view">>;
      C =:= <<"save_dashboard">>;
      C =:= <<"histogram">>;
      C =:= <<"statistics">>;
      C =:= <<"statistics_full">>;
      C =:= <<"dashboards">>;
      C =:= <<"edit_term_or_view">>;
      C =:= <<"get_sql">>;
      C =:= <<"cache_data">> ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, binary_to_list(C), ReqData, "", "", "", "", ""),
    BodyJson = jsx:decode(ReqData),
    Self = self(),
    spawn_link(fun() -> spawn_gen_process_call(Adapter, From, C, BodyJson, Sess, UserId, Self) end),
    State;

process_call({Cmd, ReqData}, Adapter, From, {SrcIp,_},
             #state{sess=Sess, user_id=UserId, adapt_priv = AdaptPriv,
                    conn_info = ConnInfo} = State)
  when Cmd =:= [<<"connect">>];
       Cmd =:= [<<"connect_change_pswd">>];
       Cmd =:= [<<"disconnect">>] ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, State#state.user_id, binary_to_list(hd(Cmd)), ReqData, "", "", "", "", ""),
    #state{id = << First:32/binary, Last:32/binary >>, registered_name = RegisteredName} = State,
    SessionId = ?Hash(list_to_binary([First, term_to_binary(RegisteredName), Last])),
    BodyJson = jsx:decode(ReqData),
    ?NoDbLog(debug, [{user, UserId}], "~p processing ~p~n~s", [Adapter, Cmd, jsx:prettify(ReqData)]),
    CurrentPriv = Adapter:add_conn_info(proplists:get_value(Adapter, AdaptPriv), ConnInfo),
    NewCurrentPriv =
        try
            Adapter:process_cmd({Cmd, BodyJson, SessionId}, Sess, UserId, From, CurrentPriv, self())
        catch Class:Error ->
                ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
                reply(From, [{<<"error">>, <<"Unable to process the request">>}], self()),
                CurrentPriv
        end,
    case proplists:is_defined(Adapter, AdaptPriv) of
        true -> NewAdaptPriv = lists:keyreplace(Adapter, 1, AdaptPriv, {Adapter, NewCurrentPriv});
        false -> NewAdaptPriv = [{Adapter, NewCurrentPriv} | AdaptPriv]
    end,
    State#state{adapt_priv = NewAdaptPriv};

process_call({Cmd, ReqData}, Adapter, From, {SrcIp,_}, #state{sess=Sess, user_id=UserId, adapt_priv=AdaptPriv} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, binary_to_list(hd(Cmd)), ReqData, "", "", "", "", ""),
    CurrentPriv = proplists:get_value(Adapter, AdaptPriv),
    BodyJson = jsx:decode(ReqData),
    Self = self(),
    spawn_link(fun() -> spawn_process_call(Adapter, CurrentPriv, From, Cmd, BodyJson, Sess, UserId, Self) end),
    State.

spawn_process_call(Adapter, CurrentPriv, From, Cmd, BodyJson, Sess, UserId, SelfPid) ->
    try Adapter:process_cmd({Cmd, BodyJson}, Sess, UserId, From, CurrentPriv, SelfPid)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, BodyJson]),
            reply(From, [{<<"error">>, <<"Unable to process the request">>}], SelfPid)
    end.

spawn_gen_process_call(Adapter, From, C, BodyJson, Sess, UserId, SelfPid) ->
    try gen_adapter:process_cmd({[C], BodyJson}, adapter_name(Adapter), Sess, UserId, From, undefined)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            reply(From, [{<<"error">>, <<"Unable to process the request">>}], SelfPid)
    end.

reply(From, Data, Master) ->
    From ! {reply, jsx:encode(Data)},
    Master ! rearm_session_idle_timer.

-spec jsq(term()) -> term().
jsq(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
jsq(OtherTypes) -> OtherTypes.

-spec logout(#state{}) -> #state{}.
logout(#state{sess = undefined, adapt_priv = AdaptPriv} = State) ->
    [Adapter:disconnect(Priv) || {Adapter, Priv} <- AdaptPriv],
    State#state{adapt_priv = []};
logout(#state{sess = Sess} = State) ->
    try Sess:close()
    catch Class:Error ->
            ?Error("Error trying to close the session ~p ~p:~p~n~p~n",
                   [Sess, Class, Error, erlang:get_stacktrace()])
    end,
    logout(State#state{sess = undefined}).

-spec get_apps_version([{atom(), list(), list()}], [atom()]) -> [{binary(), list()}].
get_apps_version([], _Deps) -> [];
get_apps_version([App|Rest], Deps) ->
    {AppName, Desc, Vsn} = App,
    Dependency = lists:member(AppName, Deps),
    AppInfo = {atom_to_binary(AppName, utf8),
                  [
                      {<<"version">>, list_to_binary(Vsn)},
                      {<<"description">>, list_to_binary(Desc)},
                      {<<"dependency">>, Dependency}
                  ]
              },
    [AppInfo | get_apps_version(Rest, Deps)].

-spec adapter_name(atom()) -> atom().
adapter_name(imem_adapter) -> imem;
adapter_name(oci_adapter) -> oci;
adapter_name(gen_adapter) -> gen;
adapter_name(AdaptMod) ->
    [BinAdapter|_] = binary:split(atom_to_binary(AdaptMod, utf8), <<"_">>),
    binary_to_existing_atom(BinAdapter, utf8).


% VIA API
register_name(Name, Pid) ->
    ?Info("Registering ~p with ~p", [Name, Pid]),
    global:register_name(Name, Pid).
register_name(Name, Pid, Resolve) ->
    ?Info("Registering ~p with ~p", [Name, Pid]),
    global:register_name(Name, Pid, Resolve).
unregister_name(Name) -> global:unregister_name(Name).
whereis_name(Name) -> global:whereis_name(Name).
send(Name, Msg) -> global:send(Name, Msg).

find_deps_app_seq(App) -> find_deps_app_seq(App, []).
find_deps_app_seq(App,Chain) ->
    case lists:foldl(
           fun({A,_,_}, Acc) ->
                   {ok, Apps} = application:get_key(A,applications),
                   case lists:member(App, Apps) of
                       true -> [A|Acc];
                       false -> Acc
                   end
           end, [], application:which_applications()) of
        [] -> Chain;
        [A|_] -> % Follow signgle chain for now
            find_deps_app_seq(A,[A|Chain])
    end.
