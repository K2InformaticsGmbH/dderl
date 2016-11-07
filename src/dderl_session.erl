-module(dderl_session).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behavior(gen_server).

-type ipport() :: {inet:ip_address(), inet:port_number()}.

-include("dderl.hrl").
-include_lib("esaml/include/esaml.hrl").

-export([start_link/3
        , get_session/4
        , process_request/6
        , get_state/1
        , get_xsrf_token/1
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

-define(SESSION_IDLE_TIMEOUT, 90000). % 90 secs
%%-define(SESSION_IDLE_TIMEOUT, 5000). % 5 sec (for testing)

-record(state, {
          id            = <<>>          :: binary()
          , adapt_priv  = []            :: list()
          , session_idle_tref           :: timer:tref()
          , inactive_tref               :: timer:tref()
          , user        = <<>>          :: binary()
          , user_id                     :: ddEntityId()
          , sess                        :: {atom, pid()}
          , active_sender               :: pid()
          , conn_info                   :: map()
          , old_state                   :: tuple()
          , lock_state  = unlocked      :: unlocked | locked | screensaver
          , xsrf_token  = <<>>          :: binary()
         }).

%% Helper functions
-spec get_session(binary() , binary(), boolean(), fun(() -> map())) -> {ok, {atom(), pid()}} | {error, term()}.
get_session(<<>>, _, _, ConnInfoFun) when is_function(ConnInfoFun, 0) ->
    SessionToken = base64:encode(crypto:rand_bytes(64)),
    XSRFToken = base64:encode(crypto:rand_bytes(32)),
    dderl_session_sup:start_session(SessionToken, XSRFToken, ConnInfoFun),
    ?Debug("new dderl session ~p from ~p", [SessionToken, self()]),
    {ok, SessionToken, XSRFToken};
get_session(SessionToken, XSRFToken, CheckXSRF, _ConnInfoFun) when is_binary(SessionToken) ->
    try
        case global:whereis_name(SessionToken) of
            undefined -> {error, <<"process not found">>};
            Pid ->
                case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                    true -> 
                        if CheckXSRF ->
                                case get_xsrf_token(SessionToken) of
                                    XSRFToken -> {ok, SessionToken, XSRFToken};
                                    _ -> {error, <<"xsrf attack">>}
                                end;
                           true -> {ok, SessionToken, XSRFToken}
                        end;
                    _ -> {error, <<"process not found">>}
                end
        end
    catch
        Error:Reason ->
            ?Warn("Request attempted on a dead session"),
            {error, {Error, Reason}}
    end;
get_session(_, XSRFToken, CheckXSRF, ConnInfoFun) ->
    get_session(<<>>, XSRFToken, CheckXSRF, ConnInfoFun).

-spec start_link(binary(), binary(), fun(() -> map())) -> {ok, pid()} | ignore | {error, term()}.
start_link(SessionToken, XSRFToken, ConnInfoFun) when is_function(ConnInfoFun, 0) ->
    {ok, Pid} = gen_server:start_link({global, SessionToken}, ?MODULE, [XSRFToken, ConnInfoFun()], []),
    Pid ! {set_id, SessionToken},
    {ok, Pid}.

-spec get_state(binary()) -> #state{}.
get_state(SessionToken) ->
    gen_server:call({global, SessionToken}, get_state, infinity).

-spec get_xsrf_token(binary()) -> binary().
get_xsrf_token(SessionToken) ->
    gen_server:call({global, SessionToken}, get_xsrf_token, infinity).

-spec process_request(atom(), [binary()], term(), pid(), {atom(), pid()},
                      ipport()) -> term().
process_request(undefined, Type, Body, ReplyPid, RemoteEp, SessionToken) ->
    process_request(gen_adapter, Type, Body, ReplyPid, RemoteEp, SessionToken);
process_request(Adapter, Type, Body, ReplyPid, RemoteEp, SessionToken) ->
    ?NoDbLog(debug, [], "request received, type ~p body~n~s", [Type, jsx:prettify(Body)]),
    gen_server:cast({global, SessionToken}, {process, Adapter, Type, Body, ReplyPid, RemoteEp}).

init([XSRFToken, ConnInfo]) ->
    process_flag(trap_exit, true),
    TRef = erlang:send_after(?SESSION_IDLE_TIMEOUT, self(), die),
    case erlimem:open({rpc, node()}, imem_meta:schema()) of
        {error, Error} ->
            ?Error("erlimem open error : ~p", [Error]),
            {stop, Error};
        {ok, ErlImemSess} ->
            {ok, #state{session_idle_tref=TRef, conn_info = ConnInfo, sess=ErlImemSess,
                        xsrf_token = XSRFToken}}
    end.

handle_call(get_state, _From, State) ->
    ?Debug("get_state, result: ~p~n", [State]),
    {reply, State, State};
handle_call(get_xsrf_token, _From, #state{xsrf_token = XSRFToken} = State) ->
    ?Debug("get_xsrf_token, result: ~p~n", [XSRFToken]),
    {reply, XSRFToken, State};
handle_call(Unknown, _From, #state{user=_User}=State) ->
    ?Error("unknown call ~p", [Unknown]),
    {reply, {not_supported, Unknown} , State}.

handle_cast({process, Adapter, Typ, WReq, From, RemoteEp}, #state{session_idle_tref=TRef, inactive_tref = ITref, user_id = UserId} = State) ->
    cancel_timer(TRef),
    ScreenSaverTimeout = ?SCREEN_SAVER_TIMEOUT,
    NewITref = 
    if 
        Typ == [<<"ping">>] orelse UserId == undefined -> ITref;
        ScreenSaverTimeout /= 0 -> cancel_timer(ITref), erlang:send_after(ScreenSaverTimeout, self(), inactive);
        true -> undefined
    end,
    State0 = try process_call({Typ, WReq}, Adapter, From, RemoteEp, State)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            reply(From, [{<<"error">>, <<"Unable to process the request">>}], self()),
            State
    end,
    {noreply, State0#state{session_idle_tref=undefined, inactive_tref = NewITref}};
handle_cast(_Unknown, #state{user=_User}=State) ->
    ?Error("~p received unknown cast ~p for ~p", [self(), _Unknown, _User]),
    {noreply, State}.

handle_info(rearm_session_idle_timer, #state{session_idle_tref=TRef} = State) ->
    cancel_timer(TRef),
    NewTRef = erlang:send_after(?SESSION_IDLE_TIMEOUT, self(), die),
    {noreply, State#state{session_idle_tref=NewTRef}};
handle_info(inactive, #state{user = User, inactive_tref = ITref} = State) ->
    ?Debug([{user, User}], "session ~p inactive for ~p ms starting screensaver", [{self(), User}, ?SCREEN_SAVER_TIMEOUT]),
    if ITref == undefined -> {noreply, State};
       true ->
            cancel_timer(ITref),
            {noreply, State#state{lock_state = screensaver}}
    end;
handle_info(die, #state{user=User}=State) ->
    ?Info([{user, User}], "session ~p idle for ~p ms", [{self(), User}, ?SESSION_IDLE_TIMEOUT]),
    {stop, normal, State};
handle_info(logout, #state{user = User} = State) ->
    ?Debug("terminating session of logged out user ~p", [User]),
    {stop, normal, State};
handle_info(invalid_credentials, #state{old_state = undefined} = State) -> %% TODO : perhaps monitor erlimemsession
    ?Debug("terminating session ~p due to invalid credentials", [self()]),
    {stop, normal, State};
handle_info(invalid_credentials, #state{sess = OldSess} = State) ->
    OldSess:close(),
    {ok, Sess} = erlimem:open({rpc, node()}, imem_meta:schema()),
    {noreply, State#state{sess = Sess}};
handle_info({'EXIT', _Pid, normal}, #state{user = _User} = State) ->
    %?Debug("Received normal exit from ~p for ~p", [Pid, User]),
    {noreply, State};
handle_info({set_id, SessionToken}, State) ->
    {noreply, State#state{id = SessionToken}};
handle_info(Info, #state{user = User} = State) ->
    ?Error("~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{user=User} = State) ->
    ?Info([{user, User}], "~p ~p terminating, reason ~p", [?MODULE, {self(), User}, Reason]),
    logout(State).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

-spec process_call({[binary()], term()}, atom(), pid(), #state{}, ipport()) -> #state{}.
process_call({[<<"login">>], ReqData}, _Adapter, From, {SrcIp, _Port}, 
    #state{lock_state = LockState} = State) when LockState == locked; LockState == screensaver ->
    #state{id = Id, conn_info = ConnInfo, old_state = OldState, xsrf_token = XSRFToken} = State,
    {ReloginTempState, NewToken} = 
    if OldState /= undefined -> {State, Id};
       true -> 
            SessionToken = base64:encode(crypto:rand_bytes(64)),
            global:unregister_name(Id),
            global:register_name(SessionToken, self()),
            From ! {newToken, SessionToken},
            {ok, Sess} = erlimem:open({rpc, node()}, imem_meta:schema()),
            {#state{sess = Sess, id = Id, conn_info = ConnInfo, 
                    lock_state = locked, old_state = State, 
                    xsrf_token = XSRFToken}, SessionToken} 
   end,
    case login(ReqData, From, SrcIp, ReloginTempState) of
        #state{user_id = undefined} = NewState -> NewState#state{id = NewToken};
        #state{sess = TmpSess, old_state = OldState} ->
            TmpSess:close(),
            OldState#state{lock_state = unlocked, id = NewToken}
    end;
process_call({[<<"login">>], ReqData}, _Adapter, From, {SrcIp, _Port}, State) ->
    login(ReqData, From, SrcIp, State);
process_call({[<<"ping">>], _ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{user_id = UserId, id = Id, lock_state = LockState} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, UserId, Id, "ping", "", "", "", "", ""),
    if LockState == unlocked -> 
            reply(From, #{<<"ping">> => node()}, self()),
            State;
       true -> 
            reply(From, #{ping => #{error => show_screen_saver}}, self()),
            State#state{lock_state = screensaver}
    end;
%% IMPORTANT:
% This function clause is placed right after login to be able to catch all
% request (other than login above) which are NOT to be allowed without a login
%
process_call(Req, _Adapter, From, {SrcIp,_}, #state{user = <<>>, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, "", Id, "invalid", io_lib:format("~p", [Req]), "", "", "", ""),
    ?Debug("Request from a not logged in user: ~n~p", [Req]),
    reply(From, [{<<"error">>, <<"user not logged in">>}], self()),
    State;

process_call(Req, _Adapter, From, {SrcIp,_}, #state{lock_state = locked, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, "", Id, "screensaver is enabled", io_lib:format("~p", [Req]), "", "", "", ""),
    ?Debug("Request when screensaver is active from user: ~n~p", [Req]),
    reply(From, [{<<"error">>, <<"Session is locked">>}], self()),
    State;

process_call({[<<"restart">>], _ReqData}, _Adapter, From, {SrcIp,_},
             #state{sess = ErlImemSess, id = Id, user_id = UserId} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, UserId, Id, "restart", "", "", "", "", ""),
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
process_call({[<<"login_change_pswd">>], ReqData}, _Adapter, From, {SrcIp,_},
             #state{sess = ErlImemSess, id = Id, user_id = UserId} = State) ->
    #{<<"change_pswd">> := BodyMap} = jsx:decode(ReqData, [return_maps]),
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "login_change_pswd", BodyMap, "", "", "", ""),
    User        = maps:get(<<"user">>, BodyMap, <<>>),
    OldPassword = list_to_binary(maps:get(<<"password">>, BodyMap, [])),
    NewPassword = maps:get(<<"new_password">>, BodyMap, []),
    case (imem_seco:password_strength_fun())(NewPassword) of
        strong ->    
            case ErlImemSess:run_cmd(
                   change_credentials,
                   [{pwdmd5, OldPassword}, {pwdmd5, erlang:md5(NewPassword)}]
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
            end;
        _ -> reply(From, #{error => <<"Password is not strong">>}, self())
    end,
    State;

process_call({[<<"logout">>], _ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{id = Id, user_id = UserId} = State) ->
    catch dderl:access(?LOGIN_CONNECT, SrcIp, UserId, Id, "logout", "", "", "", "", ""),
    NewState = logout(State),
    reply(From, [{<<"logout">>, <<"ok">>}], self()),
    self() ! logout,
    NewState;

process_call({[<<"format_erlang_term">>], ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "format_erlang_term", ReqData, "", "", "", ""),
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

process_call({[<<"format_json_to_save">>], ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "format_json_to_save", ReqData, "", "", "", ""),
    [{<<"format_json_to_save">>, BodyJson}] = jsx:decode(ReqData),
    StringToFormat = proplists:get_value(<<"json_string">>, BodyJson, <<>>),
    case jsx:is_json(StringToFormat, [{strict, [escapes]}]) of
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

process_call({[<<"about">>], _ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, UserId, Id, "about", "", "", "", "", ""),
    case application:get_key(dderl, applications) of
        undefined -> Deps = [];
        {ok, Deps} -> Deps
    end,
    Apps = application:which_applications(),
    Versions = get_apps_version(Apps, [dderl|Deps]),
    reply(From, [{<<"about">>, Versions}], self()),
    State;

process_call({[<<"connect_info">>], _ReqData}, _Adapter, From, {SrcIp,_},
             #state{sess=Sess, user_id=UserId, user=User,id = Id} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, UserId, Id, "connect_info", "", "", "", "", ""),
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

process_call({[<<"del_con">>], ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{sess = Sess, user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "del_con", ReqData, "", "", "", ""),
    [{<<"del_con">>, BodyJson}] = jsx:decode(ReqData),
    ConId = proplists:get_value(<<"conid">>, BodyJson, 0),
    ?Info([{user, State#state.user}], "connection to delete ~p", [ConId]),
    Resp = case dderl_dal:del_conn(Sess, UserId, ConId) of
        ok -> <<"success">>;
        Error -> [{<<"error">>, list_to_binary(lists:flatten(io_lib:format("~p", [Error])))}]
    end,
    reply(From, [{<<"del_con">>, Resp}], self()),
    State;

process_call({[<<"activate_sender">>], ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{active_sender = undefined, user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "activate_sender", ReqData, "", "", "", ""),
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
process_call({[<<"activate_sender">>], ReqData}, Adapter, From, {SrcIp,_} = RemoteEp, 
             #state{active_sender = PidSender, user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "activate_sender", ReqData, "", "", "", ""),
    case erlang:is_process_alive(PidSender) of
        true ->
            ?Error("Sender ~p already waiting for connection", [PidSender]), %% Log more details user, active sender etc...
            %% TODO: Add information about the active sender. ( if possible block this from browser & show status of sender)
            reply(From, [{<<"activate_sender">>, [{<<"error">>, <<"Sender already waiting for connection">>}]}], self()),
            State;
        false ->
            process_call({[<<"activate_sender">>], ReqData}, Adapter, From, RemoteEp, State#state{active_sender = undefined})
    end;

process_call({[<<"activate_receiver">>], _ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{active_sender = undefined, user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_NOARGS, SrcIp, UserId, Id, "activate_receiver", "", "", "", "", ""),
    ?Error("No active data sender found"), %% TODO: Log more 
    reply(From, [{<<"activate_receiver">>, [{<<"error">>, <<"No table sending data">>}]}], self()),
    State;
process_call({[<<"activate_receiver">>], ReqData}, _Adapter, From, {SrcIp,_}, 
             #state{active_sender = PidSender, user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "activate_receiver", ReqData, "", "", "", ""),
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
process_call({[<<"download_buffer_csv">>], ReqData}, Adapter, From, {SrcIp, _},
             #state{user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "activate_receiver", ReqData, "", "", "", ""),
    [{<<"download_buffer_csv">>, BodyJson}] = jsx:decode(ReqData),
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnPositions = proplists:get_value(<<"column_positions">>, BodyJson, []),
    Filename = proplists:get_value(<<"filename">>, BodyJson, <<>>),
    {TableId, IndexId, Nav, RowFun, Clms} = Statement:get_sender_params(),
    UsedTable = case Nav of
        raw -> TableId;
        ind -> IndexId
    end,
    Columns = gen_adapter:build_column_csv(imem, Clms),
    From ! {reply_csv, Filename, Columns, first},
    FirstKey = ets:first(UsedTable),
    RowSep = gen_adapter:get_csv_row_sep_char(adapter_name(Adapter)),
    ColSep = gen_adapter:get_csv_col_sep_char(adapter_name(Adapter)),
    spawn(fun() ->
        FirstRows = dderl_dal:rows_from(UsedTable, FirstKey, 100),
        produce_buffer_csv_rows(FirstRows, From, TableId, RowFun, ColumnPositions, ColSep, RowSep)
    end),
    %% Timer needs to be rearmed as we are replying with in a no standard way
    self() ! rearm_session_idle_timer,
    State;
process_call({[<<"password_strength">>], ReqData}, _Adapter, From, {SrcIp,_},
             #state{user_id = UserId, id = Id} = State) ->
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, "activate_receiver", ReqData, "", "", "", ""),
    #{<<"password">> := Password} = imem_json:decode(ReqData, [return_maps]),
    reply(
      From,
      [{<<"password_strength">>,
        case (imem_seco:password_strength_fun())(Password) of
            strong -> <<"strong">>;
            short -> <<"short">>;
            weak -> <<"weak">>;
            medium -> <<"medium">>
        end}], self()),
    State;

% commands handled generically
process_call({[C], ReqData}, Adapter, From, {SrcIp,_}, #state{sess = Sess, user_id = UserId, id = Id} = State) when
      C =:= <<"parse_stmt">>;
      C =:= <<"get_query">>;
      C =:= <<"save_view">>;
      C =:= <<"view_op">>;
      C =:= <<"update_view">>;
      C =:= <<"save_dashboard">>;
      C =:= <<"rename_dashboard">>;
      C =:= <<"delete_dashboard">>;
      C =:= <<"histogram">>;
      C =:= <<"statistics">>;
      C =:= <<"statistics_full">>;
      C =:= <<"dashboards">>;
      C =:= <<"edit_term_or_view">>;
      C =:= <<"get_sql">>;
      C =:= <<"cache_data">>;
      C =:= <<"list_d3_templates">>;
      C =:= <<"get_d3_template">> ->
    BodyJson = jsx:decode(ReqData),
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, binary_to_list(C), 
        maps:from_list(proplists:get_value(C, BodyJson, [])), "", "", "", ""),
    Self = self(),
    spawn_link(fun() -> spawn_gen_process_call(Adapter, From, C, BodyJson, Sess, UserId, Self) end),
    State;

process_call({Cmd, ReqData}, Adapter, From, {SrcIp,_},
             #state{sess=Sess, user_id=UserId, adapt_priv = AdaptPriv,
                    conn_info = ConnInfo, id = Id} = State)
  when Cmd =:= [<<"connect">>];
       Cmd =:= [<<"disconnect">>] ->
    BodyJson = jsx:decode(ReqData),
    {Host, ConnStr} = 
            case proplists:get_value(<<"host">>, BodyJson, none) of
                none -> case proplists:get_value(<<"tns">>, BodyJson, none) of
                    none -> {"", ""};
                    Tns -> 
                        case catch re:replace(hd(re:split(lists:nth(2, re:split(Tns, <<"HOST">>)), "\\)")), <<"=">>, <<>>, [{return, binary}]) of
                            H when is_binary(H) -> {H, Tns};
                            _ -> {"", Tns}
                        end
                end;
                H -> {H, proplists:get_value(<<"tns">>, BodyJson, "")}
    end,
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, binary_to_list(hd(Cmd)), maps:from_list(BodyJson), 
        proplists:get_value(<<"user">>, BodyJson, ""), Host, 
        proplists:get_value(<<"adapter">>, BodyJson, ""), ConnStr),
    ?NoDbLog(debug, [{user, UserId}], "~p processing ~p~n~s", [Adapter, Cmd, jsx:prettify(ReqData)]),
    CurrentPriv = Adapter:add_conn_info(proplists:get_value(Adapter, AdaptPriv), ConnInfo),
    NewCurrentPriv =
        try
            TmpPriv = Adapter:process_cmd({Cmd, BodyJson, Id}, Sess, UserId, From, CurrentPriv, self()),
            self() ! rearm_session_idle_timer,
            TmpPriv            
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

process_call({Cmd, ReqData}, Adapter, From, {SrcIp,_}, #state{sess = Sess, user_id = UserId, 
                                                              adapt_priv = AdaptPriv, id = Id} = State) ->
    BodyJson = jsx:decode(ReqData),
    catch dderl:access(?CMD_WITHARGS, SrcIp, UserId, Id, binary_to_list(hd(Cmd)), 
                maps:from_list(BodyJson), "", "", "", ""),
    CurrentPriv = proplists:get_value(Adapter, AdaptPriv),
    Self = self(),
    spawn_link(fun() -> spawn_process_call(Adapter, CurrentPriv, From, Cmd, BodyJson, Sess, UserId, Self) end),
    State.

spawn_process_call(Adapter, CurrentPriv, From, Cmd, BodyJson, Sess, UserId, SelfPid) ->
    try
        Adapter:process_cmd({Cmd, BodyJson}, Sess, UserId, From, CurrentPriv, SelfPid),
        SelfPid ! rearm_session_idle_timer
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, BodyJson]),
            reply(From, [{<<"error">>, <<"Unable to process the request">>}], SelfPid)
    end.

spawn_gen_process_call(Adapter, From, C, BodyJson, Sess, UserId, SelfPid) ->
    try
        gen_adapter:process_cmd({[C], BodyJson}, adapter_name(Adapter), Sess, UserId, From, undefined),
        SelfPid ! rearm_session_idle_timer
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            reply(From, [{<<"error">>, <<"Unable to process the request">>}], SelfPid)
    end.

reply(From, Data, Master) ->
    From ! {reply, Data},
    Master ! rearm_session_idle_timer.

-spec jsq(term()) -> term().
jsq(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
jsq(OtherTypes) -> OtherTypes.

-spec logout(#state{}) -> #state{}.
logout(#state{sess = undefined, adapt_priv = AdaptPriv} = State) ->
    [Adapter:disconnect(Priv) || {Adapter, Priv} <- AdaptPriv],
    State#state{adapt_priv = []};
logout(#state{sess = Sess, old_state = OldState} = State) ->
    try Sess:close()
    catch Class:Error ->
            ?Error("Error trying to close the session ~p ~p:~p~n~p~n",
                   [Sess, Class, Error, erlang:get_stacktrace()])
    end,
    if OldState == undefined -> logout(State#state{sess = undefined});
       true -> logout(OldState)
    end.

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

login(ReqData, From, SrcIp, State) ->
    #state{id = Id, sess = ErlImemSess, conn_info = ConnInfo} = State,
    Host =
    lists:foldl(
      fun({App,_,_}, <<>>) ->
              {ok, Apps} = application:get_key(App, applications),
              case lists:member(dderl, Apps) of
                  true -> atom_to_binary(App, utf8);
                  _ -> <<>>
              end;
         (_, App) -> App
      end, <<>>, application:which_applications()),
    {ok, Vsn} = application:get_key(dderl, vsn),
    Reply0 = #{vsn => list_to_binary(Vsn), host => Host, port => application:get_env(dderl, port),
               node => list_to_binary(imem_meta:node_shard())},
    case catch ErlImemSess:run_cmd(login,[]) of
        {error,{{'SecurityException',{?PasswordChangeNeeded,_}},ST}} ->
            ?Warn("Password expired ~s~n~p", [State#state.user, ST]),
            {[UserId],true} = imem_meta:select(ddAccount, [{#ddAccount{name=State#state.user,
                                           id='$1',_='_'}, [], ['$1']}]),
            State1 = State#state{user_id = UserId},
            if State#state.old_state /= undefined -> {#{accountName => State#state.user}, State1}; %% For screensaver login change password is hidden
               is_map(ReqData) -> {#{changePass => State#state.user}, State1};
               true -> 
                    reply(From, #{login => Reply0#{changePass=>State#state.user}}, self()),
                    State1
            end;
        {error, _} ->
            ReqDataMap = jsx:decode(ReqData, [return_maps]),
            catch dderl:access(?LOGIN_CONNECT, SrcIp, "", Id, "login", ReqDataMap, "", "", "", ""),
            try dderl_dal:process_login(
                         ReqDataMap, State,
                         #{auth => fun(Auth) ->
                                       (State#state.sess):auth(dderl, Id, Auth)
                                   end,
                           connInfo => ConnInfo,
                           relayState => fun dderl_resource:samlRelayStateHandle/2,
                           stateUpdateUsr =>  fun(St, Usr) -> St#state{user=Usr} end,
                           stateUpdateSKey =>  fun(St, _) -> St end,
                           urlPrefix => dderl:get_url_suffix()}) of
                {{E,M},St} when is_atom(E) ->
                    ?Error("Error(~p) ~p~n~p", [E,M,St]),
                    reply(From, #{login=> #{error=> format_error(M)}}, self()),
                    catch dderl:access(?LOGIN_CONNECT, SrcIp, maps:get(<<"User">>, ReqDataMap, ""),
                                Id, "login unsuccessful", "", "", "", "", ""),
                    self() ! invalid_credentials,
                    State;
                {Reply, State1} ->
                    {Reply1, State2} = case Reply of
                          ok -> login(#{}, From, SrcIp, State1);
                          _ -> {Reply, State1}
                    end,
                    case ReqDataMap of
                        #{<<"samluser">> := _} -> reply(From, {saml, dderl:get_url_suffix()}, self());
                        _ -> reply(From, #{login => maps:merge(Reply0, Reply1)}, self())
                    end,
                    State2
            catch
                _ : Error ->
                    ErrMsg = 
                    case Error of
                        {{E, M}, St} -> 
                            ?Error("~p ~p~n~p", [E,M,St]),
                            self() ! invalid_credentials,
                            M;
                        _ -> 
                            ?Error("logging in : ~p ~p", [Error , erlang:get_stacktrace()]),
                            Error
                    end,
                    catch dderl:access(?LOGIN_CONNECT, SrcIp, maps:get(<<"User">>, ReqDataMap, ""),
                                Id, "login unsuccessful", "", "", "", "", ""),
                    reply(From, #{login => #{error => format_error(ErrMsg)}}, self()),
                    State
            end;
        _ ->
            {[UserId],true} = imem_meta:select(ddAccount, [{#ddAccount{name=State#state.user,
                                           id='$1',_='_'}, [], ['$1']}]),
            catch dderl:access(?LOGIN_CONNECT, SrcIp, UserId, Id, "login successful", "", "", "", "", ""),
            if is_map(ReqData) -> {#{accountName=>State#state.user}, State#state{user_id = UserId}};
               true -> 
                    reply(From, #{login => maps:merge(Reply0, #{accountName=>State#state.user})}, self()),
                    State#state{user_id = UserId}
            end
    end.

format_error(Error) when is_binary(Error) -> Error;
format_error(Error) when is_list(Error) -> list_to_binary(Error);
format_error(Error) -> list_to_binary(io_lib:format("~p", [Error])).

-spec add_csv_separators([[binary()]], list(), list()) -> [[binary() | list()]]. 
add_csv_separators([], _ColSep, _RowSep) -> [];
add_csv_separators([Row | Rest], ColSep, RowSep) ->
    [[add_col_separator(Row, ColSep), RowSep] | add_csv_separators(Rest, ColSep, RowSep)].

-spec add_col_separator([binary()], list()) -> [binary() | list()].
add_col_separator([], _ColSep) -> [];
add_col_separator([Col], _ColSep) -> [Col];
add_col_separator([Col | Rest], ColSep) ->
    [Col, ColSep | add_col_separator(Rest, ColSep)].

produce_buffer_csv_rows('$end_of_table', From, _, _, _, _, _) ->
    From ! {reply_csv, <<>>, <<>>, last};
produce_buffer_csv_rows({[], '$end_of_table'}, From, _, _, _, _, _) ->
    From ! {reply_csv, <<>>, <<>>, last};
produce_buffer_csv_rows({Rows, '$end_of_table'}, From, TableId, RowFun, ColumnPositions, ColSep, RowSep) ->
    ExpandedRows = dderl_dal:expand_rows(Rows, TableId, RowFun, ColumnPositions),
    CsvRows = iolist_to_binary(add_csv_separators(ExpandedRows, ColSep, RowSep)),
    From ! {reply_csv, <<>>, CsvRows, last};
produce_buffer_csv_rows({Rows, Continuation}, From, TableId, RowFun, ColumnPositions, ColSep, RowSep) ->
    ExpandedRows = dderl_dal:expand_rows(Rows, TableId, RowFun, ColumnPositions),
    CsvRows = iolist_to_binary(add_csv_separators(ExpandedRows, ColSep, RowSep)),
    From ! {reply_csv, <<>>, CsvRows, continue},
    produce_buffer_csv_rows(ets:select(Continuation), From, TableId, RowFun, ColumnPositions, ColSep, RowSep).

-spec cancel_timer(undefined | reference()) -> ok.
cancel_timer(undefined) -> ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.
