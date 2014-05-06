-module(dderl_session).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behavior(gen_server).

-include("dderl.hrl").

-export([start/0
        , process_request/5
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

-define(SESSION_IDLE_TIMEOUT, 90000). % 90 secs
%%-define(SESSION_IDLE_TIMEOUT, 5000). % 5 sec (for testing)

-record(state, {
        adapt_priv = [] :: list()
        , tref :: timer:tref()
        , user = <<>> :: binary()
        , user_id :: ddEntityId()
        , sess :: {atom, pid()}
        }).

-spec start() -> {dderl_session, pid()}.
start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    {dderl_session, Pid}.

-spec get_state({atom(), pid()}) -> #state{}.
get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

-spec process_request(atom(), [binary()], term(), pid(), {atom(), pid()}) -> term().
process_request(undefined, Type, Body, ReplyPid, Ref) ->
    process_request(gen_adapter, Type, Body, ReplyPid, Ref);
process_request(Adapter, Type, Body, ReplyPid, {?MODULE, Pid}) ->
    ?NoDbLog(debug, [], "request received, type ~p body~n~s", [Type, jsx:prettify(Body)]),
    gen_server:cast(Pid, {process, Adapter, Type, Body, ReplyPid}).

init(_Args) ->
    process_flag(trap_exit, true),
    Self = self(),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    ?Debug("dderl_session ~p started!", [{dderl_session, Self}]),
    {ok, #state{tref=TRef}}.

handle_call(get_state, _From, State) ->
    ?Debug("get_state, result: ~p~n", [State]),
    {reply, State, State};
handle_call(Unknown, _From, #state{user=_User}=State) ->
    ?Error([{user, _User}], "unknown call ~p", [Unknown]),
    {reply, {no_supported, Unknown} , State}.

handle_cast({process, Adapter, Typ, WReq, ReplyPid}, #state{tref=TRef} = State) ->
    timer:cancel(TRef),
    State0 = process_call({Typ, WReq}, Adapter, ReplyPid, State),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {noreply, State0#state{tref=NewTRef}};
handle_cast(_Unknown, #state{user=_User}=State) ->
    ?Error([{user, _User}], "~p received unknown cast ~p for ~p", [self(), _Unknown, _User]),
    {noreply, State}.

handle_info(die, #state{user=_User}=State) ->
    ?Error([{user, _User}], "terminating session idle for ~p ms", [?SESSION_IDLE_TIMEOUT]),
    {stop, timeout, State};
handle_info(logout, #state{user = User} = State) ->
    ?Debug("terminating session of logged out user ~p", [User]),
    {stop, logout, State};
handle_info(invalid_credentials, #state{} = State) ->
    ?Debug("terminating session ~p due to invalid credentials", [self()]),
    {stop, invalid_credentials, State};
handle_info({'EXIT', _Pid, normal}, #state{user = _User} = State) ->
    %?Debug("Received normal exit from ~p for ~p", [Pid, User]),
    {noreply, State};
handle_info(Info, #state{user = User} = State) ->
    ?Error([{user, User}], "~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{user=User} = State) ->
    ?Info([{user, User}], "~p terminating ~p session for ~p", [?MODULE, {self(), User}, Reason]),
    logout(State).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec process_call({[binary()], term()}, atom(), pid(), #state{}) -> #state{}.
process_call({[<<"login">>], ReqData}, _Adapter, From, State) ->
    [{<<"login">>, BodyJson}] = jsx:decode(ReqData),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    case dderl_dal:login(User, Password) of
        {true, Sess, UserId} ->
            ?Debug("login successful for ~p", [User]),
            From ! {reply, jsx:encode([{<<"login">>,<<"ok">>}])},
            State#state{sess=Sess, user=User, user_id=UserId};
        {_, {error, {Exception, {"Password expired. Please change it", _} = M}}} ->
            ?Debug("Password expired for ~p, result ~p", [User, {Exception, M}]),
            From ! {reply, jsx:encode([{<<"login">>,<<"expired">>}])},
            State;
        {_, {error, {Exception, M}}} ->
            ?Error("login failed for ~p, result ~n~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply, jsx:encode([{<<"login">>,Err}])},
            self() ! invalid_credentials,
            State;
        {error, {{Exception, {"Password expired. Please change it", _} = M}, _Stacktrace}} ->
            ?Error("Password expired for ~p, result ~p", [User, {Exception, M}]),
            From ! {reply, jsx:encode([{<<"login">>,<<"expired">>}])},
            State;
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("login failed for ~p, result ~n~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply, jsx:encode([{<<"login">>, Err}])},
            self() ! invalid_credentials,
            State
    end;

process_call({[<<"login_change_pswd">>], ReqData}, _Adapter, From, State) ->
    [{<<"change_pswd">>, BodyJson}] = jsx:decode(ReqData),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    NewPassword = binary_to_list(proplists:get_value(<<"new_password">>, BodyJson, <<>>)),
    case dderl_dal:change_password(User, Password, NewPassword) of
        {true, Sess, UserId} ->
            ?Debug("change password successful for ~p", [User]),
            From ! {reply, jsx:encode([{<<"login_change_pswd">>,<<"ok">>}])},
            State#state{sess=Sess, user=User, user_id=UserId};
        {_, {error, {Exception, M}}} ->
            ?Error("change password failed for ~p, result ~n~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply, jsx:encode([{<<"login_change_pswd">>,Err}])},
            State;
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("change password failed for ~p, result ~n~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply, jsx:encode([{<<"login_change_pswd">>, Err}])},
            State
    end;

process_call({[<<"logout">>], _ReqData}, _Adapter, From, #state{} = State) ->
    NewState = logout(State),
    From ! {reply, jsx:encode([{<<"logout">>, <<"ok">>}])},
    self() ! logout,
    NewState;

process_call({[<<"format_erlang_term">>], ReqData}, _Adapter, From, #state{} = State) ->
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
            From ! {reply, jsx:encode([{<<"format_erlang_term">>,
                                        [
                                            {<<"error">>, <<"Invalid erlang term">>},
                                            {<<"originalText">>, StringToFormat}
                                        ]}])};
        Formatted ->
            ?Debug("The formatted text: ~p", [Formatted]),
            From ! {reply, jsx:encode([{<<"format_erlang_term">>, Formatted}])}
    end,
    State;

process_call({[<<"about">>], _ReqData}, _Adapter, From, #state{} = State) ->
    case application:get_key(dderl, applications) of
        undefined -> Deps = [];
        {ok, Deps} -> Deps
    end,
    Apps = application:which_applications(),
    Versions = get_apps_version(Apps, [dderl|Deps]),
    From ! {reply, jsx:encode([{<<"about">>, Versions}])},
    State;

process_call(Req, _Adapter, From, #state{user = <<>>} = State) ->
    ?Error("Request from a not logged in user: ~n~p", [Req]),
    From ! {reply, jsx:encode([{<<"error">>, <<"user not logged in">>}])},
    State;

process_call({[<<"ping">>], _ReqData}, _Adapter, From, #state{} = State) ->
    From ! {reply, jsx:encode([{<<"ping">>, <<"pong">>}])},
    State;

process_call({[<<"adapters">>], _ReqData}, _Adapter, From, #state{sess=Sess, user=User} = State) ->
    Res = jsx:encode([{<<"adapters">>,
            [ [{<<"id">>, jsq(A#ddAdapter.id)}
              ,{<<"fullName">>, A#ddAdapter.fullName}]
            || A <- dderl_dal:get_adapters(Sess)]}]),
    ?Debug([{user, User}], "adapters ~s", [jsx:prettify(Res)]),
    From ! {reply, Res},
    State;

process_call({[<<"connects">>], _ReqData}, _Adapter, From, #state{sess=Sess, user=User} = State) ->
    case dderl_dal:get_connects(Sess, User) of
        [] ->
            From ! {reply, jsx:encode([{<<"connects">>,[]}])};
        UnsortedConns ->
            Connections = lists:sort(fun(#ddConn{name = Name}, #ddConn{name = Name2}) -> Name > Name2 end, UnsortedConns),
            ?Debug([{user, User}], "conections ~p", [Connections]),
            Res = jsx:encode([{<<"connects">>,
                lists:foldl(fun(C, Acc) ->
                    [{integer_to_binary(C#ddConn.id), [
                            {<<"name">>, C#ddConn.name}
                          , {<<"adapter">>, jsq(C#ddConn.adapter)}
                          , {<<"service">>, jsq(C#ddConn.schm)}
                          , {<<"owner">>, jsq(C#ddConn.owner)}
                          ] ++
                          [{atom_to_binary(N, utf8), jsq(V)} || {N,V} <- C#ddConn.access]
                     } | Acc]
                end,
                [],
                Connections)
            }]),
            ?Debug([{user, User}], "connections as json ~s", [jsx:prettify(Res)]),
            From ! {reply, Res}
    end,
    State;

process_call({[<<"del_con">>], ReqData}, _Adapter, From, #state{sess=Sess, user=User} = State) ->
    [{<<"del_con">>, BodyJson}] = jsx:decode(ReqData),
    ConId = proplists:get_value(<<"conid">>, BodyJson, 0),
    ?Info([{user, User}], "connection to delete ~p", [ConId]),
    Resp = case dderl_dal:del_conn(Sess, ConId) of
        ok -> <<"success">>;
        Error -> [{<<"error">>, list_to_binary(lists:flatten(io_lib:format("~p", [Error])))}]
    end,
    From ! {reply, jsx:encode([{<<"del_con">>, Resp}])},
    State;

% commands handled generically
process_call({[C], ReqData}, Adapter, From, #state{sess=Sess, user_id=UserId} = State) when
      C =:= <<"parse_stmt">>;
      C =:= <<"get_query">>;
      C =:= <<"save_view">>;
      C =:= <<"view_op">>;
      C =:= <<"update_view">>;
      C =:= <<"save_dashboard">>;
      C =:= <<"histogram">>;
      C =:= <<"statistics">>;
      C =:= <<"dashboards">>;
      C =:= <<"edit_term_or_view">> ->
    BodyJson = jsx:decode(ReqData),
    try gen_adapter:process_cmd({[C], BodyJson}, adapter_name(Adapter), Sess, UserId, From, undefined)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            From ! {reply, jsx:encode([{<<"error">>, <<"Unable to process the request">>}])}
    end,
    State;

process_call({Cmd, ReqData}, Adapter, From, #state{sess=Sess, user_id=UserId, adapt_priv=AdaptPriv} = State) when
      Cmd =:= [<<"connect">>];
      Cmd =:= [<<"connect_change_pswd">>];
      Cmd =:= [<<"disconnect">>] ->
    CurrentPriv = proplists:get_value(Adapter, AdaptPriv),
    BodyJson = jsx:decode(ReqData),
    ?NoDbLog(debug, [{user, UserId}], "~p processing ~p~n~s", [Adapter, Cmd, jsx:prettify(ReqData)]),
    NewCurrentPriv =
        try Adapter:process_cmd({Cmd, BodyJson}, Sess, UserId, From, CurrentPriv, self())
        catch Class:Error ->
                ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
                From ! {reply, jsx:encode([{<<"error">>, <<"Unable to process the request">>}])},
                CurrentPriv
        end,
    case proplists:is_defined(Adapter, AdaptPriv) of
        true -> NewAdaptPriv = lists:keyreplace(Adapter, 1, AdaptPriv, {Adapter, NewCurrentPriv});
        false -> NewAdaptPriv = [{Adapter, NewCurrentPriv} | AdaptPriv]
    end,
    State#state{adapt_priv = NewAdaptPriv};

process_call({Cmd, ReqData}, Adapter, From, #state{sess=Sess, user_id=UserId, adapt_priv=AdaptPriv} = State) ->
    CurrentPriv = proplists:get_value(Adapter, AdaptPriv),
    BodyJson = jsx:decode(ReqData),
    Self = self(),
    spawn_link(fun() -> spawn_process_call(Adapter, CurrentPriv, From, Cmd, BodyJson, Sess, UserId, Self) end),
    State.

spawn_process_call(Adapter, CurrentPriv, From, Cmd, BodyJson, Sess, UserId, SelfPid) ->
    ?NoDbLog(debug, [{user, UserId}], "~p processing ~p", [Adapter, Cmd]),
    try Adapter:process_cmd({Cmd, BodyJson}, Sess, UserId, From, CurrentPriv, SelfPid)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            From ! {reply, jsx:encode([{<<"error">>, <<"Unable to process the request">>}])},
            error
    end.

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
