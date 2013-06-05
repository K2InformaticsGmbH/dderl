-module(dderl_session).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behavior(gen_server).

-include("dderl.hrl").

-export([start/0
        , process_request/5
        , get_state/1
        ]).

-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
        ]).

-define(SESSION_IDLE_TIMEOUT, 3600000). % 1 hour
%%-define(SESSION_IDLE_TIMEOUT, 5000). % 5 sec (for testing)

-record(state, {
        adapt_priv = []
        , tref
        , user = <<>>
        , user_id
        , sess
        }).

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {dderl_session, Pid}.

get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

process_request(undefined, Type, Body, ReplyPid, Ref) ->
    process_request(gen_adapter, Type, Body, ReplyPid, Ref);
process_request(Adapter, Type, Body, ReplyPid, {?MODULE, Pid}) ->
    ?Debug("request received ~p", [{Type, Body}]),
    gen_server:cast(Pid, {process, Adapter, Type, Body, ReplyPid}).

init(_Args) ->
    Self = self(),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    ?Info("dderl_session ~p started!", [{dderl_session, Self}]),
    {ok, #state{tref=TRef}}.

handle_call(get_state, _From, State) ->
    ?Debug("get_state!", []),
    {reply, State, State};
handle_call(Unknown, _From, #state{user=_User}=State) ->
    ?Error([{user, _User}], "unknown call ~p", [Unknown]),
    {reply, {no_supported, Unknown} , State}.

handle_cast({process, Adapter, Typ, WReq, ReplyPid}, #state{tref=TRef} = State) ->
    timer:cancel(TRef),
    ?Debug("processing request ~p", [{Typ, WReq}]),
    State0 = process_call({Typ, WReq}, Adapter, ReplyPid, State),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {noreply, State0#state{tref=NewTRef}};
handle_cast(_Unknown, #state{user=_User}=State) ->
    ?Error([{user, _User}], "~p received unknown cast ~p for ~p", [_Unknown, _User]),
    {noreply, State}.

handle_info(die, #state{user=_User}=State) ->
    ?Error([{user, _User}], "terminating session idle for ~p ms", [?SESSION_IDLE_TIMEOUT]),
    {stop, timeout, State};
handle_info(logout, #state{user = User} = State) ->
    ?Debug("terminating session of logged out user ~p", [User]),
    {stop, normal, State};
handle_info(not_logged_in, #state{} = State) ->
    ?Error("Session ~p not logged in trying to make a request, state: ~n~p", [self(), State]),
    {stop, normal, State};
handle_info(Info, #state{user=User}=State) ->
    ?Error([{user, User}], "~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{user=User} = State) ->
    logout(State),
    ?Info([{user, User}], "~p terminating ~p session for ~p", [?MODULE, {self(), User}, Reason]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_call({[<<"login">>], ReqData}, _Adapter, From, State) ->
    [{<<"login">>, BodyJson}] = jsx:decode(ReqData),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    case dderl_dal:login(User, Password) of
        {true, Sess, UserId} ->
            ?Debug("login successful for ~p", [User]),
            From ! {reply, jsx:encode([{<<"login">>,<<"ok">>}])},
            State#state{sess=Sess, user=User, user_id=UserId};
        {_, {error, {Exception, "Password expired. Please change it" = M}}} ->
            ?Debug("Password expired for ~p, result ~p", [User, {Exception, M}]),
            From ! {reply, jsx:encode([{<<"login">>,<<"expired">>}])},
            State;
        {_, {error, {Exception, M}}} ->
            ?Error("login failed for ~p, result ~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": "++ element(1, M)),
            From ! {reply, jsx:encode([{<<"login">>,Err}])},
            State;
        {error, {{Exception, {"Password expired. Please change it", _} = M}, _Stacktrace}} ->
            ?Error("Password expired for ~p, result ~p", [User, {Exception, M}]),
            From ! {reply, jsx:encode([{<<"login">>,<<"expired">>}])},
            State;
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("login failed for ~p, result ~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++ element(1, M)),
            From ! {reply, jsx:encode([{<<"login">>, Err}])},
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
            ?Error("change password failed for ~p, result ~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": "++ element(1, M)),
            From ! {reply, jsx:encode([{<<"login_change_pswd">>,Err}])},
            State;
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("change password failed for ~p, result ~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++ element(1, M)),
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
    %% Auto formating not supported for now, just set 1 to expansion level.
    case proplists:get_value(<<"expansion_level">>, BodyJson, 1) of
        <<"auto">> -> ExpandLevel = 1;
        ExpandLevel -> ok
    end,
    case erlformat:format(binary_to_list(StringToFormat), ExpandLevel) of
        {error, ErrorInfo} ->
            ?Error("Error trying to format the erlang term ~p~n~p", [StringToFormat, ErrorInfo]),
            From ! {reply, jsx:encode([{<<"format_erlang_term">>,
                                        [{<<"error">>, <<"Invalid erlang term">>}]}])};
        Formatted ->
            ?Debug("The formatted text: ~p", [Formatted]),
            From ! {reply, jsx:encode([{<<"format_erlang_term">>, Formatted}])}
    end,
    State;

process_call(Req, _Adapter, From, #state{user = <<>>} = State) ->
    ?Error("Request from a not logged in user: ~n~p", [Req]),
    From ! {reply, jsx:encode([{<<"error">>, <<"user not logged in">>}])},
    %%TODO: Should we really terminate here?, maybe the error reply is ok...
    self() ! not_logged_in,
    State;

process_call({[<<"adapters">>], _ReqData}, _Adapter, From, #state{sess=Sess, user=User} = State) ->
    Res = jsx:encode([{<<"adapters">>,
            [ [{<<"id">>,list_to_binary(atom_to_list(A#ddAdapter.id))}
              ,{<<"fullName">>,list_to_binary(A#ddAdapter.fullName)}]
            || A <- dderl_dal:get_adapters(Sess)]}]),
    ?Debug([{user, User}], "adapters " ++ jsx:prettify(Res)),
    From ! {reply, Res},
    State;

process_call({[<<"connects">>], _ReqData}, _Adapter, From, #state{sess=Sess, user=User} = State) ->
    case dderl_dal:get_connects(Sess, User) of
        [] ->
            From ! {reply, jsx:encode([{<<"connects">>,[]}])};
        Connections ->
            ?Debug([{user, User}], "conections ~p", [Connections]),
            Res = jsx:encode([{<<"connects">>,
                lists:foldl(fun(C, Acc) ->
                    [{list_to_binary(integer_to_list(C#ddConn.id)), [
                            {<<"name">>,jsq(C#ddConn.name)}
                          , {<<"adapter">>,jsq(C#ddConn.adapter)}
                          , {<<"service">>, jsq(C#ddConn.schema)}
                          , {<<"owner">>, jsq(C#ddConn.owner)}
                          ] ++
                          [{list_to_binary(atom_to_list(N)), jsq(V)} || {N,V} <- C#ddConn.access]
                     } | Acc]
                end,
                [],
                Connections)
            }]),
            ?Debug([{user, User}], "adapters " ++ jsx:prettify(Res)),
            From ! {reply, Res}
    end,
    State;

process_call({[<<"del_con">>], ReqData}, _Adapter, From, #state{sess=Sess, user=_User} = State) ->
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
process_call({[C], ReqData}, _Adapter, From, #state{sess=Sess, user_id=UserId} = State) when
      C =:= <<"parse_stmt">>;
      C =:= <<"get_query">>;
      C =:= <<"save_view">> ->
    BodyJson = jsx:decode(ReqData),
    gen_adapter:process_cmd({[C], BodyJson}, Sess, UserId, From, undefined),
    State;

process_call({Cmd, ReqData}, Adapter, From, #state{sess=Sess, user_id=UserId, adapt_priv=AdaptPriv} = State) ->
    CurrentPriv = proplists:get_value(Adapter, AdaptPriv),
    BodyJson = jsx:decode(ReqData),
    ?Debug([{user, User}], "~p processing ~p", [Adapter, {Cmd,BodyJson}]),
    NewCurrentPriv = Adapter:process_cmd({Cmd, BodyJson}, Sess, UserId, From, CurrentPriv),
    case proplists:is_defined(Adapter, AdaptPriv) of
        true ->
            NewAdaptPriv = lists:keyreplace(Adapter, 1, AdaptPriv, {Adapter, NewCurrentPriv});
        false ->
            NewAdaptPriv = [{Adapter, NewCurrentPriv} | AdaptPriv]
    end,
    State#state{adapt_priv = NewAdaptPriv}.

jsq(Bin) when is_binary(Bin) -> Bin;
jsq(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
jsq(Str)                     -> list_to_binary(Str).

logout(#state{sess=undefined, adapt_priv=AdaptPriv} = State) ->
    [Adapter:disconnect(Priv) || {Adapter, Priv} <- AdaptPriv],
    State#state{adapt_priv = []};
logout(#state{sess=Sess} = State) ->
    Sess:close(),
    logout(State#state{sess=undefined}).
