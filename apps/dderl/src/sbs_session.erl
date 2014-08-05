-module(sbs_session).

-behavior(gen_server).

-include("dderl.hrl").

-export([start/2
        , process_request/4
        , get_state/1
        ]).

-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SESSION_IDLE_TIMEOUT, 90000). % 90 secs

-record(state, {tref :: timer:tref()
        , user = <<>> :: binary()
        , user_id :: ddEntityId()
        , sess :: {atom, pid()}
        }).

-spec start(binary(), binary()) -> {ok, {dderl_session, pid()}} | {error, term()}.
start(Username, Password) ->
    case gen_server:start(?MODULE, [Username, Password], []) of
        {ok, Pid} -> {ok, {?MODULE, Pid}};
        Error -> Error
    end.

-spec get_state({atom(), pid()}) -> #state{}.
get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

-spec process_request([binary()], term(), pid(), {atom(), pid()}) -> term().
process_request(Type, Body, ReplyPid, {?MODULE, Pid}) ->
    ?NoDbLog(debug, [], "request received, type ~p body~n~s", [Type, jsx:prettify(Body)]),
    gen_server:cast(Pid, {process, Type, Body, ReplyPid}).

init([Username, Password]) ->
    %TODO: No clean up since there is no ping yet...
    %{ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    case dderl_dal:login(Username, Password) of
        {true, Sess, UserId} ->
            ?Info("login successful for ~p", [{self(), Username}]),
            {ok, #state{sess=Sess, user=Username, user_id=UserId}};
        _ ->
            {stop, unauthorized}
    end.

handle_call(get_state, _From, State) ->
    ?Debug("get_state, result: ~p~n", [State]),
    {reply, State, State};
handle_call(Unknown, _From, #state{user=_User}=State) ->
    ?Error([{user, _User}], "unknown call ~p", [Unknown]),
    {reply, {no_supported, Unknown} , State}.

handle_cast({process, Typ, WReq, ReplyPid}, #state{} = State) ->
%    timer:cancel(TRef),
    State0 = process_call({Typ, WReq}, ReplyPid, State),
%    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {noreply, State0};
handle_cast(_Unknown, #state{user=_User}=State) ->
    ?Error([{user, _User}], "~p received unknown cast ~p for ~p", [self(), _Unknown, _User]),
    {noreply, State}.

handle_info(die, #state{user=User}=State) ->
    ?Info([{user, User}], "session ~p idle for ~p ms", [{self(), User}, ?SESSION_IDLE_TIMEOUT]),
    {stop, normal, State};
handle_info({'EXIT', _Pid, normal}, #state{user = _User} = State) ->
    {noreply, State};
handle_info(Info, #state{user = User} = State) ->
    ?Error([{user, User}], "~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{user=User}) ->
    ?Info([{user, User}], "~p ~p terminating, reason ~p", [?MODULE, {self(), User}, Reason]),
    #state{}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

process_call({[<<"ping">>], _ReqData}, From, #state{} = State) ->
    From ! {reply, jsx:encode([{<<"ping">>, <<"pong">>}])},
    State;

process_call({[C], ReqData}, From, #state{sess=Sess, user_id=UserId} = State) ->
    BodyJson = jsx:decode(ReqData),
    spawn_link(fun() -> spawn_process_call(From, C, BodyJson, Sess, UserId) end),
    State.

spawn_process_call(From, C, BodyJson, Sess, UserId) ->
    try gen_adapter:process_cmd({[C], BodyJson}, gen, Sess, UserId, From, undefined)
    catch Class:Error ->
            ?Error("Problem processing command: ~p:~p~n~p~n", [Class, Error, erlang:get_stacktrace()]),
            From ! {reply, jsx:encode([{<<"error">>, <<"Unable to process the request">>}])}
    end.
