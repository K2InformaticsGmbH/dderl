-module(dderl_session).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-behavior(gen_server).

-include("dderl.hrl").

-export([start/0
        , process_request/3
        , set_adapter/2
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
        adapt_priv
        , statements = []
        , tref
        , user = <<>>
        , adapter = gen_adapter
    }).

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {dderl_session, Pid}.

get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

process_request(Type, Body, {?MODULE, Pid}) ->
    ?Debug("request received ~p", [{Type, Body}]),
    gen_server:call(Pid, {process, Type, Body}, infinity).

set_adapter(Adapter, {?MODULE, Pid}) ->
    AdaptMod  = list_to_existing_atom(Adapter++"_adapter"),
    gen_server:call(Pid, {adapter, AdaptMod}, infinity).

init(_Args) ->
    Self = self(),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    %%Key = erlang:phash2({dderl_session, self()}),
    ?Info("dderl_session ~p started!", [{dderl_session, Self}]),
    {ok, #state{tref=TRef}}.

handle_call({adapter, Adapter}, _From, State) ->
    ?Debug("adapter ~p initialized!", [Adapter]),
    {reply, ok, State#state{adapter=Adapter}};
handle_call(get_state, _From, State) ->
    ?Debug("get_state!", []),
    {reply, State, State};
handle_call({process, Typ, WReq}, From, #state{tref=TRef} = State) ->
    timer:cancel(TRef),
    %%NewKey = if SessKey =/= Key -> SessKey; true -> Key end,
    ?Debug("processing request ~p", [{Typ, WReq}]),
    R = process_call({Typ, WReq}, From, State),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    case R of
        {Rep, Resp, NewState} ->
            ?Debug("generated resp ~p", [{Typ, Resp}]),
            {Rep, Resp, NewState#state{tref=NewTRef}};
        {Rep, NewState} ->
            ?Debug("response deferred ~p", [Typ]),
            {Rep, NewState#state{tref=NewTRef}}
    end.

process_call({[<<"login">>], ReqData}, _From, State) ->
    [{<<"login">>, BodyJson}] = jsx:decode(ReqData),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    case dderl_dal:login(User, Password) of
        true ->
            ?Debug("login successful for ~p", [User]),
            Res = jsx:encode([{<<"login">>,<<"ok">>}]),
            {reply, binary_to_list(Res), State#state{user=User}};
        {_, {error, {Exception, M}}} ->
            ?Error("login failed for ~p, result ~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": "++ element(1, M)),
            Res = jsx:encode([{<<"login">>,Err}]),
            {reply, binary_to_list(Res), State}
    end;
process_call({[<<"adapters">>], _ReqData}, _From, #state{user=User} = State) ->
    Res = jsx:encode([{<<"adapters">>,
            [ [{<<"id">>,list_to_binary(atom_to_list(A#ddAdapter.id))}
              ,{<<"fullName">>,list_to_binary(A#ddAdapter.fullName)}]
            || A <- dderl_dal:get_adapters()]}]),
    ?Debug([{user, User}], "adapters " ++ jsx:prettify(Res)),
    {reply, binary_to_list(Res), State};

process_call({[<<"connects">>], _ReqData}, _From, #state{user=User} = State) ->
    case dderl_dal:get_connects(User) of
        [] -> {reply, binary_to_list(jsx:encode([{<<"connects">>,[]}])), State};
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
            {reply, binary_to_list(Res), State#state{user=User}}
    end;

process_call({[<<"del_con">>], ReqData}, _From, #state{user=User} = State) ->
    [{<<"del_con">>, BodyJson}] = jsx:decode(ReqData),
    ConId = proplists:get_value(<<"conid">>, BodyJson, 0),
    ?Info("connection to delete ~p", [ConId]),
    Resp = case dderl_dal:del_conn(ConId) of
        ok -> <<"success">>;
        Error -> [{<<"error">>, list_to_binary(lists:flatten(io_lib:format("~p", [Error])))}]
    end,
    {reply, binary_to_list(jsx:encode([{<<"del_con">>, Resp}])), State#state{user=User}};

process_call({Cmd, ReqData}, Parent, #state{adapt_priv=AdaptPriv,adapter=AdaptMod, user=User} = State) ->
    BodyJson = jsx:decode(ReqData),
    Self = self(),
    spawn(fun() ->
            ?Debug([{user, User}], "~p processing ~p", [AdaptMod, {Cmd,BodyJson}]),
            {NewAdaptPriv, Resp} =
                AdaptMod:process_cmd({Cmd, BodyJson}, AdaptPriv),
            ?Debug([{user, User}], "~p response ~p", [AdaptMod, {Cmd,Resp}]),
            gen_server:cast(Self, {resp, {NewAdaptPriv, Resp, Parent}})
    end),
    {noreply, State}.

handle_cast({resp, {NewAdaptPriv, Resp, Parent}}, #state{user=User}=State) ->
    ?Debug([{user, User}], "~p received response ~p for ~p", [?MODULE, Resp, Parent]),
    gen_server:reply(Parent, Resp),
    {noreply, State#state{adapt_priv=NewAdaptPriv}};
handle_cast(Request, #state{user=User}=State) ->
    ?Error([{user, User}], "~p received unknown cast ~p for ~p", [?MODULE, Request, User]),
    {noreply, State}.

handle_info(die, State) -> {stop, timeout, State};
handle_info(Info, #state{user=User}=State) ->
    ?Error([{user, User}], "~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{user=User}) ->
    ?Info([{user, User}], "~p terminating ~p session for ~p", [?MODULE, {self(), User}, Reason]).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jsq(Bin) when is_binary(Bin) -> Bin;
jsq(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
jsq(Str)                     -> list_to_binary(Str).
