-module(dderl_session).

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

-record(state, {
        key
        , session
        , statements = []
        , tref
        , user = <<>>
        , adapter = gen_adapter
    }).

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Key = erlang:phash2({dderl_session, Pid}),
    {Key, {dderl_session, Pid}}.

get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

process_request(SessKey, WReq, {?MODULE, Pid}) ->
    lager:debug([{session, SessKey}], "request received ~p", [WReq]),
    Type = wrq:disp_path(WReq),
    gen_server:call(Pid, {process, SessKey, Type, WReq}, infinity).

set_adapter(Adapter, {?MODULE, Pid}) ->
    AdaptMod  = list_to_existing_atom(Adapter++"_adapter"),
    gen_server:call(Pid, {adapter, AdaptMod}, infinity).

init(_Args) ->
    Self = self(),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    Key = erlang:phash2({dderl_session, self()}),
    lager:info([{session, Key}], "dderl_session ~p started!", [{dderl_session, Self}]),
    {ok, #state{key=Key,tref=TRef}}.

handle_call({adapter, Adapter}, _From, #state{key=Key}=State) ->
    Adapter:init(),
    lager:debug([{session, Key}], "adapter ~p initialized!", [Adapter]),
    {reply, ok, State#state{adapter=Adapter}};
handle_call(get_state, _From, #state{key=Key} = State) ->
    lager:debug([{session, Key}], "get_state!", []),
    {reply, State, State};
handle_call({process, SessKey, Typ, WReq}, From, #state{tref=TRef, key=Key} = State) ->
    timer:cancel(TRef),
    NewKey = if SessKey =/= Key -> SessKey; true -> Key end,
    lager:debug([{session, Key}], "processing request ~p", [{Typ, WReq}]),
    R = process_call({Typ, WReq}, From, State#state{key=NewKey}),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    case R of
        {Rep, Resp, NewState} ->
            lager:debug([{session, Key}], "generated resp ~p", [{Typ, Resp}]),
            {Rep, Resp, NewState#state{tref=NewTRef,key=NewKey}};
        {Rep, NewState} ->
            lager:debug([{session, Key}], "response deferred ~p", [Typ]),
            {Rep, NewState#state{tref=NewTRef,key=NewKey}}
    end.

process_call({"login", ReqData}, _From, #state{key=Key} = State) ->
    [{<<"login">>, BodyJson}] = jsx:decode(wrq:req_body(ReqData)),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    case dderl_dal:login(User, Password) of
        true ->
            lager:info([{session, Key}], "login successful for ~p", [User]),
            Res = jsx:encode([{<<"login">>,<<"ok">>},{<<"session">>,list_to_binary(integer_to_list(Key))}]),
            {reply, binary_to_list(Res), State#state{user=User}};
        {_, {error, {Exception, M}}} ->
            lager:error([{session, Key}], "login failed for ~p, result ~p", [User, {Exception, M}]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": "++ element(1, M)),
            Res = jsx:encode([{<<"login">>,Err}]),
            {reply, binary_to_list(Res), State}
    end;
process_call({"adapters", _ReqData}, _From, #state{key=Key, user=User} = State) ->
    Res = jsx:encode([{<<"adapters">>,
            [ [{<<"id">>,list_to_binary(atom_to_list(A#ddAdapter.id))}
              ,{<<"fullName">>,list_to_binary(A#ddAdapter.fullName)}]
            || A <- dderl_dal:get_adapters()]}]),
    lager:debug([{key, Key}, {user, User}], "adapters " ++ jsx:prettify(Res)),
    {reply, binary_to_list(Res), State};

process_call({"connects", _ReqData}, _From, #state{user=User, key=Key} = State) ->
    case dderl_dal:get_connects(User) of
        []          ->
            %{reply, "{\"connects\":[]}", State};
            {reply, binary_to_list(jsx:encode([{<<"connects">>,[]}])), State};
        Connections ->
            lager:debug([{session, Key}, {user, User}], "conections ~p", [Connections]),
            Res = jsx:encode([{<<"connects">>,
                lists:foldl(fun(C, Acc) ->
                    [{jsq(C#ddConn.name), [
                            {<<"adapter">>,jsq(C#ddConn.adapter)}
                          , {<<"service">>, jsq(C#ddConn.schema)}
                          , {<<"owner">>, jsq(C#ddConn.owner)}] ++
                          [{list_to_binary(atom_to_list(N)), jsq(V)} || {N,V} <- C#ddConn.access]
                     } | Acc]
                end,
                [],
                Connections)
            }]),
            lager:debug([{key, Key}, {user, User}], "adapters " ++ jsx:prettify(Res)),
            {reply, binary_to_list(Res), State#state{user=User}}
    end;
process_call({Cmd, ReqData}, Parent, #state{session=SessionHandle,adapter=AdaptMod, key=Key, user=User} = State) ->
    BodyJson = jsx:decode(wrq:req_body(ReqData)),
    Self = self(),
    spawn(fun() ->
            lager:debug([{session, Key}, {user, User}], "~p processing ~p", [AdaptMod, {Cmd,BodyJson}]),
            {NewSessionHandle, Resp} =
                AdaptMod:process_cmd({Cmd, BodyJson}, SessionHandle),
            lager:debug([{session, Key}, {user, User}], "~p response ~p", [AdaptMod, {Cmd,Resp}]),
            gen_server:cast(Self, {resp, {NewSessionHandle, Resp, Parent}})
    end),
    {noreply, State}.
% - process_call({"save_file", ReqData}, _From, #state{key=Key,user=User,file=File} = State) ->
% -     {struct, [{<<"save">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
% -     FileName     = binary_to_list(proplists:get_value(<<"file_name">>, BodyJson, <<>>)),
% -     FileContent  = binary_to_list(proplists:get_value(<<"file_content">>, BodyJson, <<>>)),
% -     logi(File, "[~p] save file ~p~n", [Key, {FileName, FileContent}]),
% -     Files = case retrieve(files, User) of
% -         {ok, undefined} -> [];
% -         {ok, Fs} -> Fs;
% -         {error, _} -> []
% -     end,
% -     NewFiles = lists:keystore(FileName, 2, Files, #file{name=FileName, content=FileContent, posX=0, posY=25, width=200, height=500}),
% -     case update_account(User, {files, NewFiles}) of
% -         ok ->
% -             logi(File, "[~p] files updated for user ~p~n", [Key, User]),
% -             {reply, "{\"save_file\": \"ok\"}", State};
% -         abort ->  {reply, "{\"save_file\": \"unable to save files\"}", State}
% -     end;
% - process_call({"del_file", ReqData}, _From, #state{key=Key,user=User,file=File} = State) ->
% -     {struct, [{<<"del">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
% -     FileName     = binary_to_list(proplists:get_value(<<"file_name">>, BodyJson, <<>>)),
% -     logi(File, "[~p] delete file ~p~n", [Key, {FileName}]),
% -     Files = case retrieve(files, User) of
% -         {ok, undefined} -> [];
% -         {ok, Fs} -> Fs;
% -         {error, _} -> []
% -     end,
% -     NewFiles = case lists:keytake(FileName, 2, Files) of
% -         {value, _, NFs} -> NFs;
% -         false -> Files
% -     end,
% -     case update_account(User, {files, NewFiles}) of
% -         ok ->
% -             logi(File, "[~p] files updated for user ~p~n", [Key, User]),
% -             {reply, "{\"delete_file\": \"ok\"}", State};
% -         abort ->  {reply, "{\"delete_file\": \"unable to delete files\"}", State}
% -     end;
% - process_call({"logs", _ReqData}, _From, #state{user=User,logdir=Dir} = State) ->
% -     Files = filelib:fold_files(Dir, User ++ "_.*\.log", false, fun(F, A) -> [filename:join(["logs", filename:basename(F)])|A] end, []),
% -     {reply, "{\"logs\": "++gen_adapter:string_list_to_json(Files, [])++"}", State};
% - process_call({"delete_log", ReqData}, _From, #state{key=Key,logdir=Dir} = State) ->
% -     {struct, [{<<"log">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
% -     LogFileName = filename:basename(binary_to_list(proplists:get_value(<<"file">>, BodyJson, <<>>))),
% -     LogFilePath = filename:join([Dir,LogFileName]),
% -     %logi(File, "[~p] deleting log file ~p ~n", [Key, LogFilePath]),
% -     case file:delete(LogFilePath) of
% -         ok ->
% -             {reply, "{\"delete_log\": \"ok\"}", State};
% -         {error, Reason} ->
% -             %logi(File, "[~p] deleting log file ~p failed for ~p~n", [Key, LogFilePath, Reason]),
% -             {reply, "{\"delete_log\": \"failed! see log for details\"}", State}
% -     end;
% - process_call({"login_change_pswd", ReqData}, _From, #state{key=Key,file=File} = State) ->
% -     {struct, [{<<"change_pswd">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
% -     User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
% -     Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
% -     logi(File, "[~p] change password ~p~n", [Key, {User, Password}]),
% -     case update_account(User, {pswd_md5, Password}) of
% -         {atomic, ok} ->  {reply, "{\"change_pswd\": \"ok\"}", State};
% -         abort ->  {reply, "{\"change_pswd\": \"unable to change password\"}", State}
% -     end;

handle_cast({resp, {NewSessionHandle, Resp, Parent}}, #state{key=Key,user=User}=State) ->
    lager:debug([{session, Key}, {user, User}], "~p received response ~p for ~p", [?MODULE, Resp, Parent]),
    gen_server:reply(Parent, Resp),
    {noreply, State#state{session=NewSessionHandle}};
handle_cast(Request, #state{key=Key,user=User}=State) ->
    lager:error([{session, Key}, {user, User}], "~p received unknown cast ~p for ~p", [?MODULE, Request, User]),
    {noreply, State}.

handle_info(die, State) -> {stop, timeout, State};
handle_info(Info, #state{key=Key,user=User}=State) ->
    lager:error([{session, Key}, {user, User}], "~p received unknown msg ~p for ~p", [?MODULE, Info, User]),
    {noreply, State}.

terminate(Reason, #state{key=Key, user=User}) ->
    lager:info([{session, Key}, {user, User}], "~p terminating session ~p", [?MODULE, Reason, {Key, User}]),
    ets:delete(dderl_req_sessions, Key).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jsq(Bin) when is_binary(Bin) -> Bin;
jsq(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
jsq(Str)                     -> list_to_binary(Str).
