-module(dderl_session).

-behavior(gen_server).

-include("dderl.hrl").

-export([start/0
        , process_request/3
        , set_adapter/2
        , get_state/1
        , log/3
        , string_list_to_json/2
        , convert_rows_to_json/1
        , convert_row_to_string/1
        , convert_rows_to_string/1
%        , create_files_json/1
        ]).

-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
%        , update_account/2
        ]).

-define(SESSION_IDLE_TIMEOUT, 3600000). % 1 hour
-define(GENLOG, "global.log").

-record(state, {
        key
        , session
        , statements = []
        , tref
        , user = <<>>
        , file
        , logdir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "www", "logs"])
        , adapter = gen_adapter
        , resps = []
    }).

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Key = erlang:phash2({dderl_session, Pid}),
    {Key, {dderl_session, Pid}}.

log(Pid, Format, Content) ->
    gen_server:cast(Pid, {log, Format, Content}).

get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

process_request(SessKey, WReq, {?MODULE, Pid}) ->
    Parent = self(),
    Type = wrq:disp_path(WReq),
    case gen_server:call(Pid, {SessKey, Type, WReq, Parent}, infinity) of
        deferred -> get_resp(Pid, Parent);
        Resp -> Resp
    end.

get_resp(Pid, Parent) ->
    case gen_server:call(Pid, {get_resp, Parent}, infinity) of
        undefined ->
            timer:sleep(10),
            get_resp(Pid, Parent);
        Resp -> Resp
    end.

set_adapter(Adapter, {?MODULE, Pid}) ->
    AdaptMod  = list_to_existing_atom(Adapter++"_adapter"),
    gen_server:call(Pid, {adapter, AdaptMod}, infinity).

init(_Args) ->
    logi(?GENLOG, "dderl_session ~p started...~n", [self()]),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {ok, #state{key=erlang:phash2({dderl_session, self()}),tref=TRef}}.

handle_call({adapter, Adapter}, _From, State) ->
    Adapter:init(),
    {reply, ok, State#state{adapter=Adapter}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({get_resp, Parent}, _From, #state{resps=Responces} = State) ->
    {NewResponces, Resp} = case lists:keytake(Parent, 1, Responces) of
        {value, {_, R}, NRs} -> {NRs, R};
        false -> {Responces, undefined}
    end,
    {reply, Resp, State#state{resps=NewResponces}};
handle_call({SessKey, Typ, WReq, Parent}, _From, #state{tref=TRef, key=Key, file=File} = State) ->
    timer:cancel(TRef),
    NewKey = if SessKey =/= Key -> SessKey; true -> Key end,
    logi(File, "[~p] process_request ~p~n", [NewKey, Typ]),
    {Rep, Resp, NewState} = process_call({Typ, WReq}, Parent, State#state{key=NewKey}),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {Rep, Resp, NewState#state{tref=NewTRef,key=NewKey}}.

process_call({"login", ReqData}, _From, #state{key=Key, logdir=Dir} = State) ->
    {struct, [{<<"login">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    {Y,M,D} = date(),
    File     = filename:join([Dir, binary_to_list(User) ++ "_" ++ io_lib:format("~p~p~p", [Y,M,D]) ++ ".log"]),
    logi(?GENLOG, "Logfile ~p~n", [File]),
    logi(File, "[~p] login ~p~n", [Key, {User, Password}]),
    case dderl_dal:verify_password(User, Password) of
        true ->  
            {reply, "{\"login\": \"ok\", \"session\":" ++ integer_to_list(Key) ++ "}", State#state{user=User,file=File}};
        {error, Msg} ->
            {reply, "{\"login\": \""++Msg++"\"}", State}
    end;
process_call({"files", _}, _From, #state{user=User} = State) ->
    CommandFiles = dderl_dal:get_commands(User, imem),
    Files = create_files_json(CommandFiles),
    {reply, "{\"files\": ["++Files++"]}", State};

% - process_call({"files", _}, _From, #state{adapter=AdaptMod, user=User} = State) ->
% -     CmnFs = case imem_if:read(common, AdaptMod) of
% -         [] -> [];
% -         [{_,_,CFs}|_] -> CFs
% -     end,
% -     Files = create_files_json(
% -         case retrieve(files, User) of
% -             {ok, undefined} -> CmnFs;
% -             {ok, Fs} -> Fs ++ CmnFs;
% -             {error, _} -> []
% -         end),
% -     {reply, "{\"files\": ["++Files++"]}", State};
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
process_call({"logs", _ReqData}, _From, #state{user=User,logdir=Dir} = State) ->
    Files = filelib:fold_files(Dir, User ++ "_.*\.log", false, fun(F, A) -> [filename:join(["logs", filename:basename(F)])|A] end, []),
    {reply, "{\"logs\": "++string_list_to_json(Files, [])++"}", State};
process_call({"delete_log", ReqData}, _From, #state{key=Key,logdir=Dir,file=File} = State) ->
    {struct, [{<<"log">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    LogFileName = filename:basename(binary_to_list(proplists:get_value(<<"file">>, BodyJson, <<>>))),
    LogFilePath = filename:join([Dir,LogFileName]),
    logi(File, "[~p] deleting log file ~p ~n", [Key, LogFilePath]),
    case file:delete(LogFilePath) of
        ok ->
            {reply, "{\"delete_log\": \"ok\"}", State};
        {error, Reason} ->
            logi(File, "[~p] deleting log file ~p failed for ~p~n", [Key, LogFilePath, Reason]),
            {reply, "{\"delete_log\": \"failed! see log for details\"}", State}
    end;
% - process_call({"login_change_pswd", ReqData}, _From, #state{key=Key,file=File} = State) ->
% -     {struct, [{<<"change_pswd">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
% -     User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
% -     Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
% -     logi(File, "[~p] change password ~p~n", [Key, {User, Password}]),
% -     case update_account(User, {pswd_md5, Password}) of
% -         {atomic, ok} ->  {reply, "{\"change_pswd\": \"ok\"}", State};
% -         abort ->  {reply, "{\"change_pswd\": \"unable to change password\"}", State}
% -     end;
process_call({"save", ReqData}, _From, #state{key=_Key, user=User,file=_File} = State) ->
    case User of
        [] -> {reply, "{\"save\": \"not logged in\"}", State};
        _ ->
            Data = binary_to_list(wrq:req_body(ReqData)),
            {struct, SaveCon} = mochijson:decode(Data),

            Con = #ddConn { id       = erlang:phash2(make_ref())
                          , name     = proplists:get_value("name", SaveCon, "")
                          , owner    = User
                          , adapter  = list_to_existing_atom(proplists:get_value("adapter", SaveCon, ""))
                          , access   = [ {ip,        proplists:get_value("ip", SaveCon, "")}
                                       , {port,      proplists:get_value("port", SaveCon, "0")}
                                       , {type,      proplists:get_value("type", SaveCon, "")}
                                       , {user,      proplists:get_value("user", SaveCon, "")}
                                       , {password,  proplists:get_value("password", SaveCon, "")}
                                       , {tnsstring, proplists:get_value("tnsstring", SaveCon, "")}
                                       ]
                          , schema   = proplists:get_value("service", SaveCon, "")
                          },
            dderl_dal:add_connect(Con),
            {reply, "{\"save\": \"success\"}", State}
    end;
process_call({"get_connects", _ReqData}, _From, #state{user=User} = State) ->
    case dderl_dal:get_connects(User) of
        []          -> {reply, "{}", State};
        Connections -> {reply, conns_json(Connections), State#state{user=User}}
    end;
process_call({Cmd, ReqData}, Parent, #state{session=SessionHandle,adapter=AdaptMod,resps=Responces} = State) ->
    BodyJson = case mochijson2:decode(wrq:req_body(ReqData)) of
        {struct, [{_, {struct, B}}]} ->  B;
        _ -> []
    end,
    Self=self(),
    spawn(fun() ->
            {NewSessionHandle, Resp} = AdaptMod:process_cmd({Cmd, BodyJson}, Self, SessionHandle),
            gen_server:cast(Self, {resp, {NewSessionHandle, Resp, Parent}})
    end),
    {reply, deferred, State#state{resps=lists:keystore(Parent, 1, Responces, {Parent, undefined})}}.

handle_cast({log, Format, Content}, #state{key=Key, file=File} = State) ->
    logi(File, "[~p] " ++ Format, [Key|Content]),
    {noreply, State};
handle_cast({resp, {NewSessionHandle, Resp, Parent}}, #state{resps=Responces}=State) ->
    NewResponces = lists:keystore(Parent, 1, Responces, {Parent, Resp}),
    {noreply, State#state{session=NewSessionHandle, resps=NewResponces}};
handle_cast(_Request, State) -> {noreply, State}.

handle_info(die, State) -> {stop, timeout, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{key=Key,file=File}) ->
    logi(File, "[~p] ~p terminating for ~p~n", [Key, self(), Reason]),
    ets:delete(dderl_req_sessions, Key).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conns_json(Connections) ->
    ConsJson = "{" ++
    string:join(lists:foldl(fun(C, Acc) ->
        Access = lists:flatten([", \""++atom_to_list(N)++"\":"++jsq(V) || {N,V} <- C#ddConn.access]),
        [lists:flatten(jsq(C#ddConn.name)++":{"
            ++"\"adapter\":\""++jsq(C#ddConn.adapter)++"\""
            ++", \"service\":"++jsq(C#ddConn.schema)
            ++Access
            ++"}") | Acc]
    end,
    [],
    Connections), ",") ++ "}",
    ConsJson.

create_files_json(Files)    -> string:join(lists:reverse(create_files_json(Files, [])), ",").
create_files_json([], Json) -> Json;
create_files_json([F|Files], Json) ->
    create_files_json(Files, [
            "{\"name\":"++jsq(F#ddCmd.name)
         ++", \"content\":"++jsq(F#ddCmd.command)
         ++", \"posX\":0"
         ++", \"posY\":25"
         ++", \"width\":200"
         ++", \"height\":500"
%% -         ++", \"posX\":"++integer_to_list(F#ddCmd.posX)
%% -         ++", \"posY\":"++integer_to_list(F#ddCmd.posY)
%% -         ++", \"width\":"++integer_to_list(F#ddCmd.width)
%% -         ++", \"height\":"++integer_to_list(F#ddCmd.height)
         ++"}"
         |Json]).

jsq(Str) -> io_lib:format("~p", [Str]).

logi(Filename, Format, Content) ->
    FormatWithDate = "[" ++ io_lib:format("~p ~p", [date(), time()]) ++ "] " ++ Format,
    case Filename of
        undefined -> file:write_file(?GENLOG, list_to_binary(io_lib:format(FormatWithDate, Content)), [append]);
        _         -> file:write_file(Filename, list_to_binary(io_lib:format(FormatWithDate, Content)), [append])
    end.

string_list_to_json(Strings) ->
    NewStrings =
        lists:foldl(fun
           (S, Acc) when is_atom(S)  -> [atom_to_list(S)|Acc];
           (S, Acc) when is_tuple(S) -> [lists:nth(1, io_lib:format("~p", S))|Acc];
           (S, Acc)                  -> [S|Acc]
           
        end,
        [],
        Strings),
    string_list_to_json(NewStrings, "").
string_list_to_json([], []) -> "[]";
string_list_to_json([], Json) -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
string_list_to_json([S|Strings], Json) ->
    string_list_to_json(Strings, Json ++ "\"" ++ lists:flatten([if X > 127 -> "&#" ++ integer_to_list(X) ++ ";";
                                                                   (X == 10) or (X == 13) -> "";
                                                                   (X == $") -> "";
                                                                   true -> X
                                                               end || X <- S]) ++ "\",").

convert_rows_to_json(Rows) -> convert_rows_to_json(Rows, "").
convert_rows_to_json([], Json) when length(Json) > 0 -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
convert_rows_to_json([], _)                          -> "[]";
convert_rows_to_json([Row|Rows], Json)               -> convert_rows_to_json(Rows, Json ++ string_list_to_json(lists:reverse(Row)) ++ ",").


convert_row_to_string([]) -> [];
convert_row_to_string(Row) -> [lists:flatten(io_lib:format("~p", [R])) || R <- Row].

convert_rows_to_string([]) -> [];
convert_rows_to_string(Rows) -> [lists:reverse([lists:flatten(io_lib:format("~p", [R])) || R <- Row]) || Row <- Rows].
