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
        ]).

-export([init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
        , update_account/2]).

-define(SESSION_IDLE_TIMEOUT, 3600000). % 1 hour
-define(GENLOG, "global.log").

-record(state, {
        key
        , session
        , statements = []
        , tref
        , user = []
        , file
        , logdir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "www", "logs"])
        , adapter = gen_adapter
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
    Type = wrq:disp_path(WReq),
    gen_server:call(Pid, {SessKey, Type, WReq}, infinity).

set_adapter(Adapter, {?MODULE, Pid}) ->
    AdaptMod  = list_to_existing_atom(Adapter++"_adapter"),
    gen_server:call(Pid, {adapter, AdaptMod}, infinity).

update_account(User, {pswd, Password}) ->
    Pswd = lists:flatten(io_lib:format(lists:flatten(array:to_list(array:new([{size,16},{default,"~2.16.0b"}])))
                                     , binary_to_list(erlang:md5(Password)))),
    update_account(User, {pswd_md5, Pswd});
update_account(User, {pswd_md5, Pswd}) ->
    Acnt = case imem_if:read(accounts, User) of
            [] -> #accounts{user=User, password=Pswd};
            [Account|_] -> Account#accounts{password=Pswd}
    end,
    imem_if:insert_into_table(accounts, Acnt);
update_account(User, {cons, Connections}) ->
    case imem_if:read(accounts, User) of
        [] -> {error, "User does not exists"};
        [Account|_] -> imem_if:insert_into_table(accounts, Account#accounts{db_connections = Connections})
    end.

retrieve(cons, User) ->
    case imem_if:read(accounts, User) of
        [] -> {error, "User does not exists"};
        [#accounts{db_connections=Connections}|_] -> {ok, Connections};
        {aborted, Reason} ->
            logi(?GENLOG, "mnesia:read failed ~p~n", [Reason]),
            {error, Reason}
    end;
retrieve(pswd, User) ->
    case imem_if:read(accounts, User) of
        [] -> {error, "User does not exists"};
        [#accounts{password=Password}|_] -> {ok, Password};
        {aborted, Reason} ->
            logi(?GENLOG, "mnesia:read failed ~p~n", [Reason]),
            {error, Reason}
    end;
retrieve(files, User) ->
    case imem_if:read(accounts, User) of
        [] -> {error, "User does not exists"};
        [#accounts{db_files=Files}|_] -> {ok, Files};
        {aborted, Reason} ->
            logi(?GENLOG, "mnesia:read failed ~p~n", [Reason]),
            {error, Reason}
    end.

init(_Args) ->
    logi(?GENLOG, "dderl_session ~p started...~n", [self()]),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {ok, #state{key=erlang:phash2({dderl_session, self()}),tref=TRef}}.

handle_call({adapter, Adapter}, _From, State) ->
    Adapter:init(),
    {reply, ok, State#state{adapter=Adapter}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({SessKey, Typ, WReq}, From, #state{tref=TRef, key=Key, file=File} = State) ->
    timer:cancel(TRef),
    NewKey = if SessKey =/= Key -> SessKey; true -> Key end,
    logi(File, "[~p] process_request ~p~n", [NewKey, Typ]),
    {Rep, Resp, NewState} = process_call({Typ, WReq}, From, State#state{key=NewKey}),
    {ok, NewTRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {Rep, Resp, NewState#state{tref=NewTRef,key=NewKey}}.

process_call({"login", ReqData}, _From, #state{key=Key, logdir=Dir} = State) ->
    {struct, [{<<"login">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    {Y,M,D} = date(),
    File     = filename:join([Dir, User ++ "_" ++ io_lib:format("~p~p~p", [Y,M,D]) ++ ".log"]),
    logi(?GENLOG, "Logfile ~p~n", [File]),
    logi(File, "[~p] login ~p~n", [Key, {User, Password}]),
    case retrieve(pswd, User) of
        {ok, Password} ->  
            {reply, "{\"login\": \"ok\", \"session\":" ++ integer_to_list(Key) ++ "}", State#state{user=User,file=File}};
        {ok, DifferentPassword} ->
            logi(File, "[~p] Password missmatch Got ~p Has ~p~n", [Key, Password, DifferentPassword]),
            {reply, "{\"login\": \"invalid password\"}", State};
        {error, Reason} ->
            {reply, "{\"login\": \"invalid user -- " ++ Reason ++ "\"}", State}
    end;
process_call({"files", _}, _From, #state{adapter=AdaptMod, user=User} = State) ->
    [{_,_,CmnFs}|_] = imem_if:read(common, AdaptMod),
    Files = create_files_json(
        case retrieve(files, User) of
            {ok, undefined} -> CmnFs;
            {ok, Fs} -> Fs ++ CmnFs;
            {error, _} -> []
        end),
    {reply, "{\"files\": ["++Files++"]}", State};
process_call({"logs", _ReqData}, _From, #state{user=User,logdir=Dir} = State) ->
    Files = filelib:fold_files(Dir, User ++ "_.*\.log", false, fun(F, A) -> [filename:join(["logs", filename:basename(F)])|A] end, []),
    {reply, "{\"logs\": "++string_list_to_json(Files, [])++"}", State};
process_call({"login_change_pswd", ReqData}, _From, #state{key=Key,file=File} = State) ->
    {struct, [{<<"change_pswd">>, {struct, BodyJson}}]} = mochijson2:decode(wrq:req_body(ReqData)),
    User     = binary_to_list(proplists:get_value(<<"user">>, BodyJson, <<>>)),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    logi(File, "[~p] change password ~p~n", [Key, {User, Password}]),
    case update_account(User, {pswd_md5, Password}) of
        {atomic, ok} ->  {reply, "{\"change_pswd\": \"ok\"}", State};
        abort ->  {reply, "{\"change_pswd\": \"unable to change password\"}", State}
    end;
process_call({"save", ReqData}, _From, #state{key=Key, user=User,file=File} = State) ->
    case User of
        [] -> {reply, "{\"save\": \"not logged in\"}", State};
        _ ->
            Data = binary_to_list(wrq:req_body(ReqData)),
            logi(File, "Saving... ~p~n", [Data]),
            case update_account(User, {cons, Data}) of
                ok ->
                    logi(File, "[~p] config updated for user ~p~n", [Key, User]),
                    {reply, "{\"save\": \"ok\"}", State};
                abort ->  {reply, "{\"save\": \"unable to save config\"}", State}
            end
    end;
process_call({"get_connects", _ReqData}, _From, #state{user=User} = State) ->
    case retrieve(cons, User) of
        {ok, []} -> {reply, "{}", State};
        {ok, undefined} ->  {reply, "{}", State};
        {ok, Connections} -> {reply, Connections, State#state{user=User}};
        {error, Reason} -> {reply, "{\"get_connects\": \"invalid user -- " ++ Reason ++ "\"}", State}
    end;
process_call({Cmd, ReqData}, _From, #state{session=SessionHandle, adapter=AdaptMod} = State) ->
    BodyJson = case mochijson2:decode(wrq:req_body(ReqData)) of
        {struct, [{_, {struct, B}}]} ->  B;
        _ -> []
    end,
    {NewSessionHandle, Resp} = AdaptMod:process_cmd({Cmd, BodyJson}, self(), SessionHandle),
    {reply, Resp, State#state{session=NewSessionHandle}}.

create_files_json(Files)    -> string:join(lists:reverse(create_files_json(Files, [])), ",").
create_files_json([], Json) -> Json;
create_files_json([{FName, Content}|Files], Json) ->
    create_files_json(Files, ["{\"name\":\""++FName++"\", \"content\":\""++Content++"\"}"|Json]).

handle_cast({log, Format, Content}, #state{key=Key, file=File} = State) ->
    logi(File, "[~p] " ++ Format, [Key|Content]),
    {noreply, State};
handle_cast(_Request, State) -> {noreply, State}.

handle_info(die, State) -> {stop, timeout, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{key=Key,file=File}) ->
    logi(File, "[~p] ~p terminating for ~p~n", [Key, self(), Reason]),
    ets:delete(dderl_req_sessions, Key).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logi(Filename, Format, Content) ->
    FormatWithDate = "[" ++ io_lib:format("~p ~p", [date(), time()]) ++ "] " ++ Format,
    case Filename of
        undefined -> file:write_file(?GENLOG, list_to_binary(io_lib:format(FormatWithDate, Content)), [append]);
        _         -> file:write_file(Filename, list_to_binary(io_lib:format(FormatWithDate, Content)), [append])
    end.

string_list_to_json([], []) -> "[]";
string_list_to_json([], Json) -> "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
string_list_to_json([S|Strings], Json) ->
    string_list_to_json(Strings, Json ++ "\"" ++ lists:flatten([if X > 127 -> "&#" ++ integer_to_list(X) ++ ";";
                                                                   (X == 10) or (X == 13) -> "";
                                                                   (X == $") -> "";
                                                                   true -> X
                                                               end || X <- S]) ++ "\",").

convert_rows_to_json(Rows) -> convert_rows_to_json(Rows, "").
convert_rows_to_json([], Json) ->
    if length(Json) > 0 ->
        "[" ++ string:substr(Json,1,length(Json)-1) ++ "]";
        true -> "[]"
    end;
convert_rows_to_json([Row|Rows], Json) ->
    convert_rows_to_json(Rows, Json ++ string_list_to_json(lists:reverse(Row), []) ++ ",").


convert_row_to_string([]) -> [];
convert_row_to_string(Row) -> [lists:flatten(io_lib:format("~p", [R])) || R <- Row].

convert_rows_to_string([]) -> [];
convert_rows_to_string(Rows) -> [lists:reverse([lists:flatten(io_lib:format("~p", [R])) || R <- Row]) || Row <- Rows].
