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

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Key = erlang:phash2({dderl_session, Pid}),
    {Key, {dderl_session, Pid}}.

log(Pid, Format, Content) ->
    gen_server:call(Pid, {log, Format, Content}).

get_state({?MODULE, Pid}) ->
    gen_server:call(Pid, get_state, infinity).

process_request(SessKey, WReq, {?MODULE, Pid}) ->
    Type = wrq:disp_path(WReq),
    gen_server:call(Pid, {SessKey, Type, WReq}, infinity).

set_adapter(Adapter, {?MODULE, Pid}) ->
    AdaptMod  = list_to_atom(Adapter++"_adapter"),
    gen_server:call(Pid, {adapter, AdaptMod}, infinity).

-record(state, {
        key
        , session
        , statements = []
        , tref
        , user = []
        , file
        , logdir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "www", "logs"])
        , adapter
    }).

update_account(User, {pswd, Password}) ->
    Pswd = lists:flatten(io_lib:format(lists:flatten(array:to_list(array:new([{size,16},{default,"~.16b"}])))
                                     , binary_to_list(erlang:md5(Password)))),
    Acnt = case mnesia:transaction(fun() -> mnesia:read({accounts, User}) end) of
            {atomic, []} -> #accounts{user=User, password = Pswd};
            {atomic, [Account|_]} -> Account#accounts{password = Pswd}
    end,
    mnesia:transaction(fun() -> mnesia:write(Acnt) end);
update_account(User, {pswd_md5, Password}) ->
    Acnt = case mnesia:transaction(fun() -> mnesia:read({accounts, User}) end) of
            {atomic, []} -> #accounts{user=User, password = Password};
            {atomic, [Account|_]} -> Account#accounts{password = Password}
    end,
    mnesia:transaction(fun() -> mnesia:write(Acnt) end);
update_account(User, {cons, Connections}) ->
    {atomic, [Account|_]} = mnesia:transaction(fun() -> mnesia:read({accounts, User}) end),
    mnesia:transaction(fun() ->
                        mnesia:write(
                            Account#accounts{db_connections = Connections})
                       end).

retrieve(cons, User) ->
    case mnesia:transaction(fun() -> mnesia:read({accounts, User}) end) of
        {atomic, []} -> {error, "User does not exists"};
        {atomic, [#accounts{db_connections=Connections}|_]} -> {ok, Connections};
        {aborted, Reason} ->
            logi(?GENLOG, "mnesia:read failed ~p~n", [Reason]),
            {error, Reason}
    end;
retrieve(pswd, User) ->
    case mnesia:transaction(fun() -> mnesia:read({accounts, User}) end) of
        {atomic, []} -> {error, "User does not exists"};
        {atomic, [#accounts{password=Password}|_]} ->
            {ok, Password};
        {aborted, Reason} ->
            logi(?GENLOG, "mnesia:read failed ~p~n", [Reason]),
            {error, Reason}
    end.

init(_Args) ->
    logi(?GENLOG, "dderl_session ~p started...~n", [self()]),
    {ok, TRef} = timer:send_after(?SESSION_IDLE_TIMEOUT, die),
    {ok, #state{key=erlang:phash2({dderl_session, self()}),tref=TRef}}.

handle_call({adapter, Adapter}, _From, State) ->
    {reply, ok, State#state{adapter=Adapter}};
handle_call({log, Format, Content}, _From, #state{key=Key, file=File} = State) ->
    logi(File, "[~p] " ++ Format, [Key|Content]),
    {reply, ok, State};
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
                {atomic, ok} ->
                    logi(File, "[~p] config updated for user ~p~n", [Key, User]),
                    {reply, "{\"save\": \"ok\"}", State};
                abort ->  {reply, "{\"save\": \"unable to save config\"}", State}
            end
    end;
process_call({"get_connects", _ReqData}, _From, #state{user=User} = State) ->
    case retrieve(cons, User) of
        {ok, []} -> {reply, "[]", State};
        {ok, undefined} ->  {reply, "[]", State};
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


convert_rows_to_string([]) -> [];
convert_rows_to_string(Rows) -> [lists:reverse([lists:flatten(io_lib:format("~p", [R])) || R <- Row]) || Row <- Rows].

%
% TEST CASES %
%

-include_lib("eunit/include/eunit.hrl").

-define (TEST_SQLS, [
"
select 
	*
from 
	abc 
where 
	eva	a=b
"
,
"
select 
	*
from 
	abc
where 
	eva	a=b 
	and	c=d
"
,
"
select 
	*
from 
	abc
where	
	eva	a=b 
	and	c=d 
	and	e=f
	and	g=h
"
,
"
select 
	*
from 
	abc
where
	not	a=b 
	and	c=d 
	and	e=f
	and	g=h
"  
,
"
select 
	*
from
	abc
where
	eva	a=b 
	and	
		not	c=d 
	and	e=f
	and	g=h
"  
, 
"
select 
	*
from 
	abc
where
	eva	a=b
	and	c=d 
	and	e=f
	and	
		not	g=h
"
,
"
select 
	*
from 
	abc
where
	eva	
		eva	a=b 
		and	c=d 
		and	e=f
	or	g=h
"
,
"
select
	*
from
	abc
where
	eva	eva	a=b 
		and	c=d 
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	
		not	a=b 
		and	c=d 
	or	e=f
	or	g=h
"
,
"
select
	*
from
	abc
where
        eva	
        	eva	a=b 
        	and	
        		not	c=d 
	or	e=f
	or	g=h
"
,
"
select
	*
from
	abc
where
	eva	
		not	a=b 
		and	
			not	c=d 
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	
		eva	a=b 
		and	c=d 
	or
		not	e=f
	or
		not	g=h
"  
,
"
select
	*
from
	abc
where
	eva	a=b 
	or	c=d 
	or
		not	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	not	(
			eva	a=b 
			and	c=d
		)
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	
		not a=b 
		and c=d
	or	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva
		eva	(
				eva	a=b 
				or	c=d
        	  	)
		and	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	(	
			eva	a=b 
			or	c=d
		) 
	and	e=f
	and	g=h
"
,
"
select
	*
from
	abc
where
	eva	a=b 
	or 
		eva	c=d 
		and
			not	e=f
	or	g=h
"  
,
"
select
	*
from
	abc
where
	eva	a=b 
	or
		eva	c=d 
		and	e=f 
		and	g=h
"
,
"
select
	*
from
	abc
where
	eva	a between b and c  
	and	d between e and f 
	and	g=h
"
,
"
select
	*
from
	abc
where
	eva	a between b and c 
	or 
		eva	d between e and f 
          	and	g=h
"
,
"
select
	*
from
	abc
where
	not	a between b and c 
	and	d between e and f 
	and	g=h
"
,
"
select
	*
from	abc
where
	eva
		eva	a between b and c
		and	d between e and f 
	or g=h
"
,
"
select
	*
from
	abc
where
	eva	(
			eva	a=b 
			or	c=d
		) 
	and 	(
			eva	e=f 
			or	g=h
		)
"
,
"
select
	*
from
	abc
where
	eva	a=b 
	or 
		eva	c=d 
		and 	(
				eva	e=f 
				or	g=h
			)
"
,
"
select
	/*+ index(t1 t1_abc) */
	*
from
	abc
where
	eva	a=b
"
,
"
select
	/*+ index(BDETAIL6 IDX_BD_UMSGGRPID) */
	NULL ROW_ID_S
	, BDETAIL6.ROWID ROW_ID_M
	, BD_UMSGGRPID MSGID
	, to_char(BD_DATESUBMIT,'DD.MM.YYYY HH24:MI:SS') SUBMITTIME
	, to_char(BD_DATEEXPIRE,'DD.MM.YYYY HH24:MI:SS') EXPIRETIME
	, to_char(BD_DATEDELIVERY,'DD.MM.YYYY HH24:MI:SS') RECTIME
	, BD_MSISDN_A SENDER
	, BD_MSISDN_B RECEIVER
	, BD_MSGSIZE MSGLEN
	, nvl(MMSCCRT_LANG01,BD_CDRRECTYPE) TYPE
	, nvl(MMSCCRT_VALUE1,BD_CDRRECTYPE) TYPE_TT1
	, nvl(MMSCCRT_VALUE2,BD_CDRRECTYPE) TYPE_TT2
	, decode(BD_MSGTYPE||BD_EVENTDISP,01,'Y',012,'Y','N') ISDELIV
	, nvl(MMSCET_LANG02,BD_EVENTDISP) EVENTDISP_STATCODE
	, nvl(MMSCMT_LANG02,BD_MSGTYPE) MSGTYPE_ERRCODE
	, nvl(MMSCET_VALUE2,BD_EVENTDISP) EVENTDISP_TT
	, nvl(MMSCMT_VALUE2,BD_MSGTYPE) MSGTYPE_TT
	, 'MMS' ROWTYPE
	, to_char(BD_DATETIME,'DD.MM.YYYY HH24:MI:SS') DATETIME
from
	BDETAIL6
	, MMSC_CDRRECTYPE
	, MMSC_EVENTDISPTYPE
	, MMSC_MSGTYPE
where	
	eva	BD_CDRRECTYPE=MMSCCRT_ID(+) 
	and	ltrim(to_char(BD_EVENTDISP))=MMSCET_ID(+)
	and	ltrim(to_char(BD_MSGTYPE))=MMSCMT_ID(+)
	and	BD_UMSGGRPID='mj78yk7r307fga5a01'
	and	BD_MSISDN_B='41796187332'
	and	BD_DATETIME>=to_date('19.06.12 11:15:09','DD.MM.YY HH24:MI:SS')-14
	and	BD_DATETIME<=to_date('19.06.12 11:15:09','DD.MM.YY HH24:MI:SS')+14
order by
	BD_DATETIME
	, nvl(BD_DATEDELIVERY,BD_DATETIME)
	, BD_MSGTYPE
"
,
"
select
	/*+ INDEX(ACCOUNT IDXU_AC_SHORT)*/
	AC_ID
	, AC_NAME
	, AC_ETID
	, AC_SHORT
	, AC_DEPTID
	, AC_LANGID
	, AC_LOGRET
	, nvl(AC_MAXLOG, SYS_MAXLOG) MAXLOG
	, AC_LASTLOGINTIME
	, AC_IPMASK
	, AC_REMOTEADDR
	, (sysdate-nvl(AC_LASTLOGINTIME,sysdate))*24*60-nvl(SYS_DELAY,3) TIME_DIFF
from 
	ACCOUNT
	, SYSPARAMETERS
where
	eva	AC_ESID='A'
	and	AC_SHORT='ADMIN'
"
]).

parse_test() -> test_parse(?TEST_SQLS).		% TODO: Replace "eva" by "" in ?TEST_SQLS
test_parse([]) -> ok;
test_parse([S|Sqls]) ->
    io:format(user, "===============================~nSql: ~p~n...............................~nParseTree:~n", [S]),
    {ok, Tokens, _} = sql_lex:string(S ++ ";"),
    case sql_parse:parse(Tokens) of
        {ok, [ParseTree|_]} -> io:format(user, "~p~n", [ParseTree]);
        Error -> io:format(user, "Failed ~p~nTokens~p~n", [Error, Tokens])
    end,
    test_parse(Sqls).

% TODO: render_test() must parse and render and return the original string ?TEST_SQLS
