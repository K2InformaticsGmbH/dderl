-module(dderl_fsm).
-behaviour(gen_fsm).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("dderl.hrl").
-include("gres.hrl").

%% TODO driver should translate for the same effect
-define(NoFilter,{undefined,[]}).   %% defn copied from imem_sql.hrl $$$

-define(RawMax,99999999).
-define(RawMin,0).
-define(IndMax,[]).     %% value bigger than any possible sort key {SortFun(Recs),Id}
-define(IndMin,{}).     %% value smaller than any possible sort key {SortFun(Recs),Id}
-define(NoSort,[]).     %% signalling no sort
-define(NoSortFunResult,{}).

-define(IndRec(__R,__SortFun),{{__SortFun(element(3,__R)),element(1,__R)},element(1,__R)}).
-define(IndKey(__R,__SortFun),{__SortFun(element(3,__R)),element(1,__R)}).
-define(NoKey,{}).      %% placeholder for unavailable key tuple within RowKey tuple

-define(TAIL_TIMEOUT, 10000). %% 10 Seconds.

%% --------------------------------------------------------------------
%% erlimem_fsm interface

-export([ start/1
        , start_link/1
        , stop/1
        ]).

-export([ rows/2        %% incoming rows          [RowList,true] | [RowList,false] | [RowList,tail]    RowList=list(KeyTuples)
        , rows_limit/3  %% limit of the rows that can be fetched in one request to the driver
        , gui_req/4     %% button <<"Button">> =  <<">">>
                        %%                        <<"|<">>
                        %%                        <<">>">>
                        %%                        <<">|">>
                        %%                        <<">|...">>
                        %%                        <<"<">>
                        %%                        <<"<<">>
                        %%                        <<"...">>
                        %%                        <<"tail">>
                        %%                        <<"close">>
                        %%                        <<"commit">>
                        %%                        <<"rollback">>
                        %%                        <<"restart">>
                        %% update ChangeList   =  [{Id,Op,[{Col1,"Value1"}..{ColN,"ValueN"}]}]
                        %% filter FilterSpec   =  {'and',[{Col1,["ValueA".."ValueN"]}, {Col2,["ValueX"]}]}
                        %% sort   GuiSortSpec  =  [{Col1,'asc'}..{ColN,'desc'}]
        , row_with_key/2
        , get_columns/1
        , get_query/1
        , get_histogram/2
        , refresh_session_ctx/2
        ]).

-record(ctx,    { %% session context
                  id
                , bl                  %% block length -> State
                , stmtCols            %% number of statement columns
                , rowFun              %% RowFun -> State
                , sortFun             %% SortFun -> State
                , sortSpec            %% SortSpec [{Ti1,Ci1'asc'}..{TiN,CiN,'desc'}]
                , replyToFun          %% reply fun
                , fetch_recs_async_fun
                , fetch_close_fun
                , stmt_close_fun
                , filter_and_sort_fun
                , update_cursor_prepare_fun
                , update_cursor_execute_fun
                , orig_qry
                }).

-record(state,  { %% fsm combined state
                  ctx                 %% statement & fetch context
                , tableId             %% ets raw buffer table id 
                , indexId             %% ets index table id 
                , bl                  %% block_length (passed .. init)
                , gl                  %% gui max length (row count) = gui_max(#state.bl)
                , stmtColsCount       %% number of statement columns
                , rowFun              %% RowFun
                , sortSpec            %% from imem statement, changed by gui events
                , sortFun             %% from imem statement, follows sortSpec (calculated by imem statement)
                , filterSpec = ?NoFilter  %% {FType,[ColF|ColFs]}  changed by gui events FType= and|or  ColF = [{Col,["value1".."valuen"]}]
                , filterFun           %% follows filterSpec

                , rawCnt = 0          %% buffer row count
                , rawTop = ?RawMax    %% id of top buffer row 
                , rawBot = 0          %% id of bottom buffer row
                , dirtyCnt = 0        %% count of dirty rows in buffer
                , dirtyTop = ?RawMax  %% record id of first dirty row in buffer
                , dirtyBot = 0        %% record id of last dirty row in buffer

                , indCnt = 0          %% count of indexed buffer entries (after filtering) 
                , indTop = ?IndMax    %% smallest index after filtering, initialized to big value
                , indBot = ?IndMin    %% biggest index after filtering, initialized to small value

                , bufCnt = 0          %% buffer row count           (either rawCnt or indCnt, depending on nav)
                , bufTop              %% id of top buffer row       (either rawTop or indTop, depending on nav)
                , bufBot              %% id of bottom buffer row    (either rawBot or indBot, depending on nav)

                , guiCnt = 0          %% count of scrollable entries in gui 
                , guiTop              %% top gui pointer (Id for raw / SortKey for ind)
                , guiBot              %% bottom gui pointer (Id for raw / SortKey for ind)
                , guiCol = false      %% index collision (stale view in gui)

                , nav = raw           %% navigation   raw | ind
                , srt = false         %% sort true | false
                , pfc=0               %% pending fetch count (in flight .. DB or back)
                , tailMode = false    %% tailMode scheduled
                , tailLock = false    %% tailMode locked
                , stack = undefined   %% command stack {button,Button,ReplyTo}
                , replyToFun          %% reply fun
                , sql = <<"">>        %% sql string
                , tRef = undefined    %% ref to the timer that triggers the timeout to when tailing and there is no data
                , colOrder = []       %% order of columns, list of column indices, [] = as defined in SQL
                }).

-define(block_size,10).
-define(MustCommit,<<"Please commit or rollback changes before clearing data">>).
-define(MustCommitSort,<<"Please commit or rollback changes before sorting or filtering data">>).
-define(UnknownCommand,<<"Unknown command">>).
-define(NoPendingUpdates,<<"No pending changes">>).


%% gen_fsm callbacks

-export([ empty/2
        , filling/2
        , autofilling/2
        , completed/2
        , tailing/2
        , aborted/2
        ]).

-export([ init/1
        , handle_event/3
        , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4
        ]).

-export([ filter_fun/1
        , filter_and/2
        , filter_or/2
        ]).

%% ====================================================================
%% External functions
%% ====================================================================

-spec start(#fsmctx{}) -> {atom(), pid()}.
start(#fsmctx{} = FsmCtx) ->
    Ctx = fsm_ctx(FsmCtx),
	{ok,Pid} = gen_fsm:start(?MODULE,Ctx,[]),
    {?MODULE,Pid}.

-spec start_link(#fsmctx{}) -> {atom(), pid()}.
start_link(#fsmctx{} = FsmCtx) ->
    Ctx = fsm_ctx(FsmCtx),
	{ok, Pid} = gen_fsm:start_link(?MODULE,Ctx,[]),
    {?MODULE,Pid}.

-spec fsm_ctx(#fsmctx{}) -> #ctx{}.
fsm_ctx(#fsmctx{ id                         = Id
               , stmtCols                   = StmtCols
               , rowFun                     = RowFun
               , sortFun                    = SortFun
               , sortSpec                   = SortSpec
               , block_length               = BL
               , fetch_recs_async_fun       = Fraf
               , fetch_close_fun            = Fcf
               , stmt_close_fun             = Scf
               , filter_and_sort_fun        = Fasf
               , update_cursor_prepare_fun  = Ucpf
               , update_cursor_execute_fun  = Ucef
               , orig_qry                   = Qry
               }) ->
    #ctx{ id                        = Id
        , bl                        = BL
        , stmtCols                  = StmtCols
        , rowFun                    = RowFun
        , sortFun                   = SortFun
        , sortSpec                  = SortSpec
        , fetch_recs_async_fun      = Fraf
        , fetch_close_fun           = Fcf
        , stmt_close_fun            = Scf
        , filter_and_sort_fun       = Fasf
        , update_cursor_prepare_fun = Ucpf
        , update_cursor_execute_fun = Ucef
        , orig_qry                  = Qry
        }.

-spec stop({atom(), pid()}) -> ok.
stop({?MODULE,Pid}) -> 
	gen_fsm:send_all_state_event(Pid,stop).

-spec refresh_session_ctx(#fsmctx{}, {atom(), pid()}) -> ok.
refresh_session_ctx(#fsmctx{} = FsmCtx, {?MODULE, Pid}) ->
    Ctx = fsm_ctx(FsmCtx),
    ?Info("Refreshing the session ctx"),
    gen_fsm:sync_send_all_state_event(Pid, {refresh_ctx, Ctx}).

-spec gui_req(atom(), term(), fun(), {atom(), pid()}) -> ok.
gui_req(button, <<"restart">>, ReplyTo, {?MODULE,Pid}) -> 
    ?Debug("button ~p", [<<"restart">>]),
    gen_fsm:send_event(Pid,{button, <<"restart">>, ReplyTo});
gui_req(button, <<">|">>, ReplyTo, {?MODULE,Pid}) -> 
    ?Debug("button ~p", [<<">|">>]),
    gen_fsm:send_event(Pid,{button, <<">|">>, ReplyTo});
gui_req(button, <<">|...">>, ReplyTo, {?MODULE,Pid}) -> 
    ?Debug("button ~p", [<<">|...">>]),
    gen_fsm:send_event(Pid,{button, <<">|...">>, ReplyTo});
gui_req(button, <<"...">>, ReplyTo, {?MODULE,Pid}) -> 
    ?Debug("button ~p", [<<"...">>]),
    gen_fsm:send_event(Pid,{button, <<"...">>, ReplyTo});
gui_req(button, <<"tail">>, ReplyTo, {?MODULE,Pid}) ->
    % ?Debug("button ~p", [<<"tail">>]),
    gen_fsm:send_event(Pid,{button, <<"tail">>, ReplyTo});
gui_req(CommandStr, Parameter, ReplyTo, {?MODULE,Pid}) when is_atom(CommandStr) ->
    ?Debug("~p ~p", [CommandStr,Parameter]),
    gen_fsm:send_all_state_event(Pid,{CommandStr, Parameter, ReplyTo}).

-spec row_with_key(integer(), {atom(), pid()}) -> tuple().
row_with_key(RowId, {?MODULE,Pid}) when is_integer(RowId) -> 
    % ?Debug("row_with_key ~p", [RowId]),
    gen_fsm:sync_send_all_state_event(Pid,{"row_with_key", RowId}).

%% the return tuple type #stmtcol{}. but is not imported
-spec get_columns({atom(), pid()}) -> [tuple()].
get_columns({?MODULE, Pid}) ->
    % ?Debug("get_columns...", []),
    gen_fsm:sync_send_all_state_event(Pid,{"get_columns"}).

-spec get_query({atom(), pid()}) -> binary().
get_query({?MODULE, Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid, get_query).

-spec get_histogram(pos_integer(), {atom(), pid()}) -> [tuple()].
get_histogram(ColumnId, {?MODULE, Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid, {histogram, ColumnId}).

-spec rows({_, _}, {atom(), pid()}) -> ok.
rows({error, _} = Error, {?MODULE, Pid}) ->
    gen_fsm:send_all_state_event(Pid, Error);
rows({Rows,Completed},{?MODULE,Pid}) -> 
    % ?Debug("rows ~p ~p", [length(Rows), Completed]),
    gen_fsm:send_event(Pid,{rows, {Rows,Completed}}).

-spec rows_limit(integer(), list(), {atom(), pid()}) -> ok.
rows_limit(NRows, Recs, {?MODULE, Pid}) ->
    gen_fsm:send_event(Pid, {rows_limit, {NRows, Recs}}).

-spec fetch(atom(), atom(), #state{}) -> #state{}.
fetch(FetchMode,TailMode, #state{ctx = #ctx{fetch_recs_async_fun = Fraf}}=State0) ->
    Opts = case {FetchMode,TailMode} of
        {none,none} ->    [];
        {FM,none} ->      [{fetch_mode,FM}];
        {FM,TM} ->        [{fetch_mode,FM},{tail_mode,TM}]
    end,
    case Fraf(Opts) of
        %% driver session maps to imem_sec:fetch_recs_async(SKey, Opts, Pid, Sock)
        %% driver session maps to imem_meta:fetch_recs_async(Opts, Pid, Sock)
        ok -> 
            % ?Debug("fetch(~p, ~p) ok", [FetchMode, TailMode]),
            State0#state{pfc=State0#state.pfc+1};
        {_, Error} -> 
            ?Error("fetch(~p, ~p) -> ~p", [FetchMode, TailMode, Error]),
            State0
    end.

-spec prefetch(atom(), #state{}) -> #state{}.
prefetch(filling,#state{pfc=0}=State) ->  fetch(none,none,State);
prefetch(filling,State) ->                State;
prefetch(_,State) ->                      State.

-spec fetch_close(#state{}) -> #state{}.
fetch_close(#state{ctx = #ctx{fetch_close_fun = Fcf}}=State) ->
    Result = Fcf(),
    ?Debug("fetch_close -- ~p", [Result]),
    State#state{pfc=0}.

-spec filter_and_sort([{atom() | integer(), term()}], [{integer() | binary(),boolean()}], list(), #state{}) -> {ok, list(), fun()}.
filter_and_sort(FilterSpec, SortSpec, Cols, #state{ctx = #ctx{filter_and_sort_fun = Fasf}}) ->
    case Fasf(FilterSpec, SortSpec, Cols) of
        %% driver session maps to imem_sec:filter_and_sort(SKey, Pid, FilterSpec, SortSpec, Cols)
        %% driver session maps to imem_meta:filter_and_sort(Pid, FilterSpec, SortSpec, Cols)
        {ok, NewSql, NewSortFun} ->
            ?Debug("filter_and_sort(~p, ~p, ~p) -> ~p", [FilterSpec, SortSpec, Cols, {ok, NewSql, NewSortFun}]),
            {ok, NewSql, NewSortFun};
        {_, Error} -> 
            ?Error("filter_and_sort(~p, ~p, ~p) -> ~p", [FilterSpec, SortSpec, Cols, Error]),
            {error, Error};
        Else ->
            ?Error("filter_and_sort(~p, ~p, ~p) -> ~p", [FilterSpec, SortSpec, Cols, Else]),
            {error, Else}            
    end.

-spec update_cursor_prepare(list(), #state{}) -> ok | {ok, term()} | {error, term()}.
update_cursor_prepare(ChangeList, #state{ctx = #ctx{update_cursor_prepare_fun = Ucpf}}) ->
    case Ucpf(ChangeList) of
        %% driver session maps to imem_sec:update_cursor_prepare()
        %% driver session maps to imem_meta:update_cursor_prepare()
        ok ->
            ?Debug("update_cursor_prepare(~p) -> ~p", [ChangeList, ok]),
            ok;
        {ok, UpdRef} ->
            ?Debug("update_cursor_prepare(~p) -> ~p", [ChangeList, {ok, UpdRef}]),
            {ok, UpdRef};
        {_, Error} ->
            ?Error("update_cursor_prepare(~p) -> ~p", [ChangeList, Error]),
            {error, Error}
    end.

-spec update_cursor_execute(atom(), #state{}, term()) -> list() | {error, term()}.
update_cursor_execute(Lock, #state{ctx = #ctx{update_cursor_execute_fun = Ucef}}, UpdRef) ->
    case Ucef(Lock, UpdRef) of
        %% driver session maps to imem_sec:update_cursor_execute()
        %% driver session maps to imem_meta:update_cursor_execute()
        {_, Error} ->
            ?Error("update_cursor_execute(~p) -> ~p", [Lock,Error]),
            {error, Error};
        ChangedKeys ->
            ?Debug("update_cursor_execute(~p) -> ~p", [Lock,ChangedKeys]),
            ChangedKeys
    end.

-spec update_cursor_execute(atom(), #state{}) -> list() | {error, term()}.
update_cursor_execute(Lock, #state{ctx = #ctx{update_cursor_execute_fun = Ucef}}) ->
    case Ucef(Lock) of
        %% driver session maps to imem_sec:update_cursor_execute()
        %% driver session maps to imem_meta:update_cursor_execute()
        {_, Error} ->
            ?Error("update_cursor_execute(~p) -> ~p", [Lock,Error]),
            {error, Error};
        ChangedKeys ->
            ?Debug("update_cursor_execute(~p) -> ~p", [Lock,ChangedKeys]),
            ChangedKeys
    end.

-spec navigation_type(fun(), {atom() | integer(), term()}) -> {raw | ind, boolean()}.
navigation_type(SortFun,FilterSpec) ->
    ?Info("The fun ~p, the filterspec ~p", [SortFun, FilterSpec]),
    case catch (SortFun(1)) of
        ?NoSortFunResult ->
            case FilterSpec of 
                ?NoFilter ->    {raw,false};
                _ ->            {ind,false}
            end;
        _ ->    
            {ind,true}
    end.

% buf_cnt(#state{nav=raw,rawCnt=RawCnt}) -> RawCnt;
% buf_cnt(#state{nav=ind,indCnt=IndCnt}) -> IndCnt.

% buf_top(#state{nav=raw,rawTop=RawTop}) -> RawTop;
% buf_top(#state{nav=ind,indTop=IndTop}) -> IndTop.

% buf_bot(#state{nav=raw,rawBot=RawBot}) -> RawBot;
% buf_bot(#state{nav=ind,indBot=IndBot}) -> IndBot.

-spec reset_buf_counters(#state{}) -> #state{}.
reset_buf_counters(#state{nav=Nav,tableId=TableId,indexId=IndexId}=State0) -> 
    RawCnt=ets:info(TableId,size),
    {RawTop,RawBot} = case RawCnt of
        0 ->    {?RawMax,?RawMin};
        _ ->    {ets:first(TableId),ets:last(TableId)}
    end,
    IndCnt = ets:info(IndexId,size),
    {IndTop,IndBot} = if 
        (Nav == ind) andalso (IndCnt > 0) ->
            {ets:first(IndexId),ets:last(IndexId)};
        true ->
            {?IndMax,?IndMin}
    end,
    set_buf_counters(State0#state{rawCnt=RawCnt,rawTop=RawTop,rawBot=RawBot
                                 ,indCnt=IndCnt,indTop=IndTop,indBot=IndBot}).

-spec set_buf_counters(#state{}) -> #state{}.
set_buf_counters(#state{nav=raw,rawCnt=RawCnt,rawTop=RawTop,rawBot=RawBot}=State0) -> 
    State0#state{bufCnt=RawCnt,bufTop=RawTop,bufBot=RawBot};
set_buf_counters(#state{nav=ind,indCnt=IndCnt,indTop=IndTop,indBot=IndBot}=State0) -> 
    State0#state{bufCnt=IndCnt,bufTop=IndTop,bufBot=IndBot}.

-spec filter_fun({'and' | 'or' | undefined, list()}) -> fun().
filter_fun(?NoFilter) ->
    fun(_) -> true end;
filter_fun({'and',Conditions}) ->
    fun(R) -> 
        filter_and(R,Conditions)
    end;
filter_fun({'or',Conditions}) ->
    fun(R) -> 
        filter_or(R,Conditions)
    end.

-spec filter_and(tuple(), [{integer(), term()}]) -> boolean().
filter_and(_,[]) -> true;
filter_and(R,[{Col,Values}|Conditions]) ->
    case lists:member(element(Col+3,R), Values) of
        true ->     filter_and(R,Conditions);
        false ->    false
    end.

-spec filter_or(tuple(), [{integer(), term()}]) -> boolean().
filter_or(_,[]) -> false;
filter_or(R,[{Col,Values}|Conditions]) ->
    case lists:member(element(Col+3,R), Values) of
        false ->    filter_or(R,Conditions);
        true ->     true
    end.

-spec reply_stack(atom(), fun(), #state{}) -> #state{}.
reply_stack(_SN,ReplyTo, #state{stack=undefined}=State0) ->
    % stack is empty, nothing to do    
    State0#state{replyToFun=ReplyTo};
reply_stack(SN,ReplyTo, #state{stack={button,_Button,RT},tRef=undefined}=State0) ->
    % stack is obsolete, overriden by new command, reply delayed request with nop    
    State1 = gui_nop(#gres{state=SN},State0#state{stack=undefined,replyToFun=RT}),
    State1#state{replyToFun=ReplyTo};
reply_stack(SN,ReplyTo, #state{stack={button,_Button,RT},tRef=TRef}=State0) ->
    % stack is obsolete, overriden by new command, reply delayed request with nop
    timer:cancel(TRef),    
    State1 = gui_nop(#gres{state=SN},State0#state{stack=undefined,replyToFun=RT,tRef=undefined}),
    State1#state{replyToFun=ReplyTo}.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, SN, StateData}          |
%%          {ok, SN, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------

init(#ctx{ bl                           = BL
         , replyToFun                   = ReplyTo
         , stmtCols                     = StmtCols
         , rowFun                       = RowFun
         , sortFun                      = SortFun
         , sortSpec                     = SortSpec
         }=Ctx) ->
    TableId=ets:new(raw, [ordered_set]),        %% {Id,Op,Keys,Col1,Col2,...Coln}
    IndexId=ets:new(ind, [ordered_set]),        %% {{SortFun(Keys),Id},Id}
    FilterSpec = ?NoFilter, 
    State0=#state{ bl                           = BL
                 , gl                           = gui_max(BL)
                 , ctx                          = Ctx
                 , tableId                      = TableId
                 , indexId                      = IndexId
                 , stmtColsCount                = length(StmtCols)
                 , rowFun                       = RowFun
                 , sortFun                      = SortFun
                 , sortSpec                     = SortSpec
                 , replyToFun                   = ReplyTo
                 },
    State1 = data_index(SortFun,FilterSpec,State0),           
    {ok, empty, reset_buf_counters(State1)}.

%% --------------------------------------------------------------------
%% Func: SN/2	 non-synchronized event handling
%% Returns: {next_state, NextSN, NextStateData}          |
%%          {next_state, NextSN, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

%% Only data input from DB and button events for <<">|">>, <<">|...">> and <<"...">> handled here
%% Other buttons and commands are handled through all_state_event in handle_event/3

empty({button, <<">|">>, ReplyTo}, State0) ->
    % start fetch
    State1 = fetch(push,none, State0#state{tailMode=false}),
    {next_state, autofilling, State1#state{stack={button,<<">|">>,ReplyTo}}};
empty({button, <<">|...">>, ReplyTo}, State0) ->
    % start fetch, schedule tail
    State1 = fetch(push,true, State0#state{tailMode=true,tailLock=false}),
    {next_state, autofilling, State1#state{stack={button,<<">|...">>,ReplyTo}}};
empty({button, <<"...">>, ReplyTo}, State0) ->
    % skip fetch, schedule tail
    State1 = fetch(skip,true, State0#state{tailMode=true,tailLock=false}),
    {next_state, tailing, State1#state{stack={button,<<"...">>,ReplyTo}}};
empty(Other, State) ->
    ?Info("empty -- unexpected erlimem_fsm event ~p in empty state", [Other]),
    {next_state, empty, State}.

filling({button, <<"restart">>, ReplyTo}, #state{bl=BL,guiTop=GuiTop,guiCol=true}=State0) ->
    State1 = reply_stack(filling, ReplyTo, State0),
    State2 = gui_replace_from(GuiTop,BL,#gres{state=filling,focus=1},State1),
    {next_state, filling, State2#state{tailMode=false}};
filling({button, <<"restart">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(filling, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(none,none,State3),
    State5 = gui_clear(#gres{state=filling,loop= <<">">>}, State4),
    {next_state, filling, State5#state{tailMode=false}};
filling({button, <<"restart">>, ReplyTo}, State0) ->
    % reject command because of uncommitted changes
    State1 = gui_nop(#gres{state=filling,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, filling, State1};
filling({button, Button, ReplyTo}=Cmd, #state{bufCnt=0}=State0) ->
    % too quick, defer request .. when we have the first block of data 
    State1 = reply_stack(filling, ReplyTo, State0),
    ?Debug("filling stack ~p", [Button]),
    {next_state, filling, State1#state{stack=Cmd}};
filling({button, <<"...">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    % close fetch and clear buffers, schedule tail mode
    State1 = reply_stack(filling, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(skip,true,State3),
    State5 = gui_clear(#gres{state=tailing,loop= <<"tail">>}, State4),
    {next_state, tailing, State5#state{tailMode=true,tailLock=false}};
filling({button, <<"...">>, ReplyTo}, State0) ->
    % reject command because of uncommitted changes
    State1 = gui_nop(#gres{state=filling,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, filling, State1};
filling({button, <<">|...">>, ReplyTo}=Cmd, State0) ->
    % switch fetch .. push mode and schedule tail mode, defer answer .. bulk fetch completed 
    State1 = reply_stack(filling, ReplyTo, State0),
    State2 = fetch(push,true,State1),
    State3 = gui_clear(State2),
    ?Debug("filling stack '>|...'"),
    {next_state, autofilling, State3#state{tailMode=true,tailLock=false,stack=Cmd}};
filling({button, <<">|">>, ReplyTo}=Cmd, State0) ->
    % switch fetch .. push mode, defer answer .. bulk fetch completed 
    State1 = reply_stack(filling, ReplyTo, State0),
    State2 = fetch(push,none,State1),
    ?Debug("filling stack '>|'"),
    {next_state, autofilling, State2#state{stack=Cmd}};
filling({rows, {Recs,false}}, #state{nav=Nav,bl=BL,stack={button,Target,_}}=State0) when is_integer(Target) ->
    % receive and store data, prefetch if a 'target sprint' is ongoing
    State1 = data_append(filling, {Recs,false},State0),
    % ?Debug("Target ~p", [Target]),
    % ?Debug("BufCnt ~p", [State1#state.bufCnt]),
    State2 = if  
        (Nav == ind) andalso (Target > State1#state.bufCnt) ->  
            prefetch(filling,State1);
        (Nav == raw) andalso (Target+BL > State1#state.bufCnt) ->  
            prefetch(filling,State1);
        true ->                     
            State1
    end,    
    {next_state, filling, State2};
filling({rows, {Recs,false}}, #state{stack={button,Button,_}}=State0) ->
    % receive and store data, prefetch if a 'button sprint' is ongoing (only necessary for Nav=ind)
    State1 = data_append(filling, {Recs,false},State0),
    NewBufBot = State1#state.bufBot,
    NewGuiBot = State1#state.guiBot,
    State2 = if  
        (Button == <<">">>) ->          prefetch(filling,State1);
        (Button == <<">>">>) ->         prefetch(filling,State1);
        (Button == <<"<">>) ->          prefetch(filling,State1);
        (Button == <<"<<">>) ->         prefetch(filling,State1);
        (NewGuiBot == NewBufBot) -> prefetch(filling,State1);
        true ->                     State1
    end,    
    {next_state, filling, State2};
filling({rows, {Recs,false}}, State0) ->
    % receive and store data, no prefetch needed here
    State1 = data_append(filling, {Recs,false},State0),
    NewBufBot = State1#state.bufBot,
    NewGuiBot = State1#state.guiBot,
    State2 = if  
        (NewGuiBot == NewBufBot) -> prefetch(filling,State1);
        true ->                     State1
    end,
    {next_state, filling, State2};
filling({rows, {Recs,true}}, State0) ->
    % receive and store data, close the fetch and switch state, no prefetch needed here
    State1 = fetch_close(State0),
    State2 = data_append(completed, {Recs,true},State1),
    {next_state, completed, State2};
filling(Other, State) ->
    ?Info("filling -- unexpected event ~p", [Other]),
    {next_state, filling, State}.

autofilling({button, <<"restart">>, ReplyTo}, #state{bl=BL,guiTop=GuiTop,guiCol=true}=State0) ->
    State1 = reply_stack(autofilling, ReplyTo, State0),
    State2 = gui_replace_from(GuiTop,BL,#gres{state=autofilling,focus=1},State1),
    {next_state, filling, State2#state{tailMode=false}};
autofilling({button, <<"restart">>, ReplyTo}, #state{dirtyCnt=DC,tailMode=TailMode}=State0) when DC==0 ->
    State1 = reply_stack(autofilling, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(push,TailMode,State3),
    State5 = gui_clear(#gres{state=filling,loop= <<">">>}, State4),
    {next_state, filling, State5#state{tailMode=false}};
autofilling({button, <<"restart">>, ReplyTo}, State0) ->
    % reject command because of uncommitted changes
    State1 = gui_nop(#gres{state=autofilling,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, filling, State1};
autofilling({button, <<"...">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0->
    % stop fetch, clear buffer and start tailing
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(skip,true, State3),
    State5 = gui_clear(#gres{state=tailing, loop= <<"tail">>},State4),
    {next_state, tailing, State5#state{tailMode=true,tailLock=false}};
autofilling({button, <<"...">>, ReplyTo}, State0) ->
    % reject because of uncommitted changes
    State1 = gui_nop(#gres{state=autofilling,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, autofilling, State1};
autofilling({button, <<">|...">>, ReplyTo}=Cmd, #state{tailMode=TailMode}=State0) ->
    if 
        (TailMode == false) ->
            % too late .. change .. seamless tail mode now
            State1 = gui_nop(#gres{state=autofilling,beep=true},State0#state{replyToFun=ReplyTo}),
            {next_state, autofilling, State1};
        true ->
            % tailing will happen anyways at the end of the bulk fetch, keep command on stack
            State1 = reply_stack(autofilling, ReplyTo, State0),
            State2 = gui_clear(State1),
            ?Debug("autofilling stack '>|...'"),
            {next_state, autofilling, State2#state{tailLock=false,stack=Cmd}}
    end;
autofilling({button, <<">|">>, ReplyTo}=Cmd, #state{tailMode=TailMode}=State0) ->
    if 
        (TailMode == true) ->
            % too late .. revoke tail mode now
            State1 = gui_nop(#gres{state=autofilling,beep=true},State0#state{replyToFun=ReplyTo}),
            {next_state, autofilling, State1};
        true ->
            % already waiting for end of fetch, keep command on stack
            State1 = reply_stack(autofilling, ReplyTo, State0),
            ?Debug("autofilling stack '>|'"),
            {next_state, autofilling, State1#state{tailLock=true,stack=Cmd}}
    end;
autofilling({rows, {Recs,false}}, State0) ->
    % revceive and store input from DB
    State1 = data_append(autofilling,{Recs,false},State0),
    {next_state, autofilling, State1#state{pfc=0}};
autofilling({rows, {Recs,true}}, #state{tailMode=false}=State0) ->
    % revceive and store last input from DB, close fetch, switch state
    State1 = fetch_close(State0),
    State2 = data_append(completed,{Recs,true},State1),
    {next_state, completed, State2#state{pfc=0}};
autofilling({rows, {Recs,true}}, State0) ->
    % revceive and store last input from DB, switch state .. tail mode
    % ?Debug("Rows received complete and tailing:~nState: ~p", [State0]),
    State1= data_append(tailing,{Recs,true},State0),
    {next_state, tailing, State1#state{pfc=0}};
autofilling({rows_limit, {_NRows, Recs}}, State0) ->
    % revceive and store input from DB
    State1 = data_append(filling,{Recs,false},State0),
    {next_state, filling, State1#state{pfc=0}};
autofilling(Other, State) ->
    ?Info("autofilling -- unexpected event ~p", [Other]),
    {next_state, autofilling, State}.

tailing({button, <<"restart">>, ReplyTo}, #state{bl=BL,guiTop=GuiTop,guiCol=true}=State0) ->
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = gui_replace_from(GuiTop,BL,#gres{state=tailing,focus=1},State1),
    {next_state, tailing, State2#state{tailMode=false}};
tailing({button, <<"restart">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(none,true,State3),
    State5 = gui_clear(#gres{state=filling,loop= <<">">>}, State4),
    {next_state, filling, State5#state{tailMode=false}};
tailing({button, <<"restart">>, ReplyTo}, State0) ->
    % reject command because of uncommitted changes
    State1 = gui_nop(#gres{state=tailing,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, filling, State1};
tailing({button, <<"...">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0->
    % clear buffer and resume tailing
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = data_clear(State1),
    State3 = gui_clear(#gres{state=tailing, loop= <<"tail">>},State2),
    {next_state, tailing, State3#state{tailLock=false}};
tailing({button, <<"...">>, ReplyTo}, State0) ->
    % reject because of uncommitted changes
    State1 = gui_nop(#gres{state=tailing,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, tailing, State1};
tailing({button, <<">|...">>, ReplyTo}, State0) ->
    % resume tailing
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = serve_bot(tailing, <<"tail">>, State1),
    {next_state, tailing, State2#state{tailLock=false}};
tailing({button, <<"tail">>, ReplyTo}=Cmd, #state{tailLock=false,bufBot=BufBot,guiBot=GuiBot}=State0) when GuiBot==BufBot ->
    State1 = reply_stack(tailing, ReplyTo, State0),
    ?Debug("tailing stack 'tail'"),
    {ok, NewTRef} = timer:send_after(?TAIL_TIMEOUT, cmd_stack_timeout),
    {next_state, tailing, State1#state{stack=Cmd, tRef=NewTRef}};
tailing({button, <<"tail">>, ReplyTo}, #state{tailLock=false,bufBot=BufBot,guiBot=GuiBot}=State0) ->
    % continue tailing
    ?Debug("tailing button in state ~n~p guibot: ~p bufbot: ~p", [tailing, GuiBot, BufBot]),
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = serve_bot(tailing, <<"tail">>, State1),
    {next_state, tailing, State2};
tailing({button, <<"tail">>, ReplyTo}, State0) ->
    % ignore loop command, stop tailing
    % ?Debug("tailing stopped~n", []),
    State1 = gui_nop(#gres{state=tailing},State0#state{replyToFun=ReplyTo}),
    {next_state, tailing, State1};
tailing({button, <<">|">>, ReplyTo}, #state{bufCnt=0}=State0) ->
    % no data, must ignore
    State1 = gui_nop(#gres{state=tailing},State0#state{replyToFun=ReplyTo}),
    {next_state, tailing, State1#state{tailLock=true}};
tailing({button, <<">|">>, ReplyTo}, State0) ->
    % show bottom
    State1 = reply_stack(tailing, ReplyTo, State0),
    State2 = serve_bot(tailing, <<"">>, State1),
    {next_state, tailing, State2#state{tailLock=true}};
% tailing({rows, {[Rec],tail}}, #state{bl=BL,tailLock=false,rawCnt=RawCnt,tableId=TableId}=State0) when RawCnt =< BL->
%     % ?Info("tracking -- row~n", []),
%     PKey = guard_wrap(element(2,element(1,Rec))),
%     case ets:select(TableId,[{'$1',[{'==',{element,2,{element,1,{element,3,'$1'}}},PKey}],['$_']}]) of
%         [Row] ->
%             ?Info("insert tracking -- row~n~p~n", [Row]),
%             State1 = data_append(tailing,{[Rec],tail},State0),      %% REPLACE $$$$$$$$$
%             {next_state, tailing, State1#state{pfc=0}};             %% REPLACE $$$$$$$$$
%         _ ->
%             ?Info("fallback to tailing -- row~n~p~n", [Rec]),
%             State1 = data_append(tailing,{[Rec],tail},State0),
%             {next_state, tailing, State1#state{pfc=0}}
%     end;      
tailing({rows, {Recs,Complete}}, State0) ->
    % ?Info("tailing  -- row~n", []),
    State1 = data_append(tailing,{Recs,Complete},State0),
    {next_state, tailing, State1#state{pfc=0}};
tailing(Other, State) ->
    ?Info("tailing -- unexpected event ~p in state~n~p", [Other,State]),
    {next_state, tailing, State}.

completed({button, <<"restart">>, ReplyTo}, #state{bl=BL,guiTop=GuiTop,guiCol=true}=State0) ->
    State1 = reply_stack(completed, ReplyTo, State0),
    State2 = gui_replace_from(GuiTop,BL,#gres{state=completed,focus=1},State1),
    {next_state, completed, State2#state{tailMode=false}};
completed({button, <<"restart">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(completed, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(none,false,State3),
    State5 = gui_clear(#gres{state=filling,loop= <<">">>}, State4),
    {next_state, filling, State5#state{tailMode=false}};
completed({button, <<"restart">>, ReplyTo}, State0) ->
    % reject command because of uncommitted changes
    State1 = gui_nop(#gres{state=completed,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, completed, State1};
completed({button, <<"...">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    % clear buffers, close and reopen fetch with skip and tail options
    State1 = reply_stack(completed, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = fetch(skip,true,State2),
    State4 = data_clear(State3),
    State5 = gui_clear(#gres{state=tailing,loop= <<"tail">>},State4),
    {next_state, tailing, State5#state{tailMode=true,tailLock=false}};
completed({button, <<"...">>, ReplyTo}, State0) ->
    % reject because of uncommitted changes
    State1 = gui_nop(#gres{state=completed,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, completed, State1};
completed({button, <<">|...">>, ReplyTo}, State0) ->
    % keep data (if any) and switch .. tail mode
    State1 = reply_stack(completed, ReplyTo, State0),
    State2 = fetch(skip,true,State1),
    State3 = gui_clear(State2),
    State4 = gui_nop(#gres{state=tailing,loop= <<"tail">>},State3),
    {next_state, tailing, State4#state{tailMode=true,tailLock=false}};
completed({button, <<">|">>, ReplyTo}, #state{bufCnt=0}=State0) ->
    % reject command because we have no data
    State1 = reply_stack(completed, ReplyTo, State0),
    State1 = gui_nop(#gres{state=completed,beep=true},State1),
    {next_state, completed, State1};
completed({button, <<">|">>, ReplyTo}, #state{bl=BL,bufBot=BufBot}=State0) ->
    % jump .. buffer bottom
    State1 = reply_stack(completed, ReplyTo, State0),
    State2 = gui_replace_until(BufBot,BL,#gres{state=completed},State1),
    {next_state, completed, State2};
completed({rows, _}, State) ->
    % ignore unsolicited rows
    {next_state, completed, State};
completed(Other, State) ->
    ?Info("completed -- unexpected event ~p", [Other]),
    {next_state, completed, State}.

aborted({button, <<"restart">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(aborted, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = data_clear(State2),
    State4 = fetch(none,false,State3),
    State5 = gui_clear(#gres{state=filling,loop= <<">">>}, State4),
    {next_state, filling, State5#state{tailMode=false}};
aborted({button, <<"restart">>, ReplyTo}, State0) ->
    % reject command because of uncommitted changes
    State1 = gui_nop(#gres{state=aborted,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, aborted, State1};
aborted({button, <<"...">>, ReplyTo}, #state{dirtyCnt=DC}=State0) when DC==0 ->
    % clear buffers, close and reopen fetch with skip and tail options
    State1 = reply_stack(aborted, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = fetch(skip,true,State2),
    State4 = data_clear(State3),
    State5 = gui_clear(#gres{state=tailing,loop= <<"tail">>},State4),
    {next_state, tailing, State5#state{tailMode=true,tailLock=false}};
aborted({button, <<"...">>, ReplyTo}, State0) ->
    % reject because of uncommitted changes
    State1 = gui_nop(#gres{state=aborted,beep=true,message= ?MustCommit},State0#state{replyToFun=ReplyTo}),
    {next_state, aborted, State1};
aborted({button, <<">|...">>, ReplyTo}, State0) ->
    % keep data (if any) and switch .. tail mode
    State1 = reply_stack(aborted, ReplyTo, State0),
    State2 = fetch(skip,true,State1),
    State3 = gui_clear(State2),
    State4 = gui_nop(#gres{state=tailing,loop= <<"tail">>},State3),
    {next_state, tailing, State4#state{tailMode=true,tailLock=false}};
aborted({button, <<">|">>, ReplyTo}, #state{bufCnt=0}=State0) ->
    % reject command because we have no data
    State1 = reply_stack(aborted, ReplyTo, State0),
    State1 = gui_nop(#gres{state=aborted,beep=true},State1),
    {next_state, aborted, State1};
aborted({button, <<">|">>, ReplyTo}, #state{bl=BL,bufBot=BufBot}=State0) ->
    % jump .. buffer bottom
    State1 = reply_stack(aborted, ReplyTo, State0),
    State2 = gui_replace_until(BufBot,BL,#gres{state=aborted},State1),
    {next_state, aborted, State2};
aborted({rows, _}, State) ->
    % ignore unsolicited rows
    {next_state, aborted, State};
aborted(Other, State) ->
    ?Info("aborted -- unexpected event ~p", [Other]),
    {next_state, aborted, State}.

%% --------------------------------------------------------------------
%% Func: SN/3	 synchronized event handling
%% Returns: {next_state, NextSN, NextStateData}            |
%%          {next_state, NextSN, NextStateData, Timeout}   |
%%          {reply, ReplyTo, NextSN, NextStateData}          |
%%          {reply, ReplyTo, NextSN, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, ReplyTo, NewStateData}
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Func: handle_event/3  handling async "send_all_state_event""
%% Returns: {next_state, NextSN, NextStateData}          |
%%          {next_state, NextSN, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

handle_event({button, <<">">>, ReplyTo}, empty, State0) ->
    State1 = fetch(none,none, State0#state{tailMode=false}),
    ?Debug("empty stack '>' the fun ~p", [ReplyTo]),
    {next_state, filling, State1#state{stack={button,<<">">>,ReplyTo}}};
handle_event({button, <<">">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    {next_state, SN, serve_fwd(SN, State1#state{tailLock=true})};
handle_event({button, <<">>">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    {next_state, SN, serve_ffwd(SN, State1#state{tailLock=true})};
handle_event({button, <<"|<">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    {next_state, SN, serve_top(SN, State1#state{tailLock=true})};
handle_event({button, <<"<">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    {next_state, SN, serve_bwd(SN, State1#state{tailLock=true})};
handle_event({button, <<"<<">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    {next_state, SN, serve_fbwd(SN, State1#state{tailLock=true})};
handle_event({button, Target, ReplyTo}, SN, State0) when is_integer(Target) ->
    State1 = reply_stack(SN, ReplyTo, State0#state{tailLock=true}),
    {next_state, SN, serve_target(SN, Target, State1)};
handle_event({update, ChangeList, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = data_update(SN, ChangeList, State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({button, <<"commit">>, ReplyTo}, SN, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = gui_nop(#gres{state=SN,beep=true,message= ?NoPendingUpdates},State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({button, <<"commit">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    {NewSN,State2} = data_commit(SN, State1),
    {next_state, NewSN, State2#state{tailLock=true}};
handle_event({button, <<"rollback">>, ReplyTo}, SN, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = gui_nop(#gres{state=SN,beep=true,message= ?NoPendingUpdates},State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({button, <<"rollback">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = data_rollback(SN, State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({reorder, ColOrder, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = data_reorder(SN, ColOrder, State1),
    {next_state, SN, State2};
handle_event({filter, FilterSpec, ReplyTo}, SN, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = data_filter(SN, FilterSpec, State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({filter, _FilterSpec, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = gui_nop(#gres{state=SN,beep=true,message= ?MustCommitSort},State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({sort, GuiSortSpec, ReplyTo}, SN, #state{dirtyCnt=DC}=State0) when DC==0->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = data_sort(SN, GuiSortSpec, State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({sort, _GuiSortSpec, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = gui_nop(#gres{state=SN,beep=true,message= ?MustCommitSort},State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({button, <<"close">>, ReplyTo}, SN, #state{dirtyCnt=DC}=State0) when DC==0 ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = fetch_close(State1),
    State3 = gui_close(#gres{state=SN},State2),
    {stop, normal, State3#state{tailLock=true}};
handle_event({button, <<"close">>, ReplyTo}, SN, State0) ->
    State1 = reply_stack(SN, ReplyTo, State0),
    State2 = gui_nop(#gres{state=SN,beep=true,message= ?MustCommit},State1),
    {next_state, SN, State2#state{tailLock=true}};
handle_event({error, Error}, SN, State) ->
    ?Error("Error on fsm ~p when State ~p Message: ~n~p", [self(), SN, Error]),
    ErrorMsg = iolist_to_binary(io_lib:format("~p", [Error])),
    State1 = gui_nop(#gres{state=SN,beep=true,message=ErrorMsg},State),
    State2 = fetch_close(State1#state{tailMode=false}),
    {next_state, aborted, State2#state{tailLock=false}};
handle_event(Event, SN, State) ->
    ?Info("handle_event -- unexpected event ~p in state ~p~n", [Event,SN]),
    {next_state, SN, State}.


%% --------------------------------------------------------------------
%% Func: handle_sync_event/4 handling sync "send_all_state_event""
%% Returns: {next_state, NextSN, NextStateData}            |
%%          {next_state, NextSN, NextStateData, Timeout}   |
%%          {reply, ReplyTo, NextSN, NextStateData}          |
%%          {reply, ReplyTo, NextSN, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, ReplyTo, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event({"get_columns"}, _From, SN, #state{ctx=#ctx{stmtCols=Columns}}=State) ->
    ?Debug("get_columns ~p", [Columns]),
    {reply, Columns, SN, State, infinity};
handle_sync_event(get_query, _From, SN, #state{ctx=#ctx{orig_qry=Qry}}=State) ->
    ?Debug("get_query ~p", [Qry]),
    {reply, Qry, SN, State, infinity};
handle_sync_event({"row_with_key", RowId}, _From, SN, #state{tableId=TableId}=State) ->
    [Row] = ets:lookup(TableId, RowId),
    % ?Debug("row_with_key ~p ~p", [RowId, Row]),
    {reply, Row, SN, State, infinity};
handle_sync_event({histogram, ColumnId}, _From, SN, #state{tableId=TableId}=State) ->
    ?Debug("Getting the histogram of the column ~p", [ColumnId]),
    IncrFun =
        fun(Row, CountList) ->
            Value = element(3 + ColumnId, Row),
            case proplists:get_value(Value, CountList) of
                undefined ->
                    [{Value, 1} | CountList];
                OldCount ->
                    lists:keyreplace(Value, 1, CountList, {Value, OldCount + 1})
            end
        end,
    Result = ets:foldl(IncrFun, [], TableId),
    {reply, Result, SN, State, infinity};
handle_sync_event({refresh_ctx, #ctx{bl = BL, replyToFun = ReplyTo} = Ctx}, _From, SN, #state{} = State) ->
    #ctx{stmtCols = StmtCols, rowFun = RowFun, sortFun = SortFun, sortSpec = SortSpec} = Ctx,
    State0 = State#state{bl        = BL
                   , gl            = gui_max(BL)
                   , ctx           = Ctx
                   , stmtColsCount = length(StmtCols)
                   , rowFun        = RowFun
                   , sortFun       = SortFun
                   , sortSpec      = SortSpec
                   , replyToFun    = ReplyTo
                   },
    {reply, ok, SN, State0, infinity};
handle_sync_event(_Event, _From, empty, StateData) ->
    {no_reply, empty, StateData, infinity}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextSN, NextStateData}          |
%%          {next_state, NextSN, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({_Pid,{Rows,Completed}}, SN, State) ->
    Fsm = {?MODULE,self()},
    Fsm:rows({Rows,Completed}),
    {next_state, SN, State, infinity};
handle_info(cmd_stack_timeout, tailing, #state{stack={button, <<"tail">>, RT}}=State) ->
    % we didn't get any new data to send, so we reply with nop.
    ?Debug("Tail timeout, replying with nop"),
    State1 = gui_nop(#gres{state=tailing, loop= <<"tail">>, focus=-1},State#state{stack=undefined,replyToFun=RT,tRef=undefined}),
    {next_state, tailing, State1, infinity};
handle_info(Unknown, SN, State) ->
    ?Info("unknown handle info ~p", [Unknown]),
    {next_state, SN, State, infinity}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, _SN, #state{ctx=Ctx}) -> 
    ?Info("fsm ~p terminating reason: ~p", [self(), Reason]),
    F= Ctx#ctx.stmt_close_fun,
    F().

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, SN, StateData, _Extra) ->
    {ok, SN, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

% guard_wrap(L) when is_list(L) ->
%     [guard_wrap(Item) || Item <- L];
% guard_wrap(T) when is_tuple(T) ->
%     {const,list_to_tuple(guard_wrap(tuple_to_list(T)))};
% guard_wrap(E) -> E.

-spec gui_max(integer()) -> integer().
gui_max(BL) when BL < 10 -> 30;
gui_max(BL) -> 3 * BL.

%% What is the return of a log ? -spec gui_response_log(#gres{}) -> ok.
gui_response_log(#gres{sql= <<"">>}=Gres) ->
    ?Debug("gui_response ~p", [Gres#gres{rows=[]}]);
gui_response_log(Gres) ->
    ?Debug("gui_response ~p", [Gres#gres.sql]).

-spec gui_response(#gres{}, #state{}) -> #state{}.
gui_response(#gres{state=SN}=Gres0, #state{nav=raw,rawCnt=RawCnt,dirtyCnt=DirtyCnt,replyToFun=ReplyTo,sql=Sql}=State0) ->
    Gres1 = gres(SN,RawCnt,integer_to_list(RawCnt),Sql,DirtyCnt,false,Gres0),
    ReplyTo(Gres1),
    gui_response_log(Gres1),
    State0#state{sql= <<"">>};
gui_response(#gres{state=SN}=Gres0, #state{nav=ind,rawCnt=RawCnt,indCnt=IndCnt,dirtyCnt=DirtyCnt,replyToFun=ReplyTo,sql=Sql,guiCol=GuiCol}=State0) ->
    ToolTip = integer_to_list(RawCnt) ++ [$/] ++ integer_to_list(IndCnt),
    Gres1 = gres(SN,IndCnt,ToolTip,Sql,DirtyCnt,GuiCol,Gres0),
    ReplyTo(Gres1),
    gui_response_log(Gres1),
    State0#state{sql= <<"">>}.

-spec gres(atom(), integer(), list(), binary(), integer(), boolean(), #gres{}) -> #gres{}.
gres(SN,Cnt,ToolTip,Sql,DirtyCount,GuiCol,Gres0) ->
    Disable = case DirtyCount of
        0 ->    
            [{<<"commit">>,<<"nothing to commit">>},{<<"rollback">>,<<"nothing to rollback">>}|Gres0#gres.disable];
        _ ->    
            Gres0#gres.disable
    end,    
    Promo = if
        (SN == aborted) ->
            [{<<"restart">>,<<"restart to see more data">>}|Gres0#gres.promote];
        (GuiCol) ->
            [{<<"restart">>,<<"refresh to see correct sorting">>}|Gres0#gres.promote];
        true ->    
            Gres0#gres.promote
    end,    
    SNbin = list_to_binary(atom_to_list(SN)),
    TTbin = list_to_binary(ToolTip),
    Gres0#gres{state=SNbin,cnt=Cnt,toolTip=TTbin,sql=Sql,disable=empty_override(Disable),promote=empty_override(Promo)}.

-spec empty_override(list()) -> list().
empty_override([]) -> [{}];
empty_override(List) -> List.

-spec gui_close(#gres{}, #state{}) -> #state{}.
gui_close(GuiResult,State) -> 
    ?Debug("gui_close () ~p", [GuiResult#gres.state]),
    gui_response(GuiResult#gres{operation= <<"close">>},State).

-spec gui_nop(#gres{}, #state{}) -> #state{}.
gui_nop(GuiResult,State) -> 
    ?Debug("gui_nop () ~p ~p", [GuiResult#gres.state, GuiResult#gres.loop]),
    gui_response(GuiResult#gres{operation= <<"nop">>},State).

-spec gui_clear(#gres{}, #state{}) -> #state{}.
gui_clear(GuiResult,#state{nav = Nav} = State0) ->
    ?Debug("gui_clear () ~p ~p", [GuiResult#gres.state, GuiResult#gres.loop]),
    case Nav of
        raw -> GuiBot = ?RawMin;
        ind -> GuiBot = ?IndMin
    end,
    State1 = State0#state{guiCnt=0,guiTop=undefined,guiBot=GuiBot,guiCol=false},
    gui_response(GuiResult#gres{operation= <<"clr">>,keep=0}, State1).

% gui_replace(NewTop,NewBot,GuiResult,State0) ->
%     ?Debug("gui_replace ~p .. ~p ~p ~p", [NewTop, NewBot, GuiResult#gres.state, GuiResult#gres.loop]),
%     Rows=all_rows(NewTop,NewBot,State0),
%     Cnt=length(Rows),
%     State1 = State0#state{guiCnt=Cnt,guiTop=NewTop,guiBot=NewBot,guiCol=false},
%     gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=Cnt},State1).
-spec gui_ins(#gres{}, #state{}) -> #state{}.
gui_ins(#gres{rows=Rows}=GuiResult, #state{guiCnt=GuiCnt}=State0) ->
    Cnt = length(Rows),
    ?Debug("gui_ins (~p) ~p ~p", [Cnt, GuiResult#gres.state, GuiResult#gres.loop]),
    gui_response(GuiResult#gres{operation= <<"ins">>},State0#state{guiCnt=GuiCnt+Cnt}).

-spec gui_replace_from(integer() | tuple(), integer(), #gres{}, #state{}) -> #state{}.
gui_replace_from(Top,Limit,GuiResult,#state{nav=raw,tableId=TableId,rowFun=RowFun}=State0) ->
    Ids = case ets:lookup(TableId, Top) of
        [] ->   ids_after(Top, Limit, State0);
        _  ->   [Top | ids_after(Top, Limit-1, State0)]
    end,
    Cnt = length(Ids),
    {Rows,NewGuiTop,NewGuiBot} = case Cnt of
        0 ->    {[],?RawMax,?RawMin};
        _ ->    {rows_for_ids(Ids,TableId,RowFun),hd(Ids),lists:last(Ids)}
    end,
    ?Debug("gui_replace_from  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=Cnt,guiTop=NewGuiTop,guiBot=NewGuiBot,guiCol=false},
    gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=Cnt},State1);
gui_replace_from(Top,Limit,GuiResult,#state{nav=ind,tableId=TableId}=State0) ->
    Keys = [Top | keys_after(Top, Limit-1, State0)],
    Cnt = length(Keys),
    {Rows,NewGuiTop,NewGuiBot} = case Cnt of
        0 ->    {[],?IndMax,?IndMin};
        _ ->    {rows_for_keys(Keys,TableId),Top,lists:last(Keys)}
    end,
    ?Debug("gui_replace_from  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=Cnt,guiTop=NewGuiTop,guiBot=NewGuiBot,guiCol=false},
    gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=Cnt}, State1).

-spec gui_replace_until(integer() | tuple(), integer(), #gres{}, #state{}) -> #state{}.
gui_replace_until(Bot,Limit,GuiResult,#state{nav=raw,tableId=TableId,rowFun=RowFun}=State0) ->
    Ids = case ets:lookup(TableId, Bot) of
        [] ->   ids_before(Bot, Limit, State0);
        _  ->   ids_before(Bot, Limit-1, State0) ++ [Bot]
    end,
    Cnt = length(Ids),
    {Rows,NewGuiTop,NewGuiBot} = case Cnt of
        0 ->    {[],?RawMax,?RawMin};
        _ ->    {rows_for_ids(Ids,TableId,RowFun),hd(Ids),lists:last(Ids)}
    end,
    ?Debug("gui_replace_until  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=Cnt,guiTop=NewGuiTop,guiBot=NewGuiBot,guiCol=false},
    gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=Cnt}, State1);
gui_replace_until(Bot,Limit,GuiResult,#state{nav=ind,tableId=TableId}=State0) ->
    Keys = keys_before(Bot, Limit-1, State0) ++ [Bot],
    Cnt = length(Keys),
    {Rows,NewGuiTop,NewGuiBot} = case Cnt of
        0 ->    {[],?IndMax,?IndMin};
        _ ->    {rows_for_keys(Keys,TableId),hd(Keys),Bot}
    end,
    ?Debug("gui_replace_until  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=Cnt,guiTop=NewGuiTop,guiBot=NewGuiBot,guiCol=false},
    gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=Cnt},State1).

-spec gui_prepend(#gres{}, #state{}) -> #state{}.
gui_prepend(GuiResult,#state{nav=raw,bl=BL,guiCnt=0}=State0) ->
    case rows_before(?RawMax, BL, State0) of
        [] ->
            gui_response(GuiResult#gres{operation= <<"clr">>,keep=0}, State0);
        Rows ->
            Cnt = length(Rows),
            NewGuiCnt = Cnt,
            NewGuiTop = hd(hd(Rows)),
            NewGuiBot = hd(lists:last(Rows)),
            ?Debug("gui_replace  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
            State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
            gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=NewGuiCnt}, State1)
    end;
gui_prepend(GuiResult,#state{nav=raw,bl=BL,gl=GL,guiCnt=GuiCnt,guiTop=GuiTop}=State0) ->
    Rows = rows_before(GuiTop, BL, State0),
    Cnt = length(Rows),
    {NewGuiCnt,NewGuiTop,NewGuiBot} = case ids_after(GuiTop,min(GuiCnt-1,GL-Cnt-1),State0) of
        [] ->       {Cnt+1,hd(hd(Rows)),GuiTop};
        IdsKept ->  {length(IdsKept)+Cnt+1,hd(hd(Rows)),lists:last(IdsKept)}
    end,
    ?Debug("gui_prepend ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
    gui_response(GuiResult#gres{operation= <<"prp">>,rows=Rows,keep=NewGuiCnt},State1);
gui_prepend(GuiResult,#state{nav=ind,bl=BL,tableId=TableId,guiCnt=0}=State0) ->
    Keys=keys_before(?IndMax, BL, State0),
    case length(Keys) of
        0 ->    
             gui_response(GuiResult#gres{operation= <<"clr">>,keep=0}, State0);
        Cnt ->  
            Rows = rows_for_keys(Keys,TableId),
            NewGuiCnt = Cnt,
            NewGuiTop = hd(Keys),
            NewGuiBot = lists:last(Keys),
            ?Debug("gui_replace  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
            State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
            gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=NewGuiCnt}, State1)
    end;
gui_prepend(GuiResult,#state{nav=ind,bl=BL,gl=GL,tableId=TableId,guiCnt=GuiCnt,guiTop=GuiTop}=State0) ->
    Keys=keys_before(GuiTop, BL, State0),
    Cnt = length(Keys),
    Rows = rows_for_keys(Keys,TableId),
    {NewGuiCnt,NewGuiTop,NewGuiBot} = case keys_after(GuiTop,min(GuiCnt-1,GL-Cnt-1),State0) of
        [] ->       {Cnt+1,hd(Keys),GuiTop};
        KeysKept -> {length(KeysKept)+Cnt+1,hd(Keys),lists:last(KeysKept)}
    end,
    ?Debug("gui_prepend ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
    gui_response(GuiResult#gres{operation= <<"prp">>,rows=Rows,keep=NewGuiCnt}, State1).

-spec gui_append(#gres{}, #state{}) -> #state{}.
gui_append(GuiResult,#state{nav=raw,bl=BL,guiCnt=0}=State0) ->
    Rows=rows_after(?RawMin, BL, State0),
    case length(Rows) of
        0 ->
             gui_response(GuiResult#gres{operation= <<"clr">>,keep=0}, State0);
        Cnt ->  
            NewGuiCnt = Cnt,
            NewGuiTop = hd(hd(Rows)),
            NewGuiBot = hd(lists:last(Rows)),
            ?Debug("gui_replace  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
            State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
            gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=NewGuiCnt}, State1)
    end;
gui_append(GuiResult,#state{nav=raw,bl=BL,gl=GL,guiCnt=GuiCnt,guiBot=GuiBot}=State0) ->
    ?Debug("GuiBot ~p", [GuiBot]),
    Rows=rows_after(GuiBot, BL, State0),
    ?Debug("Rows ~p", [Rows]),
    Cnt = length(Rows),
    {NewGuiCnt,NewGuiTop,NewGuiBot} = case ids_before(GuiBot,min(GuiCnt-1,GL-Cnt-1),State0) of
        [] ->       {Cnt+1,GuiBot,hd(lists:last(Rows))};
        IdsKept ->  {length(IdsKept)+Cnt+1,hd(IdsKept),hd(lists:last(Rows))}
    end,
    ?Debug("gui_append  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
    gui_response(GuiResult#gres{operation= <<"app">>,rows=Rows,keep=NewGuiCnt}, State1);
gui_append(GuiResult,#state{nav=ind,bl=BL,tableId=TableId,guiCnt=0}=State0) ->
    Keys=keys_after(?IndMin, BL, State0),
    case length(Keys) of
        0 ->    
             gui_response(GuiResult#gres{operation= <<"clr">>,keep=0}, State0);
        Cnt ->  
            Rows = rows_for_keys(Keys,TableId),
            NewGuiCnt = Cnt,
            NewGuiTop = hd(Keys),
            NewGuiBot = lists:last(Keys),
            ?Debug("gui_replace  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
            State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
            gui_response(GuiResult#gres{operation= <<"rpl">>,rows=Rows,keep=NewGuiCnt}, State1)
    end;
gui_append(GuiResult,#state{nav=ind,bl=BL,gl=GL,tableId=TableId,guiCnt=GuiCnt,guiBot=GuiBot}=State0) ->
    Keys=keys_after(GuiBot, BL, State0),
    Cnt = length(Keys),
    Rows = rows_for_keys(Keys,TableId),
    {NewGuiCnt,NewGuiTop,NewGuiBot} = case keys_before(GuiBot,min(GuiCnt-1,GL-Cnt-1),State0) of
        [] ->       {Cnt+1,GuiBot,lists:last(Keys)};
        KeysKept -> {length(KeysKept)+Cnt+1,hd(KeysKept),lists:last(Keys)}
    end,
    ?Debug("gui_append  ~p .. ~p ~p ~p", [NewGuiTop, NewGuiBot, GuiResult#gres.state, GuiResult#gres.loop]),
    State1 = State0#state{guiCnt=NewGuiCnt,guiTop=NewGuiTop,guiBot=NewGuiBot},
    gui_response(GuiResult#gres{operation= <<"app">>,rows=Rows,keep=NewGuiCnt}, State1).

-spec serve_top(atom(), #state{}) -> #state{}.
serve_top(SN,#state{bl=BL,bufCnt=BufCnt,bufTop=BufTop}=State0) ->
    if
        (BufCnt == 0) ->
            %% no data, serve empty page
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,beep=true},State1);
        (BufCnt >= BL+BL) ->
            %% enough data, serve it, no need for prefetch
            gui_replace_from(BufTop,BL,#gres{state=SN,focus=1},State0);
        true ->
            %% we have data but may need to prefetch
            State1 = prefetch(SN,State0),          %% only when filling
            gui_replace_from(BufTop,BL,#gres{state=SN,focus=1},State1)
    end.

-spec serve_fwd(atom(), #state{}) -> #state{}.
serve_fwd(SN,#state{nav=Nav,bl=BL,bufCnt=BufCnt,bufBot=BufBot,guiCnt=GuiCnt,guiBot=GuiBot,replyToFun=ReplyTo}=State0) ->
    if
        (BufCnt == 0) andalso (SN == filling) ->
            ?Debug("~p waiting for the fetch to complete ~p", [SN, <<">">>]),
            State0#state{stack={button,<<">">>,ReplyTo}};
        (BufCnt == 0) ->
            %% no data, serve empty gui
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,beep=true},State1);
        (GuiCnt == 0) ->
            %% (re)initialize buffer
            serve_bot(SN,<<>>,State0);
        (GuiBot == BufBot) andalso (SN == completed) ->
            serve_bot(SN,<<>>,State0);
        (GuiBot == BufBot) andalso (SN == aborted) ->
            serve_bot(SN,<<>>,State0);
        (GuiBot == BufBot) andalso (SN == tailing) ->
            serve_bot(SN,<<>>,State0);
        (GuiBot == BufBot) ->
            %% index view is at end of buffer, prefetch and defer answer
            State1 = prefetch(SN,State0),
            ?Debug("~p stack ~p", [SN,<<">">>]),
            State1#state{stack={button,<<">">>,ReplyTo}};
        (Nav == raw) andalso (GuiBot > BufBot-BL-BL) ->
            %% prefetch and go forward
            State1 = prefetch(SN,State0), 
            gui_append(#gres{state=SN},State1);
        true ->
            %% go forward
            gui_append(#gres{state=SN},State0)
    end.

-spec serve_ffwd(atom(), #state{}) -> #state{}.
serve_ffwd(SN,#state{nav=Nav,bl=BL,bufCnt=BufCnt,bufBot=BufBot,guiCnt=GuiCnt,guiBot=GuiBot,replyToFun=ReplyTo}=State0) ->
    if
        (BufCnt == 0) andalso (SN == filling) ->
            ?Debug("~p waiting for fetch to complete ~p", [SN, <<">>">>]),
            gui_nop(#gres{state=filling, loop= <<">>">>}, State0);
        (BufCnt == 0) ->
            %% no data, serve empty gui
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,beep=true},State1);
        (GuiCnt == 0) ->
            %% (re)initialize buffer
            serve_bot(SN,<<"">>,State0);
        true ->
            NewGuiBot = key_times_2(GuiBot,State0),
            if 
                (Nav == ind) andalso (NewGuiBot == undefined) -> 
                    %% jump leads outside of index table, target double buffer size
                    State1 = prefetch(SN,State0),
                    ?Debug("~p stack ~p", [SN,<<">>">>]),
                    State1#state{stack={button,<<">>">>,ReplyTo}};  %%  BufCnt+BufCnt for target based jump
                (NewGuiBot =< BufBot) ->
                    %% requested jump is possible within existing buffer, do it
                    gui_replace_until(NewGuiBot,BL,#gres{state=SN},State0);
                (Nav == raw) andalso (SN == filling) ->
                    %% jump is not possible in existing buffer, target 
                    State1 = prefetch(SN,State0),
                    ?Debug("~p stack ~p", [SN,NewGuiBot]),
                    State1#state{stack={button,NewGuiBot,ReplyTo}};
                true ->
                    %% jump is not possible in existing buffer, show end of it 
                    gui_replace_until(BufBot,BL,#gres{state=SN},State0)
            end
    end.

-spec serve_bwd(atom(), #state{}) -> #state{}.
serve_bwd(SN,#state{srt=Srt,bufCnt=BufCnt,bufTop=BufTop,guiCnt=GuiCnt,guiTop=GuiTop,replyToFun=ReplyTo}=State0) ->
    if
        (BufCnt == 0) ->
            %% no data, serve empty gui
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,beep=true},State1);
        (GuiCnt == 0) ->
            %% (re)initialize buffer
            serve_top(SN,State0);
        (GuiTop == BufTop) andalso Srt and (SN == filling) ->
            %% we are at the top of the buffer, must fetch .. go backward, stack command
            State1 = prefetch(SN,State0),       
            ?Debug("~p stack ~p", [SN,<<"<">>]),
            State1#state{stack={button,<<"<">>,ReplyTo}};
        (GuiTop == BufTop)  ->
            %% we are at the top of the buffer, cannot go backward
            gui_nop(#gres{state=SN,beep=true},State0);
        true ->
            gui_prepend(#gres{state=SN},State0)
    end.

-spec serve_fbwd(atom(), #state{}) -> #state{}.
serve_fbwd(SN,#state{bl=BL,srt=Srt,bufCnt=BufCnt,bufTop=BufTop,guiCnt=GuiCnt,guiTop=GuiTop,replyToFun=ReplyTo}=State0) ->
    if
        (BufCnt == 0) ->
            %% no data, serve empty gui
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,beep=true},State1);
        (GuiCnt == 0) ->
            %% (re)initialize buffer
            serve_top(SN,State0);
        (GuiTop == BufTop) andalso Srt and (SN == filling) ->
            %% we are at the top of the buffer, must fetch .. go backward, stack command
            State1 = prefetch(SN,State0),       
            ?Debug("~p stack ~p", [SN,<<"<<">>]),
            State1#state{stack={button,<<"<<">>,ReplyTo}};
        (GuiTop == BufTop)  ->
            %% we are at the top of the buffer, cannot go backward
            gui_nop(#gres{state=SN,beep=true},State0);
        true ->
            NewGuiTop = key_div_2(GuiTop,State0),
            gui_replace_from(NewGuiTop,BL,#gres{state=SN},State0)
    end.

-spec serve_target(atom(), integer(), #state{}) -> #state{}.
serve_target(SN,Target,#state{nav=Nav,bl=BL,tableId=TableId,indexId=IndexId,bufCnt=BufCnt,bufTop=BufTop,guiCnt=GuiCnt,replyToFun=ReplyTo}=State0) when is_integer(Target) ->
    if
        (BufCnt == 0) ->
            %% no data, serve empty gui
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,beep=true},State1);
        (GuiCnt == 0) ->
            %% (re)initialize buffer
            serve_top(SN,State0);
        (Target =< 0) andalso (BufCnt+Target > 0) ->
            %% target given relative to buffer bottom, retry with absolute target position 
            serve_target(SN,BufCnt+Target,State0);
        (Target =< 0)  ->
            %% target points to key smaller than top key
            serve_top(SN,State0);
        (Target =< BufCnt) andalso (BufCnt =< BL) ->
            %% target points to first block in buffer
            State1 = prefetch(SN,State0),          %% only when filling
            gui_replace_from(BufTop,BL,#gres{state=SN,focus=Target},State1);
        (Nav == raw) andalso (Target =< BufCnt) ->
            %% target can be served
            Key = key_at_pos(TableId,Target),
            gui_replace_until(Key,BL,#gres{state=SN,focus=Target},State0);
        (Nav == ind) andalso (Target =< BufCnt) ->
            %% target can be served
            Key = key_at_pos(IndexId,Target),
            gui_replace_until(Key,BL,#gres{state=SN,focus=Target},State0);
        (Target > BufCnt) andalso (SN == completed) ->
            serve_bot(SN,<<>>,State0);
        (Target > BufCnt) andalso (SN == aborted) ->
            serve_bot(SN,<<>>,State0);
        (Target > BufCnt) ->
            %% jump is not possible in existing buffer, defer answer
            State1 = prefetch(SN,State0),
            ?Debug("~p stack ~p", [SN,Target]),
            State1#state{stack={button,Target,ReplyTo}};
        true ->
            %% target should be in GUI already
            gui_nop(#gres{state=SN},State0)
    end.

-spec serve_bot(atom(), binary(), #state{}) -> #state{}.
serve_bot(SN, Loop, #state{nav=Nav,bl=BL,gl=GL,bufCnt=BufCnt,bufBot=BufBot,guiCnt=GuiCnt,guiBot=GuiBot,guiCol=GuiCol}=State0) ->
    %?Debug("serve_bot  (~p ~p) ~p ~p ~p ~p", [SN, Loop, BufCnt, BufBot, GuiCnt, GuiBot]),
    if
        (BufCnt == 0) ->
            %% no data, serve empty
            State1 = prefetch(SN,State0),
            gui_clear(#gres{state=SN,loop=Loop},State1);
        (GuiCnt == 0) ->
            %% uninitialized view, must refresh    
            gui_replace_until(BufBot,BL,#gres{state=SN,loop=Loop,focus=-1},State0); 
        (GuiCol == true) ->
            %% dirty index view, must refresh anyways    
            gui_replace_until(BufBot,BL,#gres{state=SN,loop=Loop,focus=-1},State0); 
        (GuiBot == BufBot) ->
            %% gui is already there, noting .. do       
            gui_nop(#gres{state=SN,loop=Loop,focus=-1},State0); 
        (Nav == raw) andalso (GuiBot < BufBot-GL) ->
            %% uninitialized view, must refresh    
            gui_replace_until(BufBot,BL,#gres{state=SN,loop=Loop,focus=-1},State0); 
        (Loop == <<"tail">>) andalso (SN == tailing) ->
            %% tailing should append (don't call this far from bottom of big buffer)                 
            gui_append(#gres{state=SN,loop=Loop,focus=-1},State0#state{tailLock=false}); 
        true ->
            %% jump to end and discard other cases (avoid scrolling big buffer)                 
            gui_replace_until(BufBot,BL,#gres{state=SN,loop=Loop,focus=-1},State0)
    end.

-spec serve_stack(atom(), #state{}) -> #state{}.
serve_stack(SN, #state{tRef=TRef} = State) when TRef =/= undefined ->
    timer:cancel(TRef),
    serve_stack(SN, State#state{tRef = undefined});
serve_stack( _, #state{stack=undefined}=State) -> 
    % no stack, nothing .. do
    State;
serve_stack( _, #state{nav=ind,bufBot=B,guiBot=B}=State) -> 
    % gui is current at the end of the buffer, no new interesting data, nothing .. do
    State;
serve_stack(completed, #state{stack={button,<<"<">>,RT}}=State0) ->
    % deferred button can be executed for backward button <<"<">> 
    ?Debug("~p stack exec ~p", [completed,<<"<">>]),
    serve_top(completed,State0#state{tailLock=true,stack=undefined,replyToFun=RT});
serve_stack(completed, #state{stack={button,<<"<<">>,RT}}=State0) ->
    % deferred button can be executed for backward button <<"<<">> 
    ?Debug("~p stack exec ~p", [completed,<<"<<">>]),
    serve_top(completed,State0#state{tailLock=true,stack=undefined,replyToFun=RT});
serve_stack(completed, #state{stack={button,_Button,RT}}=State0) ->
    % deferred button can be executed for forward buttons <<">">> <<">>">> <<">|">> <<">|...">>
    ?Debug("~p stack exec ~p", [completed,_Button]),
    serve_bot(completed,<<>>,State0#state{stack=undefined,replyToFun=RT});
serve_stack(aborted, #state{stack={button,<<"<">>,RT}}=State0) ->
    % deferred button can be executed for backward button <<"<">> 
    ?Debug("~p stack exec ~p", [aborted,<<"<">>]),
    serve_top(aborted,State0#state{tailLock=true,stack=undefined,replyToFun=RT});
serve_stack(aborted, #state{stack={button,<<"<<">>,RT}}=State0) ->
    % deferred button can be executed for backward button <<"<<">> 
    ?Debug("~p stack exec ~p", [aborted,<<"<<">>]),
    serve_top(aborted,State0#state{tailLock=true,stack=undefined,replyToFun=RT});
serve_stack(aborted, #state{stack={button,_Button,RT}}=State0) ->
    % deferred button can be executed for forward buttons <<">">> <<">>">> <<">|">> <<">|...">>
    ?Debug("~p stack exec ~p", [aborted,_Button]),
    serve_bot(aborted,<<>>,State0#state{stack=undefined,replyToFun=RT});
serve_stack(SN, #state{stack={button,<<">">>,RT},bl=BL,bufBot=BufBot,guiBot=GuiBot}=State0) ->
    case lists:member(GuiBot,keys_before(BufBot,BL-1,State0)) of
        false ->    % deferred forward can be executed now
                    ?Debug("~p stack exec ~p", [SN,<<">">>]),
                    gui_append(#gres{state=SN},State0#state{tailLock=true,stack=undefined,replyToFun=RT});
        true ->     State0#state{tailLock=true}  % buffer has not grown by 1 full block yet, keep the stack
    end;
serve_stack(SN, #state{stack={button,<<">>">>,RT},gl=GL,bufBot=BufBot,guiBot=GuiBot}=State0) ->
    case lists:member(GuiBot,keys_before(BufBot,GL-1,State0)) of
        false ->    % deferred forward can be executed now
                    ?Debug("~p stack exec ~p", [SN,<<">>">>]),
                    serve_bot(SN,<<"">>,State0#state{tailLock=true,stack=undefined, replyToFun=RT});
        true ->     State0#state{tailLock=true}  % buffer has not grown by 1 max gui length yet, keep the stack
    end;
serve_stack(SN, #state{bufCnt=BufCnt,stack={button,Target,RT}}=State0) when is_integer(Target), (BufCnt>=Target) ->
    % deferred target can be executed now
    ?Debug("~p stack exec ~p", [SN,Target]),
    serve_target(SN,Target,State0#state{tailLock=true,stack=undefined,replyToFun=RT});
serve_stack(tailing, #state{stack={button,<<">|...">>,RT}}=State0) ->
    serve_stack(tailing, State0#state{stack={button,<<"tail">>,RT}});
serve_stack(tailing, #state{stack={button,<<"...">>,RT}}=State0) ->
    serve_stack(tailing, State0#state{stack={button,<<"tail">>,RT}});
serve_stack(tailing, #state{bufCnt=BufCnt,bufBot=BufBot,guiCnt=GuiCnt,guiBot=GuiBot,guiCol=GuiCol,stack={button,<<"tail">>,RT},tailLock=TailLock,replyToFun=ReplyTo}=State0) ->
    ?Debug("~p serve_stack ~p", [tailing,<<"tail">>]),
    if
        TailLock -> 
            reply_stack(tailing, ReplyTo, State0);                  % tailing is cancelled
        (BufCnt == 0) -> State0;                                    % no data, nothing to do, keep stack
        (BufBot == GuiBot) andalso (GuiCol == false) -> State0;     % no new data, nothing to do, keep stack
        (GuiCnt == 0) ->                                            % (re)initialize to buffer bottom
            ?Debug("~p stack exec ~p", [tailing,<<"tail">>]),
            serve_bot(tailing,<<"tail">>,State0#state{stack=undefined,replyToFun=RT});
        true ->
            % serve new data at the bottom of the buffer, ask client to come back
            ?Debug("~p stack exec ~p", [tailing,<<"tail">>]),
            gui_append(#gres{state=tailing,loop= <<"tail">>,focus=-1},State0#state{stack=undefined,replyToFun=RT})
    end;
serve_stack(autofilling, #state{bufCnt=BufCnt,bufBot=BufBot,guiCnt=GuiCnt,guiBot=GuiBot,guiCol=GuiCol, stack = {button, <<">|">>, ReplyTo}} = State) ->
    if
        (BufCnt == 0) -> State;                                    % no data, nothing to do, keep stack
        (BufBot == GuiBot) andalso (GuiCol == false) -> State;     % no new data, nothing to do, keep stack
        (GuiCnt == 0) ->                                            % (re)initialize to buffer bottom
            ?Debug("~p stack exec ~p", [autofilling,<<">|">>]),
            serve_bot(autofilling, <<">|">>, State#state{stack = undefined, replyToFun = ReplyTo});
        true ->
            % serve new data at the bottom of the buffer, ask client to come back
            ?Debug("Guicnt != 0 ~p", [autofilling,<<">|">>]),
            gui_append(#gres{state = autofilling, loop = <<">|">>, focus = -1}, State#state{stack = undefined, replyToFun=ReplyTo})
    end;
serve_stack(filling, #state{stack = {button, <<">|">>, ReplyTo}} = State) ->
    serve_bot(filling, <<"">>, State#state{stack = undefined, replyToFun = ReplyTo});
serve_stack(filling, #state{bufCnt=BufCnt,bufBot=BufBot,guiCnt=GuiCnt,guiBot=GuiBot,guiCol=GuiCol, stack = {button, Target, ReplyTo}} = State) when is_integer(Target)->
    if
        (BufCnt == 0) -> State;                                    % no data, nothing to do, keep stack
        (BufBot == GuiBot) andalso (GuiCol == false) -> State;     % no new data, nothing to do, keep stack
        (GuiCnt == 0) ->                                           % (re)initialize to buffer bottom
            ?Debug("~p stack exec ~p", [integer_to_binary(Target)]),
            serve_bot(filling, integer_to_binary(Target), State#state{stack = undefined, replyToFun = ReplyTo});
        true ->
            % serve new data at the bottom of the buffer, ask client to come back
            ?Debug("Guicnt != 0 ~p", [integer_to_binary(Target)]),
            gui_append(#gres{state = filling, loop = integer_to_binary(Target), focus = -1}, State#state{stack = undefined, replyToFun=ReplyTo})
    end;
serve_stack(SN , #state{stack = Stack} = State) ->
    ?Debug("~p serve_stack nop~p", [SN, Stack]),
    State.

-spec rows_after(integer(), integer(), #state{}) -> list().
rows_after(Key, Limit, #state{nav=raw,rowFun=RowFun,tableId=TableId}) ->
    case ets:select(TableId,[{'$1',[{'>',{element,1,'$1'},Key}],['$_']}],Limit) of
        {Rs, _Cont} ->      [gui_row_expand(R, TableId, RowFun) || R <- Rs];  
        '$end_of_table' ->  []
    end.

-spec rows_for_keys(list(), integer()) -> list().
rows_for_keys([],_) -> [];
rows_for_keys(Keys,TableId) ->
    [gui_row_as_list(hd(ets:lookup(TableId, Id))) || {_,Id} <- Keys].

-spec rows_for_ids(list(), atom() | ets:tid(), fun()) -> list().
rows_for_ids([],_,_) -> [];
rows_for_ids(Ids,TableId,RowFun) ->
    [gui_row_expand(hd(ets:lookup(TableId, Id)), TableId, RowFun) || Id <- Ids].

-spec keys_before(integer() | tuple() | [], integer(), #state{}) -> list().
keys_before(_, 0, _) -> [];
keys_before(Id, Limit, #state{nav=raw}=State) ->
    ids_before(Id, Limit, State);
keys_before(Key, Limit, #state{nav=ind,indexId=IndexId}) ->
    case ets:select_reverse(IndexId,[{'$1',[{'<',{element,1,'$1'},{const,Key}}],[{element,1,'$1'}]}],Limit) of
        {Keys, _Cont} ->    lists:reverse(Keys);  
        '$end_of_table' ->  []
    end.

-spec keys_after(integer() | tuple(), integer(), #state{}) -> list().
keys_after(_, 0, _) -> [];
keys_after(Key, Limit, #state{nav=ind,indexId=IndexId}) ->
    case ets:select(IndexId,[{'$1',[{'>',{element,1,'$1'},{const,Key}}],[{element,1,'$1'}]}],Limit) of
        {Keys, _Cont} ->    Keys;  
        '$end_of_table' ->  []
    end.

-spec rows_before(integer() | tuple(), integer(), #state{}) -> list().
rows_before(_, 0, _) -> [];
rows_before(Key, Limit, #state{nav=raw,rowFun=RowFun,tableId=TableId}) ->
    case ets:select_reverse(TableId,[{'$1',[{'<',{element,1,'$1'},Key}],['$_']}],Limit) of
        {Rs, _Cont} ->      [gui_row_expand(R, TableId, RowFun) || R <- lists:reverse(Rs)];  
        '$end_of_table' ->  []
    end;
rows_before(Key, Limit, #state{tableId=TableId}=State) ->
    Keys = keys_before(Key, Limit, State),
    [gui_row_as_list(ets:lookup(TableId, Id)) || {_,Id} <- Keys].

-spec ids_before(integer(), integer(), #state{}) -> list().
ids_before(_, 0, _) -> [];
ids_before(Id, Limit, #state{nav=raw,tableId=TableId}) ->
    case ets:select_reverse(TableId,[{'$1',[{'<',{element,1,'$1'},Id}],[{element,1,'$1'}]}],Limit) of
        {Ids, _Cont} ->     lists:reverse(Ids);  
        '$end_of_table' ->  []
    end.

-spec ids_after(integer(), integer(), #state{}) -> list().
ids_after(_, 0, _) -> [];
ids_after(Id, Limit, #state{nav=raw,tableId=TableId}) ->
    case ets:select(TableId,[{'$1',[{'>',{element,1,'$1'},Id}],[{element,1,'$1'}]}],Limit) of
        {Ids, _Cont} ->     Ids;  
        '$end_of_table' ->  []
    end.

-spec key_times_2(integer() | tuple(), #state{}) -> integer() | tuple() | undefined.
key_times_2(Key,#state{nav=raw}) ->
    Key+Key;    % hd(ids_before(Key+Key, 1, State)) if within buffer
key_times_2(Key,#state{nav=ind,indexId=IndexId}) ->
    key_at_pos(IndexId,2*key_pos(IndexId,Key)).

-spec key_div_2(integer() | tuple(), #state{}) -> integer() | tuple().
key_div_2(Key,#state{nav=raw,bufTop=BufTop}=State) ->
    hd(ids_after((Key-BufTop) div 2, 1, State));
key_div_2(Key,#state{nav=ind,indexId=IndexId}) ->
    key_at_pos(IndexId,(key_pos(IndexId,Key)+1) div 2).

-spec key_pos(integer(), integer() | tuple()) -> undefined | integer().
key_pos(Tid,Key) -> key_pos(Tid,Key,ets:first(Tid),1).

-spec key_pos(integer(), integer() | tuple(), integer() | tuple(), integer()) -> integer().
key_pos(_Tid,Key,Key,Pos) -> Pos;
key_pos(_Tid,_,'$end_of_table',_) -> undefined;
key_pos(Tid,Key,Probe,Pos) -> key_pos(Tid,Key,ets:next(Tid,Probe),Pos+1).

-spec key_at_pos(integer(), integer()) -> undefined | integer() | tuple().
key_at_pos(Tid,Pos) -> key_at_pos(Tid,Pos,ets:first(Tid)).

-spec key_at_pos(integer(), undefined | integer(), integer() | tuple()) -> undefined | integer() | tuple().
key_at_pos(_Tid,undefined,_) -> undefined;
key_at_pos(_Tid,_,'$end_of_table') -> undefined;
key_at_pos(_Tid,1,Probe) -> Probe;
key_at_pos(Tid,Pos,Probe) -> key_at_pos(Tid,Pos-1,ets:next(Tid,Probe)).

% gui_row_as_list({}) ->
%     [];
-spec gui_row_as_list(tuple()) -> list().
gui_row_as_list(FullRowTuple) ->
    List = tuple_to_list(FullRowTuple),
    [hd(List),lists:nth(2,List)|lists:nthtail(3,List)].

% gui_row_expand({}, _, _) ->
%     [];
-spec gui_row_expand(tuple(), integer(), fun()) -> list().
gui_row_expand({I,Op,RK}, TableId, RowFun) ->
    Row = RowFun(RK),
    ets:insert(TableId, list_to_tuple([I, Op, RK | Row])),
    [I,Op|Row];
gui_row_expand(FullRowTuple, _TableId, _RowFun) ->
    List = tuple_to_list(FullRowTuple),
    [hd(List),lists:nth(2,List)|lists:nthtail(3,List)].

-spec raw_row_expand(tuple(), fun()) -> tuple().
raw_row_expand({I,Op,RK}, RowFun) ->
    list_to_tuple([I, Op, RK | RowFun(RK)]).

-spec data_clear(#state{}) -> #state{}.
data_clear(State) -> 
    gui_clear(ind_clear(raw_clear(State))).

-spec raw_clear(#state{}) -> #state{}.
raw_clear(#state{tableId=TableId}=State) -> 
    ?Debug("raw_clear"),
    true = ets:delete_all_objects(TableId),    
    Default = #state{}, 
    set_buf_counters(State#state{ rawCnt = Default#state.rawCnt
                                , rawTop = Default#state.rawTop          
                                , rawBot = Default#state.rawBot          
                                , dirtyCnt = Default#state.dirtyCnt
                                , dirtyTop = Default#state.dirtyTop 
                                , dirtyBot = Default#state.dirtyBot        
                    }). 

-spec ind_clear(#state{}) -> #state{}.
ind_clear(#state{indexId=IndexId}=State) -> 
    ?Debug("ind_clear"),
    true = ets:delete_all_objects(IndexId),    
    Default = #state{}, 
    set_buf_counters(State#state{ indCnt = Default#state.indCnt
                                , indTop = Default#state.indTop                
                                , indBot = Default#state.indBot
                    }). 

-spec gui_clear(#state{}) -> #state{}.
gui_clear(State) -> 
    ?Debug("gui_clear"),
    Default = #state{}, 
    State#state{  guiCnt = Default#state.guiCnt
                , guiTop = Default#state.guiTop         
                , guiBot = Default#state.guiBot         
                , guiCol = Default#state.guiCol         
                }. 

-spec data_append(atom(), tuple(), #state{}) -> #state{}.
data_append(SN, {[],_Complete},#state{nav=Nav,rawBot=RawBot}=State0) -> 
    NewPfc=State0#state.pfc-1,
    ?Debug("data_append -~p- count ~p bufBottom ~p pfc ~p", [Nav,0,RawBot,NewPfc]),
    serve_stack(SN, State0#state{pfc=NewPfc});
data_append(SN, {Recs,_Complete},#state{nav=raw,tableId=TableId,rawCnt=RawCnt,rawTop=RawTop,rawBot=RawBot}=State0) ->
    NewPfc=State0#state.pfc-1,
    Cnt = length(Recs),
    NewRawCnt = RawCnt+Cnt,
    NewRawTop = min(RawTop,RawBot+1),   % initialized .. 1 and then changed only in delete or clear
    NewRawBot = RawBot+Cnt,
    ?Debug("data_append count ~p bufBot ~p pfc ~p", [Cnt,NewRawBot,NewPfc]),
    ets:insert(TableId, [list_to_tuple([I,nop|[R]])||{I,R}<-lists:zip(lists:seq(RawBot+1, NewRawBot), Recs)]),
    serve_stack(SN, set_buf_counters(State0#state{pfc=NewPfc,rawCnt=NewRawCnt,rawTop=NewRawTop,rawBot=NewRawBot}));
data_append(SN, {Recs,_Complete},#state{nav=ind,tableId=TableId,indexId=IndexId
        ,rawCnt=RawCnt,rawTop=RawTop,rawBot=RawBot,indCnt=IndCnt
        ,guiTop=GuiTop,guiBot=GuiBot,guiCol=GuiCol
        ,rowFun=RowFun,filterFun=FilterFun,sortFun=SortFun}=State0) ->
    NewPfc=State0#state.pfc-1,
    Cnt = length(Recs),
    NewRawCnt = RawCnt+Cnt,
    NewRawTop = min(RawTop,RawBot+1),   % initialized .. 1 and then changed only in delete or clear
    NewRawBot = RawBot+Cnt,
    RawRows = [raw_row_expand({I,nop,RK}, RowFun) || {I,RK} <- lists:zip(lists:seq(RawBot+1, NewRawBot), Recs)],
    ets:insert(TableId, RawRows),
    IndRows = [?IndRec(R,SortFun) || R <- lists:filter(FilterFun,RawRows)],
    ?Debug("data_append -IndRows- ~p", [IndRows]),
    FunCol = fun({X,_},{IT,IB,C}) ->  {IT,IB,(C orelse ((X>IT) and (X<IB)))}  end, 
    {_,_,Collision} = lists:foldl(FunCol, {GuiTop, GuiBot, false}, IndRows),    %% detect data collisions with gui content
    ets:insert(IndexId, IndRows),
    NewIndCnt = IndCnt + length(IndRows),
    {NewIndTop,NewIndBot} = case NewIndCnt of
        0 ->    {?IndMax,?IndMin};
        _ ->    {ets:first(IndexId),ets:last(IndexId)}
    end,
    NewGuiCol = (GuiCol or Collision),    
    ?Debug("data_append count ~p bufBot ~p pfc=~p stale=~p", [Cnt,NewRawBot,NewPfc,NewGuiCol]),
    serve_stack(SN, set_buf_counters(State0#state{ pfc=NewPfc
                                                , rawCnt=NewRawCnt,rawTop=NewRawTop,rawBot=NewRawBot
                                                , indCnt=NewIndCnt,indTop=NewIndTop,indBot=NewIndBot
                                                , guiCol=NewGuiCol}
                                    )
                ).

-spec data_reorder(atom(), list(), #state{}) -> #state{}.
data_reorder(SN,ColOrder,#state{sortSpec=SortSpec,filterSpec=FilterSpec}=State0) ->
    ?Debug("data_sort ~p data_filter ~p col_order ~p", [SortSpec,FilterSpec,ColOrder]),
    State1 = State0#state{colOrder=ColOrder},
    case filter_and_sort(FilterSpec, SortSpec, ColOrder, State0) of
        {ok, NewSql, _} ->
            gui_nop(#gres{state=SN}, State1#state{sql=NewSql});
        {error, _Error} ->
            gui_nop(#gres{state=SN,beep=true}, State1)
    end.    

-spec data_filter(atom(), [{atom() | integer(), term()}], #state{}) -> #state{}.
data_filter(SN,?NoFilter,#state{nav=raw}=State0) ->
    %% No filter in place
    ?Debug("data_filter ~p", [?NoFilter]),
    gui_nop(#gres{state=SN,beep=true}, State0);
data_filter(SN,?NoFilter,#state{nav=ind,srt=false,colOrder=ColOrder}=State0) ->
    %% No sort in place, clear index table and go to raw navigation
    ?Debug("data_sort ~p data_filter ~p col_order ~p", [?NoSort,?NoFilter,ColOrder]),
    State1 = gui_clear(ind_clear(State0#state{nav=raw})),
    case filter_and_sort(?NoFilter, ?NoSort, ColOrder, State0) of
        {ok, NewSql, _} ->
            serve_top(SN, State1#state{filterSpec=?NoFilter, sql=NewSql});
        {error, _Error} ->
            serve_top(SN, State1)
    end;
data_filter(SN,FilterSpec,#state{sortSpec=SortSpec,sortFun=SortFun,colOrder=ColOrder}=State0) ->
    ?Debug("data_sort ~p data_filter ~p col_order ~p", [SortSpec,FilterSpec,ColOrder]),
    State1 = data_index(SortFun,FilterSpec,State0),
    case filter_and_sort(FilterSpec, SortSpec, ColOrder, State0) of
        {ok, NewSql, _} ->
            serve_top(SN, State1#state{filterSpec=FilterSpec, sql=NewSql});
        {error, _Error} ->
            serve_top(SN, State1)
    end.

-spec data_sort(atom(), [{integer() | binary(),boolean()}], #state{}) -> #state{}.
data_sort(SN,?NoSort,#state{srt=false}=State0) ->
    %% No sort in place
    ?Debug("data_sort ~p", [?NoSort]),
    gui_nop(#gres{state=SN,beep=true}, State0);
data_sort(SN,?NoSort,#state{filterSpec=?NoFilter,colOrder=ColOrder}=State0) ->
    %% No filter in place, clear index table and go to raw navigation
    ?Debug("data_sort ~p data_filter ~p col_order ~p", [?NoSort,?NoFilter,ColOrder]),
    State1 = gui_clear(ind_clear(State0#state{nav=raw,srt=false})),
    case filter_and_sort(?NoFilter, ?NoSort, ColOrder, State0) of
        {ok, NewSql, _} ->
            serve_top(SN, State1#state{sortSpec=?NoSort, sql=NewSql});
        {error, _Error} ->
            serve_top(SN, State1)
    end;    
data_sort(SN,SortSpec,#state{filterSpec=FilterSpec,colOrder=ColOrder}=State0) ->
    ?Debug("data_sort ~p data_filter ~p col_order ~p", [SortSpec,FilterSpec,ColOrder]),
    case filter_and_sort(FilterSpec, SortSpec, ColOrder, State0) of
        {ok, NewSql, NewSortFun} ->
            ?Debug("data_sort NewSql=~p NewSortFun=~p", [NewSql,NewSortFun]),
            State1 = data_index(NewSortFun,FilterSpec,State0),
            serve_top(SN, State1#state{sortSpec=SortSpec, sql=NewSql});
        {error, Error} ->
            Message = list_to_binary(io_lib:format("~p",[Error])),
            gui_nop(#gres{state=SN,beep=true,message=Message}, State0)
    end.

-spec data_index(fun(), [{atom() | integer(), term()}], #state{}) -> #state{}.
data_index(SortFun,FilterSpec, #state{tableId=TableId,indexId=IndexId,rowFun=RowFun}=State0) ->
    FilterFun = filter_fun(FilterSpec),
    {Nav,Srt} = navigation_type(SortFun,FilterSpec),
    State1 = ind_clear(State0),
    CompleteFun = fun
        ({Id,Op,RK},Acc) ->
            ets:insert(TableId, raw_row_expand({Id,Op,RK}, RowFun)),
            Acc+1;
        (_,Acc) ->  
            Acc
    end, 
    ets:foldl(CompleteFun, 0, TableId),
    IndexFun = fun(R,Acc) -> 
                case FilterFun(R) of 
                    true ->
                        ets:insert(IndexId, ?IndRec(R,SortFun)),
                        Acc+1;
                    false -> 
                        Acc
                end
    end, 
    IndCnt = ets:foldl(IndexFun, 0, TableId),
    {IndTop,IndBot} = case IndCnt of
        0 ->    {?IndMax,?IndMin};
        _ ->    {ets:first(IndexId),ets:last(IndexId)}
    end,
    ?Debug("data_index nav=~p Srt=~p IndCnt=~p IndTop=~p IndBot=~p",[Nav,Srt,IndCnt,IndTop,IndBot]),
    set_buf_counters(State1#state{nav=Nav,srt=Srt
        ,sortFun=SortFun,filterSpec=FilterSpec,filterFun=FilterFun
        ,indCnt=IndCnt,indTop=IndTop,indBot=IndBot}).

-spec data_update(atom(), list(), #state{}) -> #state{}.
data_update(SN,ChangeList,#state{stmtColsCount=StmtColsCount}=State0) ->
    {State1,InsRows} = data_update_rows(ChangeList,StmtColsCount,State0,[]),
    ?Debug("InsRows ~p",[InsRows]),
    gui_ins(#gres{state=SN,rows=InsRows}, State1).

-spec data_update_rows(list(), integer(), #state{}, list()) -> {#state{}, list()}.
data_update_rows([], _, State0, Acc) -> {set_buf_counters(State0), lists:reverse(Acc)};
data_update_rows([Ch|ChangeList], ColCount, State0, Acc) ->
    case data_update_row(Ch, ColCount, State0) of
        {[], S1} ->     data_update_rows(ChangeList, ColCount, S1, Acc);         %% upd / del
        {[Row],S2} ->   data_update_rows(ChangeList, ColCount, S2, [Row|Acc])    %% ins
    end.

-spec data_update_row(tuple(), integer(), #state{}) -> tuple().
data_update_row({Id,Op,Fields}, _ColCount, #state{tableId=TableId}=State0) when is_integer(Id) ->
    [OldRow] = ets:lookup(TableId, Id),
    ?Debug("OldRow/Ch ~p ~p", [OldRow,{Id,Op,Fields}]),
    {O,NewTuple,State1} = case {element(2,OldRow),Op} of
        {nop,upd} ->    DT =  min(State0#state.dirtyTop,Id),
                        DB =  max(State0#state.dirtyBot,Id),
                        DC =  State0#state.dirtyCnt+1,
                        {Op,  upd_tuple(Fields,OldRow), State0#state{dirtyTop=DT,dirtyBot=DB,dirtyCnt=DC}};
        {nop,del} ->    DT =  min(State0#state.dirtyTop,Id),
                        DB =  max(State0#state.dirtyBot,Id),
                        DC =  State0#state.dirtyCnt+1,
                        {Op,  upd_tuple(Fields,OldRow), State0#state{dirtyTop=DT,dirtyBot=DB,dirtyCnt=DC}};
        {del,del} ->    {del, upd_tuple(Fields,OldRow), State0};
        {del,upd} ->    {upd, upd_tuple(Fields,OldRow), State0};
        {ins,upd} ->    {ins, upd_tuple(Fields,OldRow), State0};
        {ins,del} ->    DC =  State0#state.dirtyCnt-1,
                        {nop, undefined, State0#state{dirtyCnt=DC}};
        {upd,upd} ->    {upd, upd_tuple(Fields,OldRow), State0};        
        {upd,del} ->    {del, upd_tuple(Fields,OldRow), State0}        
    end,
    case {element(2,OldRow),Op} of 
        {ins,del} ->    ets:delete(TableId, Id);
        _ ->            ets:insert(TableId, setelement(2, NewTuple, O))
    end,
    RawCnt=ets:info(TableId,size),
    {RawTop,RawBot} = case RawCnt of
        0 ->    {?RawMax,?RawMin};
        _ ->    {ets:first(TableId),ets:last(TableId)}
    end,
    {[],State1#state{rawCnt=RawCnt,rawTop=RawTop,rawBot=RawBot}};
data_update_row({_,ins,Fields}, ColCount, #state{nav=raw,tableId=TableId,rawCnt=RawCnt,rawBot=RawBot,guiCnt=GuiCnt,dirtyTop=DT0,dirtyCnt=DC0}=State0) ->
    Id = RawBot+1,          
    ?Debug("insert fields ~p", [Fields]),
    RowAsList = [Id,ins,{?NoKey}|tuple_to_list(ins_tuple(Fields,ColCount))],
    ets:insert(TableId, list_to_tuple(RowAsList)),
    {[[Id,ins|lists:nthtail(3, RowAsList)]],State0#state{rawCnt=RawCnt+1,rawBot=Id,guiCnt=GuiCnt+1,dirtyTop=min(DT0,Id),dirtyBot=Id,dirtyCnt=DC0+1}};
data_update_row({_,ins,Fields}, ColCount, #state{nav=ind,tableId=TableId,rawCnt=RawCnt,rawBot=RawBot,guiCnt=GuiCnt,dirtyTop=DT0,dirtyCnt=DC0}=State0) ->
    Id = RawBot+1,          
    ?Debug("insert fields ~p", [Fields]),
    RowAsList = [Id,ins,{?NoKey}|tuple_to_list(ins_tuple(Fields,ColCount))],
    ets:insert(TableId, list_to_tuple(RowAsList)),    
    {[[Id,ins|lists:nthtail(3, RowAsList)]],State0#state{rawCnt=RawCnt+1,rawBot=Id,guiCnt=GuiCnt+1,dirtyTop=min(DT0,Id),dirtyBot=Id,dirtyCnt=DC0+1}}.

-spec ins_tuple([tuple()], integer()) -> tuple().
ins_tuple(Fields,ColCount) ->
    ins_tuple(Fields,ColCount,erlang:make_tuple(ColCount, <<>>)).

-spec ins_tuple(list(), integer(), tuple()) -> tuple().
ins_tuple([],_,Tuple) -> Tuple;
ins_tuple([{Cp,Value}|Fields],ColCount,Tuple) when is_integer(Cp) ->
    ins_tuple(Fields,ColCount,setelement(Cp, Tuple, Value)).

-spec upd_tuple(list(), tuple()) -> tuple().
upd_tuple([],Tuple) -> Tuple;
upd_tuple([{Cp,Value}|Fields],Tuple) ->
    upd_tuple(Fields,setelement(Cp+3, Tuple, Value)).

-spec data_commit(atom(), #state{}) -> tuple().
data_commit(SN, #state{nav=Nav,gl=GL,tableId=TableId,indexId=IndexId
                      ,rowFun=RowFun,sortFun=SortFun,filterFun=FilterFun,guiTop=GuiTop0
                      ,dirtyCnt=DirtyCnt,dirtyTop=DirtyTop,dirtyBot=DirtyBot}=State0) ->
    ChangeList = change_list(TableId, DirtyCnt, DirtyTop, DirtyBot),
    ?Debug("ChangeList length must match DirtyCnt~n~p ~p",[length(ChangeList),DirtyCnt]),
    ?Debug("ChangeList~n~p",[ChangeList]),
    case update_cursor_prepare(ChangeList,State0) of
        ok ->
            NewSN = data_commit_state_name(SN),
            case update_cursor_execute(optimistic, State0) of
                {_,ExecErr} ->
                    ExecMessage = list_to_binary(io_lib:format("~p",[ExecErr])),
                    {NewSN,gui_nop(#gres{state=NewSN,beep=true,message=ExecMessage},State0)};
                ChangedKeys ->
                    {GuiCnt,GuiTop,GuiBot} = case Nav of
                        raw ->  data_commit_raw(TableId,ChangedKeys,0,?RawMax,?RawMin);
                        ind ->  data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangedKeys,0,?IndMax,?IndMin)
                    end,
                    case {change_list(TableId, DirtyCnt, DirtyTop, DirtyBot),GuiCnt} of
                        {[],0} ->
                            State1 = reset_buf_counters(State0#state{dirtyCnt=0,dirtyTop=?RawMax,dirtyBot=?RawMin}),
                            ?Debug("commit result Nav / GuiTop0~n~p, ~p, ~p", [Nav, {GuiCnt,GuiTop,GuiBot},GuiTop0]),
                            {NewSN,gui_replace_from(GuiTop0,GL,#gres{state=NewSN,focus=1},State1)};
                        {[],_} ->
                            State1 = reset_buf_counters(State0#state{dirtyCnt=0,dirtyTop=?RawMax,dirtyBot=?RawMin}),
                            ?Debug("commit result Nav / GuiTop0~n~p, ~p, ~p", [Nav, {GuiCnt,GuiTop,GuiBot},GuiTop0]),
                            {NewSN,gui_replace_from(GuiTop,GL,#gres{state=NewSN,focus=1},State1)};
                        {DL,_} ->
                            ?Error("Dirty rows after commit~n~p",[DL]),
                            Message = <<"Dirty rows after commit">>,
                            State1 = reset_buf_counters(State0),
                            {NewSN,gui_replace_from(GuiTop,GL,#gres{state=NewSN,focus=1,message=Message},State1)}
                    end
            end;
        {ok, UpdRef} ->
            NewSN = data_commit_state_name(SN),
            case update_cursor_execute(optimistic, State0, UpdRef) of
                {_,ExecErr} ->
                    ExecMessage = list_to_binary(io_lib:format("~p",[ExecErr])),
                    {NewSN,gui_nop(#gres{state=NewSN,beep=true,message=ExecMessage},State0)};
                ChangedKeys ->
                    {GuiCnt,GuiTop,GuiBot} = case Nav of
                        raw ->  data_commit_raw(TableId,ChangedKeys,0,?RawMax,?RawMin);
                        ind ->  data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangedKeys,0,?IndMax,?IndMin)
                    end,
                    case {change_list(TableId, DirtyCnt, DirtyTop, DirtyBot),GuiCnt} of
                        {[],0} ->   
                            State1 = reset_buf_counters(State0#state{dirtyCnt=0,dirtyTop=?RawMax,dirtyBot=?RawMin}),
                            ?Debug("commit result Nav / GuiTop0~n~p, ~p, ~p", [Nav, {GuiCnt,GuiTop,GuiBot},GuiTop0]),
                            {NewSN,gui_replace_from(GuiTop0,GL,#gres{state=NewSN,focus=1},State1)};
                        {[],_} ->   
                            State1 = reset_buf_counters(State0#state{dirtyCnt=0,dirtyTop=?RawMax,dirtyBot=?RawMin}),
                            ?Debug("commit result Nav / GuiTop0~n~p, ~p, ~p", [Nav, {GuiCnt,GuiTop,GuiBot},GuiTop0]),
                            {NewSN,gui_replace_from(GuiTop,GL,#gres{state=NewSN,focus=1},State1)};
                        {DL,_} ->   
                            ?Error("Dirty rows after commit~n~p",[DL]),
                            Message = <<"Dirty rows after commit">>,
                            State1 = reset_buf_counters(State0),
                            {NewSN,gui_replace_from(GuiTop,GL,#gres{state=NewSN,focus=1,message=Message},State1)}
                    end
            end;
        {_,PrepError} ->
            %%       serve errors if present (no matter the size)
            PrepMessage = list_to_binary(io_lib:format("~p",[PrepError])),
            {SN,gui_nop(#gres{state=SN,beep=true,message=PrepMessage},State0)}
    end.

-spec data_commit_state_name(atom()) -> atom().
data_commit_state_name(SN) ->
    case SN of 
        filling ->      aborted;
        autofilling ->  aborted;
        tailing ->      tailing;
        _ ->            SN
    end.    

-spec data_commit_raw(integer(), list(), integer(), integer(), integer()) -> tuple().
data_commit_raw(_,[],GuiCnt,GuiTop,GuiBot) -> {GuiCnt,GuiTop,GuiBot};
data_commit_raw(TableId,[{Id,NK}|ChangedKeys],GuiCnt,GuiTop,GuiBot) when (element(1,NK)==?NoKey)  ->
    ets:delete(TableId,Id),    
    data_commit_raw(TableId,ChangedKeys,GuiCnt,GuiTop,GuiBot);
data_commit_raw(TableId,[{Id,NK}|ChangedKeys],GuiCnt,GuiTop,GuiBot) ->
    ets:insert(TableId,{Id,nop,NK}),    
    data_commit_raw(TableId,ChangedKeys,GuiCnt+1,min(GuiTop,Id),max(GuiBot,Id)).

-spec data_commit_ind(integer(), integer(), fun(), fun(), fun(), list(), integer(), tuple(), tuple()) -> tuple().
data_commit_ind(_,_,_,_,_,[],GuiCnt,GuiTop,GuiBot) -> {GuiCnt,GuiTop,GuiBot};
data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,[{Id,NK}|ChangedKeys],GuiCnt,GuiTop,GuiBot) when (element(1,NK)==?NoKey)  ->
    [OldRow] = ets:lookup(TableId,Id),
    ets:delete(IndexId,?IndKey(OldRow,SortFun)),
    ets:delete(TableId,Id),    
    data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangedKeys,GuiCnt,GuiTop,GuiBot);
data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,[{Id,NK}|ChangedKeys],GuiCnt,GuiTop,GuiBot) ->
    [OldRow] = ets:lookup(TableId,Id),
    case(element(2,OldRow)) of
        ins -> ok;
        _ -> ets:delete(IndexId,?IndKey(OldRow,SortFun))
    end,
    NewRow = raw_row_expand({Id,nop,NK}, RowFun),
    ets:insert(TableId,NewRow),
    case FilterFun(NewRow) of
        true ->
            NewKey = ?IndKey(NewRow,SortFun),
            ets:insert(IndexId,{NewKey,Id}),
            data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangedKeys,GuiCnt+1,min(GuiTop,NewKey),max(GuiBot,NewKey));
        false ->
            data_commit_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangedKeys,GuiCnt,GuiTop,GuiBot)
    end.

-spec data_rollback(atom(), #state{})-> #state{}.
data_rollback(SN, #state{nav=Nav,gl=GL,tableId=TableId,indexId=IndexId
                      ,rowFun=RowFun,sortFun=SortFun,filterFun=FilterFun
                      ,dirtyCnt=DirtyCnt,dirtyTop=DirtyTop,dirtyBot=DirtyBot
                      ,guiTop=GuiTop0}=State0) -> 
    ChangeList = change_tuples(TableId, DirtyCnt, DirtyTop, DirtyBot),
    {_GuiCnt,GuiTop,_GuiBot} = case Nav of
        raw ->  data_rollback_raw(TableId,ChangeList,0,?RawMax,?RawMin);
        ind ->  data_rollback_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangeList,0,?IndMax,?IndMin)
    end,
    ?Debug("rollback ChangeList ~p GuiCnt ~p GuiTop ~p",[ChangeList,_GuiCnt, GuiTop]),
    case change_list(TableId, DirtyCnt, DirtyTop, DirtyBot) of
        [] ->   State1 = State0#state{dirtyCnt=0,dirtyTop=?RawMax,dirtyBot=?RawMin},
                gui_replace_from(GuiTop0,GL,#gres{state=SN,focus=1},reset_buf_counters(State1));
        DL ->   ?Error("Dirty rows after rollback ~p",[DL]),
                Message = <<"Dirty rows after rollback">>,
                gui_replace_from(GuiTop,GL,#gres{state=SN,focus=1,message=Message},reset_buf_counters(State0))
    end.

-spec data_rollback_raw(integer(), list(), integer(), integer(), integer()) -> {integer(), integer(), integer()}.
data_rollback_raw(_,[],GuiCnt,GuiTop,GuiBot) -> {GuiCnt,GuiTop,GuiBot};
data_rollback_raw(TableId,[Row|ChangeList],GuiCnt,GuiTop,GuiBot) when (element(1,element(3,Row))==?NoKey)  ->
    Id = element(1,Row),
    ets:delete(TableId,Id),    
    data_rollback_raw(TableId,ChangeList,GuiCnt,GuiTop,GuiBot);
data_rollback_raw(TableId,[Row|ChangeList],GuiCnt,GuiTop,GuiBot) ->
    Id = element(1,Row),
    ets:insert(TableId,{Id,nop,element(3,Row)}),    
    data_rollback_raw(TableId,ChangeList,GuiCnt+1,min(GuiTop,Id),max(GuiBot,Id)).

-spec data_rollback_ind(integer(), integer(), fun(), fun(), fun(), list(), integer(), tuple(), tuple()) -> {integer(), tuple(), tuple()}.
data_rollback_ind(_,_,_,_,_,[],GuiCnt,GuiTop,GuiBot) -> {GuiCnt,GuiTop,GuiBot};
data_rollback_ind(TableId,IndexId,RowFun,SortFun,FilterFun,[Row|ChangeList],GuiCnt,GuiTop,GuiBot) when (element(1,element(3,Row))==?NoKey) ->
    Id = element(1,Row),
    ets:delete(TableId,Id),    
    data_rollback_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangeList,GuiCnt,GuiTop,GuiBot);
data_rollback_ind(TableId,IndexId,RowFun,SortFun,FilterFun,[Row|ChangeList],GuiCnt,GuiTop,GuiBot) ->
    Id = element(1,Row),
    RestRow = raw_row_expand({Id,nop,element(3,Row)}, RowFun),
    ets:insert(TableId,RestRow),
    case FilterFun(RestRow) of
        true ->     
            RestKey = ?IndKey(RestRow,SortFun),
            ets:insert(IndexId,{RestKey,Id}),
            data_rollback_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangeList,GuiCnt+1,min(GuiTop,RestKey),max(GuiBot,RestKey));
        false ->    
            data_rollback_ind(TableId,IndexId,RowFun,SortFun,FilterFun,ChangeList,GuiCnt,GuiTop,GuiBot)
    end.

-spec change_tuples(integer(), integer(), integer(), integer()) -> list().
change_tuples(TableId, _DirtyCnt, DirtyTop, DirtyBot) ->
    Guard = [{'>=',{element,1,'$1'},DirtyTop}
            ,{'=<',{element,1,'$1'},DirtyBot}
            ,{'=/=',nop,{element,2,'$1'}}],
    ets:select(TableId,[{'$1',Guard,['$_']}]). 

-spec change_list(integer(), integer(), integer(), integer()) -> list().
change_list(TableId, DirtyCnt, DirtyTop, DirtyBot) ->
    [tuple_to_list(R) || R <- change_tuples(TableId, DirtyCnt, DirtyTop, DirtyBot)]. 

