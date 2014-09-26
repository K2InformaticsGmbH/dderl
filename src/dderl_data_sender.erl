-module(dderl_data_sender).

-behaviour(gen_server).

-include("dderl.hrl").

-export([start_link/2
        ,connect/2
        ,get_data_info/1
        ,fetch_first_block/1
        ,more_data/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {statement        :: {atom, pid()}
               ,column_pos       :: [integer()]
               ,table_id         :: ets:tid()
               ,index_id         :: ets:tid()
               ,nav              :: raw | ind
               ,continuation     :: ets:continuation()
               ,row_fun          :: fun()
               ,fsm_monitor      :: reference()
               ,receiver_pid     :: pid()
               ,receiver_monitor :: reference()}).

-define(CONNECT_TIMEOUT, 100000).
-define(BLOCK_SIZE, 100). %% TODO: Change this maybe to a receiver parameter ?.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API Sender-Receiver Communication (Sender API)
%%
% connect: Sync call, established a connection between server and receiver.
% get_data_info: Async request columns and types in the order they will be
%                exported, also how many rows are available at the moment.
% fetch_first_block: Async request, to initialize the data request process
% more_data : Async request of a block of data.

-spec start_link({atom(), pid()}, [integer()]) -> {ok, pid()} | {error, term()} | ignore.
start_link(Statement, ColumnPositions) ->
	gen_server:start_link(?MODULE, [Statement, ColumnPositions], []).

-spec connect(pid(), pid()) -> ok | {error, binary()}.
connect(SenderPid, ReceiverPid) ->
    gen_server:call(SenderPid, {connect, ReceiverPid}).

-spec get_data_info(pid()) -> ok.
get_data_info(SenderPid) ->
    gen_server:cast(SenderPid, get_data_info).

-spec fetch_first_block(pid()) -> ok.
fetch_first_block(SenderPid) ->
    gen_server:cast(SenderPid, fetch_first_block).

-spec more_data(pid()) -> ok.
more_data(SenderPid) ->
    gen_server:cast(SenderPid, more_data).

%% Gen server callbacks
init([{dderl_fsm, StmtPid} = Statement, ColumnPositions]) ->
    FsmMonitorRef = erlang:monitor(process, StmtPid),
    State = #state{statement   = Statement
                  ,column_pos  = ColumnPositions
                  ,fsm_monitor = FsmMonitorRef},
    {ok, State, ?CONNECT_TIMEOUT}.

handle_call({connect, ReceiverPid}, _From, #state{receiver_monitor = undefined} = State) ->
    ?Info("Connect request received from ~p", [ReceiverPid]),
    ReceiverMonitor = erlang:monitor(process, ReceiverPid),
    {reply, ok, State#state{receiver_monitor = ReceiverMonitor, receiver_pid = ReceiverPid}};
handle_call({connect, ReceiverPid}, _From, #state{} = State) ->
    ?Error("Receiver ~p trying to connect with sender ~p already connected", [ReceiverPid, self()]),
    {reply, {error, <<"Sender already connected">>}, State};
handle_call(Req, _From, State) ->
    ?Info("~p received Unexpected call ~p", [self(), Req]),
    {reply, {not_supported, Req}, State}.

handle_cast(get_data_info, #state{statement = Statement, receiver_pid = ReceiverPid, column_pos = ColumnPos} = State) ->
    %% TODO: Maybe we will need sql to check for same table sender-receiver.
    {TableId, IndexId, Nav, RowFun, Columns} = Statement:get_sender_params(),
    Size = ets:info(TableId, size),
    SelectedColumns = [lists:nth(Col, Columns) || Col <- ColumnPos],
    ?Debug("The parameters from the fsm ~p", [{TableId, IndexId, Nav, RowFun, SelectedColumns, Size}]),
    dderl_data_receiver:data_info(ReceiverPid, {SelectedColumns, Size}),
    {noreply, State#state{table_id = TableId, index_id = IndexId, nav = Nav, row_fun = RowFun}};
handle_cast(fetch_first_block, #state{nav = Nav, table_id = TableId, index_id = IndexId} = State) ->
    case Nav of
        raw -> UsedTable = TableId;
        ind -> UsedTable = IndexId
    end,
    FirstKey = ets:first(UsedTable),
    {Rows, Continuation} = case rows_from(UsedTable, FirstKey, ?BLOCK_SIZE) of
        '$end_of_table' -> {'$end_of_table', '$end_of_table'};
        Result -> Result
    end,
    send_rows(Rows, State),
    {noreply, State#state{continuation = Continuation}};
handle_cast(more_data, #state{continuation = Continuation} = State) ->
    {Rows, NewContinuation} = case ets:select(Continuation) of
        '$end_of_table' -> {'$end_of_table', '$end_of_table'};
        Result-> Result
    end,
    ?Debug("Continuation reading more data ~p", [NewContinuation]),
    send_rows(Rows, State),
    {noreply, State#state{continuation = NewContinuation}};
handle_cast(Req, State) ->
    ?Info("~p received unknown cast ~p", [self(), Req]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, StmtFsmPid, Reason}, #state{fsm_monitor = Ref} = State) ->
    ?Info("fsm process ~p down with reason ~p, monitor ref ~p", [StmtFsmPid, Reason, Ref]),
    {stop, {shutdown, <<"Statement terminated">>}, State};
handle_info({'DOWN', Ref, process, ReceiverPid, Reason}, #state{} = State) ->
    ?Info("data receiver process ~p down with reason ~p, monitor ref ~p", [ReceiverPid, Reason, Ref]),
    {stop, {shutdown, <<"Receiver terminated">>}, State};
handle_info(timeout, #state{} = State) ->
    ?Info("timeout in data sender ~p after ~p seconds without a connection", [self(), ?CONNECT_TIMEOUT div 1000]),
    {stop, {shutdown, timeout}, State};
handle_info(Msg, State) ->
    ?Info("~p received unknown msg ~p", [self(), Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    ?Info("~p ~p terminating, reason ~p", [?MODULE, self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helper internal functions
rows_from(TableId, Key, Limit) ->
    ets:select(TableId, [{'$1', [{'>=',{element,1,'$1'}, {const, Key}}],['$_']}], Limit).

send_rows('$end_of_table', #state{receiver_pid = ReceiverPid}) ->
    dderl_data_receiver:data(ReceiverPid, '$end_of_table');
send_rows(Rows, #state{receiver_pid = ReceiverPid} = State) ->
    dderl_data_receiver:data(ReceiverPid, expand_rows(Rows, State)).

expand_rows([], _) -> [];
expand_rows([{_, Id} | RestRows], #state{table_id = TableId} = State) ->
    Row = lists:nth(1, ets:lookup(TableId, Id)),
    expand_rows([Row | RestRows], State);
expand_rows([{_I,_Op, RK} | RestRows], #state{row_fun = RowFun, column_pos = ColumnPos} = State) ->
    ExpandedRow = list_to_tuple(RowFun(RK)), %% As tuple for faster access.
    SelectedColumns = [element(Col, ExpandedRow) || Col <- ColumnPos],
    [SelectedColumns | expand_rows(RestRows, State)];
expand_rows([FullRowTuple | RestRows], #state{column_pos = ColumnPos} = State) ->
    SelectedColumns = [element(3+Col, FullRowTuple) || Col <- ColumnPos],
    [SelectedColumns | expand_rows(RestRows, State)].

