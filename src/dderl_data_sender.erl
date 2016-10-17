-module(dderl_data_sender).
-behaviour(gen_server).

-include("dderl.hrl").
-include_lib("imem/include/imem_meta.hrl"). %% Included for config access

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
               ,skip             :: non_neg_integer()
               ,row_fun          :: fun()
               ,fsm_monitor      :: reference()
               ,receiver_pid     :: pid()
               ,receiver_monitor :: reference()}).

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
    ?Info("~p starting...~n", [?MODULE]),
	case gen_server:start_link(?MODULE, [Statement, ColumnPositions], []) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

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
                  ,fsm_monitor = FsmMonitorRef
                  ,skip        = 0},
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
    case dderl_dal:rows_from(UsedTable, FirstKey, ?BLOCK_SIZE) of
        '$end_of_table' ->
            retry_more_data(),
            {noreply, State};
        {Rows, '$end_of_table'} ->
            NewState = send_rows(Rows, State),
            {noreply, NewState};
        {Rows, Continuation} ->
            send_rows(Rows, State),
            {noreply, State#state{continuation = Continuation, skip = 0}}
    end;
handle_cast(more_data, #state{continuation = undefined} = State) -> handle_cast(fetch_first_block, State);
handle_cast(more_data, #state{continuation = Continuation} = State) ->
    case ets:select(Continuation) of
        '$end_of_table' ->
            retry_more_data(),
            {noreply, State};
        {Rows, '$end_of_table'} ->
            NewState = send_rows(Rows, State),
            {noreply, NewState};
        {Rows, NewContinuation} ->
            send_rows(Rows, State),
            {noreply, State#state{continuation = NewContinuation, skip = 0}}
    end;
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
send_rows(Rows, #state{receiver_pid = ReceiverPid, skip = Skip} = State) ->
    case lists:nthtail(Skip, Rows) of
        [] ->
            retry_more_data(),
            State;
        RowsToSend ->
            NewSkip = Skip + length(RowsToSend),
            #state{table_id = TableId, row_fun = RowFun, column_pos = ColumnPos} = State,
            ExpandedRows = dderl_dal:expand_rows(RowsToSend, TableId, RowFun, ColumnPos),
            dderl_data_receiver:data(ReceiverPid, ExpandedRows),
            State#state{skip = NewSkip}
    end.

-spec retry_more_data() -> ok.
retry_more_data() ->
    timer:sleep(500), %% retry after 0.5 seconds.
    more_data(self()).
