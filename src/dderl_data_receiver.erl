-module(dderl_data_receiver).
-behaviour(gen_server).

-include("dderl.hrl").
-include("gres.hrl").
-include_lib("imem/include/imem_sql.hrl").  %% Included for stmtCol record
-include_lib("imem/include/imem_meta.hrl"). %% Included for config access

-export([start_link/4
        ,get_status/2
        ,data_info/2
        ,data/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state,
        {statement                 :: {atom, pid()}
        ,columns                   :: [#rowCol{}]
        ,column_pos                :: [integer()]
        ,fsm_monitor               :: reference()
        ,update_cursor_prepare_fun :: fun()
        ,update_cursor_execute_fun :: fun()
        ,node                      :: node()
        ,sender_pid                :: pid()
        ,sender_monitor            :: reference()
        ,received_rows = 0         :: integer()
        ,errors = []               :: list()
        ,is_complete = false       :: boolean() 
        ,browser_pid               :: pid()}).

-spec start_link({atom(), pid()}, [integer()], pid(), pid()) -> {ok, pid()} | {error, term()} | ignore.
start_link(Statement, ColumnPositions, PidSender, BrowserPid) ->
    ?Info("~p starting...~n", [?MODULE]),
    case gen_server:start_link(?MODULE, [Statement, ColumnPositions, PidSender, BrowserPid], []) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

-spec get_status(pid(), pid()) -> ok.
get_status(ReceiverPid, ReplyToPid) ->
    gen_server:cast(ReceiverPid, {status, ReplyToPid}).

-spec data_info(
    pid(),
    {[#rowCol{}], non_neg_integer()}
     | {stats, non_neg_integer(), non_neg_integer()}
) -> ok.
data_info(ReceiverPid, {_Columns, _Size} = DataInfo) ->
    gen_server:cast(ReceiverPid, {data_info, DataInfo});
data_info(ReceiverPid, {stats, _ColumnCount, _Size} = DataInfo) ->
    gen_server:cast(ReceiverPid, {data_info, DataInfo}).

-spec data(pid(), list() | '$end_of_table') -> ok.
data(ReceiverPid, Rows) ->
    gen_server:cast(ReceiverPid, {data, Rows}).

%% Gen server callbacks
init([{dderl_fsm, StmtPid} = Statement, ColumnPositions, SenderPid, BrowserRespPid]) ->
    FsmMonitorRef = erlang:monitor(process, StmtPid),
    ok = dderl_data_sender:connect(SenderPid, self()), %% Todo handle case when not ok...
    SenderMonitorRef = erlang:monitor(process, SenderPid),
    State =#state{statement       = Statement
                  ,column_pos     = ColumnPositions
                  ,fsm_monitor    = FsmMonitorRef
                  ,sender_pid     = SenderPid
                  ,sender_monitor = SenderMonitorRef
                  ,browser_pid    = BrowserRespPid},
    ok = dderl_data_sender:get_data_info(SenderPid),
    {ok, State, ?RESPONSE_TIMEOUT}.

handle_call(Req, _From, State) ->
    ?Info("~p received Unexpected call ~p", [self(), Req]),
    {reply, {not_supported, Req}, State}.

handle_cast({status, ReplyToPid}, #state{is_complete = true, received_rows = RowCount} = State) ->
    Response = [{<<"received_rows">>, RowCount}, {<<"is_complete">>, true}, {<<"continue">>, false}],
    ReplyToPid ! {reply, jsx:encode([{<<"receiver_status">>, Response}])},
    ?Info("Terminating in state completed"),
    {stop, normal, State};
handle_cast({status, ReplyToPid}, #state{received_rows = RowCount, errors = Errors} = State) ->
    Response = [{<<"received_rows">>, RowCount}, {<<"errors">>, Errors}, {<<"continue">>, true}],
    ReplyToPid ! {reply, jsx:encode([{<<"receiver_status">>, Response}])},
    {noreply, State#state{errors = []}, ?RESPONSE_TIMEOUT};
handle_cast({data_info, {stats, SndColsCount, AvailableRows}}, #state{sender_pid = SenderPid, browser_pid = BrowserPid, statement = Statement, column_pos = ColumnPos} = State) ->
    {Ucpf, Ucef, Columns, Node} = Statement:get_receiver_params(),
    if
        length(ColumnPos) =:= SndColsCount ->
            Response = [{<<"available_rows">>, AvailableRows}, {<<"sender_columns">>, SndColsCount}],
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, Response}])},
            dderl_data_sender:fetch_first_block(SenderPid),
            {noreply, State#state{update_cursor_prepare_fun = hd(Ucpf), update_cursor_execute_fun = hd(Ucef), columns = Columns, node = Node}, ?RESPONSE_TIMEOUT};
        true ->
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, [{<<"error">>, <<"Columns are not compatible">>}]}])},
            {stop, {shutdown, <<"Columns mismatch">>}, State}
    end;
handle_cast({data_info, {SenderColumns, AvailableRows}}, #state{sender_pid = SenderPid, browser_pid = BrowserPid, statement = Statement, column_pos = ColumnPos} = State) ->
    ?Debug("data information from sender, columns ~n~p~n, Available rows: ~p", [SenderColumns, AvailableRows]),
    {Ucpfs, Ucefs, Columns, Node} = Statement:get_receiver_params(),
    %% TODO: Check for column names and types instead of only count.
    if
        length(ColumnPos) =:= length(SenderColumns) ->
            SenderColumnNames = [ColAlias || #rowCol{alias=ColAlias} <- SenderColumns],
            %% TODO: Change this for a callback and add information about sender columns
            Response = [{<<"available_rows">>, AvailableRows}, {<<"sender_columns">>, SenderColumnNames}],
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, Response}])},
            dderl_data_sender:fetch_first_block(SenderPid),
            {noreply, State#state{update_cursor_prepare_fun = hd(Ucpfs), update_cursor_execute_fun = hd(Ucefs), columns = Columns, node = Node}, ?RESPONSE_TIMEOUT};
        true ->
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, [{<<"error">>, <<"Columns are not compatible">>}]}])},
            {stop, {shutdown, <<"Columns mismatch">>}, State}
    end;
handle_cast({data, '$end_of_table'}, State) ->
    ?Info("End of table reached in sender"),
    {noreply, State#state{is_complete = true}, ?RESPONSE_TIMEOUT};
handle_cast({data, Rows}, #state{sender_pid=SenderPid, received_rows=ReceivedRows, errors=Errors} = State) ->
    ?Debug("got ~p rows from the data sender, adding it to fsm and asking for more", [length(Rows)]),
    case add_rows_to_statement(Rows, State) of
        ok -> 
            dderl_data_sender:more_data(SenderPid), %% TODO: Maybe change this name
            {noreply, State#state{received_rows = ReceivedRows + length(Rows)}, ?RESPONSE_TIMEOUT};
        {error, Error} ->
            dderl_data_sender:more_data(SenderPid),
            {noreply, State#state{errors = [imem_datatype:term_to_io(Error) | Errors]}, ?RESPONSE_TIMEOUT}
    end;
handle_cast(Req, State) ->
    ?Info("~p received unknown cast ~p", [self(), Req]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, StmtFsmPid, Reason}, #state{fsm_monitor = Ref} = State) ->
    ?Info("fsm process ~p down with reason ~p, monitor ref ~p", [StmtFsmPid, Reason, Ref]),
    {stop, {shutdown, <<"Statement terminated">>}, State};
handle_info({'DOWN', Ref, process, SenderPid, Reason}, #state{sender_monitor = Ref} = State) ->
    ?Info("Sender data process ~p down with reason ~p, monitor ref ~p", [SenderPid, Reason, Ref]),
    {stop, {shutdown, <<"Sender terminated">>}, State};
handle_info(timeout, #state{} = State) ->
    ?Info("timeout in data receiver ~p after ~p seconds without data from sender", [self(), ?RESPONSE_TIMEOUT div 1000]),
    {stop, {shutdown, timeout}, State};
handle_info(Msg, State) ->
    ?Info("~p received unknown msg ~p", [self(), Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    ?Info("~p ~p terminating, reason ~p", [?MODULE, self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec add_rows_to_statement([[binary()]], #state{}) -> ok | {error, term()}.
add_rows_to_statement(Rows, #state{update_cursor_prepare_fun = Ucpf, update_cursor_execute_fun = Ucef, column_pos = ColumnPos, columns = Columns, node = Node}) ->
    PreparedRows = prepare_rows(Rows, Columns, ColumnPos, 1, Node),
    case Ucpf(PreparedRows) of
        ok ->
            case Ucef(none) of
                {_, Error} -> {error, Error};
                _ChangedKeys -> ok
            end;
        {ok, UpdateRef} ->
            case Ucef(none, UpdateRef) of
                {_, Error} -> {error, Error};
                _ChangedKeys -> ok
            end;
        {_, Error} -> {error, Error}
    end.

-spec prepare_rows([[binary()]], [#rowCol{}], [binary()], pos_integer(), node()) -> [list()].
prepare_rows([], _Columns, _ColumnPos, _RowId, _Node) -> [];
prepare_rows([Row | RestRows], Columns, ColumnPos, RowId, Node) ->
    NColumns = length(Columns),
    ValuesToInsert = set_column_value(list_to_tuple(lists:duplicate(NColumns, <<>>)), ColumnPos, Row),
    [[RowId, ins, {{RowId, Node},{}} | ValuesToInsert] | prepare_rows(RestRows, Columns, ColumnPos, RowId + 1, Node)].

-spec set_column_value(tuple(), [integer()], [binary()]) -> [binary()].
set_column_value(NewRow, [], []) -> tuple_to_list(NewRow);
set_column_value(NewRow, [ColumnPos | RestCols], [Value | Row]) ->
    RowAddedValue = erlang:setelement(ColumnPos, NewRow, Value),
    set_column_value(RowAddedValue, RestCols, Row).
