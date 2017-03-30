-module(dderl_data_receiver).
-behaviour(gen_server).

-include("dderl.hrl").
-include("gres.hrl").
-include_lib("imem/include/imem_sql.hrl").  %% Included for stmtCol record
-include_lib("imem/include/imem_meta.hrl"). %% Included for config access

-export([start_link/4
        ,get_status/1
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
        ,columns                   :: [#stmtCol{}]
        ,column_pos                :: [integer()]
        ,fsm_monitor               :: reference()
        ,update_cursor_prepare_fun :: fun()
        ,update_cursor_execute_fun :: fun()
        ,sender_pid                :: pid()
        ,sender_monitor            :: reference()
        ,received_rows = 0         :: integer()
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

-spec get_status(pid()) -> ok.
get_status(ReceiverPid) ->
    gen_server:cast(ReceiverPid, status).

-spec data_info(pid(), {[#stmtCol{}], non_neg_integer()}) -> ok.
data_info(ReceiverPid, {_Columns, _Size} = DataInfo) ->
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

handle_cast(status, #state{received_rows = RowCount, browser_pid = BrowserPid} = State) ->
    Response = [{<<"received_rows">>, RowCount}],
    BrowserPid ! {reply, jsx:encode([{<<"receiver_status">>, Response}])},
    {noreply, State};
handle_cast({data_info, {SenderColumns, AvailableRows}}, #state{sender_pid = SenderPid, browser_pid = BrowserPid, statement = Statement, column_pos = ColumnPos} = State) ->
    ?Debug("data information from sender, columns ~n~p~n, Available rows: ~p", [SenderColumns, AvailableRows]),
    {Ucpf, Ucef, Columns} = Statement:get_receiver_params(),
    %% TODO: Check for column names and types instead of only count.
    if
        length(ColumnPos) =:= length(SenderColumns) ->
            SenderColumnNames = [ColAlias || #stmtCol{alias = ColAlias} <- SenderColumns],
            %% TODO: Change this for a callback and add information about sender columns
            Response = [{<<"available_rows">>, AvailableRows}, {<<"sender_columns">>, SenderColumnNames}],
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, Response}])},
            dderl_data_sender:fetch_first_block(SenderPid),
            {noreply, State#state{update_cursor_prepare_fun = Ucpf, update_cursor_execute_fun = Ucef, columns = Columns}, ?RESPONSE_TIMEOUT};
        true ->
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, [{<<"error">>, <<"Columns are not compatible">>}]}])},
            {stop, {shutdown, <<"Columns mismatch">>}, State}
    end;
handle_cast({data, '$end_of_table'}, State) ->
    ?Info("End of table reached in sender, terminating"),
    {stop, normal, State};
handle_cast({data, Rows}, #state{sender_pid = SenderPid, received_rows = ReceivedRows} = State) ->
    ?Info("got ~p rows from the data sender, adding it to fsm and asking for more", [length(Rows)]),
    add_rows_to_statement(Rows, State),
    dderl_data_sender:more_data(SenderPid), %% TODO: Maybe change this name
    ?Info("@@@@@@@@@@Received Rows : ~p", [ReceivedRows + length(Rows)]),
    {noreply, State#state{received_rows = ReceivedRows + length(Rows)}, ?RESPONSE_TIMEOUT};
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
add_rows_to_statement(Rows, #state{update_cursor_prepare_fun = Ucpf, update_cursor_execute_fun = Ucef, column_pos = ColumnPos, columns = Columns}) ->
    PreparedRows = prepare_rows(Rows, Columns, ColumnPos, 1),
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

-spec prepare_rows([[binary()]], [#stmtCol{}], [binary()], pos_integer()) -> [list()].
prepare_rows([], _Columns, _ColumnPos, _RowId) -> [];
prepare_rows([Row | RestRows], Columns, ColumnPos, RowId) ->
    NColumns = length(Columns),
    ValuesToInsert = set_column_value(list_to_tuple(lists:duplicate(NColumns, <<>>)), ColumnPos, Row),
    [[RowId, ins, {{},{}} | ValuesToInsert] | prepare_rows(RestRows, Columns, ColumnPos, RowId + 1)].

-spec set_column_value(tuple(), [integer()], [binary()]) -> [binary()].
set_column_value(NewRow, [], []) -> tuple_to_list(NewRow);
set_column_value(NewRow, [ColumnPos | RestCols], [Value | Row]) ->
    RowAddedValue = erlang:setelement(ColumnPos, NewRow, Value),
    set_column_value(RowAddedValue, RestCols, Row).
