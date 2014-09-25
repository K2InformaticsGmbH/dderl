-module(dderl_data_receiver).

-behaviour(gen_server).

-include("dderl.hrl").
-include("gres.hrl").
%% Included for stmtCol record
-include_lib("imem/include/imem_sql.hrl").

-export([start_link/4
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
        ,column_pos                :: [integer()]
        ,fsm_monitor               :: reference()
        ,update_cursor_prepare_fun :: fun()
        ,update_cursor_execute_fun :: fun()
        ,sender_pid                :: pid()
        ,sender_monitor            :: reference()
        ,browser_pid               :: pid()}).

-define(RESPONSE_TIMEOUT, 5000). %% TODO: Timeout should be 100 sec

-spec start_link({atom(), pid()}, [integer()], pid(), pid()) -> {ok, pid()} | {error, term()} | ignore.
start_link(Statement, ColumnPositions, PidSender, BrowserPid) ->
	gen_server:start_link(?MODULE, [Statement, ColumnPositions, PidSender, BrowserPid], []).

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

handle_cast({data_info, {_Columns, 0}}, #state{browser_pid = BrowserPid} = State) ->
    ?Info("No rows available from sender"),
    %% TODO: This should be a callback function.
    BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, [{<<"error">>, <<"No rows available from sender">>}]}])},
    {stop, {shutdown, <<"Empty sender">>}, State};
handle_cast({data_info, {SenderColumns, AvailableRows}}, #state{sender_pid = SenderPid, browser_pid = BrowserPid, statement = Statement, column_pos = ColumnPos} = State) ->
    ?Info("data information from sender, columns ~n~p~n, Available rows: ~p", [SenderColumns, AvailableRows]),
    {Ucpf, Ucef, _Columns} = Statement:get_receiver_params(),
    %% TODO: Check for column names and types instead of only count.
    if
        length(ColumnPos) =:= length(SenderColumns) ->
            %% TODO: Change this for a callback and add information about sender columns
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, <<"ok">>}])},
            dderl_data_sender:fetch_first_block(SenderPid),
            {noreply, State#state{update_cursor_prepare_fun = Ucpf, update_cursor_execute_fun = Ucef}, ?RESPONSE_TIMEOUT};
        true ->
            BrowserPid ! {reply, jsx:encode([{<<"activate_receiver">>, [{<<"error">>, <<"Columns are not compatible">>}]}])},
            {stop, {shutdown, <<"Columns mismatch">>}, State}
    end;
handle_cast({data, '$end_of_table'}, State) ->
    ?Info("End of table reached in sender, terminating"),
    {stop, normal, State};
handle_cast({data, Rows}, #state{sender_pid = SenderPid} = State) ->
    ?Info("got ~p rows from the data sender, adding it to fsm and asking for more", [length(Rows)]),
    add_rows_to_statement(Rows, State),
    dderl_data_sender:more_data(SenderPid), %% TODO: Maybe change this name
    {noreply, State, ?RESPONSE_TIMEOUT};
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
    ?Info("timeout in data receiver ~p after ~p seconds without a response from sender", [self(), ?RESPONSE_TIMEOUT div 1000]),
    {stop, {shutdown, timeout}, State};
handle_info(Msg, State) ->
    ?Info("~p received unknown msg ~p", [self(), Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    ?Info("~p ~p terminating, reason ~p", [?MODULE, self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

add_rows_to_statement(Rows, #state{statement = Statement, column_pos = ColumnPos}) ->
    PreparedRows = prepare_rows(Rows, ColumnPos),
    Statement:gui_req(update, PreparedRows,
                      fun(#gres{} = _IgnoredForNow) ->
                              Statement:gui_req(button, <<"commit">>, fun(_) -> ok end)
                      end).

prepare_rows([], _ColumnPos) -> [];
prepare_rows([Row | RestRows], ColumnPos) ->
    ColumnsToInsert = lists:zip(ColumnPos, Row),
    [{undefined, ins, ColumnsToInsert} | prepare_rows(RestRows, ColumnPos)].
