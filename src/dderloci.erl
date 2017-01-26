-module(dderloci).
-behaviour(gen_server).

-include("dderloci.hrl").

%% API
-export([
    exec/3,
    exec/4,
    change_password/4,
    add_fsm/2,
    fetch_recs_async/3,
    fetch_close/1,
    filter_and_sort/6,
    close/1,
    run_table_cmd/3,
    cols_to_rec/2,
    get_alias/1,
    fix_row_format/3,
    create_rowfun/3
]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(qry, {select_sections
             ,contain_rowid
             ,stmt_result
             ,fsm_ref
             ,max_rowcount
             ,pushlock
             ,contain_rownum
             }).

-define(PREFETCH_SIZE, 250).

%% ===================================================================
%% Exported functions
%% ===================================================================
-spec exec(tuple(), binary(), integer()) -> ok | {ok, pid()} | {error, term()}.
exec(Connection, Sql, MaxRowCount) ->
    exec(Connection, Sql, undefined, MaxRowCount).

-spec exec(tuple(), binary(), tuple(), integer()) -> ok | {ok, pid()} | {error, term()}.
exec({oci_port, _, _} = Connection, Sql, Binds, MaxRowCount) ->
    case sqlparse:parsetree(Sql) of
        {ok,[{{select, SelectSections},_}]} ->
            {TableName, NewSql, RowIdAdded} = inject_rowid(select_type(SelectSections), SelectSections, Sql);
        _ ->
            TableName = <<"">>,
            NewSql = Sql,
            RowIdAdded = false,
            SelectSections = []
    end,
    case catch run_query(Connection, Sql, Binds, NewSql, RowIdAdded, SelectSections) of
        {'EXIT', {Error, ST}} ->
            ?Error("run_query(~s,~p,~s)~n{~p,~p}", [Sql, Binds, NewSql, Error, ST]),
            {error, Error};
        {ok, #stmtResult{} = StmtResult, ContainRowId} ->
            LowerSql = string:to_lower(binary_to_list(Sql)),
            case string:str(LowerSql, "rownum") of
                0 -> ContainRowNum = false;
                _ -> ContainRowNum = true
            end,
            {ok, Pid} = gen_server:start(?MODULE, [SelectSections, StmtResult, ContainRowId, MaxRowCount, ContainRowNum], []),
            SortSpec = gen_server:call(Pid, build_sort_spec),
            %% Mask the internal stmt ref with our pid.
            {ok, StmtResult#stmtResult{stmtRef = Pid, sortSpec = SortSpec}, TableName};
        NoSelect ->
            NoSelect
    end.

-spec change_password(tuple(), binary(), binary(), binary()) -> ok | {error, term()}.
change_password({oci_port, _, _} = Connection, User, OldPassword, NewPassword) ->
    run_table_cmd(Connection, iolist_to_binary(["ALTER USER ", User, " IDENTIFIED BY ", NewPassword, " REPLACE ", OldPassword])).

-spec add_fsm(pid(), term()) -> ok.
add_fsm(Pid, FsmRef) ->
    gen_server:cast(Pid, {add_fsm, FsmRef}).

-spec fetch_recs_async(pid(), list(), integer()) -> ok.
fetch_recs_async(Pid, Opts, Count) ->
    gen_server:cast(Pid, {fetch_recs_async, lists:member({fetch_mode, push}, Opts), Count}).

-spec fetch_close(pid()) -> ok.
fetch_close(Pid) ->
    gen_server:call(Pid, fetch_close).

-spec filter_and_sort(pid(), tuple(), list(), list(), list(), binary()) -> {ok, binary(), fun()}.
filter_and_sort(Pid, Connection, FilterSpec, SortSpec, Cols, Query) ->
    gen_server:call(Pid, {filter_and_sort, Connection, FilterSpec, SortSpec, Cols, Query}).

-spec close(pid()) -> term().
close(Pid) ->
    gen_server:call(Pid, close).

%% Gen server callbacks
init([SelectSections, StmtResult, ContainRowId, MaxRowCount, ContainRowNum]) ->
    {ok, #qry{
            select_sections = SelectSections,
            stmt_result = StmtResult,
            contain_rowid = ContainRowId,
            max_rowcount = MaxRowCount,
            contain_rownum = ContainRowNum}}.

handle_call({filter_and_sort, Connection, FilterSpec, SortSpec, Cols, Query}, _From, #qry{stmt_result = StmtResult} = State) ->
    #stmtResult{stmtCols = StmtCols} = StmtResult,
    %% TODO: improve this to use/update parse tree from the state.
    Res = filter_and_sort_internal(Connection, FilterSpec, SortSpec, Cols, Query, StmtCols),
    {reply, Res, State};
handle_call(build_sort_spec, _From, #qry{stmt_result = StmtResult, select_sections = SelectSections} = State) ->
    #stmtResult{stmtCols = StmtCols} = StmtResult,
    SortSpec = build_sort_spec(SelectSections, StmtCols),
    {reply, SortSpec, State};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(fetch_close, _From, #qry{} = State) ->
    {reply, ok, State#qry{pushlock = true}};
handle_call(close, _From, #qry{stmt_result = StmtResult} = State) ->
    #stmtResult{stmtRef = StmtRef} = StmtResult,
    try StmtRef:close()
    catch
        exit:{noproc,_} -> ok %trying to close an already closed statement.
    end,
    {stop, normal, ok, State#qry{stmt_result = StmtResult#stmtResult{stmtRef = undefined}}};
handle_call(_Ignored, _From, State) ->
    {noreply, State}.

handle_cast({add_fsm, FsmRef}, #qry{} = State) -> {noreply, State#qry{fsm_ref = FsmRef}};
handle_cast({fetch_recs_async, _, _}, #qry{pushlock = true} = State) ->
    {noreply, State};
handle_cast({fetch_push, _, _}, #qry{pushlock = true} = State) ->
    {noreply, State};
handle_cast({fetch_recs_async, true, FsmNRows}, #qry{max_rowcount = MaxRowCount} = State) ->
    case FsmNRows rem MaxRowCount of
        0 -> RowsToRequest = MaxRowCount;
        Result -> RowsToRequest = MaxRowCount - Result
    end,
    gen_server:cast(self(), {fetch_push, 0, RowsToRequest}),
    {noreply, State};
handle_cast({fetch_recs_async, false, _}, #qry{fsm_ref = FsmRef, stmt_result = StmtResult, contain_rowid = ContainRowId} = State) ->
    #stmtResult{stmtRef = StmtRef, stmtCols = Clms} = StmtResult,
    case StmtRef:fetch_rows(?DEFAULT_ROW_SIZE) of
        {{rows, Rows}, Completed} ->
            try FsmRef:rows({fix_row_format(Rows, Clms, ContainRowId), Completed}) of
                ok -> ok
            catch
                _Class:Result ->
                    FsmRef:rows({error, Result})
            end;
        {error, Error} ->
            FsmRef:rows({error, Error})
    end,
    {noreply, State};
handle_cast({fetch_push, NRows, Target}, #qry{fsm_ref = FsmRef, stmt_result = StmtResult} = State) ->
    #qry{contain_rowid = ContainRowId, contain_rownum = ContainRowNum} = State,
    #stmtResult{stmtRef = StmtRef, stmtCols = Clms} = StmtResult,
    MissingRows = Target - NRows,
    if
        MissingRows > ?DEFAULT_ROW_SIZE ->
            RowsToFetch = ?DEFAULT_ROW_SIZE;
        true ->
            RowsToFetch = MissingRows
    end,
    case StmtRef:fetch_rows(RowsToFetch) of
        {{rows, Rows}, Completed} ->
            RowsFixed = fix_row_format(Rows, Clms, ContainRowId),
            NewNRows = NRows + length(RowsFixed),
            if
                Completed -> FsmRef:rows({RowsFixed, Completed});
                (NewNRows >= Target) andalso (not ContainRowNum) -> FsmRef:rows_limit(NewNRows, RowsFixed);
                true ->
                    FsmRef:rows({RowsFixed, false}),
                    gen_server:cast(self(), {fetch_push, NewNRows, Target})
            end;
        {error, Error} ->
            FsmRef:rows({error, Error})
    end,
    {noreply, State};
handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #qry{stmt_result = #stmtResult{stmtRef = undefined}}) -> ok;
terminate(_Reason, #qry{stmt_result = #stmtResult{stmtRef = StmtRef}}) -> StmtRef:close().

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal functions %%%
-spec select_type(list()) -> atom().
select_type(Args) ->
    Opts = proplists:get_value(opt, Args, <<>>),
    GroupBy = proplists:get_value('group by', Args),
    NotAgregation = case proplists:get_value(from, Args) of
        [] -> false;
        FromTargets -> not is_agregation(FromTargets)
    end,
    if Opts =:= <<>> andalso
       GroupBy =:= [] andalso
       NotAgregation -> select;
       true -> agregation
    end.

-spec is_agregation([binary() | tuple()]) -> boolean().
is_agregation([]) -> false;
is_agregation([Table | Rest]) when is_binary(Table) ->
    is_agregation(Rest);
is_agregation([{as, Table, Alias} | Rest]) when is_binary(Alias), is_binary(Table) ->
    is_agregation(Rest);
is_agregation(_) -> true.

-spec inject_rowid(atom(), list(), binary()) -> {binary(), binary()}.
inject_rowid(agregation, Args, Sql) ->
    {from, [FirstTable|_]=_Forms} = lists:keyfind(from, 1, Args),
    %% Do not add rowid on agregation.
    {FirstTable, Sql, false};
inject_rowid(select, Args, Sql) ->
    {fields, Flds} = lists:keyfind(fields, 1, Args),
    {from, [FirstTable|_]=Forms} = lists:keyfind(from, 1, Args),
    NewFields = expand_star(Flds, Forms) ++ [add_rowid_field(FirstTable)],
    NewArgs = lists:keyreplace(fields, 1, Args, {fields, NewFields}),
    NPT = {select, NewArgs},
    case sqlparse:pt_to_string(NPT) of
        {error, _Reason} ->
            {FirstTable, Sql, false};
        NewSql ->
            {FirstTable, NewSql, true}
    end.

-spec add_rowid_field(tuple() | binary()) -> binary().
add_rowid_field(Table) -> qualify_field(Table, "ROWID").

-spec qualify_field(tuple() | binary(), binary() | list()) -> binary().
qualify_field(Table, Field) -> iolist_to_binary(add_field(Table, Field)).

-spec add_field(tuple() | binary(), binary() | list()) -> iolist().
add_field({as, _, Alias}, Field) -> [Alias, ".", Field];
add_field({{as, _, Alias}, _}, Field) -> [Alias, ".", Field];
add_field({Tab, _}, Field) when is_binary(Tab) ->
    include_at(binary:split(Tab, <<"@">>), Field);
add_field(Tab, Field) when is_binary(Tab) ->
    include_at(binary:split(Tab, <<"@">>), Field).

-spec include_at(list(), binary() | list()) -> iolist().
include_at([TabName, TabLocation], Field) ->
    [TabName, ".", Field, $@, TabLocation];
include_at([TabName], Field) ->
    [TabName, ".", Field].

-spec expand_star(list(), list()) -> list().
expand_star([<<"*">>], Forms) -> qualify_star(Forms);
expand_star(Flds, _Forms) -> Flds.

-spec qualify_star(list()) -> list().
qualify_star([]) -> [];
qualify_star([Table | Rest]) -> [qualify_field(Table, "*") | qualify_star(Rest)].

run_query(Connection, Sql, Binds, NewSql, RowIdAdded, SelectSections) ->
    %% For now only the first table is counted.
    case Connection:prep_sql(NewSql) of
        {error, {ErrorId,Msg}} ->
            case Connection:prep_sql(Sql) of
                {error, {ErrorId,Msg}} ->
                    error({ErrorId,Msg});
                Statement ->
                    StmtExecResult = case Binds of
                                         undefined -> Statement:exec_stmt();
                                         {BindsMeta, BindVal} ->
                                             case Statement:bind_vars(BindsMeta) of
                                                 ok -> Statement:exec_stmt([list_to_tuple(BindVal)]);
                                                 Error -> error(Error)
                                             end
                                     end,
                    result_exec_stmt(StmtExecResult,Statement,Sql,Binds,NewSql,RowIdAdded,
                                     Connection,SelectSections)
            end;
        Statement ->
            StmtExecResult = case Binds of
                                 undefined -> Statement:exec_stmt();
                                 {BindsMeta, BindVal} ->
                                     case Statement:bind_vars(BindsMeta) of
                                         ok -> Statement:exec_stmt([list_to_tuple(BindVal)]);
                                         Error -> error(Error)
                                     end
                             end,
            result_exec_stmt(StmtExecResult,Statement,Sql,Binds,NewSql,RowIdAdded,Connection,
                             SelectSections)
    end.

result_exec_stmt({cols, Clms}, Statement, _Sql, _Binds, NewSql, RowIdAdded, _Connection, SelectSections) ->
    if
        RowIdAdded -> % ROWID is hidden from columns
            [_|ColumnsR] = lists:reverse(Clms),
            Columns = lists:reverse(ColumnsR);
        true ->
            Columns = Clms
    end,
    Fields = proplists:get_value(fields, SelectSections, []),
    NewClms = cols_to_rec(Columns, Fields),
    SortFun = build_sort_fun(NewSql, NewClms),
    {ok
     , #stmtResult{ stmtCols = NewClms
                    , rowFun   =
                        fun({{}, Row}) ->
                                if
                                    RowIdAdded ->
                                        [_|NewRowR] = lists:reverse(tuple_to_list(Row)),
                                        translate_datatype(Statement, lists:reverse(NewRowR), NewClms);
                                    true ->
                                        translate_datatype(Statement, tuple_to_list(Row), NewClms)
                                end
                        end
                    , stmtRef  = Statement
                    , sortFun  = SortFun
                    , sortSpec = []}
     , RowIdAdded};
result_exec_stmt({rowids, _}, Statement, _Sql, _Binds, _NewSql, _RowIdAdded, _Connection, _SelectSections) ->
    Statement:close(),
    ok;
result_exec_stmt({executed, _}, Statement, _Sql, _Binds, _NewSql, _RowIdAdded, _Connection, _SelectSections) ->
    Statement:close(),
    ok;
result_exec_stmt({executed,_,Values}, Statement, _Sql, {Binds, _BindValues}, _NewSql, _RowIdAdded, _Connection,
                 _SelectSections) ->
    NewValues =
    lists:foldl(
      fun({Var, Val}, Acc) ->
              [{Var,
                case lists:keyfind(Var, 1, Binds) of
                    {Var,out,'SQLT_VNU'} -> list_to_binary(oci_util:from_num(Val));
                    _ -> Val
                end} | Acc]
      end, [], Values),
    ?Debug("Values ~p", [Values]),
    ?Debug("Binds ~p", [Binds]),
    Statement:close(),
    {ok, NewValues};
result_exec_stmt(RowIdError, Statement, Sql, Binds, _NewSql, _RowIdAdded, Connection, SelectSections) ->
    ?Info("RowIdError ~p", [RowIdError]),
    Statement:close(),
    case Connection:prep_sql(Sql) of
        {error, {ErrorId,Msg}} ->
            error({ErrorId,Msg});
        Statement1 ->
            StmtExecResult = case Binds of
                                 undefined -> Statement1:exec_stmt();
                                 {BindsMeta, BindVal} ->
                                     case Statement1:bind_vars(BindsMeta) of
                                         ok -> Statement1:exec_stmt([list_to_tuple(BindVal)]);
                                         Error1 -> error(Error1)
                                     end
                             end,
            case StmtExecResult of
                {cols, Clms} ->
                    Fields = proplists:get_value(fields, SelectSections, []),
                    NewClms = cols_to_rec(Clms, Fields),
                    SortFun = build_sort_fun(Sql, NewClms),
                    {ok, #stmtResult{ stmtCols = NewClms, stmtRef  = Statement1, sortFun  = SortFun,
                                      rowFun   = fun({{}, Row}) ->
                                                         translate_datatype(Statement, tuple_to_list(Row), NewClms)
                                                 end, sortSpec = []}
                     , false};
                Error ->
                    Statement1:close(),
                    error(Error)
            end
    end.

-spec create_rowfun(boolean(), list(), term()) -> fun().
create_rowfun(RowIdAdded, Clms, Stmt) ->
    fun({{}, Row}) ->
            if
                RowIdAdded ->
                    [_|NewRowR] = lists:reverse(tuple_to_list(Row)),
                    translate_datatype(Stmt, lists:reverse(NewRowR), Clms);
                true ->
                    translate_datatype(Stmt, tuple_to_list(Row), Clms)
            end
    end.

expand_fields([<<"*">>], _, AllFields, Cols, Sections) ->
    NewFields = [lists:nth(N, AllFields) || N <- Cols],
    lists:keyreplace('fields', 1, Sections, {'fields', NewFields});
expand_fields(QryFields, Tables, AllFields, Cols, Sections) ->
    NormQryFlds = normalize_pt_fields(QryFields, #{}),
    LowerAllFields = [string:to_lower(binary_to_list(X)) || X <- AllFields],
    case can_expand(maps:keys(NormQryFlds), Tables, LowerAllFields) of
        true ->
            Keys = [lists:nth(N,LowerAllFields) || N <- Cols],
            NewFields = [maps:get(K, NormQryFlds) || K <- Keys],
            lists:keyreplace('fields', 1, Sections, {'fields',NewFields});
        false ->
            Sections
    end.

can_expand(LowerSelectFields, [TableName], LowerAllFields) when is_binary(TableName) ->
    length(LowerSelectFields) =:= length(LowerAllFields) andalso [] =:= (LowerSelectFields -- LowerAllFields);
can_expand(_, _, _) -> false.

normalize_pt_fields([], Result) -> Result;
normalize_pt_fields([{as, _Field, Alias} = Fld | Rest], Result) when is_binary(Alias) ->
    Normalized = string:to_lower(binary_to_list(Alias)),
    normalize_pt_fields(Rest, Result#{Normalized => Fld});
normalize_pt_fields([TupleField | Rest], Result) when is_tuple(TupleField) ->
    case element(1, TupleField) of
        'fun' ->
            BinField = sqlparse:pt_to_string(TupleField),
            Normalized = string:to_lower(binary_to_list(BinField)),
            normalize_pt_fields(Rest, Result#{Normalized => TupleField});
        _ ->
            normalize_pt_fields(Rest, Result)
    end;
normalize_pt_fields([Field | Rest], Result) when is_binary(Field) ->
    Normalized = string:to_lower(binary_to_list(Field)),
    normalize_pt_fields(Rest, Result#{Normalized => Field});
normalize_pt_fields([_Ignored | Rest], Result) ->
    normalize_pt_fields(Rest, Result).

build_sort_spec(SelectSections, StmtCols) ->
    FullMap = build_full_map(StmtCols),
    case lists:keyfind('order by', 1, SelectSections) of
        {'order by', OrderBy} ->
            [process_sort_order(ColOrder, FullMap) || ColOrder <- OrderBy];
        _ ->
            []
    end.

process_sort_order({Name, <<>>}, Map) ->
    process_sort_order({Name, <<"asc">>}, Map);
process_sort_order({Name, Dir}, []) when is_binary(Name)-> {Name, Dir};
process_sort_order({Name, Dir}, [#bind{alias = Alias, cind = Pos} | Rest]) when is_binary(Name) ->
    case string:to_lower(binary_to_list(Name)) =:= string:to_lower(binary_to_list(Alias)) of
        true -> {Pos, Dir};
        false -> process_sort_order({Name, Dir}, Rest)
    end;
process_sort_order({Fun, Dir}, Map) ->
    process_sort_order({sqlparse:pt_to_string(Fun), Dir}, Map).


%%% Model how imem gets the new filter and sort results %%%%
%       NewSortFun = imem_sql:sort_spec_fun(SortSpec, FullMaps, ColMaps),
%       %?Debug("NewSortFun ~p~n", [NewSortFun]),
%       OrderBy = imem_sql:sort_spec_order(SortSpec, FullMaps, ColMaps),
%       %?Debug("OrderBy ~p~n", [OrderBy]),
%       Filter =  imem_sql:filter_spec_where(FilterSpec, ColMaps, WhereTree),
%       %?Debug("Filter ~p~n", [Filter]),
%       Cols1 = case Cols0 of
%           [] ->   lists:seq(1,length(ColMaps));
%           _ ->    Cols0
%       end,
%       AllFields = imem_sql:column_map_items(ColMaps, ptree),
%       % ?Debug("AllFields ~p~n", [AllFields]),
%       NewFields =  [lists:nth(N,AllFields) || N <- Cols1],
%       % ?Debug("NewFields ~p~n", [NewFields]),
%       NewSections0 = lists:keyreplace('fields', 1, SelectSections, {'fields',NewFields}),
%       NewSections1 = lists:keyreplace('where', 1, NewSections0, {'where',Filter}),
%       %?Debug("NewSections1 ~p~n", [NewSections1]),
%       NewSections2 = lists:keyreplace('order by', 1, NewSections1, {'order by',OrderBy}),
%       %?Debug("NewSections2 ~p~n", [NewSections2]),
%       NewSql = sqlparse:pt_to_string({select,NewSections2}),     % sql_box:flat_from_pt({select,NewSections2}),
%       %?Debug("NewSql ~p~n", [NewSql]),
%       {ok, NewSql, NewSortFun}

filter_and_sort_internal(_Connection, FilterSpec, SortSpec, Cols, Query, StmtCols) ->
    FullMap = build_full_map(StmtCols),
    case Cols of
        [] ->   Cols1 = lists:seq(1,length(FullMap));
        _ ->    Cols1 = Cols
    end,
    % AllFields = imem_sql:column_map_items(ColMaps, ptree), %%% This should be the correct way if doing it.
    AllFields = [C#bind.alias || C <- FullMap],
    SortSpecExplicit = [{Col, Dir} || {Col, Dir} <- SortSpec, is_integer(Col)],
    NewSortFun = imem_sql_expr:sort_spec_fun(SortSpecExplicit, FullMap, FullMap),
    case sqlparse:parsetree(Query) of
        {ok,[{{select, SelectSections},_}]} ->
            {fields, Flds} = lists:keyfind(fields, 1, SelectSections),
            {from, Tables} = lists:keyfind(from, 1, SelectSections),
            {where, WhereTree} = lists:keyfind(where, 1, SelectSections),
            NewSections0 = expand_fields(Flds, Tables, AllFields, Cols1, SelectSections),
            Filter = imem_sql_expr:filter_spec_where(FilterSpec, FullMap, WhereTree),
            FilterEmptyAsNull = filter_replace_empty(Filter),
            NewSections1 = lists:keyreplace('where', 1, NewSections0, {'where',FilterEmptyAsNull}),
            OrderBy = imem_sql_expr:sort_spec_order(SortSpec, FullMap, FullMap),
            NewSections2 = lists:keyreplace('order by', 1, NewSections1, {'order by',OrderBy}),
            NewSql = sqlparse:pt_to_string({select, NewSections2});
        _->
            NewSql = Query
    end,
    {ok, NewSql, NewSortFun}.

filter_replace_empty({'=', Column, <<"''">>}) -> {is, Column, <<"null">>};
filter_replace_empty({in, Column, {list, List}} = In) ->
    EmptyRemoved = [E || E <- List, E =/= <<"''">>],
    case length(EmptyRemoved) =:= length(List) of
        true -> In; % Nothing to do
        false -> {'or', {in, Column, {list, EmptyRemoved}}, {is, Column, <<"null">>}}
    end;
filter_replace_empty({Op, Parameter1, Parameter2}) ->
    {Op, filter_replace_empty(Parameter1), filter_replace_empty(Parameter2)};
filter_replace_empty(Condition) -> Condition.

-spec to_imem_type(atom()) -> atom().
to_imem_type('SQLT_NUM') -> number;
to_imem_type(_) -> binstr.

build_full_map(Clms) ->
    [#bind{ tag = list_to_atom([$$|integer_to_list(T)])
              , name = Alias
              , alias = Alias
              , tind = 2
              , cind = T
              , type = to_imem_type(OciType)
              , len = Len
              , prec = undefined }
     || {T, #stmtCol{alias = Alias, type = OciType, len = Len}} <- lists:zip(lists:seq(1,length(Clms)), Clms)].

%   Tables = case lists:keyfind(from, 1, SelectSections) of
%       {_, TNames} ->  Tabs = [imem_sql:table_qname(T) || T <- TNames],
%                       [{_,MainTab,_}|_] = Tabs,
%                       case lists:member(MainTab,[ddSize|?DataTypes]) of
%                           true ->     ?ClientError({"Virtual table can only be joined", MainTab});
%                           false ->    Tabs
%                       end;
%       TError ->       ?ClientError({"Invalid from in select structure", TError})
%   end,
%   imem_sql:column_map(Tables,[]);

build_sort_fun(_Sql, _Clms) ->
    fun(_Row) -> {} end.

-spec cols_to_rec([tuple()], list()) -> [#stmtCol{}].
cols_to_rec([], _) -> [];
cols_to_rec([{Alias,'SQLT_NUM',_Len,63,-127}|Rest], Fields) ->
    %% Real type
    {Tag, ReadOnly, NewFields} = find_original_field(Alias, Fields),
    [#stmtCol{ tag = Tag
             , alias = Alias
             , type = 'SQLT_NUM'
             , len = 19
             , prec = dynamic
             , readonly = ReadOnly} | cols_to_rec(Rest, NewFields)];
cols_to_rec([{Alias,'SQLT_NUM',_Len,_Prec,-127}|Rest], Fields) ->
    %% Float type or unlimited number.
    {Tag, ReadOnly, NewFields} = find_original_field(Alias, Fields),
    [#stmtCol{ tag = Tag
             , alias = Alias
             , type = 'SQLT_NUM'
             , len = 38
             , prec = dynamic
             , readonly = ReadOnly} | cols_to_rec(Rest, NewFields)];
cols_to_rec([{Alias,'SQLT_NUM',_Len,0,0}|Rest], Fields) ->
    [#stmtCol{ tag = Alias
             , alias = Alias
             , type = 'SQLT_NUM'
             , len = 38
             , prec = dynamic
             , readonly = true} | cols_to_rec(Rest, Fields)];
cols_to_rec([{Alias,'SQLT_NUM',_Len,_Prec,Scale}|Rest], Fields) ->
    {Tag, ReadOnly, NewFields} = find_original_field(Alias, Fields),
    [#stmtCol{ tag = Tag
             , alias = Alias
             , type = 'SQLT_NUM'
             , len = undefined
             , prec = Scale
             , readonly = ReadOnly} | cols_to_rec(Rest, NewFields)];
cols_to_rec([{Alias,Type,Len,Prec,_Scale}|Rest], Fields) ->
    {Tag, ReadOnly, NewFields} = find_original_field(Alias, Fields),
    [#stmtCol{ tag = Tag
             , alias = Alias
             , type = Type
             , len = Len
             , prec = Prec
             , readonly = ReadOnly} | cols_to_rec(Rest, NewFields)].

-spec get_alias([#stmtCol{}]) -> [binary()].
get_alias([]) -> [];
get_alias([#stmtCol{alias = A} | Rest]) ->
    [A | get_alias(Rest)].

translate_datatype(_Stmt, [], []) -> [];
translate_datatype(Stmt, [<<>> | RestRow], [#stmtCol{} | RestCols]) ->
    [<<>> | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [R | RestRow], [#stmtCol{type = 'SQLT_DAT'} | RestCols]) ->
    [dderloci_utils:ora_to_dderltime(R) | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [null | RestRow], [#stmtCol{type = 'SQLT_NUM'} | RestCols]) ->
    [<<>> | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [Mantissa | RestRow], [#stmtCol{type = 'SQLT_NUM', len = Scale, prec = dynamic} | RestCols]) ->
    %% Float / Real type or unlimited numbers.
    Number = dderloci_utils:clean_dynamic_prec(imem_datatype:decimal_to_io(Mantissa, Scale)),
    [Number | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [Mantissa | RestRow], [#stmtCol{type = 'SQLT_NUM', prec = Prec} | RestCols]) ->
    Number = imem_datatype:decimal_to_io(Mantissa, Prec),
    [Number | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [{_Pointer, Size, Path, Name} | RestRow], [#stmtCol{type = 'SQLT_BFILEE'} | RestCols]) ->
    SizeBin = integer_to_binary(Size),
    [<<Path/binary, $#, Name/binary, 32, $[, SizeBin/binary, $]>> | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [{Pointer, Size} | RestRow], [#stmtCol{type = 'SQLT_BLOB'} | RestCols]) ->
    if
        Size > ?PREFETCH_SIZE ->
            {lob, Trunc} = Stmt:lob(Pointer, 1, ?PREFETCH_SIZE),
            SizeBin = integer_to_binary(Size),
            AsIO = imem_datatype:binary_to_io(Trunc),
            [<<AsIO/binary, $., $., 32, $[, SizeBin/binary, $]>> | translate_datatype(Stmt, RestRow, RestCols)];
        true ->
            {lob, Full} = Stmt:lob(Pointer, 1, Size),
            AsIO = imem_datatype:binary_to_io(Full),
            [AsIO | translate_datatype(Stmt, RestRow, RestCols)]
    end;
translate_datatype(Stmt, [{Pointer, Size} | RestRow], [#stmtCol{type = 'SQLT_CLOB'} | RestCols]) ->
    if
        Size > ?PREFETCH_SIZE ->
            {lob, Trunc} = Stmt:lob(Pointer, 1, ?PREFETCH_SIZE),
            SizeBin = integer_to_binary(Size),
            [<<Trunc/binary, $., $., 32, $[, SizeBin/binary, $]>> | translate_datatype(Stmt, RestRow, RestCols)];
        true ->
            {lob, Full} = Stmt:lob(Pointer, 1, Size),
            [Full | translate_datatype(Stmt, RestRow, RestCols)]
    end;
translate_datatype(Stmt, [Raw | RestRow], [#stmtCol{type = 'SQLT_BIN'} | RestCols]) ->
    [imem_datatype:binary_to_io(Raw) | translate_datatype(Stmt, RestRow, RestCols)];
translate_datatype(Stmt, [R | RestRow], [#stmtCol{} | RestCols]) ->
    [R | translate_datatype(Stmt, RestRow, RestCols)].

-spec fix_row_format([list()], [#stmtCol{}], boolean()) -> [tuple()].
fix_row_format([], _, _) -> [];
fix_row_format([Row | Rest], Columns, ContainRowId) ->
    %% TODO: we have to add the table name at the start of the rows i.e
    %  rows [
    %        {{temp,1,2,3},{}},
    %        {{temp,4,5,6},{}}
    %  ]

    %% TODO: Convert the types to imem types??
    % db_to_io(Type, Prec, DateFmt, NumFmt, _StringFmt, Val),
    % io_to_db(Item,Old,Type,Len,Prec,Def,false,Val) when is_binary(Val);is_list(Val)
    if
        ContainRowId ->
            {RestRow, [RowId]} = lists:split(length(Row) - 1, Row),
            [{{}, list_to_tuple(fix_format(RestRow, Columns) ++ [RowId])} | fix_row_format(Rest, Columns, ContainRowId)];
        true ->
            [{{}, list_to_tuple(fix_format(Row, Columns))} | fix_row_format(Rest, Columns, ContainRowId)]
    end.

fix_format([], []) -> [];
fix_format([<<0:8, _/binary>> | RestRow], [#stmtCol{type = 'SQLT_NUM'} | RestCols]) ->
    [null | fix_format(RestRow, RestCols)];
fix_format([Number | RestRow], [#stmtCol{type = 'SQLT_NUM', len = Scale, prec = dynamic} | RestCols]) ->
    {Mantissa, Exponent} = dderloci_utils:oranumber_decode(Number),
    FormattedNumber = imem_datatype:decimal_to_io(Mantissa, Exponent),
    [imem_datatype:io_to_decimal(FormattedNumber, undefined, Scale) | fix_format(RestRow, RestCols)];
fix_format([Number | RestRow], [#stmtCol{type = 'SQLT_NUM', len = Len,  prec = Prec} | RestCols]) ->
    {Mantissa, Exponent} = dderloci_utils:oranumber_decode(Number),
    FormattedNumber = imem_datatype:decimal_to_io(Mantissa, Exponent),
    [imem_datatype:io_to_decimal(FormattedNumber, Len, Prec) | fix_format(RestRow, RestCols)];
fix_format([<<0, 0, 0, 0, 0, 0, 0, _/binary>> | RestRow], [#stmtCol{type = 'SQLT_DAT'} | RestCols]) -> %% Null format for date.
    [<<>> | fix_format(RestRow, RestCols)];
fix_format([Cell | RestRow], [#stmtCol{} | RestCols]) ->
    [Cell | fix_format(RestRow, RestCols)].

-spec run_table_cmd(tuple(), atom(), binary()) -> ok | {error, term()}. %% %% !! Fix this to properly use statments.
run_table_cmd({oci_port, _, _} = _Connection, restore_table, _TableName) -> {error, <<"Command not implemented">>};
run_table_cmd({oci_port, _, _} = _Connection, snapshot_table, _TableName) -> {error, <<"Command not implemented">>};
run_table_cmd({oci_port, _, _} = Connection, truncate_table, TableName) ->
    run_table_cmd(Connection, iolist_to_binary([<<"truncate table ">>, TableName]));
run_table_cmd({oci_port, _, _} = Connection, drop_table, TableName) ->
    run_table_cmd(Connection, iolist_to_binary([<<"drop table ">>, TableName])).

-spec run_table_cmd(tuple(), binary()) -> ok | {error, term()}.
run_table_cmd(Connection, SqlCmd) ->
    Statement = Connection:prep_sql(SqlCmd),
    case Statement:exec_stmt() of
        {executed, _} ->
            Statement:close(),
            ok;
        Error ->
            {error, Error}
    end.

-spec find_original_field(binary(), list()) -> {binary(), boolean(), list()}.
find_original_field(Alias, []) -> {Alias, false, []};
find_original_field(Alias, [<<"*">>]) -> {Alias, false, []};
find_original_field(Alias, [Field | Fields]) when is_binary(Field) ->
    compare_alias(Alias, Field, Fields, Field, {Alias, false, Fields});
find_original_field(Alias, [{as, Name, Field} = CompleteAlias | Fields])
  when is_binary(Name),
       is_binary(Field) ->
    compare_alias(Alias, Field, Fields, CompleteAlias, {Name, false, Fields});
find_original_field(Alias, [{as, _Expr, Field} = CompleteAlias | Fields])
  when is_binary(Field) ->
    compare_alias(Alias, Field, Fields, CompleteAlias, {Alias, true, Fields});
find_original_field(Alias, [Field | Fields]) ->
    {ResultName, ReadOnly, RestFields} = find_original_field(Alias, Fields),
    {ResultName, ReadOnly, [Field | RestFields]}.

-spec compare_alias(binary(), binary(), list(), term(), binary()) -> {binary(), boolean(), list()}.
compare_alias(Alias, Field, Fields, OrigField, Result) ->
    LowerAlias = string:to_lower(binary_to_list(Alias)),
    LowerField = string:to_lower(binary_to_list(Field)),
    AliasQuoted = [$" | LowerAlias] ++ [$"],
    if
        LowerAlias =:= LowerField -> Result;
        AliasQuoted =:= LowerField -> Result;
        true ->
            {ResultName, ReadOnly, RestFields} = find_original_field(Alias, Fields),
            {ResultName, ReadOnly, [OrigField | RestFields]}
    end.
