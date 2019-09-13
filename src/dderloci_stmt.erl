-module(dderloci_stmt).
-behaviour(gen_server).

-include("dderloci.hrl").

-export([prepare/4,
    execute/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(NoCommit, 0).
-define(AutoCommit, 1).

-record(stmt, {columns = [],
               del_rows = [],
               upd_rows = [],
               ins_rows = [],
               del_stmt,
               upd_stmt,
               ins_stmt,
               connection}).

-record(row, {index, id, pos, op, values}).

%%% API Implementation

-spec prepare(binary(), list(), tuple(), list()) -> {error, term()} | {ok, pid()}.
prepare(TableName, ChangeList, Connection, Columns) ->
	gen_server:start(?MODULE, [TableName, ChangeList, Connection, Columns], [{timeout, ?InitTimeout}]).

-spec execute(pid()) -> {error, term()} | list().
execute(Pid) ->
	gen_server:call(Pid, execute, ?ExecTimeout).

%%% gen_server callbacks
init([TableName, ChangeList, Connection, Columns]) ->
    case create_stmts(TableName, Connection, ChangeList, Columns) of
        {ok, Stmt} -> {ok, Stmt};
        {error, Error} -> {stop, Error}
    end.

handle_call(execute, _From, #stmt{columns = Columns, connection = Connection} = Stmt) ->
    try
        case process_delete(Stmt#stmt.del_stmt, Stmt#stmt.del_rows, Columns) of
            {ok, DeleteChangeList} ->
                case process_update(Stmt#stmt.upd_stmt, Stmt#stmt.upd_rows, Columns) of
                    {ok, UpdateChangeList} ->
                        case process_insert(Stmt#stmt.ins_stmt, Stmt#stmt.ins_rows, Columns) of
                            {ok, InsertChangeList} ->
                                Connection:commit(),
                                {stop, normal, DeleteChangeList ++ UpdateChangeList ++ InsertChangeList, Stmt};
                            Error ->
                                Connection:rollback(),
                                {stop, normal, Error, Stmt}
                        end;
                    Error ->
                        Connection:rollback(),
                        {stop, normal, Error, Stmt}
                end;
            Error ->
                Connection:rollback(),
                {stop, normal, Error, Stmt}
        end
    catch _Class:Error2 ->
            Connection:rollback(),
            {stop, normal, {error, Error2}, Stmt}
    end.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #stmt{del_stmt = DelStmt, upd_stmt = UpdStmt, ins_stmt = InsStmt}) ->
    %% Delete is not a list since it is always only one.
    close_stmts([Stmt || Stmt <- lists:flatten([DelStmt, UpdStmt, InsStmt]), Stmt =/= undefined]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions

-spec table_name(binary() | {as, binary(), binary()}) -> binary().
table_name(TableName) when is_binary(TableName) -> TableName;
table_name({as, TableName, Alias}) -> iolist_to_binary([TableName, " ", Alias]).

-spec alias_name(binary() | {as, binary(), binary()}) -> binary().
alias_name(TableName) when is_binary(TableName) -> TableName;
alias_name({as, _TableName, Alias}) -> Alias.

-spec create_stmts(binary(), tuple(), list(), list()) -> {ok, #stmt{}} | {error, term()}.
create_stmts(TableName, Connection, ChangeList, Columns) ->
    {DeleteList, UpdateListTotal, InsertListTotal} = split_changes(ChangeList),
    UpdateList = split_by_columns_mod(UpdateListTotal, Columns, []),
    InsertList = split_by_non_empty(InsertListTotal, []),
    case create_stmts([{del, DeleteList}, {upd, UpdateList}, {ins, InsertList}], TableName, Connection, Columns, []) of
        {ok, Stmt} ->
            {ok, Stmt#stmt{del_rows = DeleteList, upd_rows = UpdateList, ins_rows = InsertList, connection = Connection}};
        Error ->
            Error
    end.

-spec create_stmts([{atom(), list()}], binary(), tuple(), list(), list()) -> {ok, #stmt{}} | {error, term()}.
create_stmts([], _TableName, _Connection, _Columns, []) ->
    {error, <<"empty change list">>};
create_stmts([], _TableName, _Connection, Columns, ResultStmts) ->
    DelStmt = proplists:get_value(del, ResultStmts),
    UpdStmt = proplists:get_value(upd, ResultStmts),
    InsStmt = proplists:get_value(ins, ResultStmts),
    {ok, #stmt{columns = Columns, del_stmt = DelStmt, upd_stmt = UpdStmt, ins_stmt = InsStmt}};
create_stmts([{del, []} | Rest], TableName, Connection, Columns, ResultStmt) ->
    create_stmts(Rest, TableName, Connection, Columns, ResultStmt);
create_stmts([{del, _DelList} | Rest], TableName, Connection, Columns, ResultStmt) ->
    Sql = iolist_to_binary([<<"delete from ">>, table_name(TableName), " where ", alias_name(TableName), ".ROWID = :IDENTIFIER"]),
    case Connection:prep_sql(Sql) of
        {error, {_ErrorCode, ErrorMsg}} ->
            %TODO: add ?Error ...(ErrorCode)...
            close_stmts(ResultStmt),
            {error, ErrorMsg};
        Stmt ->
            BindTypes = [{<<":IDENTIFIER">>, 'SQLT_STR'}],
            Stmt:bind_vars(BindTypes),
            create_stmts(Rest, TableName, Connection, Columns, [{del, Stmt} | ResultStmt])
    end;
create_stmts([{upd, []} | Rest], TableName, Connection, Columns, ResultStmt) ->
    create_stmts(Rest, TableName, Connection, Columns, ResultStmt);
%[{{1,2,3},
% [{row,{{<<"AAAHNHAABAAALGRAAO">>,<<"Á\"">>,<<>>,<<"Á\v3">>,<<>>,<<128>>},{}},
%       <<"AAAHNHAABAAALGRAAO">>,15,upd,
%       [<<"22">>,<<"0.29">>,<<"12.0">>,<<>>,<<"0">>]}]},
%{{1,3},
% [{row,{{<<"AAAHNHAABAAALGRAAR">>,
%         <<128>>,
%         <<194,23>>,
%         <<"?DFOf">>,
%         <<190,4,34,47>>,
%         <<>>},
%        {}},
%       <<"AAAHNHAABAAALGRAAR">>,18,upd,
%       [<<"12">>,<<"2200">>,<<"9383.393">>,<<"0.0000033346">>,<<>>]},
%  {row,{{<<"AAAHNHAABAAALGRAAT">>,<<"Á\"">>,<<>>,<<128>>,<<>>,<<>>},{}},
%       <<"AAAHNHAABAAALGRAAT">>,20,upd,
%       [<<"44">>,<<>>,<<"8">>,<<>>,<<>>]}]}]
create_stmts([{upd, UpdList} | Rest], TableName, Connection, Columns, ResultStmts) ->
    [{ModifiedCols, _Rows} | RestUpdList] = UpdList,
    FilterColumns = filter_columns(ModifiedCols, Columns),
    UpdVars = create_upd_vars(FilterColumns),
    % TODO: use where part to do optimistic locking.
    % WhereVars = create_where_vars(Columns),
    Sql = iolist_to_binary([<<"update ">>, table_name(TableName), " set ", UpdVars, " where ", alias_name(TableName), ".ROWID = :IDENTIFIER"]),
    case Connection:prep_sql(Sql) of
        {error, {_ErrorCode, ErrorMsg}} ->
            %TODO: add ?Error ...(ErrorCode)...
            close_stmts(ResultStmts),
            {error, ErrorMsg};
        Stmt ->
            BindTypes = [{<<":IDENTIFIER">>, 'SQLT_STR'} | create_bind_types(FilterColumns)],
            Stmt:bind_vars(BindTypes),
            case proplists:get_value(upd, ResultStmts) of
                undefined ->
                    NewResultStmts = [{upd, [Stmt]} | ResultStmts];
                UpdtStmts ->
                    NewResultStmts = lists:keyreplace(upd, 1, ResultStmts, {upd, UpdtStmts ++ [Stmt]})
            end,
            create_stmts([{upd, RestUpdList} | Rest], TableName, Connection, Columns, NewResultStmts)
    end;
create_stmts([{ins, []} | Rest], TableName, Connection, Columns, ResultStmt) ->
    create_stmts(Rest, TableName, Connection, Columns, ResultStmt);
%[{{1,2,3},[{row,{{}},undefined,2,ins,[<<"3.33">>,<<"22">>,<<"23">>]}]},
%{{1,3},
% [{row,{{}},undefined,3,ins,[<<"4.0">>,<<>>,<<"231">>]},
%  {row,{{}},undefined,4,ins,[<<"87.4">>,<<>>,<<"44">>]}]},
%{{2,3},
% [{row,{{}},undefined,5,ins,[<<>>,<<"12">>,<<"55">>]},
%  {row,{{}},undefined,6,ins,[<<>>,<<"33">>,<<"22">>]}]}]
create_stmts([{ins, InsList} | Rest], TableName, Connection, Columns, ResultStmts) ->
    [{NonEmptyCols, _Rows} | RestInsList] = InsList,
    InsColumns = ["(", create_ins_columns(filter_columns(NonEmptyCols, Columns)), ")"],
    Sql = iolist_to_binary([<<"insert into ">>, table_name(TableName), " ", InsColumns, " values ", "(", create_ins_vars(filter_columns(NonEmptyCols, Columns)), ")"]),
    case Connection:prep_sql(Sql) of
        {error, {_ErrorCode, ErrorMsg}} ->
            %%TODO: ?Error...
            close_stmts(ResultStmts),
            {error, ErrorMsg};
        Stmt ->
            BindTypes = create_bind_types(filter_columns(NonEmptyCols, Columns)),
            Stmt:bind_vars(BindTypes),
            case proplists:get_value(ins, ResultStmts) of
                undefined ->
                    NewResultStmts = [{ins, [Stmt]} | ResultStmts];
                InsStmts ->
                    NewResultStmts = lists:keyreplace(ins, 1, ResultStmts, {ins, InsStmts ++ [Stmt]})
            end,
            create_stmts([{ins, RestInsList} | Rest], TableName, Connection, Columns, NewResultStmts)
    end.

-spec process_delete(term(), list(), list()) -> {ok, list()} | {error, term()}.
process_delete(undefined, [], _Columns) -> {ok, []};
process_delete(PrepStmt, Rows, _Columns) ->
    RowsToDelete = [list_to_tuple(create_bind_vals([Row#row.id], [#rowCol{type = 'SQLT_STR'}])) || Row <- Rows],
    case PrepStmt:exec_stmt(RowsToDelete, ?NoCommit) of
        {rowids, _RowIds} -> %% TODO: Check if the modified rows are the correct.
            ChangedKeys = [{Row#row.pos, {{}, {}}} || Row <- Rows],
            {ok, ChangedKeys};
        {error, {_ErrorCode, ErrorMsg}}->
            {error, ErrorMsg}
    end.

-spec process_update(list(), list(), [#rowCol{}]) -> {ok, list()} | {error, term()}.
process_update(undefined, [], _Columns) -> {ok, []};
process_update([], [], _Colums) -> {ok, []};
process_update([PrepStmt | RestStmts], [{ModifiedCols, Rows} | RestRows], Columns) ->
    %% TODO: No need to filter the rows for optimistic locking...
    FilterRows = [Row#row{values = filter_columns(ModifiedCols, Row#row.values)} || Row <- Rows],
    case process_one_update(PrepStmt, FilterRows, filter_columns(ModifiedCols, Columns), Rows, Columns) of
        {ok, ChangedKeys} ->
            case process_update(RestStmts, RestRows, Columns) of
               {ok, RestChangedKeys} ->
                    {ok, ChangedKeys ++ RestChangedKeys};
               ErrorRest ->
                    ErrorRest
            end;
        Error ->
            Error
    end.

-spec process_one_update(term(), [#row{}], [#rowCol{}], [#row{}], [#rowCol{}]) -> {ok, list()} | {error, term()}.
process_one_update(PrepStmt, FilterRows, FilterColumns, Rows, Columns) ->
    %% TODO: Implement updates using the old values on the where clause, (optimistic locking).
    RowsToUpdate = [list_to_tuple(create_bind_vals([Row#row.id | Row#row.values], [#rowCol{type = 'SQLT_STR'} | FilterColumns])) || Row <- FilterRows],
    case PrepStmt:exec_stmt(RowsToUpdate, ?NoCommit) of
        {rowids, RowIds} ->
            case check_rowid(RowIds, Rows) of
                true->
                    ChangedKeys = [{Row#row.pos, {{}, list_to_tuple(create_changedkey_vals(Row#row.values ++ [Row#row.id], Columns ++ [#rowCol{type = 'SQLT_STR'}]))}} || Row <- Rows],
                    {ok, ChangedKeys};
                false ->
                    {error, <<"Unknown error updating the rows.">>};
                long_rowid ->
                    {error, <<"Updating tables with universal rowids is not supported yet.">>}
            end;
        {error, {_ErrorCode, ErrorMsg}}->
            {error, ErrorMsg}
    end.

-spec process_insert(term(), list(), list()) -> {ok, list()} | {error, term()}.
process_insert(undefined, [], _Columns) -> {ok, []};
process_insert([], [], _Columns) -> {ok, []};
process_insert([PrepStmt | RestStmts], [{NonEmptyCols, Rows} | RestRows], Columns) ->
    FilterRows = [Row#row{values = filter_columns(NonEmptyCols, Row#row.values)} || Row <- Rows],
    case process_one_insert(PrepStmt, FilterRows, filter_columns(NonEmptyCols, Columns), Rows, Columns) of
        {ok, ChangedKeys} ->
            case process_insert(RestStmts, RestRows, Columns) of
                {ok, RestChangedKeys} ->
                    {ok, ChangedKeys ++ RestChangedKeys};
                ErrorRest ->
                    ErrorRest
            end;
        Error ->
            Error
    end.

-spec process_one_insert(term(), [#row{}], [#rowCol{}], [#row{}], [#rowCol{}]) -> {ok, list()} | {error, term()}.
process_one_insert(PrepStmt, FilterRows, FilterColumns, Rows, Columns) ->
    RowsToInsert = [list_to_tuple(create_bind_vals(Row#row.values, FilterColumns)) || Row <- FilterRows],
    case PrepStmt:exec_stmt(RowsToInsert, ?NoCommit) of
        {rowids, RowIds} ->
            if
                length(RowIds) =:= length(RowsToInsert) ->
                    case inserted_changed_keys(RowIds, Rows, Columns) of
                        {error, ErrorMsg} ->
                            {error, ErrorMsg};
                        ChangedKeys ->
                            {ok, ChangedKeys}
                    end;
                true ->
                    %% TODO: What is a good message here ?
                    {error, <<"Error inserting the rows.">>}
            end;
        {error, {_ErrorCode, ErrorMsg}}->
            {error, ErrorMsg}
    end.

-spec split_changes(list()) -> {[#row{}], [#row{}], [#row{}]}.
split_changes(ChangeList) ->
    split_changes(ChangeList, {[], [], []}).

split_changes([], Result) -> Result;
split_changes([ListRow | ChangeList], Result) ->
    [Pos, Op, Index | Values] = ListRow,
    case Index of
        {{}, {}}  -> RowId = undefined;
        {{}, Idx} -> RowId = element(tuple_size(Idx), Idx);
        _ ->         RowId = undefined
    end,
    Row = #row{index  = Index,
               id     = RowId,
               pos    = Pos,
               op     = Op,
               values = Values},
    NewResult = add_to_split_result(Row, Result),
    split_changes(ChangeList, NewResult).

%%TODO: Change for less verbose option setelement...
add_to_split_result(#row{op = del} = Row, {DeleteRows, UpdateRows, InsertRows}) ->
    {[Row | DeleteRows], UpdateRows, InsertRows};
add_to_split_result(#row{op = upd} = Row, {DeleteRows, UpdateRows, InsertRows}) ->
    {DeleteRows, [Row | UpdateRows], InsertRows};
add_to_split_result(#row{op = ins} = Row, {DeleteRows, UpdateRows, InsertRows}) ->
    {DeleteRows, UpdateRows, [Row | InsertRows]}.

filter_columns(ModifiedCols, Columns) ->
    ModifiedColsList = tuple_to_list(ModifiedCols),
    [lists:nth(ColIdx, Columns) || ColIdx <- ModifiedColsList].

create_upd_vars([#rowCol{} = Col]) -> [Col#rowCol.tag, " = :", "\"", Col#rowCol.tag, "\""];
create_upd_vars([#rowCol{} = Col | Rest]) -> [Col#rowCol.tag, " = :", "\"", Col#rowCol.tag, "\"", ", ", create_upd_vars(Rest)].

create_bind_types([]) -> [];
create_bind_types([#rowCol{} = Col | Rest]) ->
    [{iolist_to_binary([":", "\"", Col#rowCol.tag, "\""]), bind_types_map(Col#rowCol.type)} | create_bind_types(Rest)].

create_ins_columns([#rowCol{} = Col]) -> [Col#rowCol.tag];
create_ins_columns([#rowCol{} = Col | Rest]) -> [Col#rowCol.tag, ", ", create_ins_columns(Rest)].

create_ins_vars([#rowCol{} = Col]) -> [":", "\"", Col#rowCol.tag, "\""];
create_ins_vars([#rowCol{} = Col | Rest]) -> [":", "\"", Col#rowCol.tag, "\"", ", ", create_ins_vars(Rest)].

create_changedkey_vals([], _Cols) -> [];
create_changedkey_vals([<<>> | Rest], [#rowCol{} | RestCols]) ->
    [<<>> | create_changedkey_vals(Rest, RestCols)];
create_changedkey_vals([Value | Rest], [#rowCol{type = 'SQLT_NUM', len = Scale, prec = dynamic} | RestCols]) ->
    Number = imem_datatype:io_to_decimal(Value, undefined, Scale),
    [Number | create_changedkey_vals(Rest, RestCols)];
create_changedkey_vals([Value | Rest], [#rowCol{type = Type, len = Len, prec = Prec} | RestCols]) ->
    FormattedValue = case Type of
        'SQLT_DAT' -> dderloci_utils:dderltime_to_ora(Value);
        'SQLT_TIMESTAMP' -> dderloci_utils:dderlts_to_ora(Value);
        'SQLT_TIMESTAMP_TZ' -> dderloci_utils:dderltstz_to_ora(Value);
        'SQLT_NUM' -> imem_datatype:io_to_decimal(Value, Len, Prec);
        'SQLT_BIN' -> imem_datatype:binary_to_io(Value);
        _ -> Value 
    end,
    [FormattedValue | create_changedkey_vals(Rest, RestCols)].

create_bind_vals([], _Cols) -> [];
create_bind_vals([<<>> | Rest], [_Col | RestCols]) ->
    [<<>> | create_bind_vals(Rest, RestCols)];
create_bind_vals([Value | Rest], [#rowCol{type = Type, len = Len} | RestCols]) ->
    FormattedValue = case Type of
        'SQLT_DAT' -> dderloci_utils:dderltime_to_ora(Value);
        'SQLT_TIMESTAMP' -> dderloci_utils:dderlts_to_ora(Value);
        'SQLT_TIMESTAMP_TZ' -> dderloci_utils:dderltstz_to_ora(Value);
        'SQLT_NUM' -> dderloci_utils:oranumber_encode(Value);
        'SQLT_BIN' -> imem_datatype:io_to_binary(Value, Len);
        _ -> Value
    end,
    [FormattedValue | create_bind_vals(Rest, RestCols)].

bind_types_map('SQLT_NUM') -> 'SQLT_VNU';
%% There is no really support for this types at the moment so use string to send the data...
bind_types_map('SQLT_INT') -> 'SQLT_STR';
bind_types_map('SQLT_FLT') -> 'SQLT_STR';
bind_types_map('SQLT_CLOB') -> 'SQLT_STR';
bind_types_map(Type) -> Type.

-spec inserted_changed_keys([binary()], [#row{}], list()) -> [tuple()].
inserted_changed_keys([], [], _) -> [];
inserted_changed_keys([RowId | RestRowIds], [Row | RestRows], Columns) ->
    [{Row#row.pos, {{}, list_to_tuple(create_changedkey_vals(Row#row.values ++ [RowId], Columns ++ [#rowCol{type = 'SQLT_STR'}]))}} | inserted_changed_keys(RestRowIds, RestRows, Columns)];
inserted_changed_keys(_, _, _) ->
    {error, <<"Invalid row keys returned by the oracle driver">>}.

-spec split_by_columns_mod([#row{}], [#rowCol{}], [{tuple(), [#row{}]}]) -> [{tuple(), [#row{}]}].
split_by_columns_mod([], _Columns, Result) -> Result;
split_by_columns_mod([#row{} = Row | RestRows], Columns, Result) ->
    case list_to_tuple(get_modified_cols(Row, Columns)) of
        {} -> %% No changes in the row, nothing to do
            NewResult = Result;
        ModifiedCols ->
            case proplists:get_value(ModifiedCols, Result) of
                undefined ->
                    NewResult = [{ModifiedCols, [Row]} | Result];
                RowsSameCol ->
                    NewResult = lists:keyreplace(ModifiedCols, 1, Result, {ModifiedCols, [Row | RowsSameCol]})
            end
    end,
    split_by_columns_mod(RestRows, Columns, NewResult).

-spec get_modified_cols(#row{}, [#rowCol{}]) -> [integer()].
get_modified_cols(#row{index = Index, values = Values}, Columns) ->
    {{}, OriginalValuesTuple} = Index,
    [_RowId | OriginalValuesR] = lists:reverse(tuple_to_list(OriginalValuesTuple)),
    OriginalValues = lists:reverse(OriginalValuesR),
    %% If we dont have rowid should be read only field.
    LengthOrig = length(OriginalValues),
    LengthOrig = length(Values),
    get_modified_cols(OriginalValues, Values, Columns, 1).

%% TODO: This should apply the same functions used by the rowfun to avoid repeating the code here again
%% null -> <<>> should be the default convertion .
-spec get_modified_cols([binary()], [binary()], [#rowCol{}], pos_integer()) -> [integer()].
get_modified_cols([], [], [], _) -> [];
get_modified_cols([_Orig | RestOrig], [_Value | RestValues], [#rowCol{readonly=true} | Columns], Pos) ->
    get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
get_modified_cols([<<>> | RestOrig], [<<>> | RestValues], [#rowCol{} | Columns], Pos) ->
    get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
get_modified_cols([<<>> | RestOrig], [_Value | RestValues], [#rowCol{} | Columns], Pos) ->
    [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)];
get_modified_cols([OrigVal | RestOrig], [Value | RestValues], [#rowCol{type = 'SQLT_DAT'} | Columns], Pos) ->
    case dderloci_utils:ora_to_dderltime(OrigVal) of
        Value ->
            get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
        _ ->
            [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)]
    end;
get_modified_cols([OrigVal | RestOrig], [Value | RestValues], [#rowCol{type = 'SQLT_TIMESTAMP'} | Columns], Pos) ->
    case dderloci_utils:ora_to_dderlts(OrigVal) of
        Value ->
            get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
        _ ->
            [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)]
    end;
get_modified_cols([OrigVal | RestOrig], [Value | RestValues], [#rowCol{type = 'SQLT_TIMESTAMP_TZ'} | Columns], Pos) ->
    case dderloci_utils:ora_to_dderltstz(OrigVal) of
        Value ->
            get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
        _ ->
            [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)]
    end;
get_modified_cols([null | RestOrig], [<<>> | RestValues], [#rowCol{type = 'SQLT_NUM'} | Columns], Pos) ->
    get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
get_modified_cols([null | RestOrig], [_Value | RestValues], [#rowCol{type = 'SQLT_NUM'} | Columns], Pos) ->
    [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)];
get_modified_cols([Mantissa | RestOrig], [Value | RestValues], [#rowCol{type = 'SQLT_NUM', len = Scale, prec = dynamic} | Columns], Pos) ->
    Number = dderloci_utils:clean_dynamic_prec(imem_datatype:decimal_to_io(Mantissa, Scale)),
    if
        Number =:= Value ->
            get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
        true ->
            [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)]
    end;
get_modified_cols([Mantissa | RestOrig], [Value | RestValues], [#rowCol{type = 'SQLT_NUM', prec = Prec} | Columns], Pos) ->
    Number = imem_datatype:decimal_to_io(Mantissa, Prec),
    if
        Number =:= Value ->
            get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
        true ->
            [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)]
    end;
get_modified_cols([OrigVal | RestOrig], [OrigVal | RestValues], [#rowCol{} | Columns], Pos) ->
    get_modified_cols(RestOrig, RestValues, Columns, Pos + 1);
get_modified_cols([_OrigVal | RestOrig], [_Value | RestValues], [#rowCol{} | Columns], Pos) ->
    [Pos | get_modified_cols(RestOrig, RestValues, Columns, Pos + 1)].

-spec split_by_non_empty([#row{}], [{tuple(),[#row{}]}]) -> [{tuple(), [#row{}]}].
split_by_non_empty([], Result) -> Result;
split_by_non_empty([#row{values = Values} = Row | RestRows], Result) ->
    NonEmptyCols = list_to_tuple(get_non_empty_cols(Values, 1)),
    case proplists:get_value(NonEmptyCols, Result) of
        undefined ->
            NewResult = [{NonEmptyCols, [Row]} | Result];
        RowsSameCol ->
            NewResult = lists:keyreplace(NonEmptyCols, 1, Result, {NonEmptyCols, [Row | RowsSameCol]})
    end,
    split_by_non_empty(RestRows, NewResult).

-spec get_non_empty_cols([binary()], pos_integer()) -> [integer()].
get_non_empty_cols([], _) -> [];
get_non_empty_cols([<<>> | RestValues], Pos) ->
    get_non_empty_cols(RestValues, Pos + 1);
get_non_empty_cols([_Value | RestValues], Pos) ->
    [Pos | get_non_empty_cols(RestValues, Pos + 1)].

-spec close_stmts(list() | undefined) -> ok.
close_stmts(undefined) -> ok;
close_stmts([]) -> ok;
close_stmts([{del, Stmt} | RestStmts]) ->
    Stmt:close(),
    close_stmts(RestStmts);
close_stmts([{upd, Stmts} | RestStmts]) ->
    [Stmt:close() || Stmt <- Stmts],
    close_stmts(RestStmts);
close_stmts([{ins, Stmt} | RestStmts]) ->
    Stmt:close(),
    close_stmts(RestStmts);
close_stmts([Stmt | RestStmts]) ->
    Stmt:close(),
    close_stmts(RestStmts).

-spec check_rowid([binary()], [#row{}]) -> true | false | long_rowid.
check_rowid(RowIds, Rows) when length(RowIds) =:= length(Rows) ->
    check_member(RowIds, Rows);
check_rowid(_RowIds, _Rows) ->
    false.

-spec check_member([binary()], [#row{}]) -> true | false | long_rowid.
check_member(_, []) -> true;
check_member(RowIds, [Row | RestRows]) ->
    case lists:member(Row#row.id, RowIds) of
        true ->
            check_member(RowIds, RestRows);
        false ->
            case size(Row#row.id) > 19 of
                true -> long_rowid;
                false -> false
            end
    end.
