-module(dderl_diff).

-include("dderl.hrl").
-include("gres.hrl").
-include_lib("imem/include/imem_sql.hrl").

-export([
    term_diff/6
]).

-spec term_diff(term(), pid(), atom(), binary(), atom(), binary()) -> binary().
term_diff(Sess, SessPid, LeftType, LeftValue, RightType, RightValue) ->
    case Sess:run_cmd(term_diff, [LeftType, LeftValue, RightType, RightValue, [ignore_whitespace,ignore_casing,ignore_dquotes]]) of
        {error, {{Ex, M}, _Stacktrace} = Error} ->
            ?Error("Error on term_diff ~p: ~p", [{LeftType, LeftValue, RightType, RightValue}, Error], _Stacktrace),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            #{<<"error">> => Err};
        {error, {Ex,M}} ->
            ?Error("Error on term_diff ~p: ~p", [{LeftType, LeftValue, RightType, RightValue}, {Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            #{<<"error">> => Err};
        {error, Reason} ->
            ?Error("Error on term_diff ~p: ~p", [{LeftType, LeftValue, RightType, RightValue}, Reason]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Reason]))),
            #{<<"error">> => Err};
        DiffResult ->
            ?Debug("The diff result ~p", [DiffResult]),
            FsmCtx = get_fsmctx(DiffResult),
            StmtFsm = dderl_fsm:start(FsmCtx, SessPid),
            Columns = gen_adapter:build_column_json(lists:reverse(get_columns())),
            #{
                <<"columns">> => Columns,
                <<"sort_spec">> => [],
                <<"statement">> => base64:encode(term_to_binary(StmtFsm)),
                <<"connection">> => gen_adapter:encrypt_to_binary(Sess)
            }
    end.

-spec get_fsmctx([{ddTermDiff, integer(), binary(), binary(), binary()}]) -> #fsmctxs{}.
get_fsmctx(Result) ->
    % <<Id:32>> = crypto:strong_rand_bytes(4),
    RowCols = get_columns(),
    FullMap = build_full_map(RowCols),
    #fsmctxs{stmtRefs       = [self()]
            ,stmtTables     = [<<"term_diff">>]
            ,rowCols        = RowCols
            ,rowFun         = get_rowfun()
            ,sortFun        = get_sortfun()
            ,sortSpec       = []
            ,orig_qry       = <<>>
            ,bind_vals      = []
            ,block_length   = ?DEFAULT_ROW_SIZE
            ,fetch_recs_async_funs = 
                [fun(_Opts, _Count) ->
                    Rows = [{{}, {RowId, Left, Cmp, Right}} || {ddTermDiff, RowId, Left, Cmp, Right} <- Result],
                    % This seems hackish but we don't want to keep a process here.
                    % TODO: Revisit after tuple calls have been removed.
                    dderl_fsm:rows({self(), {Rows, true}}, {dderl_fsm, self()})
                end]
            ,fetch_close_funs = [fun() -> ok end]
            ,stmt_close_funs  = [fun() -> ok end]
            ,filter_and_sort_funs =
                [fun(_FilterSpec, SortSpec, _Cols) ->
                    SortSpecExplicit = [{Col, Dir} || {Col, Dir} <- SortSpec, is_integer(Col)],
                    NewSortFun = imem_sql_expr:sort_spec_fun(SortSpecExplicit, FullMap, FullMap),
                    {ok, <<>>, NewSortFun}
                end]
            ,update_cursor_prepare_funs =
                [fun(_ChangeList) ->
                    {error, <<"Updates not implmented for diff result table.">>}
                end]
            ,update_cursor_execute_funs =
                [fun(_Lock, _PrepStmt) ->
                    {error, <<"Updates not implmented for diff result table.">>}
                end]
    }.

-spec get_columns() -> [#rowCol{}].
get_columns() -> [
    #rowCol{tag=1, alias= <<"id">>, type=integer, readonly=false},
    #rowCol{tag=2, alias= <<"left">>, type=binstr, readonly=false},
    #rowCol{tag=3, alias= <<"cmp">>, type=binstr, readonly=false},
    #rowCol{tag=4, alias= <<"right">>, type=binstr, readonly=false}
].

-spec get_rowfun() -> fun().
get_rowfun() ->
    fun({{}, Row}) -> [fix_format(Col) || Col <- tuple_to_list(Row)] end.

-spec get_sortfun() -> fun().
get_sortfun() ->
    fun(_Row) -> {} end.

-spec fix_format(term()) -> term().
fix_format('$not_a_value') -> <<>>;
fix_format(Value) -> Value.

%% TODO: Taken from dderloci, put in a common place
%        after migration to oranif

build_full_map(Clms) ->
    [#bind{tag = list_to_atom([$$|integer_to_list(Tag)])
        , name = Alias
        , alias = Alias
        , tind = 2
        , cind = Tag
        , type = Type
        , len = Len
        , prec = undefined
    } || #rowCol{tag=Tag, alias=Alias, type=Type, len=Len} <- Clms].
