-module(gen_adapter).

-include("dderl.hrl").
-include("gres.hrl").
-include("dderl_sqlbox.hrl").

-include_lib("imem/include/imem_sql.hrl").

-export([ process_cmd/6
        , init/0
        , add_cmds_views/5
        , gui_resp/2
        , build_resp_fun/3
        , process_query/2
        ]).

init() -> ok.

-spec add_cmds_views({atom(), pid()} | undefined, ddEntityId(), atom(), boolean(), [tuple()]) -> [ddEntityId() | need_replace].
add_cmds_views(_, _, _, _, []) -> [];
add_cmds_views(Sess, UserId, A, R, [{N,C,Con}|Rest]) ->
    add_cmds_views(Sess, UserId, A, R, [{N,C,Con,#viewstate{}}|Rest]);
add_cmds_views(Sess, UserId, A, Replace, [{N,C,Con,#viewstate{}=V}|Rest]) ->
    case dderl_dal:get_view(Sess, N, A, UserId) of
        undefined ->
            Id = dderl_dal:add_command(Sess, UserId, A, N, C, Con, []),
            ViewId = dderl_dal:add_view(Sess, UserId, N, Id, V),
            [ViewId | add_cmds_views(Sess, UserId, A, Replace, Rest)];
        View ->
            if
                Replace ->
                    dderl_dal:update_command(Sess, View#ddView.cmd, UserId, A, N, C, Con, []),
                    ViewId = dderl_dal:add_view(Sess, UserId, N, View#ddView.cmd, V),
                    [ViewId | add_cmds_views(Sess, UserId, A, Replace, Rest)];
                true ->
                    [need_replace]
            end
    end.

-spec box_to_json(#box{}) -> [{binary(), term()}].
box_to_json(Box) ->
    [ {<<"ind">>, Box#box.ind}
    , {<<"name">>, any_to_bin(Box#box.name)}
    , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
    , {<<"collapsed">>, Box#box.collapsed}
    , {<<"error">>, Box#box.error}
    , {<<"color">>, Box#box.color}
    , {<<"pick">>, Box#box.pick}].

-spec any_to_bin(term()) -> binary().
any_to_bin(C) when is_list(C) -> list_to_binary(C);
any_to_bin(C) when is_binary(C) -> C;
any_to_bin(C) -> list_to_binary(lists:nth(1, io_lib:format("~p", [C]))).

-spec process_cmd({[binary()], [{binary(), list()}]}, binary(), {atom(), pid()}, ddEntityId(), pid(), term()) -> term().
process_cmd({[<<"parse_stmt">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"parse_stmt">>,BodyJson}] = ReqBody,
    Sql = string:strip(binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>))),
    ?Info("parsing ~p", [Sql]),
    case sqlparse:parsetree(Sql) of
        {ok, {[{ParseTree,_}|_], _}} ->
            case sqlparse:fold(ParseTree) of
                {error, Reason} ->
                    ?Error("parse_stmt error in fold ~p~n", [Reason]),
                    ReasonBin = iolist_to_binary(io_lib:format("Error parsing the sql: ~p", [Reason])),
                    From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}])};
                Flat ->
                    FlatTuple = {<<"flat">>, Flat},
                    BoxTuple = try dderl_sqlbox:boxed_from_pt(ParseTree) of
                        {error, BoxReason} ->
                            ?Error("Error ~p trying to get the box of the parse tree ~p", [BoxReason, ParseTree]),
                            {<<"boxerror">>, BoxReason};
                        Box ->
                            ?Info("The big box ~p", [Box]),
                            {<<"sqlbox">>, box_to_json(Box)}
                    catch
                        Class:Error ->
                            ?Error("Error ~p:~p trying to get the box of the parse tree ~p, the st: ~n~p~n", [Class, Error, ParseTree, erlang:get_stacktrace()]),
                            {<<"boxerror">>, iolist_to_binary(io_lib:format("~p:~p", [Class, Error]))}
                    end,
                    PrettyTuple = try dderl_sqlbox:pretty_from_pt(ParseTree) of
                        {error, PrettyReason} ->
                            ?Error("Error ~p trying to get the pretty of the parse tree ~p", [PrettyReason, ParseTree]),
                            {<<"prettyerror">>, PrettyReason};
                        Pretty ->
                            {<<"pretty">>, Pretty}
                    catch
                        Class1:Error1 ->
                            ?Error("Error ~p:~p trying to get the pretty from the parse tree ~p, the st: ~n~p~n", [Class1, Error1, ParseTree, erlang:get_stacktrace()]),
                            {<<"prettyerror">>, iolist_to_binary(io_lib:format("~p:~p", [Class1, Error1]))}
                    end,
                    ParseStmt = jsx:encode([{<<"parse_stmt">>, [BoxTuple, PrettyTuple, FlatTuple]}]),
                    ?Debug("Json -- ~s", [jsx:prettify(ParseStmt)]),
                    From ! {reply, ParseStmt}
            end;
        {parse_error, {PError, Tokens}} ->
            ?Error("parse_stmt error in parsetree ~p~n", [{PError, Tokens}]),
            ReasonBin = iolist_to_binary(io_lib:format("Error parsing the sql: ~p", [{PError, Tokens}])),
            From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}])};
        {lex_error, LError} ->
            ?Error("lexer error in parsetree ~p~n", [LError]),
            ReasonBin = iolist_to_binary(io_lib:format("Lexer error: ~p", [LError])),
            From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}])}
    end;
process_cmd({[<<"get_query">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"get_query">>,BodyJson}] = ReqBody,
    Table = proplists:get_value(<<"table">>, BodyJson, <<>>),
    Query = "SELECT * FROM " ++ binary_to_list(Table),
    ?Debug("get query ~p~n", [Query]),
    Res = jsx:encode([{<<"qry">>,[{<<"name">>,Table},{<<"content">>,list_to_binary(Query)}]}]),
    From ! {reply, Res};
process_cmd({[<<"save_view">>], ReqBody}, Adapter, Sess, UserId, From, _Priv) ->
    [{<<"save_view">>,BodyJson}] = ReqBody,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, <<>>),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, <<>>),
    ReplaceView = proplists:get_value(<<"replace">>, BodyJson, false),
    ?Info("save_view for ~p layout ~p", [Name, TableLay]),
    case add_cmds_views(Sess, UserId, Adapter, ReplaceView, [{Name, Query, undefined, #viewstate{table_layout=TableLay, column_layout=ColumLay}}]) of 
        [need_replace] ->
            Res = jsx:encode([{<<"save_view">>,[{<<"need_replace">>, Name}]}]);
        [ViewId] ->
            Res = jsx:encode([{<<"save_view">>,[{<<"view_id">>, ViewId}]}])
    end,
    From ! {reply, Res};
process_cmd({[<<"update_view">>], ReqBody}, Adapter, Sess, UserId, From, _Priv) ->
    [{<<"update_view">>,BodyJson}] = ReqBody,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, <<>>),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, <<>>),
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    ?Info("update view ~s with id ~p layout ~p", [Name, ViewId, TableLay]),
    ViewState = #viewstate{table_layout=TableLay, column_layout=ColumLay},
    if
        %% System tables can't be overriden.
        Name =:= <<"All Views">> orelse Name =:= <<"All Tables">> ->
            add_cmds_views(Sess, UserId, Adapter, true, [{Name, Query, undefined, ViewState}]),
            Res = jsx:encode([{<<"update_view">>, <<"ok">>}]);
        true ->
            %% TODO: We need to pass the userid to provide authorization.
            case dderl_dal:update_view(Sess, ViewId, ViewState, Query) of
                {error, Reason} ->
                    Res = jsx:encode([{<<"update_view">>, [{<<"error">>, Reason}]}]);
                _ ->
                    Res = jsx:encode([{<<"update_view">>, <<"ok">>}])
            end
    end,
    From ! {reply, Res};
process_cmd({[<<"save_dashboard">>], ReqBody}, _Adapter, Sess, UserId, From, _Priv) ->
    [{<<"dashboard">>, BodyJson}] = ReqBody,
    Id = proplists:get_value(<<"id">>, BodyJson, -1),
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Views = proplists:get_value(<<"views">>, BodyJson, []),
    case dderl_dal:save_dashboard(Sess, UserId, Id, Name, Views) of
        {error, Reason} ->
            Res = jsx:encode([{<<"save_dashboard">>, [{<<"error">>, Reason}]}]);
        NewId ->
            Res = jsx:encode([{<<"save_dashboard">>, NewId}])
    end,
    From ! {reply, Res};
process_cmd({[<<"dashboards">>], _ReqBody}, _Adapter, Sess, UserId, From, _Priv) ->
    case dderl_dal:get_dashboards(Sess, UserId) of
        [] ->
            ?Debug("No dashboards found for the user ~p", [UserId]),
            Res = jsx:encode([{<<"dashboards">>,[]}]);
        Dashboards ->
            ?Debug("dashboards of the user ~p, ~n~p", [UserId, Dashboards]),
            DashboardsAsProplist = [[{<<"id">>, D#ddDash.id}, {<<"name">>, D#ddDash.name}, {<<"views">>, D#ddDash.views}] || D <- Dashboards],
            Res = jsx:encode([{<<"dashboards">>, DashboardsAsProplist}]),
            ?Debug("dashboards as json ~s", [jsx:prettify(Res)])
    end,
    From ! {reply, Res};
process_cmd({Cmd, _BodyJson}, _Adapter, _Sess, _UserId, From, _Priv) ->
    ?Error("Unknown cmd ~p ~p~n", [Cmd, _BodyJson]),
    From ! {reply, jsx:encode([{<<"error">>, <<"unknown command">>}])}.

-spec process_query(binary(), tuple()) -> list().
process_query(Query, Connection) ->
    imem_adapter:process_query(Query, Connection).

%%%%%%%%%%%%%%%

-spec col2json([#stmtCol{}]) -> [binary()].
col2json(Cols) -> col2json(lists:reverse(Cols), [], length(Cols)).

-spec col2json([#stmtCol{}], [binary()], integer()) -> [binary()].
col2json([], JCols, _Counter) -> [<<"id">>,<<"op">>|JCols];
col2json([C|Cols], JCols, Counter) ->
    Nm = C#stmtCol.alias,
    BinCounter = integer_to_binary(Counter),
    Nm1 = <<Nm/binary, $_, BinCounter/binary>>,
    col2json(Cols, [Nm1 | JCols], Counter - 1).

-spec gui_resp(#gres{}, [#stmtCol{}]) -> [{binary(), term()}].
gui_resp(#gres{} = Gres, Columns) ->
    JCols = col2json(Columns),
    ?Debug("processing resp ~p cols ~p jcols ~p", [Gres, Columns, JCols]),
    % refer to erlimem/src/gres.hrl for the descriptions of the record fields
    [{<<"op">>,         Gres#gres.operation}
    ,{<<"cnt">>,        Gres#gres.cnt}
    ,{<<"toolTip">>,    Gres#gres.toolTip}
    ,{<<"message">>,    Gres#gres.message}
    ,{<<"beep">>,       Gres#gres.beep}
    ,{<<"state">>,      Gres#gres.state}
    ,{<<"loop">>,       Gres#gres.loop}
    ,{<<"rows">>,       r2jsn(Gres#gres.rows, JCols)}
    ,{<<"keep">>,       Gres#gres.keep}
    ,{<<"focus">>,      Gres#gres.focus}
    ,{<<"sql">>,        Gres#gres.sql}
    ,{<<"disable">>,    Gres#gres.disable}
    ,{<<"promote">>,    Gres#gres.promote}
    ,{<<"max_width_vec">>, lists:flatten(r2jsn([widest_cell_per_clm(Gres#gres.rows)], JCols))}
    ].

-spec widest_cell_per_clm(list()) -> [binary()].
widest_cell_per_clm([]) -> [];
widest_cell_per_clm([R|_] = Rows) ->
    widest_cell_per_clm(Rows, lists:duplicate(length(R), <<>>)).

-spec widest_cell_per_clm(list(), [binary()]) -> [binary()].
widest_cell_per_clm([],V) -> V;
widest_cell_per_clm([R|Rows],V) ->
    NewV = 
    [case {Re, Ve} of
        {Re, Ve} ->
            ReS = if
                is_atom(Re)     -> atom_to_binary(Re, utf8);
                is_integer(Re)  -> integer_to_binary(Re);
                %is_list(Re)     -> list_to_binary(Re);
                true            -> Re
            end,
            ReL = byte_size(ReS),
            VeL = byte_size(Ve),
            if ReL > VeL -> ReS; true -> Ve end
     end
    || {Re, Ve} <- lists:zip(R,V)],
    widest_cell_per_clm(Rows,NewV).

-spec r2jsn(list(), [binary()]) -> list().
r2jsn(Rows, JCols) -> r2jsn(Rows, JCols, []).
r2jsn([], _, NewRows) -> lists:reverse(NewRows);
r2jsn([[]], _, NewRows) -> lists:reverse(NewRows);
r2jsn([Row|Rows], JCols, NewRows) ->
    r2jsn(Rows, JCols, [
        [{C, case R of
                R when is_integer(R) -> R;
                R when is_atom(R)    -> atom_to_binary(R, utf8);
                R when is_binary(R)  ->
                     %% check if it is a valid utf8
                     %% else we convert it from latin1
                     case unicode:characters_to_binary(R) of
                         Invalid when is_tuple(Invalid) -> 
                             unicode:characters_to_binary(R, latin1, utf8);
                         UnicodeBin ->
                             UnicodeBin
                     end
             end
         }
        || {C, R} <- lists:zip(JCols, Row)]
    | NewRows]).

-spec build_resp_fun(binary(), [#stmtCol{}], pid()) -> fun().
build_resp_fun(Cmd, Clms, From) ->
    fun(#gres{} = GuiResp) ->
        GuiRespJson = gui_resp(GuiResp, Clms),
        case (catch jsx:encode([{Cmd,GuiRespJson}])) of
            {'EXIT', Error} -> ?Error("Encoding problem ~p ~p~n~p~n~p", [Cmd, Error, GuiResp, GuiRespJson]);
            Resp -> From ! {reply, Resp}
        end
    end.
