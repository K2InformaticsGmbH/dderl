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
        , process_query/4
        , build_column_json/1
        , build_column_csv/1
        , extract_modified_rows/1
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
                    dderl_dal:update_command(Sess, View#ddView.cmd, UserId, N, C, []),
                    ViewId = dderl_dal:add_view(Sess, UserId, N, View#ddView.cmd, V),
                    [ViewId | add_cmds_views(Sess, UserId, A, Replace, Rest)];
                true ->
                    [need_replace]
            end
    end.

-spec process_cmd({[binary()], [{binary(), list()}]}, binary(), {atom(), pid()}, ddEntityId(), pid(), term()) -> term().
process_cmd({[<<"parse_stmt">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"parse_stmt">>,BodyJson}] = ReqBody,
    Sql = string:strip(binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>))),
    if
        Sql =:= [] ->
            From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, <<"Empty sql string">>}, {<<"flat">>, <<>>}]}])};
        true ->
            case sqlparse:parsetree(Sql) of
                {ok, {ParseTrees, _}} ->
                    case ptlist_to_string(ParseTrees) of
                        {error, Reason} ->
                            ?Error("parse_stmt error in fold ~p~n", [Reason]),
                            ReasonBin = iolist_to_binary(io_lib:format("Error parsing the sql: ~p", [Reason])),
                            From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}, {<<"flat">>, list_to_binary(Sql)}]}])};
                        {multiple, _Flat, FirstPT} ->
                            SqlTitle = get_sql_title(FirstPT),
                            FlatTuple = {<<"flat">>, Sql},
                            BoxTuple = {<<"boxerror">>, <<"multiple statements not supported by the box">>},
                            PrettyTuple = {<<"prettyerror">>, <<"pretty print doesn't support mulple statements">>},
                            case SqlTitle of
                                <<>> ->
                                    ParseStmt = jsx:encode([{<<"parse_stmt">>, [BoxTuple, PrettyTuple, FlatTuple]}]);
                                _ ->
                                    ParseStmt = jsx:encode([{<<"parse_stmt">>, [{<<"sqlTitle">>, SqlTitle}, BoxTuple, PrettyTuple, FlatTuple]}])
                            end,
                            From ! {reply, ParseStmt};
                        Flat ->
                            [{ParseTree, _} | _] = ParseTrees,
                            SqlTitle = get_sql_title(ParseTree),
                            FlatTuple = {<<"flat">>, Flat},
                            BoxTuple = try dderl_sqlbox:boxed_from_pt(ParseTree) of
                                           {error, BoxReason} ->
                                               ?Error("Error ~p trying to get the box of the parse tree ~p", [BoxReason, ParseTree]),
                                               {<<"boxerror">>, iolist_to_binary(io_lib:format("~p", [BoxReason]))};
                                           {ok, Box} ->
                                               %% ?Debug("The box ~p", [Box]),
                                               try dderl_sqlbox:box_to_json(Box) of
                                                   JsonBox ->
                                                       {<<"sqlbox">>, JsonBox}
                                               catch
                                                   Class:Error ->
                                                       ?Error("Error ~p:~p converting the box ~p to json: ~n~p~n", [Class, Error, Box, erlang:get_stacktrace()]),
                                                       {<<"boxerror">>, iolist_to_binary(io_lib:format("~p:~p", [Class, Error]))}
                                               end
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
                            case SqlTitle of
                                <<>> ->
                                    ParseStmt = jsx:encode([{<<"parse_stmt">>, [BoxTuple, PrettyTuple, FlatTuple]}]);
                                _ ->
                                    ParseStmt = jsx:encode([{<<"parse_stmt">>, [{<<"sqlTitle">>, SqlTitle}, BoxTuple, PrettyTuple, FlatTuple]}])
                            end,
                            %% ?Debug("Json -- ~s", [jsx:prettify(ParseStmt)]),
                            From ! {reply, ParseStmt}
                    end;
                {parse_error, {PError, Tokens}} ->
                    ?Error("parse_stmt error in parsetree ~p~n", [{PError, Tokens}]),
                    ReasonBin = iolist_to_binary(io_lib:format("Error parsing the sql: ~p", [{PError, Tokens}])),
                    From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}, {<<"flat">>, list_to_binary(Sql)}]}])};
                {lex_error, LError} ->
                    ?Error("lexer error in parsetree ~p~n", [LError]),
                    ReasonBin = iolist_to_binary(io_lib:format("Lexer error: ~p", [LError])),
                    From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}, {<<"flat">>, list_to_binary(Sql)}]}])}
            end
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
    ConnIdBin = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
    case string:to_integer(binary_to_list(ConnIdBin)) of
        {ConnId, []} -> Conns = [ConnId];
        _ -> Conns = undefined
    end,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, []),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, []),
    ReplaceView = proplists:get_value(<<"replace">>, BodyJson, false),
    ?Info("save_view for ~p layout ~p", [Name, TableLay]),
    case add_cmds_views(Sess, UserId, Adapter, ReplaceView, [{Name, Query, Conns, #viewstate{table_layout=TableLay, column_layout=ColumLay}}]) of
        [need_replace] ->
            Res = jsx:encode([{<<"save_view">>,[{<<"need_replace">>, Name}]}]);
        [ViewId] ->
            Res = jsx:encode([{<<"save_view">>,[{<<"name">>, Name}, {<<"view_id">>, ViewId}]}])
    end,
    From ! {reply, Res};
process_cmd({[<<"view_op">>], ReqBody}, _Adapter, Sess, _UserId, From, _Priv) ->
    [{<<"view_op">>,BodyJson}] = ReqBody,
    Operation = string:to_lower(binary_to_list(proplists:get_value(<<"operation">>, BodyJson, <<>>))),
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    ?Info("view_op ~s for ~p", [Operation, ViewId]),
    Res = case Operation of
        "rename" ->
            Name = proplists:get_value(<<"newname">>, BodyJson, <<>>),
            case dderl_dal:rename_view(Sess, ViewId, Name) of
                ok -> jsx:encode([{<<"view_op">>,    [{<<"op">>, <<"rename">>}, {<<"result">>,<<"ok">>}, {<<"newname">>, Name}]}]);
                {error, Error} ->
                    jsx:encode([{<<"view_op">>, [{<<"error">>
                                                , iolist_to_binary(["View rename failed : ", io_lib:format("~p", [Error])])}
                                                ]
                                }])
            end;
        "delete" ->
            case dderl_dal:delete_view(Sess, ViewId) of
                ok -> jsx:encode([{<<"view_op">>, [{<<"op">>, <<"delete">>}, {<<"result">>,<<"ok">>}]}]);
                {error, Error} ->
                    jsx:encode([{<<"view_op">>, [{<<"error">>
                                                , iolist_to_binary(["View delete failed : ", io_lib:format("~p", [Error])])}
                                                ]
                                }])
            end
    end,
    From ! {reply, Res};
process_cmd({[<<"update_view">>], ReqBody}, Adapter, Sess, UserId, From, _Priv) ->
    [{<<"update_view">>,BodyJson}] = ReqBody,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, []),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, []),
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    ?Info("update view ~s with id ~p layout ~p", [Name, ViewId, TableLay]),
    ViewState = #viewstate{table_layout=TableLay, column_layout=ColumLay},
    if
        %% System tables can't be overriden.
        Name =:= <<"All Views">> orelse Name =:= <<"Remote Tables">> ->
            %% TODO: This should indicate that a new view has been saved and should replace it in the gui.
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
process_cmd({[<<"histogram">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"histogram">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    [ColumnId|_] = proplists:get_value(<<"column_ids">>, BodyJson, []),
    {Total, Cols, HistoRows, SN} = Statement:get_histogram(ColumnId),
    ColRecs = [#stmtCol{alias = C, type = if C =:= <<"value">> -> text; true -> float end, readonly = true}
              || C <- Cols],
    HistoJson = gui_resp(#gres{ operation    = <<"rpl">>
                              , cnt          = Total
                              , toolTip      = <<"">>
                              , message      = <<"">>
                              , beep         = <<"">>
                              , state        = SN
                              , loop         = <<"">>
                              , rows         = HistoRows
                              , keep         = <<"">>
                              , focus        = 0
                              , sql          = <<"">>
                              , disable      = <<"">>
                              , promote      = <<"">>}
        , ColRecs),
    RespJson = jsx:encode([{<<"histogram">>, [ {type, <<"histo">>}
                                              , {column_ids, [ColumnId]}
                                              , {cols, build_column_json(lists:reverse(ColRecs))}
                                              , {gres, HistoJson}]}]),
    From ! {reply, RespJson};
process_cmd({[<<"statistics">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"statistics">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnIds = proplists:get_value(<<"column_ids">>, BodyJson, []),
    RowIds = proplists:get_value(<<"row_ids">>, BodyJson, 0),
    RespJson = case Statement:get_statistics(ColumnIds, RowIds) of
        {error, Error, St} ->
            ?Error("Stats error ~p~n~p", [Error, St]),
            jsx:encode([{<<"statistics">>, [{error, Error}]}]);
        {Total, Cols, StatsRows, SN} ->
            ColRecs = [#stmtCol{alias = C, type = if C =:= <<"column">> -> text; true -> float end, readonly = true}
                      || C <- Cols],
            ?Debug("statistics rows ~p, cols ~p", [StatsRows, ColRecs]),
            StatsJson = gui_resp(#gres{ operation    = <<"rpl">>
                                      , cnt          = Total
                                      , toolTip      = <<"">>
                                      , message      = <<"">>
                                      , beep         = <<"">>
                                      , state        = SN
                                      , loop         = <<"">>
                                      , rows         = StatsRows
                                      , keep         = <<"">>
                                      , focus        = 0
                                      , sql          = <<"">>
                                      , disable      = <<"">>
                                      , promote      = <<"">>}
                , ColRecs),
            jsx:encode([{<<"statistics">>, [ {type, <<"stats">>}
                                           , {column_ids, ColumnIds}
                                           , {row_ids, RowIds}
                                           , {cols, build_column_json(lists:reverse(ColRecs))}
                                           , {gres, StatsJson}]}])
    end,
    From ! {reply, RespJson};
process_cmd({[<<"statistics_full">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"statistics_full">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnIds = proplists:get_value(<<"column_ids">>, BodyJson, []),
    RespJson = case Statement:get_statistics(ColumnIds) of
        {error, Error, St} ->
            ?Error("Stats error ~p~n~p", [Error, St]),
            jsx:encode([{<<"statistics_full">>, [{error, Error}]}]);
        {Total, Cols, StatsRows, SN} ->
            ColRecs = [#stmtCol{alias = C, type = if C =:= <<"column">> -> text; true -> float end, readonly = true}
                      || C <- Cols],
            StatsJson = gui_resp(#gres{ operation    = <<"rpl">>
                                      , cnt          = Total
                                      , toolTip      = <<"">>
                                      , message      = <<"">>
                                      , beep         = <<"">>
                                      , state        = SN
                                      , loop         = <<"">>
                                      , rows         = StatsRows
                                      , keep         = <<"">>
                                      , focus        = 0
                                      , sql          = <<"">>
                                      , disable      = <<"">>
                                      , promote      = <<"">>}
                , ColRecs),
            jsx:encode([{<<"statistics_full">>, [ {type, <<"stats">>}
                                           , {column_ids, ColumnIds}
                                           , {cols, build_column_json(lists:reverse(ColRecs))}
                                           , {gres, StatsJson}]}])
    end,
    From ! {reply, RespJson};

process_cmd({[<<"edit_term_or_view">>], ReqBody}, _Adapter, Sess, _UserId, From, _Priv) ->
    [{<<"edit_term_or_view">>, BodyJson}] = ReqBody,
    StringToFormat = proplists:get_value(<<"erlang_term">>, BodyJson, <<>>),
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Row = proplists:get_value(<<"row">>, BodyJson, 0),
    R = Statement:row_with_key(Row),
    ?Debug("Row with key ~p",[R]),
    Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
    IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
    case {IsView, element(3, R)} of
        {true, {_, #ddView{}, #ddCmd{}=OldC}} ->
            C = dderl_dal:get_command(Sess, OldC#ddCmd.id),
            From ! {reply, jsx:encode([{<<"edit_term_or_view">>,
                                        [{<<"isView">>, true}
                                         ,{<<"title">>, StringToFormat}
                                         ,{<<"cmd">>, C#ddCmd.command}]
                                       }])};
        _ ->
            ?Debug("The string to format: ~p", [StringToFormat]),
            format_json_or_term(jsx:is_json(StringToFormat), StringToFormat, From, BodyJson)
    end;

process_cmd({Cmd, _BodyJson}, _Adapter, _Sess, _UserId, From, _Priv) ->
    ?Error("Unknown cmd ~p ~p~n", [Cmd, _BodyJson]),
    From ! {reply, jsx:encode([{<<"error">>, <<"unknown command">>}])}.

% TODO: Change this to params as list
%-spec process_query(binary(), tuple(), list()) -> list().
%process_query(Query, Connection, Params) ->
%    imem_adapter:process_query(Query, Connection, Params).
-spec process_query(binary(), tuple(), {binary(), atom()}, pid()) -> list().
process_query(Query, Connection, Params, SessPid) ->
    imem_adapter:process_query(Query, Connection, Params, SessPid).

%%%%%%%%%%%%%%%

-spec format_json_or_term(boolean(), binary(), pid(), term()) -> term().
format_json_or_term(true, StringToFormat, From, _) ->
    Formatted = jsx:prettify(StringToFormat),
    From ! {reply, jsx:encode([{<<"edit_term_or_view">>,
                                [{<<"isJson">>, true},
                                 {<<"formattedJson">>, Formatted}]
                               }])};
format_json_or_term(_, StringToFormat, From, BodyJson) ->
    case proplists:get_value(<<"expansion_level">>, BodyJson, 1) of
        <<"auto">> -> ExpandLevel = auto;
        ExpandLevel -> ok
    end,
    Force = proplists:get_value(<<"force">>, BodyJson, false),
    ?Debug("Forced value: ~p", [Force]),
    case erlformat:format(StringToFormat, ExpandLevel, Force) of
        {error, ErrorInfo} ->
            ?Debug("Error trying to format the erlang term ~p~n~p", [StringToFormat, ErrorInfo]),
            From ! {reply, jsx:encode([{<<"edit_term_or_view">>,
                                        [
                                         {<<"error">>, <<"Invalid erlang term">>},
                                         {<<"originalText">>, StringToFormat}
                                        ]}])};
        Formatted ->
            ?Debug("The formatted text: ~p", [Formatted]),
            From ! {reply, jsx:encode([{<<"edit_term_or_view">>, Formatted}])}
    end.

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
    ?NoDbLog(debug, [], "processing resp ~p cols ~p jcols ~p", [Gres, Columns, JCols]),
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
                is_float(Re)    -> float_to_binary(Re,[{decimals,20},compact]);
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
                R when is_integer(R)    -> R;
                R when is_float(R)      -> float_to_binary(R,[{decimals,20},compact]);
                R when is_atom(R)       -> atom_to_binary(R, utf8);
                R when is_binary(R)     ->
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

-spec build_column_json([#stmtCol{}]) -> list().
build_column_json(Cols) ->
    build_column_json(Cols, [], length(Cols)).

-spec build_column_json([#stmtCol{}], list(), integer()) -> list().
build_column_json([], JCols, _Counter) ->
    [[{<<"id">>, <<"sel">>},
      {<<"name">>, <<"">>},
      {<<"field">>, <<"id">>},
      {<<"behavior">>, <<"select">>},
      {<<"cssClass">>, <<"id-cell-selection">>},
      {<<"width">>, 38},
      {<<"minWidth">>, 2},
      {<<"cannotTriggerInsert">>, true},
      {<<"resizable">>, true},
      {<<"sortable">>, false},
      {<<"selectable">>, false}] | JCols];
build_column_json([C|Cols], JCols, Counter) ->
    Nm = C#stmtCol.alias,
    BinCounter = integer_to_binary(Counter),
    Nm1 = <<Nm/binary, $_, BinCounter/binary>>,
    case C#stmtCol.type of
        integer -> Type = <<"numeric">>;
        float -> Type = <<"numeric">>;
        decimal -> Type = <<"numeric">>;
        number -> Type = <<"numeric">>;
        'SQLT_NUM' -> Type = <<"numeric">>;
        _ -> Type = <<"text">>
    end,
    JC = [{<<"id">>, Nm1},
          {<<"type">>, Type},
          {<<"name">>, Nm},
          {<<"field">>, Nm1},
          {<<"resizable">>, true},
          {<<"sortable">>, false},
          {<<"selectable">>, true}],
    JCol = if C#stmtCol.readonly =:= false -> [{<<"editor">>, <<"true">>} | JC]; true -> JC end,
    build_column_json(Cols, [JCol | JCols], Counter - 1).

-spec build_column_csv([#stmtCol{}]) -> binary().
build_column_csv(Cols) ->
    list_to_binary([string:join([binary_to_list(C#stmtCol.alias) || C <- Cols], ?CSV_FIELD_SEP), "\n"]).

-spec extract_modified_rows([]) -> [{undefined | integer(), atom(), list()}].
extract_modified_rows([]) -> [];
extract_modified_rows([ReceivedRow | Rest]) ->
    case proplists:get_value(<<"rowid">>, ReceivedRow) of
        undefined ->
            RowId = undefined,
            Op = ins;
        RowId ->
            Op = upd
    end,
    Cells = [{proplists:get_value(<<"cellid">>, Cell), proplists:get_value(<<"value">>, Cell)} || Cell <- proplists:get_value(<<"cells">>, ReceivedRow, [])],
    Row = {RowId, Op, Cells},
    [Row | extract_modified_rows(Rest)].

-spec get_sql_title(tuple()) -> binary().
get_sql_title({select, Args}) ->
    From = lists:keyfind(from, 1, Args),
    <<"from ", Result/binary>> = sqlparse:pt_to_string(From),
    Result;
get_sql_title(_) -> <<>>.

%% TODO: Implement ptlist_to_string for multiple statements when it is supported byt the sqlparse.
ptlist_to_string([{ParseTree,_}]) -> sqlparse:pt_to_string(ParseTree);
ptlist_to_string([{FirstPT,_} | _]) -> {mulitple, sqlparse:pt_to_string(FirstPT), FirstPT}.