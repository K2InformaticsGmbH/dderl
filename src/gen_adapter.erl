-module(gen_adapter).

-include("dderl.hrl").
-include("gres.hrl").

-include_lib("imem/include/imem_sql.hrl").

-export([ process_cmd/6
        , init/0
        , add_cmds_views/5
        , gui_resp/2
        , build_resp_fun/3
        , process_query/4
        , build_column_json/1
        , build_column_csv/3
        , extract_modified_rows/1
        , decrypt_to_term/1
        , encrypt_to_binary/1
        , get_deps/0
        , opt_bind_json_obj/2
        , add_conn_info/2
        , make_csv_rows/4
        , is_exec_query/1
        , term_diff/4
        ]).

init() -> ok.

-spec add_conn_info(any(), map()) -> any().
add_conn_info(Priv, ConnInfo) when is_map(ConnInfo) -> Priv.

-spec add_cmds_views({atom(), pid()} | undefined, ddEntityId(), atom(), boolean(), [tuple()]) -> [ddEntityId() | need_replace] | {error, binary()}.
add_cmds_views(Sess, UserId, A, R, Views) -> add_cmds_views(Sess, UserId, A, R, Views, false).

-spec add_cmds_views({atom(), pid()} | undefined, ddEntityId(), atom(), boolean(), [tuple()], boolean()) -> [ddEntityId() | need_replace] | {error, binary()}.
add_cmds_views(_, _, _, _, [], _) -> [];
add_cmds_views(Sess, UserId, A, R, [{N,C,Con}|Rest], ReplaceConns) ->
    add_cmds_views(Sess, UserId, A, R, [{N,C,Con,#viewstate{}}|Rest], ReplaceConns);
add_cmds_views(Sess, UserId, A, Replace, [{N,C,Con,#viewstate{}=V}|Rest], ReplaceConns) ->
    case dderl_dal:get_view(Sess, N, A, UserId) of
        {error, _} = Error -> Error;
        undefined ->
            Id = dderl_dal:add_command(Sess, UserId, A, N, C, Con, []),
            ViewId = dderl_dal:add_view(Sess, UserId, N, Id, V),
            [ViewId | add_cmds_views(Sess, UserId, A, Replace, Rest, ReplaceConns)];
        View ->
            if
                Replace ->
                    case ReplaceConns of
                        false -> dderl_dal:update_command(Sess, View#ddView.cmd, UserId, N, C, []);
                        true -> dderl_dal:update_command(Sess, View#ddView.cmd, UserId, N, C, Con, [])
                    end,
                    ViewId = dderl_dal:add_view(Sess, UserId, N, View#ddView.cmd, V),
                    [ViewId | add_cmds_views(Sess, UserId, A, Replace, Rest, ReplaceConns)];
                true ->
                    [need_replace | add_cmds_views(Sess, UserId, A, Replace, Rest, ReplaceConns)]
            end
    end.

opt_bind_json_obj(Sql, Adapter) ->
    AdapterMod = list_to_existing_atom(atom_to_list(Adapter) ++ "_adapter"),
    Types = AdapterMod:bind_arg_types(),
    case sql_params(Sql, Types) of
        {match, []} -> [];
        {match, Parameters} ->
            [{<<"binds">>,
              [{<<"types">>, Types},
               {<<"pars">>,
                [{P, [{<<"typ">>,T},
                      {<<"dir">>,case D of
                                     <<"_IN_">> -> <<"in">>;
                                     <<"_OUT_">> -> <<"out">>;
                                     <<"_INOUT_">> -> <<"inout">>;
                                     _ -> <<"in">>
                                 end},
                      {<<"val">>,<<>>}]}
                 || [P,T,D] <- lists:usort(Parameters)]}]
             }];
        % No bind parameters can be extracted
        % possibly query string is not parameterized
        _ -> []
    end.

sql_params(Sql, Types) ->
    RegEx = "[^a-zA-Z0-9() =><]*:(" ++ string:join([binary_to_list(T) || T <- Types], "|")
        ++ ")((_IN_|_OUT_|_INOUT_){0,1})[^ ,\)\n\r;]+",
    try
        {match, dderl_sql_params:get_params(Sql, RegEx)}
    catch C:R ->
        ?Warn("~p~n~p", [{C,R}, erlang:get_stacktrace()]),
        re:run(Sql, RegEx, [global,{capture, [0,1,2], binary}])
    end.

-spec process_cmd({[binary()], [{binary(), list()}]}, atom(), {atom(), pid()}, ddEntityId(), pid(), term()) -> term().
process_cmd({[<<"parse_stmt">>], ReqBody}, Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"parse_stmt">>,BodyJson}] = ReqBody,
    Sql = string:strip(binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>))),
    if
        Sql =:= [] ->
            From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, <<"Empty sql string">>}, {<<"flat">>, <<>>}]}])};
        true ->
            case sqlparse:parsetree(Sql) of
                {ok, ParseTrees} ->
                    case ptlist_to_string(ParseTrees, format_options(Adapter)) of
                        {error, Reason} ->
                            ?Error("parse_stmt error in fold ~p~n", [Reason]),
                            ReasonBin = iolist_to_binary(io_lib:format("Error parsing the sql: ~p", [Reason])),
                            From ! {reply, jsx:encode([{<<"parse_stmt">>,
                                                        [{<<"error">>, ReasonBin},
                                                         {<<"flat">>, list_to_binary(Sql)}
                                                         | opt_bind_json_obj(Sql, Adapter)]
                                                       }])};
                        {multiple, FlatList, [FirstPt | _] = PtList} ->
                            SqlTitle = get_sql_title(FirstPt),
                            FlatTuple = {<<"flat">>, iolist_to_binary([[Flat, ";\n"] || Flat <- FlatList])}, %% Add ;\n after each statement.
                            FlatListTuple = {<<"flat_list">>, FlatList},
                            PrettyTuple = get_pretty_tuple_multiple(PtList, Adapter),
                            ParseStmt = jsx:encode([{<<"parse_stmt">>,
                                case SqlTitle of
                                    <<>> ->
                                        [PrettyTuple, FlatTuple, FlatListTuple | opt_bind_json_obj(Sql, Adapter)];
                                _ ->
                                    [{<<"sqlTitle">>, SqlTitle}, PrettyTuple, FlatTuple, FlatListTuple
                                      | opt_bind_json_obj(Sql, Adapter)]
                            end}]),
                            From ! {reply, ParseStmt};
                        Flat ->
                            [{ParseTree, _}] = ParseTrees,
                            SqlTitle = get_sql_title(ParseTree),
                            FlatTuple = {<<"flat">>, Flat},
                            PrettyTuple = get_pretty_tuple(ParseTree, Adapter),
                            ParseStmt = jsx:encode([{<<"parse_stmt">>,
                                case SqlTitle of
                                    <<>> ->
                                        [PrettyTuple, FlatTuple | opt_bind_json_obj(Sql, Adapter)];
                                    _ ->
                                        [{<<"sqlTitle">>, SqlTitle}, PrettyTuple,
                                            FlatTuple | opt_bind_json_obj(Sql, Adapter)]
                                end
                            }]),
                            From ! {reply, ParseStmt}
                    end;
                {parse_error, {PError, Tokens}} ->
                    case is_exec_query(Sql) of
                        {multiple, FlatList} ->
                            FlatTuple = {<<"flat">>, iolist_to_binary([[Flat, ";\n"] || Flat <- FlatList])}, %% Add ;\n after each statement.
                            FlatListTuple = {<<"flat_list">>, FlatList},
                            ParseStmt = jsx:encode([{<<"parse_stmt">>,
                                [FlatTuple, FlatListTuple | opt_bind_json_obj(Sql, Adapter)]
                            }]),
                            From ! {reply, ParseStmt};
                        {true, Flat} ->
                            FlatTuple = {<<"flat">>, Flat},
                            ParseStmt = jsx:encode([{<<"parse_stmt">>,
                                [FlatTuple | opt_bind_json_obj(Sql, Adapter)]
                            }]),
                            From ! {reply, ParseStmt};
                        false ->
                            ?Error("parse_stmt error in parsetree ~p~n", [{PError, Tokens}]),
                            ReasonBin = iolist_to_binary(io_lib:format("Error parsing the sql: ~p", [{PError, Tokens}])),
                            From ! {reply, jsx:encode([{<<"parse_stmt">>,
                                                        [{<<"error">>, ReasonBin}, {<<"flat">>, list_to_binary(Sql)}
                                                        | opt_bind_json_obj(Sql, Adapter)]}])}
                    end;
                {lex_error, LError} ->
                    ?Error("lexer error in parsetree ~p~n", [LError]),
                    ReasonBin = iolist_to_binary(io_lib:format("Lexer error: ~p", [LError])),
                    From ! {reply, jsx:encode([{<<"parse_stmt">>,
                                                [{<<"error">>, ReasonBin},
                                                 {<<"flat">>, list_to_binary(Sql)}
                                                 | opt_bind_json_obj(Sql, Adapter)]}])}
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
    Conns = case proplists:get_value(<<"selected_connections">>, BodyJson, undefined) of
        undefined ->
            ConnIdBin = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
            case string:to_integer(binary_to_list(ConnIdBin)) of
                {ConnId, []} -> [ConnId];
                _ -> undefined
            end;
        SelectedConns -> SelectedConns
    end,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, []),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, []),
    ReplaceView = proplists:get_value(<<"replace">>, BodyJson, false),
    ViewParams = [{Name, Query, Conns, #viewstate{table_layout=TableLay, column_layout=ColumLay}}],
    Res = case add_cmds_views(Sess, UserId, Adapter, ReplaceView, ViewParams, true) of
        [need_replace] -> #{need_replace => Name, selected_connections => Conns};
        [ViewId] -> #{name => Name, view_id => ViewId, selected_connections => Conns}
    end,
    From ! {reply, jsx:encode(#{save_view => Res})};
process_cmd({[<<"get_view_connections">>], [{<<"id">>, ViewId}]}, _Adapter, Sess, _UserId, From, _Priv) ->
    CurrentConns = case dderl_dal:get_view(Sess, ViewId) of
        #ddView{cmd = CmdId} ->
            case dderl_dal:get_command(Sess, CmdId) of
                #ddCmd{conns = local} -> [];
                #ddCmd{conns = Conns} -> Conns;
                _ -> []
            end;
        _ -> []
    end,
    From ! {reply, jsx:encode(#{get_view_connections => #{conns => CurrentConns}})};
process_cmd({[<<"get_view_params">>], [{<<"name">>, ViewName}]}, Adapter, Sess, UserId, From, _Priv) ->
    Params = case get_view_params(Sess, ViewName, Adapter, UserId) of
        #{error := <<"View not found">>} -> get_view_params(Sess, ViewName, Adapter, '_');
        ViewParams -> ViewParams
    end,
    From ! {reply, jsx:encode(#{get_view_params => Params})};
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
                                                , iolist_to_binary(["ddView rename failed : ", io_lib:format("~p", [Error])])}
                                                ]
                                }])
            end;
        "delete" ->
            case dderl_dal:delete_view(Sess, ViewId) of
                ok -> jsx:encode([{<<"view_op">>, [{<<"op">>, <<"delete">>}, {<<"result">>,<<"ok">>}]}]);
                {error, Error} ->
                    jsx:encode([{<<"view_op">>, [{<<"error">>
                                                , iolist_to_binary(["ddView delete failed : ", io_lib:format("~p", [Error])])}
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
        Name =:= <<"All ddViews">> orelse Name =:= <<"Remote Tables">> ->
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
    Res = case dderl_dal:save_dashboard(Sess, UserId, Id, Name, Views) of
        {error, Reason} ->
            jsx:encode([{<<"save_dashboard">>, [{<<"error">>, Reason}]}]);
        NewId ->
            jsx:encode([{<<"save_dashboard">>, NewId}])
    end,
    From ! {reply, Res};
process_cmd({[<<"rename_dashboard">>], ReqBody}, _Adapter, Sess, _UserId, From, _Priv) ->
    Id = proplists:get_value(<<"id">>, ReqBody),
    Name = proplists:get_value(<<"name">>, ReqBody),
    case dderl_dal:rename_dashboard(Sess, Id, Name) of
        {error, Reason} ->
            From ! {reply, jsx:encode([{<<"rename_dashboard">>, [{<<"error">>, Reason}]}])};
        Name ->
            From ! {reply, jsx:encode([{<<"rename_dashboard">>, Name}])}
    end;
process_cmd({[<<"delete_dashboard">>], ReqBody}, _Adapter, Sess, _UserId, From, _Priv) ->
    Id = proplists:get_value(<<"id">>, ReqBody),
    case dderl_dal:delete_dashboard(Sess, Id) of
        {error, Reason} ->
            From ! {reply, jsx:encode([{<<"delete_dashboard">>, [{<<"error">>, Reason}]}])};
        Id ->
            From ! {reply, jsx:encode([{<<"delete_dashboard">>, Id}])}
    end;
process_cmd({[<<"dashboards">>], _ReqBody}, _Adapter, Sess, UserId, From, _Priv) ->
    case dderl_dal:get_dashboards(Sess, UserId) of
        {error, Reason} ->
            Res = jsx:encode([{<<"error">>, Reason}]);
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
process_cmd({[<<"distinct_count">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"distinct_count">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    [ColumnId|_] = proplists:get_all_values(<<"column_ids">>, BodyJson),
    RespJson = case Statement:get_distinct_count(ColumnId) of
        {error, Error, St} ->
            ?Error("Distinct count error ~p", [Error], St),
            jsx:encode([{<<"distinct_count">>, [{error, Error}]}]);
        {Total, ColRecs, DistinctCountRows, SN} ->
            DistinctCountJson = gui_resp(#gres{operation = <<"rpl">>
                                      ,cnt       = Total
                                      ,toolTip   = <<"">>
                                      ,message   = <<"">>
                                      ,beep      = <<"">>
                                      ,state     = SN
                                      ,loop      = <<"">>
                                      ,rows      = DistinctCountRows
                                      ,keep      = <<"">>
                                      ,focus     = 0
                                      ,sql       = <<"">>
                                      ,disable   = <<"">>
                                      ,promote   = <<"">>}
                                ,ColRecs),
            jsx:encode([{<<"distinct_count">>, [{type, <<"distinct_count">>}
                                          ,{column_ids, [ColumnId]}
                                          ,{cols, build_column_json(lists:reverse(ColRecs))}
                                          ,{gres, DistinctCountJson}]}])
    end,
    From ! {reply, RespJson};
process_cmd({[<<"distinct_statistics">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"distinct_statistics">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    [ColumnId|_] = proplists:get_all_values(<<"column_ids">>, BodyJson),
    RespJson = case Statement:get_distinct_statistics(ColumnId) of
                   {error, Error, St} ->
                       ?Error("Distinct statistics error ~p", [Error], St),
                       jsx:encode([{<<"distinct_statistics">>, [{error, Error}]}]);
                   {Total, ColRecs, DistinctStatisticsRows, SN} ->
                       DistinctStatisticsJson = gui_resp(#gres{operation = <<"rpl">>
                           ,cnt       = Total
                           ,toolTip   = <<"">>
                           ,message   = <<"">>
                           ,beep      = <<"">>
                           ,state     = SN
                           ,loop      = <<"">>
                           ,rows      = DistinctStatisticsRows
                           ,keep      = <<"">>
                           ,focus     = 0
                           ,sql       = <<"">>
                           ,disable   = <<"">>
                           ,promote   = <<"">>}
                           ,ColRecs),
                       jsx:encode([{<<"distinct_statistics">>, [{type, <<"stats">>}
                           ,{column_ids, [ColumnId]}
                           ,{cols, build_column_json(lists:reverse(ColRecs))}
                           ,{gres, DistinctStatisticsJson}]}])
               end,
    From ! {reply, RespJson};
process_cmd({[<<"statistics">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"statistics">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnIds = proplists:get_value(<<"column_ids">>, BodyJson, []),
    RowIds = proplists:get_value(<<"row_ids">>, BodyJson, 0),
    RespJson = case Statement:get_statistics(ColumnIds, RowIds) of
        {error, Error, St} ->
            ?Error("Stats error ~p", [Error], St),
            jsx:encode([{<<"statistics">>, [{error, Error}]}]);
        {Total, Cols, StatsRows, SN} ->
            ColRecs = [#stmtCol{alias = C, type = case C of <<"column">> -> binstr; <<"count">> -> binstr; _ -> float end, readonly = true}
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
            ?Error("Stats error ~p", [Error], St),
            jsx:encode([{<<"statistics_full">>, [{error, Error}]}]);
        {Total, Cols, StatsRows, SN} ->
            ColRecs = [#stmtCol{alias = C, type = case C of <<"column">> -> binstr; <<"count">> -> binstr; _  -> float end, readonly = true}
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
    ?Debug("Row with key ~p~n~n",[R]),
    Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
    IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
    case {IsView, element(3, R)} of
        {true, {_, #ddView{}=OldV, #ddCmd{}=OldC}} ->
            case dderl_dal:get_view(Sess, OldV#ddView.id) of
                undefined ->
                    NotFound = jsx:encode(#{edit_term_or_view => #{error => <<"ddView not found">>}}),
                    From ! {reply, NotFound};
                {error, Error} ->
                    ?Error("error ~p reading view ~p", [Error, OldV#ddView.id]),
                    Error = jsx:encode(#{edit_term_or_view => #{error => Error}}),
                    From ! {reply, Error};
                View ->
                    C = dderl_dal:get_command(Sess, OldC#ddCmd.id),
                    From ! {reply, jsx:encode([{<<"edit_term_or_view">>,
                                                [{<<"isView">>, true}
                                                ,{<<"view_id">>, View#ddView.id}
                                                ,{<<"title">>, StringToFormat}
                                                ,{<<"cmd">>, C#ddCmd.command}
                                                ,{<<"table_layout">>, (View#ddView.state)#viewstate.table_layout}
                                                ,{<<"column_layout">>, (View#ddView.state)#viewstate.column_layout}]
                                            }])}
            end;
        _ ->
            ?Debug("The string to format: ~p", [StringToFormat]),
            format_json_or_term(jsx:is_json(StringToFormat, [strict]), StringToFormat, From, BodyJson)
    end;
% generate sql from table data
process_cmd({[<<"get_sql">>], ReqBody}, Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"get_sql">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnIds = proplists:get_value(<<"columnIds">>, BodyJson, []),
    RowIds = proplists:get_value(<<"rowIds">>, BodyJson, []),
    Operation = proplists:get_value(<<"op">>, BodyJson, <<>>),
    Columns = Statement:get_columns(),
    case Statement:get_table_name() of
        {as, Tab, _Alias} -> TableName = Tab;
        {{as, Tab, _Alias}, _} -> TableName = Tab;
        {Tab, _} -> TableName = Tab;
        Tab when is_binary(Tab) -> TableName = Tab;
        _ -> TableName = <<>>
    end,
    Rows = [Statement:row_with_key(Id) || Id <- RowIds],
    Sql = generate_sql(TableName, Operation, Rows, Columns, ColumnIds, Adapter),
    Response = jsx:encode([{<<"get_sql">>, [{<<"sql">>, Sql}, {<<"title">>, <<"Generated Sql">>}]}]),
    From ! {reply, Response};

process_cmd({[<<"cache_data">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"cache_data">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RespJson = case Statement:cache_data() of
        {error, ErrorMsg, St} ->
            ?Error("cache_data error ~p", [ErrorMsg], St),
            jsx:encode([{<<"cache_data">>, [{error, ErrorMsg}]}]);
        ok -> jsx:encode([{<<"cache_data">>, <<"ok">>}])
    end,
    From ! {reply, RespJson};

process_cmd({[<<"list_d3_templates">>], _ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    TemplateList = list_d3_templates(dderl_dal:get_d3_templates()),
    RespJson = jsx:encode([{<<"list_d3_templates">>, TemplateList}]),
    From ! {reply, RespJson};

process_cmd({[<<"get_d3_template">>], ReqBody}, _Adapter, _Sess, _UserId, From, _Priv) ->
    [{<<"get_d3_template">>, BodyJson}] = ReqBody,
    TemplateName = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>)),
    Application = binary_to_existing_atom(proplists:get_value(<<"application">>, BodyJson), utf8),
    Path = dderl_dal:get_d3_templates_path(Application),
    Filename = filename:join(Path, TemplateName ++ ".js"),
    TemplateJs = case file:read_file(Filename) of
        {ok, Content} -> Content;
        {error, Reason} ->
            ?Error("Error: ~p reading content of graph template, filename: ~p", [Reason, Filename]),
            <<>>
    end,
    RespJson = jsx:encode([{<<"get_d3_template">>, TemplateJs}]),
    From ! {reply, RespJson};

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

-spec term_diff(list(), term(), pid(), pid()) -> term().
term_diff(BodyJson, Sess, SessPid, From) ->
    ?Debug("Term diff called with args: ~p", [BodyJson]),
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    {LeftType, LeftValue} = get_cell_value(proplists:get_value(<<"left">>, BodyJson, 0), Statement),
    {RightType, RightValue} = get_cell_value(proplists:get_value(<<"right">>, BodyJson, 0), Statement),
    Result = dderl_diff:term_diff(Sess, SessPid, LeftType, LeftValue, RightType, RightValue),
    From ! { reply, jsx:encode(#{term_diff => Result}) }.

-spec get_view_params({atom(), pid()} | undefined, binary(), atom(), ddEntityId()) -> map().
get_view_params(Sess, ViewName, Adapter, UserId) ->
    case dderl_dal:get_view(Sess, ViewName, Adapter, UserId) of
        #ddView{cmd = CmdId} ->
            case dderl_dal:get_command(Sess, CmdId) of
                #ddCmd{command = Sql} -> #{params => opt_bind_json_obj(Sql, Adapter)};
                _ -> #{error => <<"Command not found">>}
            end;
        _ -> #{error => <<"View not found">>}
    end.

-spec format_options(oci | imem) -> list().
format_options(oci) -> [];
format_options(imem) -> [{case_keyword, lower}].

-spec get_pretty_tuple(term(), oci | imem) -> {binary(), binary()}.
get_pretty_tuple(ParseTree, Adapter) ->
    Opts = format_options(Adapter),
    try sqlparse_fold:top_down(sqlparse_format_pretty, ParseTree, Opts) of
        {error, PrettyReason} ->
            ?Debug("Error ~p trying to get the pretty of the parse tree ~p",
                   [PrettyReason, ParseTree]),
            {<<"prettyerror">>, PrettyReason};
        Pretty ->
            {<<"pretty">>, Pretty}
    catch
        Class1:Error1 ->
            ?Debug("Error ~p:~p trying to get the pretty from the parse tree ~p~n",
                   [Class1, Error1, ParseTree]),
            {<<"prettyerror">>, iolist_to_binary(io_lib:format("~p:~p", [Class1, Error1]))}
    end.

-spec get_pretty_tuple_multiple(list(), oci | imem) -> {binary(), binary()}.
get_pretty_tuple_multiple(ParseTrees, Adapter) ->
    get_pretty_tuple_multiple(ParseTrees, [], Adapter).

-spec get_pretty_tuple_multiple(list(), list(), oci | imem) -> {binary(), binary()}.
get_pretty_tuple_multiple([], Result, _Adapter) ->
    %% Same as the flat we add ; and new line after each statement.
    {<<"pretty">>, iolist_to_binary([[Pretty, ";\n"] || Pretty <- lists:reverse(Result)])};
get_pretty_tuple_multiple([ParseTree | Rest], Result, Adapter) ->
    case get_pretty_tuple(ParseTree, Adapter) of
        {<<"prettyerror">>, _} = Error -> Error;
        {<<"pretty">>, Pretty} -> get_pretty_tuple_multiple(Rest, [Pretty | Result], Adapter)
    end.

-spec format_json_or_term(boolean(), binary(), pid(), term()) -> term().
format_json_or_term(true, StringToFormat, From, _) ->
    From ! {reply, jsx:encode([{<<"edit_term_or_view">>,
                                [{<<"isJson">>, true},
                                 {<<"stringToFormat">>, StringToFormat}]
                               }])};
format_json_or_term(_, <<>>, From, _BodyJson) ->
    Result = #{<<"edit_term_or_view">> => #{
        <<"error">> => <<"Invalid erlang term">>,
        <<"string">> => <<>>
    }},
    From ! {reply, jsx:encode(Result)};
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
                                         {<<"string">>, StringToFormat}
                                        ]}])};
        Formatted ->
            ?Debug("The formatted text: ~p", [Formatted]),
            From ! {reply, jsx:encode([{<<"edit_term_or_view">>,
                                        [{<<"string">>, Formatted}, {<<"isFormatted">>, true}]}])}
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
        [{C, case {C, R} of
                {<<"id">>, R}             -> R; % Id is the only value sent as integer.
                {_, R} when is_integer(R) -> integer_to_binary(R);
                {_, R} when is_float(R)   -> float_to_binary(R,[{decimals,20},compact]);
                {_, R} when is_atom(R)    -> atom_to_binary(R, utf8);
                {_, R} when is_binary(R)  ->
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
        try
            Resp = jsx:encode([{Cmd,GuiRespJson}]),
            From ! {reply, Resp}
        catch
            _:Error ->
                ?Error("Encoding problem ~p ~p~n~p~n~p",
                       [Cmd, Error, GuiResp, GuiRespJson], erlang:get_stacktrace())
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
        float   -> Type = <<"numeric">>;
        decimal -> Type = <<"numeric">>;
        number  -> Type = <<"numeric">>;
        binstr  -> Type = <<"text">>;
        string  -> Type = <<"text">>;
%% Oracle types:
        'SQLT_NUM' -> Type = <<"numeric">>;
        'SQLT_CHR' -> Type = <<"text">>;
        'SQLT_STR' -> Type = <<"text">>;
        'SQLT_VCS' -> Type = <<"text">>;
        'SQLT_LVC' -> Type = <<"text">>;
        'SQLT_CLOB'-> Type = <<"text">>;
        'SQLT_VST' -> Type = <<"text">>;
        _ -> Type = <<"undefined">>
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
    <<"from ", Result/binary>> = sqlparse_fold:top_down(sqlparse_format_flat, From, []),
    Result;
get_sql_title(_) -> <<>>.

ptlist_to_string([{ParseTree,_}], Opts) ->
    sqlparse_fold:top_down(sqlparse_format_flat, ParseTree, Opts);
ptlist_to_string(Input, Opts) when is_list(Input) ->
    PtList = [Pt || {Pt, _Extra} <- Input],
    FlatList = [sqlparse_fold:top_down(sqlparse_format_flat, Pt, Opts) || Pt <- PtList],
    {multiple, FlatList, PtList}.

-spec decrypt_to_term(binary()) -> any().
decrypt_to_term(Bin) when is_binary(Bin) ->
    binary_to_term(?Decrypt(Bin)).

-spec encrypt_to_binary(term()) -> binary().
encrypt_to_binary(Term) ->
    ?Encrypt(term_to_binary(Term)).

-spec generate_sql(binary(), binary(), [tuple()], [#stmtCol{}], [integer()], atom()) -> binary().
generate_sql(TableName, <<"upd">>, Rows, Columns, ColumnIds, Adapter) ->
    iolist_to_binary(generate_upd_sql(TableName, Rows, Columns, ColumnIds, Adapter));
generate_sql(TableName, <<"ins">>, Rows, Columns, ColumnIds, Adapter) ->
    InsCols = generate_ins_cols(Columns, ColumnIds),
    iolist_to_binary(generate_ins_sql(TableName, Rows, InsCols, Columns, ColumnIds, Adapter)).

-spec generate_ins_sql(binary(), [tuple()], iolist(), [#stmtCol{}], [integer()], atom()) -> iolist().
generate_ins_sql(_, [], _, _, _, _) -> [];
generate_ins_sql(TableName, [Row | Rest], InsCols, Columns, ColumnIds, Adapter) ->
    [<<"insert into ">>, TableName, <<" (">>,
     InsCols,
     <<") values (">>,
     generate_ins_values(Row, Columns, ColumnIds, Adapter),
     <<");\n">>,
     generate_ins_sql(TableName, Rest, InsCols, Columns, ColumnIds, Adapter)].

-spec generate_ins_cols([#stmtCol{}], [integer()]) -> iolist().
generate_ins_cols(_, []) -> [];
generate_ins_cols(Columns, [ColId]) ->
    Col = lists:nth(ColId, Columns),
    ColName = Col#stmtCol.alias,
    [ColName];
generate_ins_cols(Columns, [ColId | Rest]) ->
    Col = lists:nth(ColId, Columns),
    ColName = Col#stmtCol.alias,
    [ColName, ",", generate_ins_cols(Columns, Rest)].

-spec generate_ins_values(tuple(), [#stmtCol{}], [integer()], atom()) -> iolist().
generate_ins_values(_, _, [], _) -> [];
generate_ins_values(Row, Columns, [ColId], Adapter) ->
    Col = lists:nth(ColId, Columns),
    Value = element(3 + ColId, Row),
    [add_function_type(Col#stmtCol.type, Value, Adapter)];
generate_ins_values(Row, Columns, [ColId | Rest], Adapter) ->
    Col = lists:nth(ColId, Columns),
    Value = element(3 + ColId, Row),
    [add_function_type(Col#stmtCol.type, Value, Adapter)
    ,", ", generate_ins_values(Row, Columns, Rest, Adapter)].

-spec generate_upd_sql(binary(), [tuple()], [#stmtCol{}], [integer()], atom()) -> iolist().
generate_upd_sql(_, [], _, _, _) -> [];
generate_upd_sql(TableName, [Row | Rest], Columns, ColumnIds, Adapter) ->
    [<<"update ">>, TableName, <<" set ">>,
     generate_set_value(Row, Columns, ColumnIds, Adapter),
     <<" where ">>,
     generate_set_value(Row, Columns, [1], Adapter),
     <<";\n">>,
     generate_upd_sql(TableName, Rest, Columns, ColumnIds, Adapter)].

-spec generate_set_value(tuple(), [#stmtCol{}], [integer()], atom()) -> iolist().
generate_set_value(_, _, [], _) -> [];
generate_set_value(Row, Columns, [ColId], Adapter) ->
    Col = lists:nth(ColId, Columns),
    ColName = Col#stmtCol.alias,
    Value = element(3 + ColId, Row),
    [ColName, <<" = ">>, add_function_type(Col#stmtCol.type, Value, Adapter)];
generate_set_value(Row, Columns, [ColId | Rest], Adapter) ->
    Col = lists:nth(ColId, Columns),
    ColName = Col#stmtCol.alias,
    Value = element(3 + ColId, Row),
    [ColName, <<" = ">>, add_function_type(Col#stmtCol.type, Value, Adapter)
    , ", ", generate_set_value(Row, Columns, Rest, Adapter)].

-spec add_function_type(atom(), binary(), atom()) -> binary().
add_function_type(_, <<>>, _) -> <<"NULL">>;
add_function_type('SQLT_NUM', Value, oci) -> Value;
add_function_type('SQLT_DAT', Value, oci) ->
    ImemDatetime = imem_datatype:io_to_datetime(Value),
    NewValue = imem_datatype:datetime_to_io(ImemDatetime),
    iolist_to_binary([<<"to_date('">>, NewValue, <<"','DD.MM.YYYY HH24:MI:SS')">>]);
add_function_type(integer, Value, imem) -> Value;
add_function_type(float, Value, imem) -> Value;
add_function_type(decimal, Value, imem) -> Value;
add_function_type(binary, Value, imem) -> Value;
add_function_type(boolean, Value, imem) -> Value;
add_function_type(datetime, Value, imem) ->
    ImemDatetime = imem_datatype:io_to_datetime(Value),
    NewValue = imem_datatype:datetime_to_io(ImemDatetime),
    iolist_to_binary([<<"to_date('">>, NewValue, <<"','DD.MM.YYYY HH24:MI:SS')">>]);
add_function_type(timestamp, Value, imem) ->
    ImemDatetime = imem_datatype:io_to_timestamp(Value),
    NewValue = imem_datatype:timestamp_to_io(ImemDatetime),
    iolist_to_binary([<<"to_timestamp('">>, NewValue, <<"','DD.MM.YYYY HH24:MI:SS.FF6')">>]);
add_function_type(_, Value, _) ->
    iolist_to_binary([$', escape_quotes(binary_to_list(Value)), $']).


-spec escape_quotes(list()) -> list().
escape_quotes([]) -> [];
escape_quotes([$' | Rest]) -> [$', $' | escape_quotes(Rest)];
escape_quotes([Char | Rest]) -> [Char | escape_quotes(Rest)].

-spec list_d3_templates([{atom(), string()}]) -> [map()].
list_d3_templates([]) -> [];
list_d3_templates([{Application, Path} | Rest]) ->
    case file:list_dir(Path) of
        {ok, AllFiles} ->
            Templates = [
                #{
                    application => Application,
                    name => list_to_binary(filename:rootname(F))
                } || F <- AllFiles, filename:extension(F) =:= ".js"],
            Templates ++ list_d3_templates(Rest);
        {error, Reason} ->
            ?Error("Error reading the ~p d3 templates on: ~p", [Application, Path]),
            ?Error("list_dir failed with reason ~p", [Reason]),
            list_d3_templates(Rest)
    end.

-spec get_deps() -> [atom()].
get_deps() -> [sqlparse].

-spec build_column_csv(integer(), atom(),[#stmtCol{}]) -> binary().
build_column_csv(UserId, Adapter, Cols) ->
    <<(case catch unicode:encoding_to_bom(?CSV_BOM(UserId, Adapter)) of
           {'EXIT', _} -> <<>>;
           Bom when is_binary(Bom) -> Bom
       end)/binary,
      (unicode:characters_to_binary(
         [csv_row([C#stmtCol.alias || C <- Cols],
                  ?COL_SEP_CHAR(UserId, Adapter),
                  ?CSV_ESCAPE(UserId, Adapter)),
          ?ROW_SEP_CHAR(UserId, Adapter)],
         utf8, ?CSV_ENC(UserId, Adapter)))/binary>>.

-spec make_csv_rows(integer(), list(), fun(), atom()) -> binary().
make_csv_rows(UserId, Rows, RowFun, Adapter)
  when is_list(Rows), is_function(RowFun, 1), is_atom(Adapter) ->
    make_csv_rows(UserId, [RowFun(R) || R <- Rows], expanded, Adapter);
make_csv_rows(UserId, Rows, expanded, Adapter) when is_list(Rows),
                                                    is_atom(Adapter) ->
    unicode:characters_to_binary(
      make_csv_rows_internal(Rows, ?COL_SEP_CHAR(UserId, Adapter),
                    ?ROW_SEP_CHAR(UserId, Adapter), ?CSV_ESCAPE(UserId, Adapter)), utf8,
      ?CSV_ENC(UserId, Adapter)).

make_csv_rows_internal([], _ColSepChar, _RowSepChar, _EscapeMode) -> [];
make_csv_rows_internal([Row|Rows], ColSepChar, RowSepChar, EscapeMode) ->
    [csv_row(Row, ColSepChar, EscapeMode), RowSepChar
     | make_csv_rows_internal(Rows, ColSepChar, RowSepChar, EscapeMode)].

csv_row([], _ColSepChar, _EscapeMode) -> [];
csv_row([Cell | Row], ColSepChar, EscapeMode) ->
    [case should_escape(EscapeMode, ColSepChar, Cell) of
         false -> Cell;
         true -> <<$",
                (re:replace(Cell, "\"", "\"\"",
                            [global, {return, binary}]))/binary,
                $">>
     end,
     if length(Row) > 0 -> ColSepChar; true -> [] end
     | csv_row(Row, ColSepChar, EscapeMode)].

-spec should_escape(atom(), list(), binary()) -> boolean().
should_escape(always, _ColSepChar, _Cell) -> true;
should_escape(never, _ColSepChar, _Cell) -> false;
should_escape(strict, ColSepChar, Cell) ->
    re:run(Cell, "[\"\r\n"++ColSepChar++"]") =/= nomatch;
should_escape(normal, ColSepChar, Cell) ->
    re:run(Cell, "^\"|[\r\n"++ColSepChar++"]") =/= nomatch;
should_escape(_Other, ColSepChar, Cell) ->
    should_escape(normal, ColSepChar, Cell).

-spec is_exec_query(list()) -> {true, binary()} | {multiple, [binary()]} | false.
is_exec_query(Sql) ->
    Sql0 = string:trim(Sql),
    case string:split(Sql0, ";", all) of
        [Sql0] -> process_exec_query(Sql0);
        [Sql0, []] -> process_exec_query(Sql0);
        SqlList -> process_exec_query_list(SqlList, [])
    end.

process_exec_query_list([], Acc) -> {multiple, lists:reverse(Acc)};
process_exec_query_list([[]], Acc) -> {multiple, lists:reverse(Acc)};
process_exec_query_list([Sql | Rest], Acc) ->
    case process_exec_query(Sql) of
        {true, ProcessedSql} -> process_exec_query_list(Rest, [ProcessedSql | Acc]);
        false -> false
    end.

process_exec_query(Sql) ->
    Sql0 = string:lowercase(string:trim(Sql)),
    case Sql0 of
        [$e,$x,$e,$c,32 | RestSql] -> {true, iolist_to_binary(["begin ", RestSql, "; end"])};
        _ -> false
    end.

-spec get_cell_value([{binary(), integer()}], term()) -> {atom(), binary()}.
get_cell_value(Cell, Statement) ->
    % Get a proplist with row and column and get the value from a statement.
    Row = proplists:get_value(<<"row">>, Cell, 0),
    Col = proplists:get_value(<<"col">>, Cell, 0),
    R = Statement:row_with_key(Row),
    #stmtCol{type = Type} = lists:nth(Col, Statement:get_columns()),
    {Type, element(3 + Col, R)}.
