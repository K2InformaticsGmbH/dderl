-module(imem_adapter).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").
-include("gres.hrl").

-include_lib("imem/include/imem_sql.hrl").

-export([ init/0
        , process_cmd/5
        , disconnect/1
        , process_query/2
        , rows/2
        ]).

-record(priv, {connections = []}).

-spec init() -> ok.
init() ->
    dderl_dal:add_adapter(imem, <<"IMEM DB">>),
    dderl_dal:add_connect(undefined,
                          #ddConn{ id = undefined
                                 , name = <<"local imem">>
                                 , owner = system
                                 , adapter = imem
                                 , access = [{ip, <<"local">>},
                                             {user, <<"admin">>},
                                             {port, <<>>},
                                             {type, local}]
                                 }),
    gen_adapter:add_cmds_views(undefined, system, imem, true, [
        { <<"All Tables">>
        , <<"select name(qname),size as rows, memory, nodef(expiry) as expires, nodef(tte) as expires_after from all_tables, ddSize where name = element(2,qname) and size <> undefined order by qname asc">>
        , remote},
        { <<"All Views">>
        , <<"select
                c.owner,
                v.name
            from
                ddView as v,
                ddCmd as c
            where
                c.id = v.cmd
                and c.adapters = \"[imem]\"
                and (c.owner = user or c.owner = system)
            order by
                v.name,
                c.owner">>
        , local}
    ]).

-spec process_cmd({[binary()], term()}, {atom(), pid()}, ddEntityId(), pid(), undefined | #priv{}) -> #priv{}.
process_cmd({[<<"connect">>], ReqBody}, Sess, UserId, From, undefined) ->
    process_cmd({[<<"connect">>], ReqBody}, Sess, UserId, From, #priv{connections = []});
process_cmd({[<<"connect">>], ReqBody}, Sess, UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"connect">>,BodyJson}] = ReqBody,
    Ip       = proplists:get_value(<<"ip">>, BodyJson, <<>>),
    Port     = proplists:get_value(<<"port">>, BodyJson, <<>>),
    Schema   = proplists:get_value(<<"service">>, BodyJson, <<>>),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = list_to_binary(hexstr_to_list(binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)))),
    Type = get_connection_type(Ip),
    ?Debug("session:open ~p", [{Type, Ip, Port, Schema, User}]),
    ResultConnect = connect_to_erlimem(Type, binary_to_list(Ip), Port, Schema, {User, Password}),
    case ResultConnect of
        {error, {{Exception, {"Password expired. Please change it", _} = M}, _Stacktrace}} ->
            ?Error("Password expired for ~p, result ~p", [User, {Exception, M}]),
            From ! {reply, jsx:encode([{<<"connect">>,<<"expired">>}])},
            Priv;
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("Db connect failed for ~p, result: ~n~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply, jsx:encode([{<<"connect">>, [{<<"error">>, Err}]}])},
            Priv;
        {error, Error} ->
            ?Error("DB connect error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            From ! {reply, jsx:encode([{<<"connect">>,[{<<"error">>, Err}]}])},
            Priv;
        {ok, {_,_ConPid} = Connection} ->
            ?Debug("session ~p", [Connection]),
            ?Debug("connected to params ~p", [{Type, {Ip, Port, Schema}}]),
            %% Id undefined if we are creating a new connection.
            Con = #ddConn { id      = proplists:get_value(<<"id">>, BodyJson)
                          , name    = proplists:get_value(<<"name">>, BodyJson, <<>>)
                          , owner   = UserId
                          , adapter = imem
                          , access  = [ {ip,   Ip}
                                       , {port, Port}
                                       , {type, Type}
                                       , {user, User}
                                       ]
                          , schm    = binary_to_atom(Schema, utf8)
                          },
            ?Debug([{user, User}], "may save/replace new connection ~p", [Con]),
            dderl_dal:add_connect(Sess, Con),
            From ! {reply, jsx:encode([{<<"connect">>, list_to_binary(?EncryptPid(Connection))}])},
            Priv#priv{connections = [Connection|Connections]}
    end;
process_cmd({[<<"connect_change_pswd">>], ReqBody}, Sess, UserId, From, undefined) ->
    process_cmd({[<<"connect_change_pswd">>], ReqBody}, Sess, UserId, From, #priv{connections = []});
process_cmd({[<<"connect_change_pswd">>], ReqBody}, Sess, UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"connect">>,BodyJson}] = ReqBody,
    Ip     = proplists:get_value(<<"ip">>, BodyJson, <<>>),
    Port   = proplists:get_value(<<"port">>, BodyJson, <<>>),
    Schema = proplists:get_value(<<"service">>, BodyJson, <<>>),
    User = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = list_to_binary(hexstr_to_list(binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)))),
    NewPassword = list_to_binary(hexstr_to_list(binary_to_list(proplists:get_value(<<"new_password">>, BodyJson, <<>>)))),
    Type = get_connection_type(Ip),
    ?Debug("connect change password ~p", [{Type, Ip, Port, Schema, User}]),
    ResultConnect = connect_to_erlimem(Type, binary_to_list(Ip), Port, Schema, {User, Password, NewPassword}),
    case ResultConnect of
        {error, {{Exception, M}, _Stacktrace} = Error} ->
            ?Error("Db connect failed for ~p, result ~n~p", [User, Error]),
            Err = list_to_binary(atom_to_list(Exception) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply, jsx:encode([{<<"connect_change_pswd">>, [{<<"error">>, Err}]}])},
            Priv;
        {error, Error} ->
            ?Error("DB connect error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            From ! {reply, jsx:encode([{<<"connect_change_pswd">>,[{<<"error">>, Err}]}])},
            Priv;
        {ok, {_,_ConPid} = Connection} ->
            ?Debug("session ~p", [Connection]),
            ?Debug("connected to params ~p", [{Type, {Ip, Port, Schema}}]),
            %% Id undefined if we are creating a new connection.
            Con = #ddConn { id      = proplists:get_value(<<"id">>, BodyJson)
                          , name    = proplists:get_value(<<"name">>, BodyJson, <<>>)
                          , owner   = UserId
                          , adapter = imem
                          , access  = [ {ip,   Ip}
                                       , {port, Port}
                                       , {type, Type}
                                       , {user, User}
                                       ]
                          , schm    = binary_to_atom(Schema, utf8)
                          },
            ?Debug([{user, User}], "may save/replace new connection ~p", [Con]),
            dderl_dal:add_connect(Sess, Con),
            From ! {reply, jsx:encode([{<<"connect_change_pswd">>, list_to_binary(?EncryptPid(Connection))}])},
            Priv#priv{connections = [Connection|Connections]}
    end;

process_cmd({[<<"disconnect">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"disconnect">>, BodyJson}] = ReqBody,
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    case lists:member(Connection, Connections) of
        true ->
            Connection:close(),
            RestConnections = lists:delete(Connection, Connections),
            From ! {reply, jsx:encode([{<<"disconnect">>, <<"ok">>}])},
            Priv#priv{connections = RestConnections};
        false ->
            From ! {reply, jsx:encode([{<<"error">>, <<"Connection not found">>}])},
            Priv
    end;
process_cmd({[<<"remote_apps">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"remote_apps">>, BodyJson}] = ReqBody,
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    case lists:member(Connection, Connections) of
        true ->
            Apps = Connection:run_cmd(which_applications, []),
            Versions = dderl_session:get_apps_version(Apps, []),
            From ! {reply, jsx:encode([{<<"remote_apps">>, Versions}])},
            Priv;
        false ->
            From ! {reply, jsx:encode([{<<"error">>, <<"Connection not found">>}])},
            Priv
    end;

process_cmd({[<<"query">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"query">>,BodyJson}] = ReqBody,
    Query = proplists:get_value(<<"qstr">>, BodyJson, <<>>),
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    ?Info("query ~p", [Query]),
    case lists:member(Connection, Connections) of
        true ->
            R = case dderl_dal:is_local_query(Query) of
                    true -> process_query(Query, Sess);
                    _ -> process_query(Query, Connection)
                end,
            From ! {reply, jsx:encode([{<<"query">>,R}])};
        false ->
            From ! {reply, error_invalid_conn(Connection, Connections)}
    end,
    Priv;

process_cmd({[<<"browse_data">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"browse_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    Row = proplists:get_value(<<"row">>, BodyJson, 0),
    Col = proplists:get_value(<<"col">>, BodyJson, 0),
    R = Statement:row_with_key(Row),
    ?Debug("Row with key ~p",[R]),
    Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
    IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
    ?Debug("browse_data (view ~p) ~p - ~p", [IsView, Tables, {R, Col}]),
    if
        IsView ->
            {#ddView{name=Name,owner=Owner},#ddCmd{}=OldC,_} = element(3, R),
            Name = element(5, R),
            V = dderl_dal:get_view(Sess, Name, imem, Owner),
            C = dderl_dal:get_command(Sess, OldC#ddCmd.id),
            ?Debug("Cmd ~p Name ~p", [C#ddCmd.command, Name]),
            case C#ddCmd.conns of
                'local' ->
                    Resp = process_query(C#ddCmd.command, Sess),
                    RespJson = jsx:encode([{<<"browse_data">>,
                        [{<<"content">>, C#ddCmd.command}
                         ,{<<"name">>, Name}
                         ,{<<"table_layout">>, (V#ddView.state)#viewstate.table_layout}
                         ,{<<"column_layout">>, (V#ddView.state)#viewstate.column_layout}
                         ,{<<"view_id">>, V#ddView.id}] ++ Resp}]),
                    ?Debug("loading ~p at ~p", [Name, (V#ddView.state)#viewstate.table_layout]);
                _ ->
                    case lists:member(Connection, Connections) of
                        true ->
                            Resp = process_query(C#ddCmd.command, Connection),
                            RespJson = jsx:encode([{<<"browse_data">>,
                                [{<<"content">>, C#ddCmd.command}
                                 ,{<<"name">>, Name}
                                 ,{<<"table_layout">>, (V#ddView.state)#viewstate.table_layout}
                                 ,{<<"column_layout">>, (V#ddView.state)#viewstate.column_layout}
                                 ,{<<"view_id">>, V#ddView.id}] ++ Resp}]),
                            ?Debug("loading ~p at ~p", [Name, (V#ddView.state)#viewstate.table_layout]);
                        false ->
                            RespJson = error_invalid_conn(Connection, Connections)
                    end
            end,
            From ! {reply, RespJson};
        true ->
            case lists:member(Connection, Connections) of
                true ->
                    Name = element(3 + Col, R),
                    Query = <<"select * from ", Name/binary>>,
                    Resp = process_query(Query, Connection),
                    RespJson = jsx:encode([{<<"browse_data">>,
                        [{<<"content">>, Query}
                         ,{<<"name">>, Name}] ++ Resp }]),
                    From ! {reply, RespJson};
                false ->
                    From ! {reply, error_invalid_conn(Connection, Connections)}
            end
    end,
    Priv;

% views
process_cmd({[<<"views">>], _}, Sess, UserId, From, Priv) ->
    %% TODO: This should be replaced by dashboard.
    case dderl_dal:get_view(Sess, <<"All Views">>, imem, UserId) of
        undefined ->
            ?Debug("Using system view All Views"),
            F = dderl_dal:get_view(Sess, <<"All Views">>, imem, system);
        UserView ->
            ?Debug("Using a personalized view All Views"),
            F = UserView
    end,
    C = dderl_dal:get_command(Sess, F#ddView.cmd),
    Resp = process_query(C#ddCmd.command, Sess),
    ?Debug("Views ~p~n~p", [C#ddCmd.command, Resp]),
    RespJson = jsx:encode([{<<"views">>,
        [{<<"content">>, C#ddCmd.command}
        ,{<<"name">>, <<"All Views">>}
        ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
        ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
        ,{<<"view_id">>, F#ddView.id}]
        ++ Resp
    }]),
    From ! {reply, RespJson},
    Priv;

%  system views
process_cmd({[<<"system_views">>], _}, Sess, _UserId, From, Priv) ->
    F = dderl_dal:get_view(Sess, <<"All Views">>, imem, system),
    C = dderl_dal:get_command(Sess, F#ddView.cmd),
    Resp = process_query(C#ddCmd.command, Sess),
    ?Debug("Views ~p~n~p", [C#ddCmd.command, Resp]),
    RespJson = jsx:encode([{<<"system_views">>,
        [{<<"content">>, C#ddCmd.command}
        ,{<<"name">>, <<"All Views">>}
        ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
        ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
        ,{<<"view_id">>, F#ddView.id}]
        ++ Resp
    }]),
    From ! {reply, RespJson},
    Priv;

% open view by id
process_cmd({[<<"open_view">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"open_view">>, BodyJson}] = ReqBody,
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    case dderl_dal:get_view(Sess, ViewId) of
        undefined ->
            From ! {reply, jsx:encode([{<<"error">>, <<"View not found">>}])},
            Priv;
        F ->
            C = dderl_dal:get_command(Sess, F#ddView.cmd),
            case C#ddCmd.conns of
                local ->
                    Resp = process_query(C#ddCmd.command, Sess),
                    RespJson = jsx:encode([{<<"open_view">>,
                                          [{<<"content">>, C#ddCmd.command}
                                           ,{<<"name">>, F#ddView.name}
                                           ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                                           ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                                           ,{<<"view_id">>, F#ddView.id}]
                                            ++ Resp
                                           }]);
                _ ->
                    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
                    case lists:member(Connection, Connections) of
                        true ->
                            Resp = process_query(C#ddCmd.command, Connection),
                            RespJson = jsx:encode([{<<"open_view">>,
                                [{<<"content">>, C#ddCmd.command}
                                 ,{<<"name">>, F#ddView.name}
                                 ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                                 ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                                 ,{<<"view_id">>, F#ddView.id}] ++ Resp}]);
                        false ->
                            RespJson = error_invalid_conn(Connection, Connections)
                    end
            end,
            From ! {reply, RespJson},
            Priv
    end;

% events
process_cmd({[<<"sort">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"sort">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    SrtSpc = proplists:get_value(<<"spec">>, BodyJson, []),
    SortSpec = sort_json_to_term(SrtSpc),
    ?Debug("The sort spec from json: ~p", [SortSpec]),
    Statement:gui_req(sort, SortSpec, gui_resp_cb_fun(<<"sort">>, Statement, From)),
    Priv;
process_cmd({[<<"filter">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"filter">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    FltrSpec = proplists:get_value(<<"spec">>, BodyJson, []),
    FilterSpec = filter_json_to_term(FltrSpec),
    Statement:gui_req(filter, FilterSpec, gui_resp_cb_fun(<<"filter">>, Statement, From)),
    Priv;
process_cmd({[<<"reorder">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"reorder">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnOrder = proplists:get_value(<<"column_order">>, BodyJson, []),
    Statement:gui_req(reorder, ColumnOrder, gui_resp_cb_fun(<<"reorder">>, Statement, From)),
    Priv;
process_cmd({[<<"drop_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"drop_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(drop_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"drop_table">>, Results),
    Priv;
process_cmd({[<<"truncate_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"truncate_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(truncate_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"truncate_table">>, Results),
    Priv;
process_cmd({[<<"snapshot_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"snapshot_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(snapshot_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"snapshot_table">>, Results),
    Priv;
process_cmd({[<<"restore_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"restore_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(restore_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"restore_table">>, Results),
    Priv;

% gui button events
process_cmd({[<<"button">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"button">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ButtonOrig = proplists:get_value(<<"btn">>, BodyJson, <<">">>),
    case ButtonOrig of
        ButtonInt when is_integer(ButtonInt) ->
            Button = ButtonInt;
        ButtonBin when is_binary(ButtonBin) ->
            case string:to_integer(binary_to_list(ButtonBin)) of
                {error, _} -> Button = ButtonBin;
                {Target, []} -> Button = Target
            end
    end,
    Statement:gui_req(button, Button, gui_resp_cb_fun(<<"button">>, Statement, From)),
    Priv;
process_cmd({[<<"update_data">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"update_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    CellId = proplists:get_value(<<"cellid">>, BodyJson, <<>>),
    Value = proplists:get_value(<<"value">>, BodyJson, <<>>),
    Statement:gui_req(update, [{RowId,upd,[{CellId,Value}]}], gui_resp_cb_fun(<<"update_data">>, Statement, From)),
    Priv;
process_cmd({[<<"delete_row">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"delete_row">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowIds = proplists:get_value(<<"rowids">>, BodyJson, []),
    DelSpec = [{RowId,del,[]} || RowId <- RowIds],
    ?Debug("delete ~p ~p", [RowIds, DelSpec]),
    Statement:gui_req(update, DelSpec, gui_resp_cb_fun(<<"delete_row">>, Statement, From)),
    Priv;
process_cmd({[<<"insert_data">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"insert_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ClmIdx = proplists:get_value(<<"col">>, BodyJson, <<>>),
    Value =  proplists:get_value(<<"value">>, BodyJson, <<>>),
    Statement:gui_req(update, [{undefined,ins,[{ClmIdx,Value}]}], gui_resp_cb_fun(<<"insert_data">>, Statement, From)),
    Priv;
process_cmd({[<<"paste_data">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"paste_data">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ReceivedRows = proplists:get_value(<<"rows">>, BodyJson, []),
    Rows = gen_adapter:extract_modified_rows(ReceivedRows),
    Statement:gui_req(update, Rows, gui_resp_cb_fun(<<"paste_data">>, Statement, From)),
    Priv;
process_cmd({[<<"histogram">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"histogram">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnId = proplists:get_value(<<"column_id">>, BodyJson, 0),
    {Total, HistogramResult, SN} = Statement:get_histogram(ColumnId),
    RespJson = jsx:encode([{<<"histogram">>, [{total, Total}, {column_id, ColumnId}, {rows, HistogramResult}, {state, SN}]}]),
    From ! {reply, RespJson},
%    Statement:gui_req(histogram, ColumnId, gui_resp_cb_fun(<<"histogram">>, Statement, From)),
    Priv;

process_cmd({[<<"download_query">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"download_query">>, BodyJson}] = ReqBody,
    FileName = proplists:get_value(<<"fileToDownload">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"queryToDownload">>, BodyJson, <<>>),
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    case check_funs(Connection:exec(Query, ?DEFAULT_ROW_SIZE)) of
        ok ->
            ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
            From ! {reply_csv, FileName, <<>>, single};
        {ok, #stmtResult{stmtCols = Clms, stmtRef = StmtRef, rowFun = RowFun}} ->
            Columns = gen_adapter:build_column_csv(Clms),
            From ! {reply_csv, FileName, Columns, first},
            ProducerPid = spawn(fun() ->
                produce_csv_rows(Connection, From, StmtRef, RowFun)
            end),
            Connection:add_stmt_fsm(StmtRef, {?MODULE, ProducerPid}),
            Connection:run_cmd(fetch_recs_async, [[{fetch_mode,push}], StmtRef]),
            ?Debug("process_query created statement ~p for ~p", [ProducerPid, Query]);
        {error, {{Ex, M}, _Stacktrace} = Error} ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply_csv, FileName, Err, single};
        {error, {Ex,M}} ->
            ?Error([{session, Connection}], "query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply_csv, FileName, Err, single};
        Error ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            Error = if is_binary(Error) -> Error;
                true -> list_to_binary(lists:flatten(io_lib:format("~p", [Error])))
            end,
            From ! {reply_csv, FileName, Error, single}
    end,
    Priv;


% unsupported gui actions
process_cmd({Cmd, BodyJson}, _Sess, _UserId, From, Priv) ->
    ?Error("unsupported command ~p content ~p and priv ~p", [Cmd, BodyJson, Priv]),
    CmdBin = lists:last(Cmd),
    From ! {reply, jsx:encode([{CmdBin,[{<<"error">>, <<"command '", CmdBin/binary, "' is unsupported">>}]}])},
    Priv.

% dderl_fsm like row receive interface for compatibility
rows(Rows, {?MODULE, Pid}) -> Pid ! Rows.
produce_csv_rows(Connection, From, StmtRef, RowFun) when is_function(RowFun) andalso is_pid(From) ->
    receive
        Data ->
            case erlang:process_info(From) of
                undefined -> ?Error("Request aborted (response pid ~p invalid)", [From]);
                _ ->
                    produce_csv_rows_result(Data, Connection, From, StmtRef, RowFun)
            end
    end.

produce_csv_rows_result({error, Error}, Connection, From, StmtRef, _RowFun) ->
    From ! {reply_csv, <<>>, list_to_binary(io_lib:format("Error: ~p", [Error])), last},
    Connection:run_cmd(close, [StmtRef]);
produce_csv_rows_result({Rows,false}, Connection, From, StmtRef, RowFun) when is_list(Rows) ->
    CsvRows = list_to_binary([list_to_binary([string:join([binary_to_list(TR) || TR <- Row], ?CSV_FIELD_SEP), "\n"])
                             || Row <- [RowFun(R) || R <- Rows]]),
    ?Debug("Rows intermediate ~p", [CsvRows]),
    From ! {reply_csv, <<>>, CsvRows, continue},
    produce_csv_rows(Connection, From, StmtRef, RowFun);
produce_csv_rows_result({Rows,true}, Connection, From, StmtRef, RowFun) when is_list(Rows) ->
    CsvRows = list_to_binary([list_to_binary([string:join([binary_to_list(TR) || TR <- Row], ?CSV_FIELD_SEP), "\n"])
                             || Row <- [RowFun(R) || R <- Rows]]),
    ?Debug("Rows last ~p", [CsvRows]),
    From ! {reply_csv, <<>>, CsvRows, last},
    Connection:run_cmd(close, [StmtRef]).
    
-spec disconnect(#priv{}) -> #priv{}.
disconnect(#priv{connections = []} = Priv) -> Priv;
disconnect(#priv{connections = [Connection | Rest]} = Priv) ->
    ?Debug("closing the connection ~p", [Connection]),
    try Connection:close()
    catch Class:Error ->
            ?Error("Error trying to close the connection ~p ~p:~p~n~p~n",
                   [Connection, Class, Error, erlang:get_stacktrace()])
    end,
    disconnect(Priv#priv{connections = Rest}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec gui_resp_cb_fun(binary(), {atom(), pid()}, pid()) -> fun().
gui_resp_cb_fun(Cmd, Statement, From) ->
    Clms = Statement:get_columns(),
    gen_adapter:build_resp_fun(Cmd, Clms, From).

-spec sort_json_to_term(list()) -> [tuple()].
sort_json_to_term([]) -> [];
sort_json_to_term([[{C,T}|_]|Sorts]) ->
    case string:to_integer(binary_to_list(C)) of
        {Index, []} -> Index;
        {error, _R} -> Index = C
    end,
    [{Index, if T -> <<"asc">>; true -> <<"desc">> end}|sort_json_to_term(Sorts)].

-spec filter_json_to_term([{binary(), term()} | [{binary(), term()}]]) -> [{atom() | integer(), term()}].
filter_json_to_term([{<<"undefined">>,[]}]) -> {'undefined', []};
filter_json_to_term([{<<"and">>,Filters}]) -> {'and', filter_json_to_term(Filters)};
filter_json_to_term([{<<"or">>,Filters}]) -> {'or', filter_json_to_term(Filters)};
filter_json_to_term([]) -> [];
filter_json_to_term([[{C,Vs}]|Filters]) ->
    [{binary_to_integer(C), Vs} | filter_json_to_term(Filters)].

-spec process_query(binary(), tuple()) -> list().
process_query(Query, {_,ConPid}=Connection) ->
    case check_funs(Connection:exec(Query, ?DEFAULT_ROW_SIZE)) of
        ok ->
            ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
            [{<<"result">>, <<"ok">>}];
        {ok, #stmtResult{ stmtCols = Clms
                        , rowFun   = RowFun
                        , stmtRef  = StmtRef
                        , sortFun  = SortFun
                        , sortSpec = SortSpec} = StmtRslt} ->
            StmtFsm = dderl_fsm:start_link(
                                #fsmctx{ id                         = "what is it?"
                                       , stmtCols                   = Clms
                                       , rowFun                     = RowFun
                                       , sortFun                    = SortFun
                                       , sortSpec                   = SortSpec
                                       , orig_qry                   = Query
                                       , block_length               = ?DEFAULT_ROW_SIZE
                                       , fetch_recs_async_fun       = fun(Opts) -> Connection:run_cmd(fetch_recs_async, [Opts, StmtRef]) end
                                       , fetch_close_fun            = fun() -> Connection:run_cmd(fetch_close, [StmtRef]) end
                                       , stmt_close_fun             = fun() -> Connection:run_cmd(close, [StmtRef]) end
                                       , filter_and_sort_fun        = fun(FilterSpec, SrtSpec, Cols) ->
                                                                            Connection:run_cmd(filter_and_sort, [StmtRef, FilterSpec, SrtSpec, Cols])
                                                                        end
                                       , update_cursor_prepare_fun  = fun(ChangeList) ->
                                                                            Connection:run_cmd(update_cursor_prepare, [StmtRef, ChangeList])
                                                                        end
                                       , update_cursor_execute_fun  = fun(Lock) ->
                                                                            Connection:run_cmd(update_cursor_execute, [StmtRef, Lock])
                                                                        end
                                       }),
            Connection:add_stmt_fsm(StmtRef, StmtFsm),
            ?Debug("StmtRslt ~p ~p", [Clms, SortSpec]),
            Columns = gen_adapter:build_column_json(lists:reverse(Clms)),
            JSortSpec = build_srtspec_json(SortSpec),
            ?Debug("JColumns~n ~s~n JSortSpec~n~s", [jsx:prettify(jsx:encode(Columns)), jsx:prettify(jsx:encode(JSortSpec))]),
            ?Debug("process_query created statement ~p for ~p", [StmtFsm, Query]),
            [{<<"columns">>, Columns},
             {<<"sort_spec">>, JSortSpec},
             {<<"statement">>, base64:encode(term_to_binary(StmtFsm))},
             {<<"connection">>, list_to_binary(?EncryptPid(Connection))}];
        {error, {{Ex, M}, _Stacktrace} = Error} ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            [{<<"error">>, Err}];
        {error, {Ex,M}} ->
            ?Error([{session, Connection}], "query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            [{<<"error">>, Err}];
        Error ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            if
                is_binary(Error) ->
                    [{<<"error">>, Error}];
                true ->
                    Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
                    [{<<"error">>, Err}]
            end
    end.

-spec send_result_table_cmd(pid(), binary(), list()) -> ok.
send_result_table_cmd(From, BinCmd, Results) ->
    TableErrors = [TableName || {error, TableName} <- Results],
    case TableErrors of
        [] ->
            From ! {reply, jsx:encode([{BinCmd, [{<<"result">>, <<"ok">>}]}])};
        [invalid_connection | _Rest] ->
            From ! {reply, error_invalid_conn()};
        _ ->
            ListNames = [binary_to_list(X) || X <- TableErrors],
            BinTblError = list_to_binary(string:join(ListNames, ",")),
            [CmdSplit|_] = binary:split(BinCmd, <<"_">>),
            Err = iolist_to_binary([<<"Unable to ">>, CmdSplit, <<" the following tables: ">>,  BinTblError]),
            ?Error("Error: ~p",  [Err]),
            From ! {reply, jsx:encode([{BinCmd, [{<<"error">>, Err}]}])}
    end,
    ok.

-spec process_table_cmd(atom(), binary(), term(), [{atom(), pid()}]) -> term().
process_table_cmd(Cmd, TableName, BodyJson, Connections) ->
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    case lists:member(Connection, Connections) of
        true ->
            case Connection:run_cmd(Cmd, [TableName]) of
                ok ->
                    ok;
                {error, {{_Ex, _M}, _Stacktrace} = Error} ->
                    ?Error([{session, Connection}], "query error ~p", [Error]),
                    {error, TableName};
                {error, {Ex, M}} ->
                    ?Error([{session, Connection}], "query error ~p", [{Ex,M}]),
                    {error, TableName};
                Error ->
                    ?Error([{session, Connection}], "query error ~p", [Error]),
                    {error, TableName}
            end;
        false ->
            {error, invalid_connection}
    end.

-spec build_srtspec_json([{integer()| binary(), boolean()}]) -> list().
build_srtspec_json(SortSpecs) ->
    ?Debug("The sort spec ~p", [SortSpecs]),
    [{if is_integer(SP) -> integer_to_binary(SP); true -> SP end
     , [{<<"id">>, if is_integer(SP) -> SP; true -> -1 end}
       ,{<<"asc">>, if AscDesc =:= <<"asc">> -> true; true -> false end}]
     } || {SP,AscDesc} <- SortSpecs].

-spec int(integer()) -> integer().
int(C) when $0 =< C, C =< $9 -> C - $0;
int(C) when $A =< C, C =< $F -> C - $A + 10;
int(C) when $a =< C, C =< $f -> C - $a + 10.

-spec hexstr_to_list(list()) -> list().
hexstr_to_list([]) -> [];
hexstr_to_list([X,Y|T]) -> [int(X)*16 + int(Y) | hexstr_to_list(T)].

-spec connect_to_erlimem(atom(), list(), binary(), binary(), tuple()) -> {ok, {atom(), pid()}} | {error, term()}.
connect_to_erlimem(rpc, _Ip, Port, Schema, Credentials) ->
    try binary_to_existing_atom(Port, utf8) of
        AtomPort -> erlimem:open(rpc, {AtomPort, Schema}, Credentials)
    catch _:_ -> {error, "Invalid port for connection type rpc"}
    end;
connect_to_erlimem(tcp, Ip, Port, Schema, Credentials) ->
    try binary_to_integer(Port) of
        IntPort -> erlimem:open(tcp, {Ip, IntPort, Schema}, Credentials)
    catch _:_ -> {error, "Invalid port for connection type tcp"}
    end;
connect_to_erlimem(Type, _Ip, _Port, Schema, Credentials) ->
    erlimem:open(Type, {Schema}, Credentials).

-spec get_connection_type(binary()) -> atom().
get_connection_type(<<"local_sec">>) -> local_sec;
get_connection_type(<<"local">>) -> local;
get_connection_type(<<"rpc">>) -> rpc;
get_connection_type(_Ip) -> tcp.

-spec error_invalid_conn({atom(), pid()}, [{atom(), pid()}]) -> term().
error_invalid_conn(Connection, Connections) ->
    Err = <<"Trying to process a query with an unowned connection">>,
    ?Error("~s: ~p~n connections list: ~p", [Err, Connection, Connections]),
    jsx:encode([{<<"error">>, Err}]).

-spec error_invalid_conn() -> term().
error_invalid_conn() ->
    Err = <<"Trying to process a query with an unowned connection">>,
    jsx:encode([{<<"error">>, Err}]).

-spec check_fun_vsn(fun()) -> boolean().
check_fun_vsn(Fun) when is_function(Fun)->
    {module, Mod} = erlang:fun_info(Fun, module),
    [ModVsn] = proplists:get_value(vsn, Mod:module_info(attributes)),
    ?Debug("The Module version: ~p~n", [ModVsn]),
    {new_uniq, <<FunVsn:16/unit:8>>} = erlang:fun_info(Fun, new_uniq),
    ?Debug("The function version: ~p~n", [FunVsn]),
    ModVsn =:= FunVsn;
check_fun_vsn(_) ->
    false.

-spec check_funs(term()) -> term().
check_funs({ok, #stmtResult{rowFun = RowFun, sortFun = SortFun} = StmtRslt}) ->
    ValidFuns = check_fun_vsn(RowFun) andalso check_fun_vsn(SortFun),
    if
        ValidFuns -> {ok, StmtRslt};
        true -> <<"Unsupported target database version">>
    end;
check_funs(Error) ->
    Error.
