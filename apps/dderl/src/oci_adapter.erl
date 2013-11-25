-module(oci_adapter).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").
-include("gres.hrl").

-include_lib("imem/include/imem_sql.hrl").

-export([ init/0
        , process_cmd/5
        , disconnect/1
        ]).

-record(priv, {connections = [], stmts_info = []}).

-spec init() -> ok.
init() ->
    dderl_dal:add_adapter(oci, <<"Oracle/OCI">>),
    dderl_dal:add_connect(undefined,
                          #ddConn{ id = undefined
                                 , name = <<"local oracle">>
                                 , owner = system
                                 , adapter = oci
                                 , access = [{ip, <<"localhost">>},
                                             {user, <<"scott">>},
                                             {port, 1521},
                                             {type, <<"DB Name">>},
                                             {service, xe}]
                                 }),
    gen_adapter:add_cmds_views(undefined, system, oci, true, [
        { <<"Users.sql">>
        , <<"select USERNAME from ALL_USERS">>
        , remote },
        { <<"Tables.sql">>
        , <<"select concat(OWNER,concat('.', TABLE_NAME)) as QUALIFIED_TABLE_NAME from ALL_TABLES where OWNER=user order by TABLE_NAME">>
        , remote },
        { <<"Views.sql">>
        , <<"select concat(OWNER,concat('.', VIEW_NAME)) as QUALIFIED_TABLE_NAME from ALL_VIEWS where OWNER=user order by VIEW_NAME">>
        , remote },
        { <<"All Views">>
        , <<"select
                c.owner,
                v.name
            from
                ddView as v,
                ddCmd as c
            where
                c.id = v.cmd
                and c.adapters = \"[oci]\"
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
    IpAddr   = proplists:get_value(<<"ip">>, BodyJson, <<>>),
    Port     =
        try list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>)))
        catch _:_ -> undefined
        end,
    Service  = proplists:get_value(<<"service">>, BodyJson, <<>>),
    Type     = proplists:get_value(<<"type">>, BodyJson, <<>>),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = proplists:get_value(<<"password">>, BodyJson, <<>>),
    Tnsstr   = proplists:get_value(<<"tnsstring">>, BodyJson, <<>>),
    ?Info("session:open ~p", [{IpAddr, Port, Service, Type, User, Password, Tnsstr}]),    
    if
        Tnsstr =:= <<>> ->
            case Port of
            undefined ->
                    ErlOciSession = {error, "Invalid port"};
            Port ->
                NewTnsstr = list_to_binary(io_lib:format(
                    "(DESCRIPTION="
                    "  (ADDRESS_LIST="
                    "      (ADDRESS=(PROTOCOL=tcp)"
                    "          (HOST=~s)"
                    "          (PORT=~p)"
                    "      )"
                    "  )"
                    "  (CONNECT_DATA=(SERVICE_NAME=~s)))",
                        [ binary_to_list(IpAddr)
                        , Port
                        , binary_to_list(Service)])),
                ?Info("session:open ~p", [{User, Password, NewTnsstr}]),
                OciPort = oci_port:start_link([{logging, true}]),
                ErlOciSession = OciPort:get_session(NewTnsstr, User, Password)
            end;
        true ->
            ?Info("session:open ~p", [{User, Password, Tnsstr}]),
            OciPort = oci_port:start_link([{logging, true}]),
            ErlOciSession = OciPort:get_session(Tnsstr, User, Password)
    end,
    case ErlOciSession of
        {_, ErlOciSessionPid, _} = Connection when is_pid(ErlOciSessionPid) ->
            ?Info("ErlOciSession ~p", [ErlOciSession]),
            Con = #ddConn { id       = proplists:get_value(<<"id">>, BodyJson)
                          , name     = proplists:get_value(<<"name">>, BodyJson, <<>>)
                          , owner    = UserId
                          , adapter  = oci
                          , access   = [ {ip,       IpAddr}
                                       , {port,     Port}
                                       , {service,  Service}
                                       , {type,     Type}
                                       , {user,     User}
                                       , {tnsstr,   Tnsstr}
                                       ]
                          , schm    = binary_to_atom(Service, utf8)
                          },
                    ?Debug([{user, User}], "may save/replace new connection ~p", [Con]),
                    dderl_dal:add_connect(Sess, Con),
            From ! {reply, jsx:encode([{<<"connect">>,list_to_binary(?EncryptPid(Connection))}])},
            Priv#priv{connections = [ErlOciSession|Connections]};
        {error, {_Code, Msg}} = Error when is_list(Msg) ->
            ?Error("DB connect error ~p", [Error]),
            From ! {reply, jsx:encode([{<<"connect">>,[{<<"error">>, list_to_binary(Msg)}]}])},
            Priv;
        Error ->
            ?Error("DB connect error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            From ! {reply, jsx:encode([{<<"connect">>,[{<<"error">>, Err}]}])},
            Priv
    end;

process_cmd({[<<"connect_change_pswd">>], ReqBody}, Sess, UserId, From, undefined) ->
    process_cmd({[<<"connect_change_pswd">>], ReqBody}, Sess, UserId, From, #priv{connections = []});
process_cmd({[<<"connect_change_pswd">>], ReqBody}, Sess, UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"connect">>,BodyJson}] = ReqBody,
    Ip     = proplists:get_value(<<"ip">>, BodyJson, <<>>),
    Port   = proplists:get_value(<<"port">>, BodyJson, <<>>),
    Schema = proplists:get_value(<<"service">>, BodyJson, <<>>),
    User   = proplists:get_value(<<"user">>, BodyJson, <<>>),
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
        {ok, Connection} when is_tuple(Connection) ->
            ?Debug("session ~p", [Connection]),
            ?Debug("connected to params ~p", [{Type, {Ip, Port, Schema}}]),
            %% Id undefined if we are creating a new connection.
            Con = #ddConn { id      = proplists:get_value(<<"id">>, BodyJson)
                          , name    = proplists:get_value(<<"name">>, BodyJson, <<>>)
                          , owner   = UserId
                          , adapter = oci
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
                    true -> gen_adapter:process_query(Query, Sess);
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
    IsView = try
        Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
        _IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
        ?Debug("browse_data (view ~p) ~p - ~p", [_IsView, Tables, {R, Col}]),
        _IsView
    catch
        _:_ -> false
    end,
    if
        IsView ->
            ?Debug("Row with key ~p",[R]),
            {#ddView{name=Name,owner=Owner},#ddCmd{}=OldC,_} = element(3, R),
            Name = element(5, R),
            V = dderl_dal:get_view(Sess, Name, oci, Owner),
            C = dderl_dal:get_command(Sess, OldC#ddCmd.id),
            ?Debug("Cmd ~p Name ~p", [C#ddCmd.command, Name]),
            case C#ddCmd.conns of
                'local' ->
                    Resp = gen_adapter:process_query(C#ddCmd.command, Sess),
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
                            ?Debug("View ~p", [V]),
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
    case dderl_dal:get_view(Sess, <<"All Views">>, oci, UserId) of
        undefined ->
            ?Debug("Using system view All Views"),
            F = dderl_dal:get_view(Sess, <<"All Views">>, oci, system);
        UserView ->
            ?Debug("Using a personalized view All Views"),
            F = UserView
    end,
    C = dderl_dal:get_command(Sess, F#ddView.cmd),
    Resp = gen_adapter:process_query(C#ddCmd.command, Sess),
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
    F = dderl_dal:get_view(Sess, <<"All Views">>, oci, system),
    C = dderl_dal:get_command(Sess, F#ddView.cmd),
    Resp = gen_adapter:process_query(C#ddCmd.command, Sess),
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
                    Resp = gen_adapter:process_query(C#ddCmd.command, Sess),
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
    case proplists:get_value(<<"btn">>, BodyJson, <<">">>) of
        <<"restart">> ->
            Query = Statement:get_query(),
            case dderl_dal:is_local_query(Query) of
                true ->
                    Statement:gui_req(button, <<"restart">>, gui_resp_cb_fun(<<"button">>, Statement, From));
                _ ->
                    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
                    case dderloci:exec(Connection, Query) of
                        {ok, #stmtResult{} = StmtRslt, TableName, ContainRowId} ->
                            FsmCtx = generate_fsmctx_oci(StmtRslt, Query, Connection, TableName, ContainRowId),
                            Statement:refresh_session_ctx(FsmCtx),
                            Statement:gui_req(button, <<"restart">>, gui_resp_cb_fun(<<"button">>, Statement, From));
                        _ ->
                            From ! {reply, jsx:encode([{<<"button">>, [{<<"error">>, <<"unable to refresh the table">>}]}])}
                    end
            end;
        ButtonInt when is_integer(ButtonInt) ->
            Statement:gui_req(button, ButtonInt, gui_resp_cb_fun(<<"button">>, Statement, From));
        ButtonBin when is_binary(ButtonBin) ->
            case string:to_integer(binary_to_list(ButtonBin)) of
                {error, _} -> Button = ButtonBin;
                {Target, []} -> Button = Target
            end,
            Statement:gui_req(button, Button, gui_resp_cb_fun(<<"button">>, Statement, From))
    end,
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
    Rows = extract_modified_rows(ReceivedRows),
    Statement:gui_req(update, Rows, gui_resp_cb_fun(<<"paste_data">>, Statement, From)),
    Priv;
process_cmd({[<<"histogram">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"histogram">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnId = proplists:get_value(<<"column_id">>, BodyJson, 0),
    HistogramResult = Statement:get_histogram(ColumnId),
    RespJson = jsx:encode([{<<"histogram">>, [{column_id, ColumnId},{rows, HistogramResult}]}]),
    From ! {reply, RespJson},
%    Statement:gui_req(histogram, ColumnId, gui_resp_cb_fun(<<"histogram">>, Statement, From)),
    Priv;

process_cmd({[<<"download_query">>], ReqBody}, _Sess, _UserId, From, Priv) ->
    [{<<"download_query">>, BodyJson}] = ReqBody,
    FileName = proplists:get_value(<<"fileToDownload">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"queryToDownload">>, BodyJson, <<>>),
    Connection = ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>))),
    case dderloci:exec(Connection, Query) of
        {ok, #stmtResult{stmtCols = Clms, stmtRef = StmtRef, rowFun = RowFun}, _, ContainRowId} ->
            ProducerPid = spawn(fun() ->
                produce_csv_rows(From, Clms, ContainRowId, StmtRef, RowFun)
            end),
            ?Debug("StmtRslt ~p", [Clms]),
            Columns = build_column_csv(Clms),
            ?Debug("process_query created statement ~p for ~p", [ProducerPid, Query]),
            From ! {reply_csv, FileName, Columns, first};
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

-spec build_column_csv([#stmtCol{}]) -> binary().
build_column_csv(Cols) ->
    list_to_binary([string:join([binary_to_list(C#stmtCol.alias) || C <- Cols], ?CSV_FIELD_SEP), "\n"]).

produce_csv_rows(From, Clms, ContainRowId, StmtRef, RowFun) when is_function(RowFun) andalso is_pid(From) ->
    case erlang:process_info(From) of
        undefined ->
            ?Error("Request aborted (response pid ~p invalid)", [From]),
            StmtRef:close();
        _ ->
            produce_csv_rows_result(StmtRef:fetch_rows(?DEFAULT_ROW_SIZE), Clms, ContainRowId, From, StmtRef, RowFun)
    end.

produce_csv_rows_result({error, Error}, _Clms, _ContainRowId, From, StmtRef, _RowFun) ->
    From ! {reply_csv, <<>>, list_to_binary(io_lib:format("Error: ~p", [Error])), last},
    StmtRef:close();
produce_csv_rows_result({{rows, Rows}, false}, Clms, ContainRowId, From, StmtRef, RowFun) when is_list(Rows) andalso is_function(RowFun) ->
    RowsFixed = fix_row_format(Rows, Clms, ContainRowId),
    CsvRows = list_to_binary([list_to_binary([string:join([binary_to_list(TR) || TR <- Row], ?CSV_FIELD_SEP), "\n"])
                             || Row <- [RowFun(R) || R <- RowsFixed]]),
    ?Debug("Rows intermediate ~p", [CsvRows]),
    From ! {reply_csv, <<>>, CsvRows, continue},
    produce_csv_rows(From, Clms, ContainRowId, StmtRef, RowFun);
produce_csv_rows_result({{rows, Rows}, true}, Clms, ContainRowId, From, StmtRef, RowFun) when is_list(Rows) andalso is_function(RowFun) ->
    RowsFixed = fix_row_format(Rows, Clms, ContainRowId),
    CsvRows = list_to_binary([list_to_binary([string:join([binary_to_list(TR) || TR <- Row], ?CSV_FIELD_SEP), "\n"])
                             || Row <- [RowFun(R) || R <- RowsFixed]]),
    ?Debug("Rows last ~p", [CsvRows]),
    From ! {reply_csv, <<>>, CsvRows, last},
    StmtRef:close().

-spec disconnect(#priv{}) -> #priv{}.
disconnect(#priv{connections = Connections} = Priv) ->
    ?Debug("closing the connections ~p", [Connections]),
    [Connection:close() || Connection <- Connections],
    Priv#priv{connections = []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec gui_resp_cb_fun(binary(), {atom(), pid()}, pid()) -> fun().
gui_resp_cb_fun(Cmd, Statement, From) ->
    Clms = Statement:get_columns(),
    ?Info("The columns ~p", [Clms]),
    gen_adapter:build_resp_fun(Cmd, Clms, From).

-spec sort_json_to_term(list()) -> [tuple()].
sort_json_to_term([]) -> [];
sort_json_to_term([[{C,T}|_]|Sorts]) ->
    case string:to_integer(binary_to_list(C)) of
        {Index, []} -> Index;
        {error, _R} -> Index = C
    end,
    [{Index, if T -> <<"asc">>; true -> <<"desc">> end}|sort_json_to_term(Sorts)].

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

-spec filter_json_to_term([{binary(), term()} | [{binary(), term()}]]) -> [{atom() | integer(), term()}].
filter_json_to_term([{<<"undefined">>,[]}]) -> {'undefined', []};
filter_json_to_term([{<<"and">>,Filters}]) -> {'and', filter_json_to_term(Filters)};
filter_json_to_term([{<<"or">>,Filters}]) -> {'or', filter_json_to_term(Filters)};
filter_json_to_term([]) -> [];
filter_json_to_term([[{C,Vs}]|Filters]) ->
    [{binary_to_integer(C), Vs} | filter_json_to_term(Filters)].

-spec process_query(tuple(), tuple()) -> list().
process_query(Query, {oci_port, _, _} = Connection) ->
    process_query(check_funs(dderloci:exec(Connection, Query)), Query, Connection).

-spec process_query(term(), binary(), tuple()) -> list().
process_query(ok, Query, Connection) ->
    ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
    [{<<"result">>, <<"ok">>}];
process_query({ok, #stmtResult{sortSpec = SortSpec, stmtCols = Clms} = StmtRslt, TableName, ContainRowId}, Query, {oci_port, _, _} = Connection) ->
    FsmCtx = generate_fsmctx_oci(StmtRslt, Query, Connection, TableName, ContainRowId),
    StmtFsm = dderl_fsm:start_link(FsmCtx),
    ?Debug("StmtRslt ~p ~p", [Clms, SortSpec]),
    Columns = build_column_json(lists:reverse(Clms)),
    JSortSpec = build_srtspec_json(SortSpec),
    ?Debug("JColumns~n ~s~n JSortSpec~n~s", [jsx:prettify(jsx:encode(Columns)), jsx:prettify(jsx:encode(JSortSpec))]),
    ?Debug("process_query created statement ~p for ~p", [StmtFsm, Query]),
    [{<<"columns">>, Columns},
     {<<"sort_spec">>, JSortSpec},
     {<<"statement">>, base64:encode(term_to_binary(StmtFsm))},
     {<<"connection">>, list_to_binary(?EncryptPid(Connection))}];
process_query({error, {Code, Msg}}, _Query, Connection) when is_list(Msg) ->
    ?Error([{session, Connection}], "query error ~p", [{Code, Msg}]),
    Err = list_to_binary(Msg),
    [{<<"error">>, Err}];
process_query(Error, _Query, Connection) ->
    ?Error([{session, Connection}], "query error ~p", [Error]),
    if
        is_binary(Error) ->
            [{<<"error">>, Error}];
        true ->
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            [{<<"error">>, Err}]
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
            ?Error("Error: ",  [Err]),
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

-spec error_invalid_conn() -> term().
error_invalid_conn() ->
    Err = <<"Trying to process a query with an unowned connection">>,
    jsx:encode([{<<"error">>, Err}]).

-spec build_srtspec_json([{integer()| binary(), boolean()}]) -> list().
build_srtspec_json(SortSpecs) ->
    [{if is_integer(SP) -> integer_to_binary(SP); true -> SP end
     , [{<<"id">>, if is_integer(SP) -> SP; true -> -1 end}
       ,{<<"asc">>, if AscDesc =:= <<"asc">> -> true; true -> false end}]
     } || {SP,AscDesc} <- SortSpecs].

-spec build_column_json([#stmtCol{}]) -> list().
build_column_json(Cols) ->
    build_column_json(Cols, [], length(Cols)).

-spec build_column_json([#stmtCol{}], list(), integer()) -> list().
build_column_json([], JCols, _Counter) ->
    [[{<<"id">>, <<"sel">>},
      {<<"name">>, <<"">>},
      {<<"field">>, <<"id">>},
      {<<"behavior">>, <<"select">>},
      {<<"cssClass">>, <<"cell-selection">>},
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

-spec check_fun_vsn(fun()) -> boolean().
check_fun_vsn(Fun) when is_function(Fun)->
    {module, Mod} = erlang:fun_info(Fun, module),
    ?Info("The module: ~p", [Mod]),
    [ModVsn] = proplists:get_value(vsn, Mod:module_info(attributes)),
    ?Info("The Module version: ~p~n", [ModVsn]),
    {new_uniq, <<FunVsn:16/unit:8>>} = erlang:fun_info(Fun, new_uniq),
    ?Info("The function version: ~p~n", [FunVsn]),
    ModVsn =:= FunVsn;
check_fun_vsn(Something) ->
    ?Error("Not a function ~p", [Something]),
    false.

-spec check_funs(term()) -> term().
check_funs({ok, #stmtResult{rowFun = RowFun, sortFun = SortFun} = StmtRslt, TableName, ContainRowId}) ->
    ValidFuns = check_fun_vsn(RowFun) andalso check_fun_vsn(SortFun),
    if
        ValidFuns -> {ok, StmtRslt, TableName, ContainRowId};
        true -> <<"Unsupported target database version">>
    end;
check_funs({ok, #stmtResult{rowFun = RowFun, sortFun = SortFun} = StmtRslt}) ->
    ValidFuns = check_fun_vsn(RowFun) andalso check_fun_vsn(SortFun),
    if
        ValidFuns -> {ok, StmtRslt};
        true -> <<"Unsupported target database version">>
    end;
check_funs(Error) ->
    ?Error("Error on checking the fun versions ~p", [Error]),
    Error.

-spec generate_fsmctx_oci(#stmtResult{}, binary(), tuple(), binary(), boolean()) -> #fsmctx{}.
generate_fsmctx_oci(#stmtResult{
                  stmtCols = Clms
                , rowFun   = RowFun
                , stmtRef  = StmtRef
                , sortFun  = SortFun
                , sortSpec = SortSpec}, Query, {oci_port, _, _} = Connection, TableName, ContainRowId) ->
    #fsmctx{id            = "what is it?"
           ,stmtCols      = Clms
           ,rowFun        = RowFun
           ,sortFun       = SortFun
           ,sortSpec      = SortSpec
           ,orig_qry      = Query
           ,block_length  = ?DEFAULT_ROW_SIZE
           ,fetch_recs_async_fun =
                fun(Opts) ->
                        %% TODO: change this to store the fsm ref in the stmt on dderloci, when dderloci is changed to a gen_server.
                        FsmRef = {dderl_fsm, self()},
                        spawn(
                          fun() ->
                                  PushMode = lists:member({fetch_mode,push}, Opts),
                                  fetch_recs_async(FsmRef, StmtRef, PushMode, Clms, ContainRowId)
                          end),
                        ok
                end
            ,fetch_close_fun = fun() -> ok end
            ,stmt_close_fun  = fun() -> StmtRef:close() end
            ,filter_and_sort_fun = fun(FilterSpec, SrtSpec, Cols) -> dderloci:filter_and_sort(FilterSpec, SrtSpec, Cols, Query, Clms, ContainRowId) end
            ,update_cursor_prepare_fun =
                fun(ChangeList) ->
                        ?Info("The stmtref ~p, the table name: ~p and the change list: ~n~p", [StmtRef, TableName, ChangeList]),
                        dderloci_stmt:prepare(TableName, ChangeList, Connection, Clms)
                end
            ,update_cursor_execute_fun =
                fun(_Lock, PrepStmt) ->
                        Result = dderloci_stmt:execute(PrepStmt),
                        ?Info("The result from the exec ~p", [Result]),
                        Result
                end
           }.

%%%%%%% This should be moved to dderloci %%%%%%%
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
            [RowId | RestRow] = lists:reverse(Row),
            [{list_to_tuple([RowId | fix_null(RestRow, Columns)]), {}} | fix_row_format(Rest, Columns, ContainRowId)];
        true ->
            [{list_to_tuple(fix_null(lists:reverse(Row), Columns)), {}} | fix_row_format(Rest, Columns, ContainRowId)]
    end.

fix_null([], []) -> [];
fix_null([<<Length:8, RestNum/binary>> | RestRow], [#stmtCol{type = 'SQLT_NUM'} | RestCols]) ->
    <<Number:Length/binary, _Discarded/binary>> = RestNum,
    [Number | fix_null(RestRow, RestCols)];
fix_null([<<0, 0, 0, 0, 0, 0, 0, _/binary>> | RestRow], [#stmtCol{type = 'SQLT_DAT'} | RestCols]) -> %% Null format for date.
    [<<>> | fix_null(RestRow, RestCols)];
fix_null([Cell | RestRow], [#stmtCol{} | RestCols]) ->
    [Cell | fix_null(RestRow, RestCols)].

fetch_recs_async(FsmRef, StmtRef, PushMode, Clms, ContainRowId) ->
    case StmtRef:fetch_rows(?DEFAULT_ROW_SIZE) of
        {{rows, Rows}, Completed} ->
            RowsFixed = fix_row_format(Rows, Clms, ContainRowId),
            ?Info("StmtRef ~p, Rows ~p, Completed ~p", [StmtRef, RowsFixed, Completed]),
            FsmRef:rows({RowsFixed, Completed}),
            if
                Completed -> ok;
                PushMode -> fetch_recs_async(FsmRef, StmtRef, true, Clms, ContainRowId);
                true -> ok
            end;
        {error, Error} ->
            FsmRef:rows({error, Error})
    end.
