-module(oci_adapter).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").
-include("gres.hrl").

-include_lib("imem/include/imem_sql.hrl").

-export([ init/0
        , process_cmd/6
        , disconnect/1
        , rows/2
        , get_deps/0
        , rows_limit/3
        , bind_arg_types/0
        , logfun/1
        , add_conn_info/2
        , connect_map/1
        ]).

-record(priv, {connections = [], stmts_info = []}).

-define(E2B(__T), gen_adapter:encrypt_to_binary(__T)).
-define(D2T(__B), gen_adapter:decrypt_to_term(__B)).

bind_arg_types() -> erloci:bind_arg_types().

-spec init() -> ok.
init() ->
    dderl_dal:add_adapter(oci, <<"Oracle/OCI">>),
    gen_adapter:add_cmds_views(undefined, system, oci, false, [
        { <<"Remote Users">>
        , <<"select USERNAME from ALL_USERS">>
        , [] },
        { <<"Remote Tables">>
        , <<"select concat(OWNER,concat('.', TABLE_NAME)) as QUALIFIED_TABLE_NAME from ALL_TABLES where OWNER=user order by TABLE_NAME">>
        , [] },
        { <<"Remote Views">>
        , <<"select concat(OWNER,concat('.', VIEW_NAME)) as QUALIFIED_TABLE_NAME from ALL_VIEWS where OWNER=user order by VIEW_NAME">>
        , [] }
    ]).

-spec add_conn_info(any(), any()) -> any().
add_conn_info(Priv, _ConnInfo) -> Priv.

-spec connect_map(#ddConn{}) -> map().
connect_map(#ddConn{adapter = oci} = C) ->
    add_conn_extra(C, #{id => C#ddConn.id,
                        name => C#ddConn.name,
                        adapter => <<"oci">>,
                        owner => dderl_dal:user_name(C#ddConn.owner)}).

add_conn_extra(#ddConn{access = Access}, Conn)
  when is_map(Access), is_map(Conn) ->
       	maps:merge(Conn, maps:remove(owner,maps:remove(<<"owner">>,Access)));
add_conn_extra(#ddConn{access = Access}, Conn0) when is_list(Access), is_map(Conn0) ->
    Conn = Conn0#{user => proplists:get_value(user, Access, <<>>),
                  language => proplists:get_value(languange, Access, proplists:get_value(language, Access, <<>>)),
                  territory => proplists:get_value(territory, Access, <<>>),
                  charset => proplists:get_value(charset, Access, <<>>),
                  tns => proplists:get_value(tnsstr, Access, <<>>),
                  service => proplists:get_value(service, Access, <<>>),
                  sid => proplists:get_value(sid, Access, <<>>),
                  host => proplists:get_value(ip, Access, <<>>),
                  port => proplists:get_value(port, Access, <<>>)},
    case proplists:get_value(type, Access, service) of
        Type when Type == tns; Type == <<"tns">> ->
            Conn#{method => <<"tns">>};
        Type when Type == service; Type == <<"service">>; Type == <<"DB Name">> ->
            Conn#{method => <<"service">>};
        Type when Type == sid; Type == <<"sid">> ->
            Conn#{method => <<"sid">>}
    end.

-define(LogOci(__L,__File,__Func,__Line,__Msg),
    begin
        lager:__L(__File, "[" ++ ?LOG_TAG ++ "] {~s:~s:~p} ~ts", [__File,__Func,__Line,__Msg])
    end).

-spec process_cmd({[binary()], term()}, {atom(), pid()}, ddEntityId(), pid(),
                  undefined | #priv{}, pid()) -> #priv{}.
process_cmd({[<<"connect">>], ReqBody, _SessionId}, Sess, UserId, From,
            undefined, SessPid) ->
    process_cmd({[<<"connect">>], ReqBody, _SessionId}, Sess, UserId, From,
                #priv{connections = []}, SessPid);
process_cmd({[<<"connect">>], BodyJson5, _SessionId}, Sess, UserId, From,
            #priv{connections = Connections} = Priv, _SessPid) ->
    {value, {<<"password">>, Password}, BodyJson4} = lists:keytake(<<"password">>, 1, BodyJson5),
    {value, {<<"owner">>, _Owner}, BodyJson3} = lists:keytake(<<"owner">>, 1, BodyJson4),
    {value, {<<"id">>, Id}, BodyJson2} = lists:keytake(<<"id">>, 1, BodyJson3),
    {value, {<<"name">>, Name}, BodyJson1} = lists:keytake(<<"name">>, 1, BodyJson2),
    {value, {<<"adapter">>, <<"oci">>}, BodyJson} = lists:keytake(<<"adapter">>, 1, BodyJson1),

    Method    = proplists:get_value(<<"method">>, BodyJson, <<"service">>),
    User      = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Defaults  = ?NLSLANG,
    Language  = get_value_empty_default(<<"languange">>, BodyJson, Defaults),
    Territory = get_value_empty_default(<<"territory">>, BodyJson, Defaults),
    Charset   = get_value_empty_default(<<"charset">>, BodyJson, Defaults),
    NLS_LANG  = binary_to_list(<<Language/binary, $_, Territory/binary, $., Charset/binary>>),

    ErlOciSession
    = case Method of
          <<"tns">> ->
              Tns = proplists:get_value(<<"tns">>, BodyJson, <<>>),
              ?Info("user ~p, TNS ~p", [User, Tns]),
              OciPort = erloci:new([{logging, true}, {env, [{"NLS_LANG", NLS_LANG}]}], fun oci_adapter:logfun/1),
              OciPort:get_session(Tns, User, Password);
          ServiceOrSid when ServiceOrSid == <<"service">>; ServiceOrSid == <<"sid">> ->
              IpAddr   = proplists:get_value(<<"host">>, BodyJson, <<>>),
              Port     = binary_to_integer(proplists:get_value(<<"port">>, BodyJson, <<>>)),
              NewTnsstr
              = list_to_binary(
                  io_lib:format(
                    "(DESCRIPTION="
                    "  (ADDRESS_LIST="
                    "      (ADDRESS=(PROTOCOL=tcp)"
                    "          (HOST=~s)"
                    "          (PORT=~p)"
                    "      )"
                    "  )"
                    "  (CONNECT_DATA=("++
                    case ServiceOrSid of
                        <<"service">> -> "SERVICE_NAME";
                        <<"sid">> -> "SID"
                    end
                    ++"=~s)))",
                    [IpAddr, Port,
                     case ServiceOrSid of
                         <<"service">> -> proplists:get_value(<<"service">>, BodyJson, <<>>);
                         <<"sid">> -> proplists:get_value(<<"sid">>, BodyJson, <<>>)
                     end])),
              ?Info("user ~p, TNS ~p", [User, NewTnsstr]),
              OciPort = erloci:new([{logging, true}, {env, [{"NLS_LANG", NLS_LANG}]}], fun oci_adapter:logfun/1),
              OciPort:get_session(NewTnsstr, User, Password)
      end,
    case ErlOciSession of
        {_, ErlOciSessionPid, _} = Connection when is_pid(ErlOciSessionPid) ->
            ?Debug("ErlOciSession ~p", [ErlOciSession]),
            Con = #ddConn {id = Id, name = Name, owner = UserId, adapter = oci,
                           access  = jsx:decode(jsx:encode(BodyJson),
                                                [return_maps])},
                    ?Debug([{user, User}], "may save/replace new connection ~p", [Con]),
            case dderl_dal:add_connect(Sess, Con) of
                {error, Msg} ->
                    Connection:close(port_close),
                    From ! {reply, jsx:encode([{<<"connect">>,[{<<"error">>, Msg}]}])};
                #ddConn{owner = Owner} = NewConn ->
                    From ! {reply
                            , jsx:encode(
                                [{<<"connect">>
                                  , [{<<"conn_id">>, NewConn#ddConn.id}
                                     , {<<"owner">>, Owner}
                                     , {<<"conn">>
                                        , ?E2B(Connection)}
                                    ]}])}
            end,
            Priv#priv{connections = [ErlOciSession|Connections]};
        {error, {_Code, Msg}} = Error when is_list(Msg) ->
            ?Error("DB connect error ~p", [Error]),
            From ! {reply, jsx:encode(#{connect=>#{error=>list_to_binary(Msg)}})},
            Priv;
        Error ->
            ?Error("DB connect error ~p", [Error]),
            From ! {reply, jsx:encode(#{connect=>#{error=>list_to_binary(io_lib:format("~p",[Error]))}})},
            Priv
    end;

process_cmd({[<<"change_conn_pswd">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"change_pswd">>, BodyJson}] = ReqBody,
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)),
    NewPassword = binary_to_list(proplists:get_value(<<"new_password">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            case dderloci:change_password(Connection, User, Password, NewPassword) of
                {error, Error} ->
                    ?Error("change password exception ~n~p~n", [Error]),
                    Err = iolist_to_binary(io_lib:format("~p", [Error])),
                    From ! {reply, jsx:encode([{<<"change_conn_pswd">>,[{<<"error">>, Err}]}])},
                    Priv;
                ok ->
                    From ! {reply, jsx:encode([{<<"change_conn_pswd">>,<<"ok">>}])},
                    Priv
            end;
        false ->
            From ! {reply, jsx:encode([{<<"error">>, <<"Connection not found">>}])},
            Priv
    end;

process_cmd({[<<"disconnect">>], ReqBody, _SessionId}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"disconnect">>, BodyJson}] = ReqBody,
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            Connection:close(port_close),
            RestConnections = lists:delete(Connection, Connections),
            From ! {reply, jsx:encode([{<<"disconnect">>, <<"ok">>}])},
            Priv#priv{connections = RestConnections};
        false ->
            From ! {reply, jsx:encode([{<<"error">>, <<"Connection not found">>}])},
            Priv
    end;
process_cmd({[<<"remote_apps">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"remote_apps">>, BodyJson}] = ReqBody,
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
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

process_cmd({[<<"query">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv, SessPid) ->
    [{<<"query">>,BodyJson}] = ReqBody,
    case make_binds(proplists:get_value(<<"binds">>, BodyJson, null)) of
        {error, Error} -> From ! {reply, jsx:encode([{<<"error">>, Error}])};
        BindVals ->
            Query = proplists:get_value(<<"qstr">>, BodyJson, <<>>),
            Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
            ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>), %% TODO: This should be change to params...
            case lists:member(Connection, Connections) of
                true ->
                    R = case dderl_dal:is_local_query(Query) of
                            true -> gen_adapter:process_query(Query, Sess, {ConnId, oci}, SessPid);
                            _ -> process_query({Query, BindVals}, Connection, SessPid)
                        end,
                    From ! {reply, jsx:encode([{<<"query">>,[{<<"qstr">>, Query} | R]}])};
                false ->
                    From ! {reply, error_invalid_conn(Connection, Connections)}
            end
    end,
    Priv;

process_cmd({[<<"browse_data">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv, SessPid) ->
    [{<<"browse_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>), %% This should be change to params...
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
            {_,#ddView{name=Name,owner=Owner},#ddCmd{}=OldC} = element(3, R),
            V = dderl_dal:get_view(Sess, Name, oci, Owner),
            C = dderl_dal:get_command(Sess, OldC#ddCmd.id),
            ?Debug("Cmd ~p Name ~p", [C#ddCmd.command, Name]),
            case C#ddCmd.conns of
                'local' ->
                    Resp = gen_adapter:process_query(C#ddCmd.command, Sess, {ConnId, oci}, SessPid),
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
                            ?Debug("View ~p", [V]),
                            case {gen_adapter:opt_bind_json_obj(C#ddCmd.command, oci),
                                  make_binds(proplists:get_value(<<"binds">>, BodyJson, null))} of
                                {[], _} ->
                                    Resp = process_query(C#ddCmd.command, Connection, SessPid),
                                    RespJson = jsx:encode(
                                                 [{<<"browse_data">>,[{<<"content">>, C#ddCmd.command},
                                                                      {<<"name">>, Name},
                                                                      {<<"table_layout">>, (V#ddView.state)#viewstate.table_layout},
                                                                      {<<"column_layout">>, (V#ddView.state)#viewstate.column_layout},
                                                                      {<<"view_id">>, V#ddView.id}] ++ Resp}]
                                                ),
                                    ?Debug("loading ~p at ~p", [Name, (V#ddView.state)#viewstate.table_layout]);
                                {JsonBindInfo, Binds} when Binds == undefined; element(1, Binds) == error ->
                                    RespJson = jsx:encode(
                                                 [{<<"browse_data">>,
                                                   [{<<"content">>, C#ddCmd.command},
                                                    {<<"name">>, Name},
                                                    {<<"view_id">>, V#ddView.id}
                                                    | JsonBindInfo]}]
                                                );
                                {_, Binds} ->
                                    Resp = process_query({C#ddCmd.command, Binds}, Connection, SessPid),
                                    RespJson = jsx:encode(
                                                 [{<<"browse_data">>,[{<<"content">>, C#ddCmd.command},
                                                                      {<<"name">>, Name},
                                                                      {<<"table_layout">>, (V#ddView.state)#viewstate.table_layout},
                                                                      {<<"column_layout">>, (V#ddView.state)#viewstate.column_layout},
                                                                      {<<"view_id">>, V#ddView.id}] ++ Resp}]
                                                ),
                                    ?Debug("loading ~p at ~p", [Name, (V#ddView.state)#viewstate.table_layout])
                            end;
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
                    Resp = process_query(Query, Connection, SessPid),
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
process_cmd({[<<"views">>], ReqBody}, Sess, UserId, From, Priv, SessPid) ->
    [{<<"views">>,BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>), %% This should be change to params...
    %% TODO: This should be replaced by dashboard.
    case dderl_dal:get_view(Sess, <<"All Views">>, oci, UserId) of
        {error, _} = Error->
            F = Error;
        undefined ->
            ?Debug("Using system view All Views"),
            F = dderl_dal:get_view(Sess, <<"All Views">>, oci, system);
        UserView ->
            ?Debug("Using a personalized view All Views"),
            F = UserView
    end,
    case F of
        {error, Reason} ->
            RespJson = jsx:encode([{<<"error">>, Reason}]);
        _ ->
            C = dderl_dal:get_command(Sess, F#ddView.cmd),
            Resp = gen_adapter:process_query(C#ddCmd.command, Sess, {ConnId, oci}, SessPid),
            ?Debug("Views ~p~n~p", [C#ddCmd.command, Resp]),
            RespJson = jsx:encode([{<<"views">>,
                [{<<"content">>, C#ddCmd.command}
                ,{<<"name">>, <<"All Views">>}
                ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                ,{<<"view_id">>, F#ddView.id}]
                ++ Resp
            }])
    end,
    From ! {reply, RespJson},
    Priv;

%  system views
process_cmd({[<<"system_views">>], ReqBody}, Sess, _UserId, From, Priv, SessPid) ->
    [{<<"system_views">>,BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>), %% This should be change to params...
    case dderl_dal:get_view(Sess, <<"All Views">>, oci, system) of
        {error, Reason} ->
            RespJson = jsx:encode([{<<"error">>, Reason}]);
        F ->
            C = dderl_dal:get_command(Sess, F#ddView.cmd),
            Resp = gen_adapter:process_query(C#ddCmd.command, Sess, {ConnId, oci}, SessPid),
            ?Debug("Views ~p~n~p", [C#ddCmd.command, Resp]),
            RespJson = jsx:encode([{<<"system_views">>,
                [{<<"content">>, C#ddCmd.command}
                ,{<<"name">>, <<"All Views">>}
                ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                ,{<<"view_id">>, F#ddView.id}]
                ++ Resp
            }])
    end,
    From ! {reply, RespJson},
    Priv;

% open view by id
process_cmd({[<<"open_view">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv, SessPid) ->
    [{<<"open_view">>, BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>), %% This should be change to params...
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    case dderl_dal:get_view(Sess, ViewId) of
        {error, Reason} ->
            From ! {reply, jsx:encode([{<<"open_view">>, [{<<"error">>, Reason}]}])},
            Priv;
        undefined ->
            From ! {reply, jsx:encode([{<<"open_view">>, [{<<"error">>, <<"View not found">>}]}])},
            Priv;
        F ->
            C = dderl_dal:get_command(Sess, F#ddView.cmd),
            case C#ddCmd.conns of
                local ->
                    Resp = gen_adapter:process_query(C#ddCmd.command, Sess, {ConnId, oci}, SessPid),
                    RespJson = jsx:encode([{<<"open_view">>,
                                          [{<<"content">>, C#ddCmd.command}
                                           ,{<<"name">>, F#ddView.name}
                                           ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                                           ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                                           ,{<<"view_id">>, F#ddView.id}]
                                            ++ Resp
                                           }]);
                _ ->
                    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
                    case lists:member(Connection, Connections) of
                        true ->
                            Resp = process_query(C#ddCmd.command, Connection, SessPid),
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
process_cmd({[<<"sort">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"sort">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    SrtSpc = proplists:get_value(<<"spec">>, BodyJson, []),
    SortSpec = sort_json_to_term(SrtSpc),
    ?Debug("The sort spec from json: ~p", [SortSpec]),
    Statement:gui_req(sort, SortSpec, gui_resp_cb_fun(<<"sort">>, Statement, From)),
    Priv;
process_cmd({[<<"filter">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"filter">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    FltrSpec = proplists:get_value(<<"spec">>, BodyJson, []),
    FilterSpec = filter_json_to_term(FltrSpec),
    Statement:gui_req(filter, FilterSpec, gui_resp_cb_fun(<<"filter">>, Statement, From)),
    Priv;
process_cmd({[<<"reorder">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"reorder">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ColumnOrder = proplists:get_value(<<"column_order">>, BodyJson, []),
    Statement:gui_req(reorder, ColumnOrder, gui_resp_cb_fun(<<"reorder">>, Statement, From)),
    Priv;
process_cmd({[<<"drop_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"drop_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(drop_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"drop_table">>, Results),
    Priv;
process_cmd({[<<"truncate_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"truncate_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(truncate_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"truncate_table">>, Results),
    Priv;
process_cmd({[<<"snapshot_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"snapshot_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(snapshot_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"snapshot_table">>, Results),
    Priv;
process_cmd({[<<"restore_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"restore_table">>, BodyJson}] = ReqBody,
    TableNames = proplists:get_value(<<"table_names">>, BodyJson, []),
    Results = [process_table_cmd(restore_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"restore_table">>, Results),
    Priv;

% gui button events
process_cmd({[<<"button">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"button">>,BodyJson}] = ReqBody,
    FsmStmt = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    case proplists:get_value(<<"btn">>, BodyJson, <<">">>) of
        <<"restart">> ->
            Query = FsmStmt:get_query(),
            case dderl_dal:is_local_query(Query) of
                true ->
                    FsmStmt:gui_req(button, <<"restart">>, gui_resp_cb_fun(<<"button">>, FsmStmt, From));
                _ ->
                    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
%% TODO: Fix restart if there is a need to link again.
                    BindVals = case make_binds(proplists:get_value(<<"binds">>, BodyJson, null)) of
                                   {error, _Error} -> undefined;
                                   BindVals0 -> BindVals0
                               end,
                    case dderloci:exec(Connection, Query, BindVals, ?GET_ROWNUM_LIMIT) of
                        {ok, #stmtResult{} = StmtRslt, TableName} ->
                            dderloci:add_fsm(StmtRslt#stmtResult.stmtRef, FsmStmt),
                            FsmCtx = generate_fsmctx_oci(StmtRslt, Query, BindVals, Connection, TableName),
                            FsmStmt:gui_req(button, <<"restart">>, gui_resp_cb_fun(<<"button">>, FsmStmt, From)),
                            FsmStmt:get_count(),
                            FsmStmt:refresh_session_ctx(FsmCtx);
                        _ ->
                            From ! {reply, jsx:encode([{<<"button">>, [{<<"error">>, <<"unable to refresh the table">>}]}])}
                    end
            end;
        ButtonInt when is_integer(ButtonInt) ->
            FsmStmt:gui_req(button, ButtonInt, gui_resp_cb_fun(<<"button">>, FsmStmt, From));
        ButtonBin when is_binary(ButtonBin) ->
            case string:to_integer(binary_to_list(ButtonBin)) of
                {error, _} -> Button = ButtonBin;
                {Target, []} -> Button = Target
            end,
            FsmStmt:gui_req(button, Button, gui_resp_cb_fun(<<"button">>, FsmStmt, From))
    end,
    Priv;
process_cmd({[<<"update_data">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"update_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    CellId = proplists:get_value(<<"cellid">>, BodyJson, <<>>),
    Value = proplists:get_value(<<"value">>, BodyJson, <<>>),
    Statement:gui_req(update, [{RowId,upd,[{CellId,Value}]}], gui_resp_cb_fun(<<"update_data">>, Statement, From)),
    Priv;
process_cmd({[<<"delete_row">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"delete_row">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowIds = proplists:get_value(<<"rowids">>, BodyJson, []),
    DelSpec = [{RowId,del,[]} || RowId <- RowIds],
    ?Debug("delete ~p ~p", [RowIds, DelSpec]),
    Statement:gui_req(update, DelSpec, gui_resp_cb_fun(<<"delete_row">>, Statement, From)),
    Priv;
process_cmd({[<<"insert_data">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"insert_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ClmIdx = proplists:get_value(<<"col">>, BodyJson, <<>>),
    Value =  proplists:get_value(<<"value">>, BodyJson, <<>>),
    Statement:gui_req(update, [{undefined,ins,[{ClmIdx,Value}]}], gui_resp_cb_fun(<<"insert_data">>, Statement, From)),
    Priv;
process_cmd({[<<"paste_data">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"paste_data">>, BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ReceivedRows = proplists:get_value(<<"rows">>, BodyJson, []),
    Rows = gen_adapter:extract_modified_rows(ReceivedRows),
    Statement:gui_req(update, Rows, gui_resp_cb_fun(<<"paste_data">>, Statement, From)),
    Priv;
process_cmd({[<<"download_query">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"download_query">>, BodyJson}] = ReqBody,
    FileName = proplists:get_value(<<"fileToDownload">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"queryToDownload">>, BodyJson, <<>>),
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    BindVals = case make_binds(proplists:get_value(<<"binds">>, BodyJson, null)) of
                                   {error, _Error} -> undefined;
                                   BindVals0 -> BindVals0
                                end, 
    case dderloci:exec(Connection, Query, BindVals, ?GET_ROWNUM_LIMIT) of
        {ok, #stmtResult{stmtCols = Clms, stmtRef = StmtRef, rowFun = RowFun}, _} ->
            Columns = gen_adapter:build_column_csv(oci,Clms),
            From ! {reply_csv, FileName, Columns, first},
            ProducerPid = spawn(fun() ->
                produce_csv_rows(From, StmtRef, RowFun)
            end),
            dderloci:add_fsm(StmtRef, {?MODULE, ProducerPid}),
            dderloci:fetch_recs_async(StmtRef, [{fetch_mode, push}], 0),
            ?Debug("process_query created statement ~p for ~p", [ProducerPid, Query]);
        Error ->
            ?Error("query error ~p", [Error]),
            Error = if is_binary(Error) -> Error;
                true -> list_to_binary(lists:flatten(io_lib:format("~p", [Error])))
            end,
            From ! {reply_csv, FileName, Error, single}
    end,
    Priv;

% unsupported gui actions
process_cmd({Cmd, BodyJson}, _Sess, _UserId, From, Priv, _SessPid) ->
    ?Error("unsupported command ~p content ~p and priv ~p", [Cmd, BodyJson, Priv]),
    CmdBin = lists:last(Cmd),
    From ! {reply, jsx:encode([{CmdBin,[{<<"error">>, <<"command '", CmdBin/binary, "' is unsupported">>}]}])},
    Priv.

% dderl_fsm like row receive interface for compatibility
rows(Rows, {?MODULE, Pid}) -> Pid ! Rows.
rows_limit(_NRows, Rows, {?MODULE, Pid}) -> Pid ! {Rows, true}. %% Fake a completed to send the last cvs part.
produce_csv_rows(From, StmtRef, RowFun) when is_function(RowFun) andalso is_pid(From) ->
    receive
        Data ->
            case erlang:process_info(From) of
                undefined ->
                    ?Error("Request aborted (response pid ~p invalid)", [From]),
                    dderloci:close(StmtRef);
                _ ->
                    produce_csv_rows_result(Data, From, StmtRef, RowFun)
            end
    end.

produce_csv_rows_result({error, Error}, From, StmtRef, _RowFun) ->
    From ! {reply_csv, <<>>, list_to_binary(io_lib:format("Error: ~p", [Error])), last},
    dderloci:close(StmtRef);
produce_csv_rows_result({Rows, false}, From, StmtRef, RowFun) when is_list(Rows) andalso is_function(RowFun) ->
    CsvRows = list_to_binary([list_to_binary([string:join([binary_to_list(TR) || TR <- Row],
                                                          gen_adapter:get_csv_col_sep_char(oci)), gen_adapter:get_csv_row_sep_char(oci)])
                             || Row <- [RowFun(R) || R <- Rows]]),
    From ! {reply_csv, <<>>, CsvRows, continue},
    produce_csv_rows(From, StmtRef, RowFun);
produce_csv_rows_result({Rows, true}, From, StmtRef, RowFun) when is_list(Rows) andalso is_function(RowFun) ->
    CsvRows = list_to_binary([list_to_binary([string:join([binary_to_list(TR) || TR <- Row],
                                                          gen_adapter:get_csv_col_sep_char(oci)), gen_adapter:get_csv_row_sep_char(oci)])
                             || Row <- [RowFun(R) || R <- Rows]]),
    From ! {reply_csv, <<>>, CsvRows, last},
    dderloci:close(StmtRef).


-spec disconnect(#priv{}) -> #priv{}.
disconnect(#priv{connections = Connections} = Priv) ->
    ?Debug("closing the connections ~p", [Connections]),
    [Connection:close(port_close) || Connection <- Connections],
    Priv#priv{connections = []}.

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


-spec process_query(tuple()|binary(), tuple(), pid()) -> list().
process_query({Query, BindVals}, {oci_port, _, _} = Connection, SessPid) ->
    process_query(check_funs(dderloci:exec(Connection, Query, BindVals,
                                           ?GET_ROWNUM_LIMIT)),
                  Query, BindVals, Connection, SessPid);
process_query(Query, {oci_port, _, _} = Connection, SessPid) ->
    process_query(check_funs(dderloci:exec(Connection, Query,
                                           ?GET_ROWNUM_LIMIT)),
                  Query, [], Connection, SessPid).

-spec process_query(term(), binary(), list(), tuple(), pid()) -> list().
process_query(ok, Query, _BindVals, Connection, _SessPid) ->
    ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
    [{<<"result">>, <<"ok">>}];
process_query({ok, #stmtResult{sortSpec = SortSpec, stmtCols = Clms} = StmtRslt, TableName},
              Query, BindVals, {oci_port, _, _} = Connection, SessPid) ->
    FsmCtx = generate_fsmctx_oci(StmtRslt, Query, BindVals, Connection, TableName),
    StmtFsm = dderl_fsm:start(FsmCtx, SessPid),
    dderloci:add_fsm(StmtRslt#stmtResult.stmtRef, StmtFsm),
    ?Debug("StmtRslt ~p ~p", [Clms, SortSpec]),
    Columns = gen_adapter:build_column_json(lists:reverse(Clms)),
    JSortSpec = build_srtspec_json(SortSpec),
    ?Debug("JColumns~n ~s~n JSortSpec~n~s", [jsx:prettify(jsx:encode(Columns)), jsx:prettify(jsx:encode(JSortSpec))]),
    ?Debug("process_query created statement ~p for ~p", [StmtFsm, Query]),
    [{<<"columns">>, Columns},
     {<<"sort_spec">>, JSortSpec},
     {<<"statement">>, base64:encode(term_to_binary(StmtFsm))},
     {<<"connection">>, ?E2B(Connection)}];
process_query({error, {Code, Msg}}, _Query, _BindVals, _Connection, _SessPid) when is_binary(Msg) ->
    ?Error("query error ~p", [{Code, Msg}]),
    [{<<"error">>, Msg}];
process_query(Error, _Query, _BindVals, _Connection, _SessPid) ->
    ?Error("query error ~p", [Error]),
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
            ?Error("Error: ~p",  [Err]),
            From ! {reply, jsx:encode([{BinCmd, [{<<"error">>, Err}]}])}
    end,
    ok.

process_table_cmd(Cmd, TableName, BodyJson, Connections) ->
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            case dderloci:run_table_cmd(Connection, Cmd, TableName) of
                ok -> ok;
                {error, Error} ->
                    ?Error("query error ~p", [Error]),
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
    [build_srtspec_json(SP, AscDesc) || {SP, AscDesc} <- SortSpecs].

build_srtspec_json(SP, <<"asc">>) ->
    build_srtspec_json(SP, true);
build_srtspec_json(SP, <<"desc">>) ->
    build_srtspec_json(SP, false);
build_srtspec_json(SP, IsAsc) when is_integer(SP) ->
    {integer_to_binary(SP), [{<<"id">>, SP}, {<<"asc">>, IsAsc}]};
build_srtspec_json(SP, IsAsc) when is_binary(SP) ->
    case string:to_integer(binary_to_list(SP)) of
        {SPInt, []} ->
            {SP, [{<<"id">>, SPInt}, {<<"asc">>, IsAsc}]};
        _ ->
            {SP, [{<<"id">>, -1}, {<<"asc">>, IsAsc}]}
    end.

-spec error_invalid_conn({atom(), pid()}, [{atom(), pid()}]) -> term().
error_invalid_conn(Connection, Connections) ->
    Err = <<"Trying to process a query with an unowned connection">>,
    ?Error("~s: ~p~n connections list: ~p", [Err, Connection, Connections]),
    jsx:encode([{<<"error">>, Err}]).

-spec check_fun_vsn(fun()) -> boolean().
check_fun_vsn(Fun) when is_function(Fun)->
    {module, Mod} = erlang:fun_info(Fun, module),
    ?Debug("The module: ~p", [Mod]),
    [ModVsn] = proplists:get_value(vsn, Mod:module_info(attributes)),
    ?Debug("The Module version: ~p~n", [ModVsn]),
    {new_uniq, <<FunVsn:16/unit:8>>} = erlang:fun_info(Fun, new_uniq),
    ?Debug("The function version: ~p~n", [FunVsn]),
    ModVsn =:= FunVsn;
check_fun_vsn(Something) ->
    ?Error("Not a function ~p", [Something]),
    false.

-spec check_funs(term()) -> term().
check_funs({ok, #stmtResult{rowFun = RowFun, sortFun = SortFun} = StmtRslt, TableName}) ->
    ValidFuns = check_fun_vsn(RowFun) andalso check_fun_vsn(SortFun),
    if
        ValidFuns -> {ok, StmtRslt, TableName};
        true -> <<"Unsupported target database version">>
    end;
check_funs(Error) ->
    ?Error("Error on checking the fun versions ~p", [Error]),
    Error.

-spec generate_fsmctx_oci(#stmtResult{}, binary(), list(), tuple(), term()) -> #fsmctx{}.
generate_fsmctx_oci(#stmtResult{
                  stmtCols = Clms
                , rowFun   = RowFun
                , stmtRef  = StmtRef
                , sortFun  = SortFun
                , sortSpec = SortSpec}, Query, BindVals, {oci_port, _, _} = Connection, TableName) ->
    #fsmctx{id            = "what is it?"
           ,stmtCols      = Clms
           ,rowFun        = RowFun
           ,sortFun       = SortFun
           ,sortSpec      = SortSpec
           ,orig_qry      = Query
           ,bind_vals     = BindVals
           ,table_name    = TableName
           ,block_length  = ?DEFAULT_ROW_SIZE
           ,fetch_recs_async_fun = fun(Opts, Count) -> dderloci:fetch_recs_async(StmtRef, Opts, Count) end
           ,fetch_close_fun = fun() -> dderloci:fetch_close(StmtRef) end
           ,stmt_close_fun  = fun() -> dderloci:close(StmtRef) end
           ,filter_and_sort_fun =
                fun(FilterSpec, SrtSpec, Cols) ->
                        dderloci:filter_and_sort(StmtRef, Connection, FilterSpec, SrtSpec, Cols, Query)
                end
           ,update_cursor_prepare_fun =
                fun(ChangeList) ->
                        ?Debug("The stmtref ~p, the table name: ~p and the change list: ~n~p", [StmtRef, TableName, ChangeList]),
                        dderloci_stmt:prepare(TableName, ChangeList, Connection, Clms)
                end
           ,update_cursor_execute_fun =
                fun(_Lock, PrepStmt) ->
                        Result = dderloci_stmt:execute(PrepStmt),
                        ?Debug("The result from the exec ~p", [Result]),
                        Result
                end
           }.

get_value_empty_default(Key, Proplist, Defaults) ->
    proplists:get_value(
      Key, Proplist,
      case Key of
          <<"languange">> -> maps:get(languange, Defaults, <<>>);
          <<"territory">> -> maps:get(territory, Defaults, <<>>);
          <<"charset">> -> maps:get(charset, Defaults, <<>>)
      end).

-spec get_deps() -> [atom()].
get_deps() -> [dderloci, erloci].

make_binds(null) -> undefined;
make_binds(Binds) ->
    try
        {Vars, Values} = lists:foldl(
            fun({B, TV}, {NewBinds, NewVals}) ->
                Typ = binary_to_existing_atom(proplists:get_value(<<"typ">>, TV), utf8),
                Val = proplists:get_value(<<"val">>, TV, <<>>),
                {[{B, Typ} | NewBinds], [dderloci_utils:to_ora(Typ, Val) | NewVals]}
            end,
            {[], []}, Binds),
        {lists:reverse(Vars), lists:reverse(Values)}
    catch
        _:Exception ->
            {error, list_to_binary(io_lib:format("bind process error : ~p", [Exception]))}
    end.

logfun({Lvl, File, Func, Line, Msg}) ->
    case Lvl of
        debug       -> ?LogOci(debug,File,Func,Line,Msg);
        info        -> ?LogOci(info,File,Func,Line,Msg);
        notice      -> ?LogOci(info,File,Func,Line,Msg);
        error       -> ?LogOci(error,File,Func,Line,Msg);
        warn        -> ?LogOci(warning,File,Func,Line,Msg);
        critical    -> ?LogOci(error,File,Func,Line,Msg);
        fatal       -> ?LogOci(error,File,Func,Line,Msg);
        unknown     -> ?LogOci(error,File,Func,Line,Msg)
    end;
logfun(Log) ->
    io:format(user, "Log in unsupported format ~p~n", [Log]).
