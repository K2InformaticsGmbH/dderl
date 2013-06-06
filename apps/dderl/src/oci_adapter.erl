-module(oci_adapter).

-include("dderl.hrl").

-export([ init/0
        , process_cmd/5
        , disconnect/1
        ]).

-record(priv, {connections = []}).

init() ->
    dderl_dal:add_adapter(oci, "Oracle/OCI"),
    gen_adapter:add_cmds_views(undefined, system, oci, [
        { <<"Users.sql">>
        , <<"SELECT USERNAME FROM ALL_USERS">>
        , remote },
        { <<"Tables.sql">>
        , <<"SELECT CONCAT(OWNER,CONCAT('.', TABLE_NAME)) AS QUALIFIED_TABLE_NAME FROM ALL_TABLES WHERE OWNER=user ORDER BY TABLE_NAME">>
        , remote },
        { <<"Views.sql">>
        , <<"SELECT CONCAT(OWNER,CONCAT('.', VIEW_NAME)) AS QUALIFIED_TABLE_NAME FROM ALL_VIEWS WHERE OWNER=user ORDER BY VIEW_NAME">>
        , remote },
        { <<"All Views">>
        , <<"select
                c.owner,
                v.name
            from
                ddView as v,
                ddCmd as c, disconnect/1
        
            where
                c.id = v.cmd
                and c.adapters = \"[oci]\"
                and (c.owner = user or c.owner = system)
            order by
                v.name,
                c.owner">>
        , local}
    ]).

process_cmd({[<<"connect">>], ReqBody}, Sess, UserId, From, undefined) ->
    process_cmd({[<<"connect">>], ReqBody}, Sess, UserId, From, #priv{connections = []});
process_cmd({[<<"connect">>], ReqBody}, Sess, UserId, From, #priv{connections = Connections} = Priv) ->
    [{<<"connect">>,BodyJson}] = ReqBody,
    IpAddr   = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    Service  = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Type     = binary_to_list(proplists:get_value(<<"type">>, BodyJson, <<>>)),
    User     = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = proplists:get_value(<<"password">>, BodyJson, <<>>),
    Tnsstr   = proplists:get_value(<<"tnsstring">>, BodyJson, <<>>),
    ?Info("session:open ~p", [{IpAddr, Port, Service, Type, User, Password, Tnsstr}]),    
    %%ErlOciSession = erloci_session:start_link(Tnsstr, User, Password, <<>>, [{logging, true}]),
    ErlOciSession = {a,self()},
    case ErlOciSession of
        {_, ErlOciSessionPid} when is_pid(ErlOciSessionPid) ->
            ?Info("ErlOciSession ~p", [ErlOciSession]),
            Con = #ddConn { id = erlang:phash2(make_ref())
                          , name     = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>))
                          , owner    = UserId
                          , adapter  = oci
                          , access   = [ {ip,       IpAddr}
                                       , {port,     Port}
                                       , {service,  Service}
                                       , {type,     Type}
                                       , {user,     User}
                                       , {tnsstr,   Tnsstr}
                                       ]
                          , schema   = list_to_atom(Service)
                          },
                    ?Debug([{user, User}], "may save/replace new connection ~p", [Con]),
                    dderl_dal:add_connect(Sess, Con),
            From ! {reply, jsx:encode([{<<"connect">>,list_to_binary(?EncryptPid(ErlOciSessionPid))}])},
            Priv#priv{connections = [ErlOciSession|Connections]};
        Error ->
            ?Error("DB connect error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            From ! {reply, jsx:encode([{<<"connect">>,[{<<"error">>, Err}]}])},
            Priv
    end;

% views
process_cmd({[<<"views">>], _}, Sess, _UserId, From, Priv) ->
    [F|_] = dderl_dal:get_view(Sess, <<"All Views">>),
    C = dderl_dal:get_command(Sess, F#ddView.cmd),
    Resp = process_query(C#ddCmd.command, Sess),
    ?Debug("Views ~p~n~p", [C#ddCmd.command, Resp]),
    RespJson = jsx:encode([{<<"views">>,
        [{<<"content">>, C#ddCmd.command}
        ,{<<"name">>, <<"All Views">>}
        ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
        ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}]
        ++ Resp
    }]),
    From ! {reply, RespJson},
    Priv;

% unsupported gui actions
process_cmd({Cmd, BodyJson}, _Sess, _UserId, From, Priv) ->
    ?Error("unsupported command ~p content ~p and priv ~p", [Cmd, BodyJson, Priv]),
    CmdBin = lists:last(Cmd),
    From ! {reply, jsx:encode([{CmdBin,[{<<"error">>, <<"command ", CmdBin/binary, " is unsupported">>}]}])},
    Priv.

disconnect(#priv{connections = Connections} = Priv) ->
    ?Debug("closing the connections ~p", [Connections]),
    [Connection:close() || Connection <- Connections],
    Priv#priv{connections = []}.

process_query(Query, {_,ConPid}=Connection) ->
    case Connection:exec(Query, ?DEFAULT_ROW_SIZE) of
        {ok, StmtRslt, {_,_,ConPid}=Statement} ->
            Clms = proplists:get_value(cols, StmtRslt, []),
            SortSpec = proplists:get_value(sort_spec, StmtRslt, []),
            ?Debug("StmtRslt ~p ~p", [Clms, SortSpec]),
            Columns = build_column_json(lists:reverse(Clms), []),
            JSortSpec = build_srtspec_json(SortSpec),
            ?Debug("JColumns~n"++binary_to_list(jsx:prettify(jsx:encode(Columns)))++
                   "~n JSortSpec~n"++binary_to_list(jsx:prettify(jsx:encode(JSortSpec)))),
            ?Debug("process_query created statement ~p for ~p", [Statement, Query]),
            [{<<"columns">>, Columns},
             {<<"sort_spec">>, JSortSpec},
             {<<"statement">>, base64:encode(term_to_binary(Statement))},
             {<<"connection">>, list_to_binary(?EncryptPid(ConPid))}];
        ok ->
            ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
            [{<<"result">>, <<"ok">>}];
        {error, {{Ex, M}, _Stacktrace} = Error} ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            [{<<"error">>, Err}];
        {error, {Ex,M}} ->
            ?Error([{session, Connection}], "query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            [{<<"error">>, Err}];
        Error ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            [{<<"error">>, Err}]
    end.

build_srtspec_json(SortSpecs) ->
    [{if is_integer(SP) -> integer_to_binary(SP); true -> SP end
     , [{<<"id">>, if is_integer(SP) -> SP; true -> -1 end}
       ,{<<"asc">>, if AscDesc =:= <<"asc">> -> true; true -> false end}]
     } || {SP,AscDesc} <- SortSpecs].

build_column_json([], JCols) ->
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
build_column_json([C|Cols], JCols) ->
    Nm = C#stmtCol.alias,
    Nm1 = if Nm =:= <<"id">> -> <<"_id">>; true -> Nm end,
    JC = [{<<"id">>, Nm1},
          {<<"name">>, Nm},
          {<<"field">>, Nm1},
          {<<"resizable">>, true},
          {<<"sortable">>, false},
          {<<"selectable">>, true}],
    JCol = if C#stmtCol.readonly =:= false -> [{<<"editor">>, <<"true">>} | JC]; true -> JC end,
    build_column_json(Cols, [JCol | JCols]).

