-module(imem_adapter).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").

-export([ init/0
        , process_cmd/2
        ]).

-record(priv, { sess
              , stmts
       }).

init() ->
    dderl_dal:add_adapter(imem, "IMEM DB"),
    dderl_dal:add_connect(#ddConn{ id = erlang:phash2(make_ref())
                                 , name = "local imem"
                                 , adapter = imem
                                 , access = [{ip, "local"}, {user, "admin"}]
                                 }),
    gen_adapter:add_cmds_views(imem, [
        { "All Tables"
        , "select name(qname) from all_tables"
        , remote},
        %{"All Tables", "select name(qname) from all_tables where not is_member(\"{virtual, true}\", opts)"},
        { "All Views"
        , "select c.owner, v.name from ddView as v, ddCmd as c where c.id = v.cmd and c.adapters = \"[imem]\" and (c.owner = user or c.owner = system)"
        , local}
        %{"All Views", "select v.name from ddView as v, ddCmd as c where c.id = v.cmd and c.adapters = \"[imem]\" and (c.owner = system)"}
        %{"All Views", "select name, owner, command from ddCmd where adapters = '[imem]' and (owner = user or owner = system)"}
    ]).

build_column_json([], JCols) ->
    [[{<<"id">>, <<"sel">>},
      {<<"name">>, <<"">>},
      {<<"field">>, <<"id">>},
      {<<"behavior">>, <<"select">>},
      {<<"cssClass">>, <<"cell-selection">>},
      {<<"width">>, 30},
      {<<"minWidth">>, 2},
      {<<"cannotTriggerInsert">>, true},
      {<<"resizable">>, true},
      {<<"sortable">>, false},
      {<<"selectable">>, false}] | JCols];
build_column_json([C|Cols], JCols) ->
    Nm = C#stmtCol.alias,
    JC = [{<<"id">>, << "_", Nm/binary >>},
          {<<"name">>, Nm},
          {<<"field">>, Nm},
          {<<"resizable">>, true},
          {<<"sortable">>, false},
          {<<"selectable">>, true}],
    JCol = if C#stmtCol.readonly =:= false -> [{<<"editor">>, <<"true">>} | JC]; true -> JC end,
    build_column_json(Cols, [JCol | JCols]).

int(C) when $0 =< C, C =< $9 -> C - $0;
int(C) when $A =< C, C =< $F -> C - $A + 10;
int(C) when $a =< C, C =< $f -> C - $a + 10.

hexstr_to_list([]) -> [];
hexstr_to_list([X,Y|T]) -> [int(X)*16 + int(Y) | hexstr_to_list(T)].

process_cmd({"connect", ReqBody}, _) ->
    [{<<"connect">>,BodyJson}] = ReqBody,
    Schema = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Port   = binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>)),
    Ip     = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    case Ip of
        Ip when Ip =:= "local_sec" ->
            Type    = local_sec,
            Opts    = {Schema};
        Ip when Ip =:= "local" ->
            Type    = local,
            Opts    = {Schema};
        Ip when Ip =:= "rpc" ->
            Type    = rpc,
            Opts    = {list_to_existing_atom(Port), Schema};
        Ip ->
            Type    = tcp,
            Opts    = {Ip, list_to_integer(Port), Schema}
    end,
    User = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = list_to_binary(hexstr_to_list(binary_to_list(proplists:get_value(<<"password">>, BodyJson, <<>>)))),
    ?Info("session:open ~p", [{Type, Opts, {User, Password}}]),
    case erlimem:open(Type, Opts, {User, Password}) of
        {error, {Ex,M}} ->
            ?Error("DB connect error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            {#priv{}, binary_to_list(jsx:encode([{<<"connect">>,[{<<"error">>, Err}]}]))};
        {error, Error} ->
            ?Error("DB connect error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {#priv{}, binary_to_list(jsx:encode([{<<"connect">>,[{<<"error">>, Err}]}]))};
        {ok, {_,ConPid} = Connection} ->
            ?Debug("session ~p", [Connection]),
            ?Debug("connected to params ~p", [{Type, Opts}]),
            Statements = [],
            Con = #ddConn { id       = erlang:phash2(make_ref())
                          , name     = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>))
                          , adapter  = imem
                          , access   = [ {ip,   Ip}
                                       , {port, Port}
                                       , {type, Type}
                                       , {user, User}
                                       ]
                          , schema   = list_to_atom(Schema)
                          },
            ?Info([{user, User}], "saving new connection ~p", [Con]),
            dderl_dal:add_connect(Con),
            {#priv{sess=Connection, stmts=Statements}, binary_to_list(jsx:encode([{<<"connect">>,list_to_binary(?EncryptPid(ConPid))}]))}
    end;
process_cmd({"query", ReqBody}, Priv) ->
    [{<<"query">>,BodyJson}] = ReqBody,
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    Connection = {erlimem_session, ?DecryptPid(binary_to_list(proplists:get_value(<<"connection">>, BodyJson, <<>>)))},
    ?Info("query ~p", [{Connection, Query}]),
    {NewPriv, R} = process_query(Query, Connection, Priv),
    {NewPriv, binary_to_list(jsx:encode([{<<"query">>,R}]))};

process_cmd({"row_prev", ReqBody}, Priv) ->
    [{<<"row">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ?Debug("row_prev ~p", [self()]),
    Rows = gen_adapter:prepare_json_rows(prev, -1, Statement, Statement),
    ?Info("row_prev ~p rows ~p", [self(), length(Rows)]),
    {Priv, binary_to_list(jsx:encode([{<<"row_prev">>, Rows}]))};
process_cmd({"row_next", ReqBody}, Priv) ->
    [{<<"row">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowNum = proplists:get_value(<<"row_num">>, BodyJson, -1),
    ?Info("row_next from ~p", [RowNum]),
    Rows = gen_adapter:prepare_json_rows(next, RowNum, Statement, Statement),
    ?Info("row_next sending ~p rows", [length(proplists:get_value(<<"rows">>, Rows, []))]),
    {Priv, binary_to_list(jsx:encode([{<<"row_next">>, Rows}]))};
process_cmd({"stmt_close", ReqBody}, Priv) ->
    [{<<"stmt_close">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ?Debug("remove statement ~p", [Statement]),
    Statement:close(),
    {Priv, binary_to_list(jsx:encode([{<<"stmt_close">>, <<"ok">>}]))};
process_cmd({"get_buffer_max", ReqBody}, Priv) ->
    [{<<"get_buffer_max">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    {ok, Finished, CacheSize} = Statement:get_buffer_max(),
    ?Debug("[~p] get_buffer_max ~p finished ~p ~p", [Statement, CacheSize, Finished, self()]),
    {Priv, binary_to_list(jsx:encode([{<<"get_buffer_max">>,
                                        [{<<"count">>, CacheSize}
                                        ,{<<"finished">>, Finished}]}]))};
process_cmd({"update_data", ReqBody}, Priv) ->
    [{<<"update_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    CellId = proplists:get_value(<<"cellid">>, BodyJson, <<>>),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    Result = format_return(Statement:update_row(RowId, CellId, Value)),
    {Priv, binary_to_list(jsx:encode([{<<"update_data">>, Result}]))};
process_cmd({"delete_row", ReqBody}, Priv) ->
    [{<<"delete_row">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    Result = format_return(Statement:delete_row(RowId)),
    {Priv, binary_to_list(jsx:encode([{<<"delete_row">>, Result}]))};
process_cmd({"insert_data", ReqBody}, Priv) ->
    [{<<"insert_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ClmName = binary_to_list(proplists:get_value(<<"col">>, BodyJson, <<>>)),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    Result = format_return(Statement:insert_row(ClmName, Value)),
    ?Info("inserted ~p", [Result]),
    {Priv, binary_to_list(jsx:encode([{<<"insert_data">>, Result}]))};
process_cmd({"commit_rows", ReqBody}, Priv) ->
    [{<<"commit_rows">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    try
        ok = Statement:prepare_update(),
        ok = Statement:execute_update(),
        Ret = Statement:fetch_close(),
        {Priv, binary_to_list(jsx:encode([{<<"commit_rows">>, format_return(Ret)}]))}
    catch
        _:Reason ->
            {Priv, binary_to_list(jsx:encode([{<<"commit_rows">>, [{<<"error">>, format_return({error, Reason})}]}]))}
    end;

process_cmd({"browse_data", ReqBody}, #priv{sess={_,ConnPid}} = Priv) ->
    [{<<"browse_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Connection = {erlimem_session, ConnPid},
    Row = proplists:get_value(<<"row">>, BodyJson, <<>>),
    Col = proplists:get_value(<<"col">>, BodyJson, <<>>),
    R = Statement:row_with_key(Row+1),
    Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
    IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
    ?Info("browse_data (view ~p) ~p - ~p", [IsView, Tables, {R, Col}]),
    if IsView ->
        {#ddView{name=Name,owner=Owner},#ddCmd{}=C,_} = element(3, R),
        Name = element(5, R),
        V = dderl_dal:get_view(Name, Owner),
        ?Info("Cmd ~p Name ~p", [C#ddCmd.command, Name]),
        AdminConn =
            case C#ddCmd.conns of
            'local' -> dderl_dal:get_session();
            _ -> Connection
        end,
        {NewPriv, Resp} = process_query(C#ddCmd.command, AdminConn, Priv),
        RespJson = jsx:encode([{<<"browse_data">>,
            [{<<"content">>, list_to_binary(C#ddCmd.command)}
            ,{<<"name">>, list_to_binary(Name)}
            ,{<<"table_layout">>, (V#ddView.state)#viewstate.table_layout}
            ,{<<"column_layout">>, (V#ddView.state)#viewstate.column_layout}] ++
            Resp
        }]),
        ?Info("loading ~p at ~p", [Name, (V#ddView.state)#viewstate.table_layout]),
        {NewPriv, binary_to_list(RespJson)};
    true ->                
        Name = lists:last(tuple_to_list(R)),
        Query = "SELECT * FROM " ++ Name,
        {NewPriv, Resp} = process_query(Query, Connection, Priv),
        RespJson = jsx:encode([{<<"browse_data">>,
            [{<<"content">>, list_to_binary(Query)}
            ,{<<"name">>, list_to_binary(Name)}] ++
            Resp
        }]),
        {NewPriv, binary_to_list(RespJson)}
    end;

process_cmd({"tail", ReqBody}, Priv) ->
    [{<<"tail">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Push = proplists:get_value(<<"push">>, BodyJson, <<>>),
    Tail = proplists:get_value(<<"tail">>, BodyJson, <<>>),
    Opts = case {Push, Tail} of
                {true, true}    -> [{fetch_mode,push},{tail_mode, true}];
                {true, false}   -> [{fetch_mode,push},{tail_mode,false}];
                {false, true}   -> [{fetch_mode,skip},{tail_mode, true}];
                {false, false}  -> [{fetch_mode,skip},{tail_mode,false}]
    end,
    ?Info(">>>>>>>> ~p tail Opts ~p~n", [{?MODULE,?LINE}, Opts]),
    Statement:start_async_read(Opts),
    {Priv, binary_to_list(jsx:encode([{<<"tail">>, <<"ok">>}]))};

process_cmd({"views", _}, Priv) ->
    [F|_] = dderl_dal:get_view("All Views"),
    C = dderl_dal:get_command(F#ddView.cmd),
    AdminSession = dderl_dal:get_session(),
    {NewPriv, Resp} = process_query(C#ddCmd.command, AdminSession, Priv),
    ?Info("View ~p~n", [F]),
    RespJson = jsx:encode([{<<"views">>,
        [{<<"content">>, list_to_binary(C#ddCmd.command)}
        ,{<<"name">>, <<"All Views">>}
        ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
        ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}]
        ++ Resp
    }]),
%io:format(user, "views ~p~n", [RespJson]),
    {NewPriv, binary_to_list(RespJson)};
process_cmd({"save_view", BodyJson}, Priv) -> gen_adapter:process_cmd({"save_view", BodyJson}, Priv);
process_cmd({"get_query", BodyJson}, Priv) -> gen_adapter:process_cmd({"get_query", BodyJson}, Priv);
process_cmd({"parse_stmt", BodyJson}, Priv) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, Priv);
process_cmd({Cmd, BodyJson}, Priv) ->
    ?Error("unsupported command ~p content ~p", [Cmd, BodyJson]),
    {Priv, binary_to_list(jsx:encode([{<<"rows">>,[]}]))}.

process_query(Query, {_,ConPid}=Connection, Priv) ->
    case Connection:exec(Query, ?DEFAULT_ROW_SIZE) of
        {ok, Clms, {_,_,ConPid}=Statement} ->
            ?Info([{session, Connection}], "Cols ~p", [Clms]),
            Columns = build_column_json(lists:reverse(Clms), []),
            ?Debug("JColumns~n" ++ binary_to_list(jsx:prettify(jsx:encode(Columns)))),
            Statement:start_async_read([]),
            ?Info("process_query created statement ~p for ~p", [Statement, Query]),
            {Priv, [{<<"columns">>, Columns}
                   ,{<<"statement">>, base64:encode(term_to_binary(Statement))}
                   ,{<<"connection">>, list_to_binary(?EncryptPid(ConPid))}]};
        {error, {Ex,M}} ->
            ?Error([{session, Connection}], "query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            {Priv, [{<<"error">>, Err}]}
    end.

format_return({error, {_,{error, _}=Error}}) -> format_return(Error);
format_return({error, {E,{R,_Ext}} = Excp}) ->
    ?Debug("exception ~p", [Excp]),
    list_to_binary([atom_to_list(E),": ",R,"\n",lists:nth(1,io_lib:format("~p", [_Ext]))]);
format_return(Result)             -> list_to_binary(io_lib:format("~p", [Result])).
