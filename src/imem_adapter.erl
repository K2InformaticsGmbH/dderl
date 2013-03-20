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

process_cmd({[<<"connect">>], ReqBody}, _) ->
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
    ?Debug("session:open ~p", [{Type, Opts, {User, Password}}]),
    case erlimem:open(Type, Opts, {User, Password}) of
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
            ?Debug([{user, User}], "may save/replace new connection ~p", [Con]),
            dderl_dal:add_connect(Con),
            {#priv{sess=Connection, stmts=Statements}, binary_to_list(jsx:encode([{<<"connect">>,list_to_binary(?EncryptPid(ConPid))}]))}
    end;
process_cmd({[<<"query">>], ReqBody}, #priv{sess=Connection}=Priv) ->
    [{<<"query">>,BodyJson}] = ReqBody,
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    {NewPriv, R} = case dderl_dal:is_local_query(Query) of
        true -> process_query(Query, dderl_dal:get_session(), Priv);
        _ -> process_query(Query, Connection, Priv)
    end,
    ?Debug("query ~p~n~p", [Query, R]),
    {NewPriv, binary_to_list(jsx:encode([{<<"query">>,R}]))};

process_cmd({[<<"browse_data">>], ReqBody}, #priv{sess={_,ConnPid}} = Priv) ->
    [{<<"browse_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Connection = {erlimem_session, ConnPid},
    Row = proplists:get_value(<<"row">>, BodyJson, <<>>),
    Col = proplists:get_value(<<"col">>, BodyJson, <<>>),
    R = Statement:row_with_key(Row+1),
    ?Debug("Row with key ~p",[R]),
    Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
    IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
    ?Debug("browse_data (view ~p) ~p - ~p", [IsView, Tables, {R, Col}]),
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

% views
process_cmd({[<<"views">>], _}, Priv) ->
    [F|_] = dderl_dal:get_view("All Views"),
    C = dderl_dal:get_command(F#ddView.cmd),
    AdminSession = dderl_dal:get_session(),
    {NewPriv, Resp} = process_query(C#ddCmd.command, AdminSession, Priv),
    ?Debug("Views ~p~n~p", [C#ddCmd.command, Resp]),
    RespJson = jsx:encode([{<<"views">>,
        [{<<"content">>, list_to_binary(C#ddCmd.command)}
        ,{<<"name">>, <<"All Views">>}
        ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
        ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}]
        ++ Resp
    }]),
    {NewPriv, binary_to_list(RespJson)};
process_cmd({[<<"save_view">>], BodyJson}, Priv) -> gen_adapter:process_cmd({[<<"save_view">>], BodyJson}, Priv);

% query
process_cmd({[<<"get_query">>], BodyJson}, Priv) -> gen_adapter:process_cmd({[<<"get_query">>], BodyJson}, Priv);
process_cmd({[<<"parse_stmt">>], BodyJson}, Priv) -> gen_adapter:process_cmd({[<<"parse_stmt">>], BodyJson}, Priv);

% sort and filter
process_cmd({[<<"sort">>], ReqBody}, Priv) ->
    [{<<"sort">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    SrtSpc = proplists:get_value(<<"spec">>, BodyJson, []),
    SortSpec = sort_json_to_term(SrtSpc),
    GuiResp = Statement:gui_req(sort, SortSpec),
    GuiRespJson = gen_adapter:gui_resp(GuiResp, Statement:get_columns()),
    ?Debug("sort ~p ~p", [SortSpec, GuiResp]),
    {Priv, binary_to_list(jsx:encode([{<<"sort">>,GuiRespJson}]))};
process_cmd({[<<"filter">>], ReqBody}, Priv) ->
    [{<<"filter">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    FltrSpec = proplists:get_value(<<"spec">>, BodyJson, []),
    FilterSpec = filter_json_to_term(FltrSpec),
    GuiResp = Statement:gui_req(filter, FilterSpec),
    GuiRespJson = gen_adapter:gui_resp(GuiResp, Statement:get_columns()),
    ?Debug("filter ~p ~p", [FilterSpec, GuiResp]),
    {Priv, binary_to_list(jsx:encode([{<<"filter">>,GuiRespJson}]))};

% gui button events
process_cmd({[<<"button">>], ReqBody}, Priv) ->
    [{<<"button">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Button = proplists:get_value(<<"btn">>, BodyJson, <<">">>),
    GuiResp = Statement:gui_req(button, Button),
    GuiRespJson = gen_adapter:gui_resp(GuiResp, Statement:get_columns()),
    ?Debug("GUI response ~p", [GuiRespJson]),
    {Priv, binary_to_list(jsx:encode([{<<"button">>, GuiRespJson}]))};
process_cmd({[<<"update_data">>], ReqBody}, Priv) ->
    [{<<"update_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    CellId = proplists:get_value(<<"cellid">>, BodyJson, <<>>),
    Value = binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    GuiResp = Statement:gui_req(update, [{RowId,upd,[{CellId,Value}]}]),
    GuiRespJson = gen_adapter:gui_resp(GuiResp, Statement:get_columns()),
    ?Info("updated ~p", [GuiResp]),
    {Priv, binary_to_list(jsx:encode([{<<"update_data">>, GuiRespJson}]))};
process_cmd({[<<"delete_row">>], ReqBody}, Priv) ->
    [{<<"delete_row">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    GuiResp = Statement:gui_req(update, [{RowId,del,[]}]),
    GuiRespJson = gen_adapter:gui_resp(GuiResp, Statement:get_columns()),
    ?Info("deleted ~p", [GuiResp]),
    {Priv, binary_to_list(jsx:encode([{<<"delete_row">>, GuiRespJson}]))};
process_cmd({[<<"insert_data">>], ReqBody}, Priv) ->
    [{<<"insert_data">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    ClmName = binary_to_list(proplists:get_value(<<"col">>, BodyJson, <<>>)),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    GuiResp = Statement:gui_req(update, [{undefined,ins,[{ClmName,Value}]}]),
    GuiRespJson = gen_adapter:gui_resp(GuiResp, Statement:get_columns()),
    ?Info("inserted ~p", [GuiResp]),
    {Priv, binary_to_list(jsx:encode([{<<"insert_data">>, GuiRespJson}]))};

% unsupported gui actions
process_cmd({Cmd, BodyJson}, Priv) ->
    ?Error("unsupported command ~p content ~p and priv ~p", [Cmd, BodyJson, Priv]),
    CmdBin = lists:last(Cmd),
    {Priv, binary_to_list(jsx:encode([{CmdBin,[{<<"error">>, <<"command ", CmdBin/binary, " is unsupported">>}]}]))}.

sort_json_to_term([]) -> [];
sort_json_to_term([[{C,T}|_]|Sorts]) ->
    [{binary_to_integer(C), if T -> 'asc'; true -> 'desc' end}|sort_json_to_term(Sorts)].

filter_json_to_term([{<<"and">>,Filters}]) -> {'and', filter_json_to_term(Filters)};
filter_json_to_term([{<<"or">>,Filters}]) -> {'or', filter_json_to_term(Filters)};
filter_json_to_term([]) -> [];
filter_json_to_term([[{C,Vs}]|Filters]) ->
    Tail = filter_json_to_term(Filters),
    [{binary_to_integer(C), [binary_to_list(V) || V <- Vs]} | Tail].

process_query(Query, {_,ConPid}=Connection, Priv) ->
    case Connection:exec(Query, ?DEFAULT_ROW_SIZE) of
        {ok, Clms, {_,_,ConPid}=Statement} ->
            ?Debug([{session, Connection}], "Cols ~p", [Clms]),
            Columns = build_column_json(lists:reverse(Clms), []),
            ?Debug("JColumns~n" ++ binary_to_list(jsx:prettify(jsx:encode(Columns)))),
            ?Info("process_query created statement ~p for ~p", [Statement, Query]),
            {Priv, [{<<"columns">>, Columns}
                   ,{<<"statement">>, base64:encode(term_to_binary(Statement))}
                   ,{<<"connection">>, list_to_binary(?EncryptPid(ConPid))}]};
        {error, {Ex,M}} ->
            ?Error([{session, Connection}], "query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            {Priv, [{<<"error">>, Err}]};
        Error ->
            ?Error([{session, Connection}], "query error ~p", [Error]),
            Err = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            {Priv, [{<<"error">>, Err}]}
    end.

format_return({error, {_,{error, _}=Error}}) -> format_return(Error);
format_return({error, {E,{R,_Ext}} = Excp}) ->
    ?Debug("exception ~p", [Excp]),
    list_to_binary([atom_to_list(E),": ",R,"\n",lists:nth(1,io_lib:format("~p", [_Ext]))]);
format_return(Result)             -> list_to_binary(io_lib:format("~p", [Result])).

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
    Nm1 = if Nm =:= <<"id">> -> <<"_id">>; true -> Nm end,
    JC = [{<<"id">>, Nm1},
          {<<"name">>, Nm},
          {<<"field">>, Nm1},
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
