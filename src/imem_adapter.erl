-module(imem_adapter).

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
        {"All Tables", "select name(qname) from all_tables"},
        %{"All Views", "select name, owner, command from ddCmd where adapters = '[imem]' and (owner = user or owner = system)"}
        {"All Views", "select v.name from ddView as v, ddCmd as c where c.id = v.cmd and c.adapters = \"[imem]\" and (c.owner = user or c.owner = system)"}
    ]).

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
    lager:debug("session:open ~p", [{ype, Opts, {User, Password}}]),
    case erlimem:open(Type, Opts, {User, Password}) of
        {error, {Ex,M}} ->
            lager:error("DB connect error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            {#priv{}, binary_to_list(jsx:encode([{<<"connect">>,Err}]))};
        {ok, Session} ->
            lager:debug("session ~p", [Session]),
            lager:debug("connected to params ~p", [{Type, Opts}]),
            Statements = [],
            Con = #ddConn { id       = erlang:phash2(make_ref())
                          , name     = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>))
                          , owner    = binary_to_list(User)
                          , adapter  = imem
                          , access   = [ {ip,   Ip}
                                       , {port, Port}
                                       , {type, Type}
                                       , {user, User}
                                       ]
                          , schema   = list_to_atom(Schema)
                          },
            lager:debug([{user, User}], "saving new connection ~p", [Con]),
            dderl_dal:add_connect(Con),
            {#priv{sess=Session, stmts=Statements}, binary_to_list(jsx:encode([{<<"connect">>,<<"ok">>}]))}
    end;
process_cmd({"query", ReqBody}, #priv{sess=Session} = Priv) ->
    [{<<"query">>,BodyJson}] = ReqBody,
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    lager:debug([{session, Session}], "query ~p", [{Session, Query}]),
    {NewPriv, R} = process_query(Query, Priv),
    {NewPriv, binary_to_list(jsx:encode(R))};

process_cmd({"row_prev", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"row">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:error("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"rows">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} -> {Priv, gen_adapter:prepare_json_rows(prev, -1, Statement, StmtKey)}
    end;
process_cmd({"row_next", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"row">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowNum = proplists:get_value(<<"row_num">>, BodyJson, -1),
    lager:info("row_next ~p", [self()]),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:error("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"rows">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} -> {Priv, gen_adapter:prepare_json_rows(next, RowNum, Statement, StmtKey)}
    end;
process_cmd({"stmt_close", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"stmt_close">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:info("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"stmt_close">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            lager:debug("[~p] remove statement ~p", [StmtKey, Statement]),
            Statement:close(),
            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
            {Priv#priv{stmts=NewStatements}, binary_to_list(jsx:encode([{<<"stmt_close">>, <<"ok">>}]))}
    end;
process_cmd({"get_buffer_max", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"get_buffer_max">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"get_buffer_max">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            {ok, Finished, CacheSize} = Statement:get_buffer_max(),
            lager:info("[~p] get_buffer_max ~p finished ~p ~p", [StmtKey, CacheSize, Finished, self()]),
            {Priv, binary_to_list(jsx:encode([{<<"get_buffer_max">>,
                                                [{<<"count">>, CacheSize}
                                                ,{<<"finished">>, Finished}]}]))}
    end;
process_cmd({"update_data", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"update_data">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    CellId = proplists:get_value(<<"cellid">>, BodyJson, <<>>),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"update_data">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            Result = format_return(Statement:update_row(RowId, CellId, Value)),
            {Priv, binary_to_list(jsx:encode([{<<"update_data">>, Result}]))}
    end;
process_cmd({"delete_row", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"delete_row">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"delete_row">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            Result = format_return(Statement:delete_row(RowId)),
            {Priv, binary_to_list(jsx:encode([{<<"delete_row">>, Result}]))}
    end;
process_cmd({"insert_data", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"insert_data">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    ClmName = binary_to_list(proplists:get_value(<<"col">>, BodyJson, <<>>)),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. Statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"insert_data">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            Result = format_return(Statement:insert_row(ClmName, Value)),
            {Priv, binary_to_list(jsx:encode([{<<"insert_data">>, Result}]))}
    end;
process_cmd({"commit_rows", ReqBody}, #priv{stmts=Statements} = Priv) ->
    [{<<"commit_rows">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"commit_rows">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            Result = format_return(Statement:commit_modified()),
            {Priv, binary_to_list(jsx:encode([{<<"commit_rows">>, Result}]))}
    end;

process_cmd({"browse_data", ReqBody}, #priv{sess=_Session, stmts=Statements} = Priv) ->
    [{<<"browse_data">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    Row = proplists:get_value(<<"row">>, BodyJson, <<>>),
    Col = proplists:get_value(<<"col">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"browse_data">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            R = Statement:row_with_key(Row+1),
            Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
            IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables),
            lager:debug("browse_data (view ~p) ~p - ~p", [IsView, Tables, {R, Col}]),
            if IsView ->
            {_,_,{_,C,_},Name} = R,
                lager:debug("Cmd ~p Name ~p", [C#ddCmd.command, Name]),
                {NewPriv, Resp} = process_query(C#ddCmd.command, Priv),
                RespJson = jsx:encode([{<<"browse_data">>,
                    [{<<"content">>, list_to_binary(C#ddCmd.command)}
                    ,{<<"name">>, list_to_binary(Name)}] ++
                    Resp
                }]),
                {NewPriv, binary_to_list(RespJson)};
            true ->                
                Name = lists:last(tuple_to_list(R)),
                Query = "SELECT * FROM " ++ Name,
                {NewPriv, Resp} = process_query(Query, Priv),
                RespJson = jsx:encode([{<<"browse_data">>,
                    [{<<"content">>, list_to_binary(Query)}
                    ,{<<"name">>, list_to_binary(Name)}] ++
                    Resp
                }]),
                {NewPriv, binary_to_list(RespJson)}
            end
    end;

process_cmd({"tail", ReqBody}, #priv{sess=_Session, stmts=Statements} = Priv) ->
    [{<<"tail">>,BodyJson}] = ReqBody,
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    Start = proplists:get_value(<<"start">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, binary_to_list(jsx:encode([{<<"tail">>, [{<<"error">>, <<"invalid statement">>}]}]))};
        {Statement, _, _} ->
            if Start =:= true ->
lager:info(">>>>>>>> ~p tail ~p~n", [{?MODULE,?LINE}, Start]),
%                Statement:fetch_close(),
                Statement:start_async_read([{tail_mode,Start}]);
                true -> Statement:fetch_close()
            end,
            {Priv, binary_to_list(jsx:encode([{<<"tail">>, Start}]))}
    end;

process_cmd({"views", _}, Priv) ->
    [F|_] = dderl_dal:get_view("All Views"),
    C = dderl_dal:get_command(F#ddView.cmd),
    #priv{sess=ClientSess} = Priv,
    AdminSession = dderl_dal:get_session(),
    {NewPriv, Resp} = process_query(C#ddCmd.command, Priv#priv{sess=AdminSession}),
    RespJson = jsx:encode([{<<"views">>,
    [{<<"content">>, list_to_binary(C#ddCmd.command)}
    ,{<<"name">>, <<"All Views">>}]
    ++ Resp
    }]),
%io:format(user, "views ~p~n", [RespJson]),
    {NewPriv#priv{sess=ClientSess}, binary_to_list(RespJson)};
process_cmd({"get_query", BodyJson}, Priv) -> gen_adapter:process_cmd({"get_query", BodyJson}, Priv);
process_cmd({"parse_stmt", BodyJson}, Priv) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, Priv);
process_cmd({Cmd, BodyJson}, Priv) ->
    lager:error("unsupported command ~p content ~p", [Cmd, BodyJson]),
    {Priv, binary_to_list(jsx:encode([{<<"rows">>,[]}]))}.

process_query(Query, #priv{sess=Session, stmts=Statements} = Priv) ->
    case Session:exec(Query, ?DEFAULT_ROW_SIZE) of
        {ok, Clms, Statement} ->
            StmtHndl = erlang:phash2(Statement),
            Columns = lists:reverse([atom_to_list(C#ddColMap.name)||C<-Clms]),
            lager:debug([{session, Session}], "columns ~p", [Columns]),
            Statement:start_async_read([]),
            {Priv#priv{stmts=[{StmtHndl, {Statement, Query, []}}|Statements]}
            , [{<<"columns">>, gen_adapter:strs2bins(Columns)}
              ,{<<"statement">>,StmtHndl}]};
        {error, {Ex,M}} ->
            lager:error([{session, Session}], "query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++ element(1, M)),
            {Priv, [{<<"columns">>,[]},{<<"statement">>,0},{<<"error">>, Err}]};
        Res ->
            lager:debug("qry ~p~nResult ~p", [Query, Res]),
            {Priv, [{<<"columns">>,[]},{<<"statement">>,0}]}
    end.

format_return({error, {E,{R,_}}}) -> list_to_binary(atom_to_list(E)++": "++R);
format_return(Result)             -> list_to_binary(io_lib:format("~p", [Result])).
