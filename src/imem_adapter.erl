-module(imem_adapter).

-include("dderl.hrl").

-export([ init/0
        , process_cmd/2
        ]).

init() ->
    dderl_dal:add_adapter(imem, "IMEM DB"),
    dderl_dal:add_connect(#ddConn{ id = erlang:phash2(make_ref())
                                 , name = "local imem"
                                 , adapter = imem
                                 , access = [{ip, "local"}, {user, "admin"}]
                                 }),
    dderl_dal:add_command(imem, "All Tables", "select name(qname) from all_tables", []),
    dderl_dal:add_command(imem, "All Views", "select name, owner, command from ddCmd where adapters = '[imem]' and (owner = user or owner = system)", []).

-record(priv, { sess
              , stmts
       }).

int(C) when $0 =< C, C =< $9 -> C - $0;
int(C) when $A =< C, C =< $F -> C - $A + 10;
int(C) when $a =< C, C =< $f -> C - $a + 10.

hexstr_to_list([]) -> [];
hexstr_to_list([X,Y|T]) -> [int(X)*16 + int(Y) | hexstr_to_list(T)].

process_cmd({"connect", BodyJson}, _) ->
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
    case erlimem_session:open(Type, Opts, {User, Password}) of
        {error, {Ex,M}} ->
            lager:error("DB connect error ~p", [{Ex,M}]),
            Err = atom_to_list(Ex) ++ ": " ++ element(1, M),
            {#priv{}, "{\"connect\":\""++Err++"\"}"};
        Session ->
            lager:debug("session ~p", [Session]),
            lager:debug("connected to params ~p", [{Type, Opts}]),
            Statements = [],
            Con = #ddConn { id       = erlang:phash2(make_ref())
                          , name     = binary_to_list(proplists:get_value(<<"name">>, BodyJson, <<>>))
                          , owner    = binary_to_list(User)
                          , adapter  = imem
                          , access   = [ {ip,        Ip}
                                       , {port,      Port}
                                       , {type,      Type}
                                       , {user,      User}
                                       ]
                          , schema   = list_to_atom(Schema)
                          },
            lager:debug([{user, User}], "saving new connection ~p", [Con]),
            dderl_dal:add_connect(Con),
            {#priv{sess=Session, stmts=Statements}, "{\"connect\":\"ok\"}"}
    end;
process_cmd({"query", BodyJson}, #priv{sess=Session} = Priv) ->
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    lager:debug([{session, Session}], "query ~p", [{Session, Query}]),
    process_query(Query, Priv);

process_cmd({"row_prev", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("[~p] statements ~p", [StmtKey, Statements]),
            {Priv, "{\"rows\":[]}"};
        {Statement, _, _} -> {Priv, gen_adapter:prepare_json_rows(prev, -1, Statement, StmtKey)}
    end;
process_cmd({"row_next", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowNum = proplists:get_value(<<"row_num">>, BodyJson, -1),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("[~p] statements ~p", [StmtKey, Statements]),
            {Priv, "{\"rows\":[]}"};
        {Statement, _, _} -> {Priv, gen_adapter:prepare_json_rows(next, RowNum, Statement, StmtKey)}
    end;
process_cmd({"stmt_close", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, "{\"rows\":[]}"};
        {Statement, _, _} ->
            lager:debug("[~p] remove statement ~p", [StmtKey, Statement]),
            Statement:close(),
            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
            {Priv#priv{stmts=NewStatements}, "{\"rows\":[]}"}
    end;
process_cmd({"get_buffer_max", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, "-1"};
        {Statement, _, _} ->
            {ok, Finished, CacheSize} = Statement:get_buffer_max(),
            lager:debug("[~p] get_buffer_max ~p finished ~p", [StmtKey, CacheSize, Finished]),
            {Priv, "{\"count\":"++integer_to_list(CacheSize)++", \"finished\":"++atom_to_list(Finished)++"}"}
    end;
process_cmd({"update_data", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    CellId = proplists:get_value(<<"cellid">>, BodyJson, <<>>),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, "{\"update_data\":\"invalid statement\"}"};
        {Statement, _, _} ->
            Result = format_return(Statement:update_row(RowId, CellId, Value)),
            {Priv, "{\"update_data\":"++Result++"}"}
    end;
process_cmd({"delete_row", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowId = proplists:get_value(<<"rowid">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, "{\"delete_row\":\"invalid statement\"}"};
        {Statement, _, _} ->
            Result = format_return(Statement:delete_row(RowId)),
            {Priv, "{\"delete_row\":"++Result++"}"}
    end;
process_cmd({"insert_data", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    ClmName = binary_to_list(proplists:get_value(<<"col">>, BodyJson, <<>>)),
    Value =  binary_to_list(proplists:get_value(<<"value">>, BodyJson, <<>>)),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. Statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, "{\"insert_data\":\"invalid statement\"}"};
        {Statement, _, _} ->
            Result = format_return(Statement:insert_row(ClmName, Value)),
            {Priv, "{\"insert_data\":"++Result++"}"}
    end;
process_cmd({"commit_rows", BodyJson}, #priv{stmts=Statements} = Priv) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            lager:debug("statement ~p not found. statements ~p", [StmtKey, proplists:get_keys(Statements)]),
            {Priv, "{\"commit_rows\":\"invalid statement\"}"};
        {Statement, _, _} ->
            Result = format_return(Statement:commit_modified()),
            {Priv, "{\"commit_rows\":"++Result++"}"}
    end;

process_cmd({"views", _}, Priv) ->
    [F|_] = [C || C <- dderl_dal:get_commands(system, imem), C#ddCmd.name == "All Views"],
%% -     Files = "{\"name\":"++jsq(F#ddCmd.name)
%% -          ++", \"id\":"++jsq(F#ddCmd.id)
%% -          ++", \"content\":"++jsq(F#ddCmd.command)
%% -          ++", \"posX\":0"
%% -          ++", \"posY\":25"
%% -          ++", \"width\":200"
%% -          ++", \"height\":500"
%% - %% -         ++", \"posX\":"++integer_to_list(F#ddCmd.posX)
%% - %% -         ++", \"posY\":"++integer_to_list(F#ddCmd.posY)
%% - %% -         ++", \"width\":"++integer_to_list(F#ddCmd.width)
%% - %% -         ++", \"height\":"++integer_to_list(F#ddCmd.height)
%% -          ++"}",
    {Priv, "{\"views\":{\"content\":\""++F#ddCmd.command++"\", \"name\":\"All Views\"}}"};
process_cmd({"get_query", BodyJson}, Priv) -> gen_adapter:process_cmd({"get_query", BodyJson}, Priv);
process_cmd({"parse_stmt", BodyJson}, Priv) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, Priv);
process_cmd({Cmd, BodyJson}, Priv) ->
    lager:error("unsupported command ~p content ~p", [Cmd, BodyJson]),
    {Priv, "{\"rows\":[]}"}.

process_query(Query, #priv{sess=Session, stmts=Statements} = Priv) ->
    case Session:exec(Query, ?DEFAULT_ROW_SIZE) of
        {ok, Clms, Statement} ->
            StmtHndl = erlang:phash2(Statement),
            Columns = [atom_to_list(C#ddColMap.name)||C<-Clms],
            lager:debug([{session, Session}], "columns ~p", [Columns]),
            Statement:start_async_read(),
            Resp = "{\"headers\":"++gen_adapter:string_list_to_json(Columns)++
            ",\"statement\":"++integer_to_list(StmtHndl)++"}",
            {Priv#priv{stmts=[{StmtHndl, {Statement, Query, []}}|Statements]}, Resp};
        {error, {Ex,M}} ->
            lager:error([{session, Session}], "query error ~p", [{Ex,M}]),
            Err = atom_to_list(Ex) ++ ": " ++ element(1, M),
            Resp = "{\"headers\":[],\"statement\":0,\"error\":\""++Err++"\"}",
            {Priv, Resp};
        Res ->
            lager:debug("qry ~p~nResult ~p", [Query, Res]),
            {Priv, "{\"headers\":[],\"statement\":1234}"}
    end.

format_return({error, {E,{R,_}}}) ->  "\""++atom_to_list(E)++": "++R++"\"";
format_return(Result) -> lists:flatten(io_lib:format("~p", [Result])).
