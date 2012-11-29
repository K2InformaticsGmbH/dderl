-module(imem_adapter).

-include("dderl.hrl").

-export([ init/0
        , process_cmd/3
        ]).

init() ->    
    dderl_dal:add_adapter(imem, "IMEM DB"),
    dderl_dal:add_connect(#ddConn{ id = erlang:phash2(make_ref())
                                 , name = "local imem"
                                 , adapter = imem
                                 , access = [{ip, "local"}, {user, "admin"}]
                                 }),
    dderl_dal:add_command(imem, "All Tables", "select qname from all_tables", [{rowfun, fun([I,{_,F}|R]) ->
                                                                                            [integer_to_list(I),atom_to_list(F)|R] end
                                                                              }]).

process_cmd({"connect", BodyJson}, SrvPid, _) ->
    Schema = binary_to_list(proplists:get_value(<<"service">>, BodyJson, <<>>)),
    Port   = binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>)),
    case binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)) of
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
            Opts    = {inet:getaddr(Ip, inet), list_to_integer(Port), Schema}
    end,
    User = proplists:get_value(<<"user">>, BodyJson, <<>>),
    Password = proplists:get_value(<<"password">>, BodyJson, <<>>),
    Session = erlimem_session:open(Type, Opts, {User, Password}),
    io:format(user, "Session ~p~n", [Session]),
    dderl_session:log(SrvPid, "Connected to Params ~p~n", [{Type, Opts}]),
    Statements = [],
    {{Session, Statements}, "{\"connect\":\"ok\"}"};
process_cmd({"query", BodyJson}, SrvPid, {Session,Statements}) ->
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    Id = proplists:get_value(<<"id">>, BodyJson, <<>>),
    Cmd = dderl_dal:get_command(Id),
    Fun = case lists:keyfind(rowfun,1,Cmd#ddCmd.opts) of
        {_, F} when is_function(F) -> F;
        _ -> undefined
    end,
    ParseTree = [],
    io:format(user, "query ~p~n", [Query]),
    dderl_session:log(SrvPid, "[~p] Query ~p~n", [SrvPid, {Session, Query}]),
    case Session:exec(Query, ?DEFAULT_ROW_SIZE, Fun) of
        {ok, Clms, Statement} ->
            StmtHndl = erlang:phash2(Statement),
            io:format(user, "Clms ~p~n", [Clms]),
            Columns = [atom_to_list(C#ddColMap.name)||C<-Clms],
            Statement:start_async_read(),
            Resp = "{\"headers\":"++gen_adapter:string_list_to_json(Columns, [])++
            ",\"statement\":"++integer_to_list(StmtHndl)++"}",
            {{Session,[{StmtHndl, {Statement, Query, ParseTree}}|Statements]}, Resp};
        {error, Error} ->
            io:format(user, "query error ~p~n", [Error]),
            dderl_session:log(SrvPid, "[~p] Query Error ~p~n", [SrvPid, Error]),
            Resp = "{\"headers\":[],\"statement\":0,\"error\":\""++Error++"\"}",
            {{Session,Statements}, Resp}
    end;
process_cmd({"row_prev", BodyJson}, SrvPid, {_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p, ~p] Statements ~p~n", [SrvPid, StmtKey, Statements]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} -> {MPort, gen_adapter:prepare_json_rows(prev, -1, Statement, StmtKey, SrvPid)}
    end;
process_cmd({"row_next", BodyJson}, SrvPid, {_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowNum = proplists:get_value(<<"row_num">>, BodyJson, -1),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p, ~p] Statements ~p~n", [SrvPid, StmtKey, Statements]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} -> {MPort, gen_adapter:prepare_json_rows(next, RowNum, Statement, StmtKey, SrvPid)}
    end;
process_cmd({"stmt_close", BodyJson}, SrvPid, {Session,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p] Statement ~p not found. Statements ~p~n", [SrvPid, StmtKey, proplists:get_keys(Statements)]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} ->
            dderl_session:log(SrvPid, "[~p, ~p] Remove statement ~p~n", [SrvPid, StmtKey, Statement]),
            Statement:close(),
            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
            {{Session,NewStatements}, "{\"rows\":[]}"}
    end;
process_cmd({"get_buffer_max", BodyJson}, SrvPid, {_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p] Statement ~p not found. Statements ~p~n", [SrvPid, StmtKey, proplists:get_keys(Statements)]),
            {MPort, "-1"};
        {Statement, _, _} ->
            {ok, Finished, CacheSize} = Statement:get_buffer_max(),
            dderl_session:log(SrvPid, "[~p, ~p] get_buffer_max ~p Fînished ~p~n", [SrvPid, StmtKey, CacheSize, Finished]),
            {MPort, "{\"count\":"++integer_to_list(CacheSize)++", \"finished\":"++atom_to_list(Finished)++"}"}
    end;
process_cmd({"get_query", BodyJson}, SrvPid, MPort) -> gen_adapter:process_cmd({"get_query", BodyJson}, SrvPid, MPort);
process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort);
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.
