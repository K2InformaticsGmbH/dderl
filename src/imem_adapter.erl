-module(imem_adapter).

-include("dderl.hrl").

-export([ process_cmd/3
        , init/0
%        , iprocess/2
        ]).

init() ->
    erlimem:start(),
    % TODO - modify to adapt to new structure
    imem_if:insert_into_table(common, {?MODULE, [
                #file{name="Nodes.sql",
                      content="imem_nodes",
                      posX=0, posY=25, width=200, height=500}
              , #file{name="Tables.sql",
                      content="tables",
                      posX=0, posY=25, width=200, height=500}
              , #file{name="Views.sql",
                      content="views",
                      posX=0, posY=25, width=200, height=500}
            ]}).


process_cmd({"connect", BodyJson}, SrvPid, _) ->
    IpAddr   = inet:getaddr(binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)), inet),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    Type     = tcp,
    Schema   = "Mnesia",
    Session  = erlimem_session:open(Type, {IpAddr, Port, Schema}),
    dderl_session:log(SrvPid, "Connected to Params ~p~n", [{IpAddr, Port}]),
    SeCo = {},
    Statements = [],
    {{Session, SeCo, Statements}, "{\"connect\":\"ok\"}"};
process_cmd({"query", BodyJson}, SrvPid, {Session,SeCo,Statements}) ->
    Query = binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>)),
    ParseTree = [],
    dderl_session:log(SrvPid, "[~p] Query ~p~n", [SrvPid, {Session, Query}]),
    case Session:exec(SeCo, Query, ?DEFAULT_ROW_SIZE) of
        {ok, Clms, Statement} ->
            StmtHndl = erlang:phash2(Statement),
            Columns = [tuple_to_list(C)||C<-Clms],
            Resp = "{\"headers\":"++dderl_session:string_list_to_json(Columns, [])++
            ",\"statement\":"++integer_to_list(StmtHndl)++"}",
            {{Session,SeCo,[{StmtHndl, {Statement, Query, ParseTree}}|Statements]}, Resp};
        {error, Error} ->
            dderl_session:log(SrvPid, "[~p] Query Error ~p~n", [SrvPid, Error]),
            Resp = "{\"headers\":[],\"statement\":0,\"error\":\""++Error++"\"}",
            {{Session,SeCo,Statements}, Resp}
    end;
process_cmd({"row_prev", BodyJson}, SrvPid, {_,_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p, ~p] Statements ~p~n", [SrvPid, StmtKey, Statements]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} -> {MPort, gen_adapter:prepare_json_rows(prev, -1, Statement, StmtKey, SrvPid)}
    end;
process_cmd({"row_next", BodyJson}, SrvPid, {_,_,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    RowNum = proplists:get_value(<<"row_num">>, BodyJson, -1),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p, ~p] Statements ~p~n", [SrvPid, StmtKey, Statements]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} -> {MPort, gen_adapter:prepare_json_rows(next, RowNum, Statement, StmtKey, SrvPid)}
    end;
process_cmd({"stmt_close", BodyJson}, SrvPid, {Session,Pool,Statements} = MPort) ->
    StmtKey = proplists:get_value(<<"statement">>, BodyJson, <<>>),
    case proplists:get_value(StmtKey, Statements) of
        undefined ->
            dderl_session:log(SrvPid, "[~p] Statement ~p not found. Statements ~p~n", [SrvPid, StmtKey, proplists:get_keys(Statements)]),
            {MPort, "{\"rows\":[]}"};
        {Statement, _, _} ->
            dderl_session:log(SrvPid, "[~p, ~p] Remove statement ~p~n", [SrvPid, StmtKey, Statement]),
            Statement:close(),
            {_,NewStatements} = proplists:split(Statements, [StmtKey]),
            {{Session,Pool,NewStatements}, "{\"rows\":[]}"}
    end;
process_cmd({"get_buffer_max", BodyJson}, SrvPid, {_,_,Statements} = MPort) ->
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
process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort) -> gen_adapter:process_cmd({"parse_stmt", BodyJson}, SrvPid, MPort);
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.
%% - process_cmd({"users", _BodyJson}, SrvPid, MPort) ->
%% -     dderl_session:log(SrvPid, "users ~p~n", [MPort]),
%% -     iprocess({users},MPort),
%% -     {MPort, "{\"rows\":[]}"};
%% - process_cmd({"tables", _BodyJson}, SrvPid, MPort) ->
%% -     Tables = iprocess({tables},MPort),
%% -     Tabs = dderl_session:string_list_to_json([atom_to_list(X) || X <- Tables], []),
%% -     dderl_session:log(SrvPid, "tables ~p~n", [Tabs]),
%% -     {MPort, "{\"rows\":"++Tabs++"}"};
%% - process_cmd({"views", _BodyJson}, SrvPid, MPort) ->
%% -     dderl_session:log(SrvPid, "views~n", []),
%% -     iprocess({views},MPort),
%% -     {MPort, "{\"rows\":[]}"};
%% - process_cmd({"query", BodyJson}, SrvPid, MPort) ->
%% -     Table = list_to_atom(binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>))),
%% -     dderl_session:log(SrvPid, "query table ~p~n", [Table]),
%% -     Columns = iprocess({columns, Table}, MPort),
%% -     Cols = dderl_session:string_list_to_json([atom_to_list(X) || X <- Columns], []),
%% -     dderl_session:log(SrvPid, "table ~p columns ~p~n", [Table, Cols]),
%% -     {MPort, "{\"table\":\""++atom_to_list(Table)++"\",\"headers\":"++Cols++",\"statement\":\""++atom_to_list(Table)++"\"}"};
%% - process_cmd({"row", BodyJson}, SrvPid, MPort) ->
%% -     Table = list_to_atom(binary_to_list(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
%% -     RowsTerm = iprocess({row, Table}, MPort),
%% -     Rows = dderl_session:convert_rows_to_string(RowsTerm),
%% -     Rs = dderl_session:convert_rows_to_json(Rows),
%% -     dderl_session:log(SrvPid, "table ~p rows ~p~n~p~n~p~n", [Table, RowsTerm, Rows, Rs]),
%% -     {MPort, "{\"rows\":"++Rs++"}"};
%% - process_cmd({"create_table", BodyJson}, SrvPid, MPort) ->
%% -     Table = list_to_atom(binary_to_list(proplists:get_value(<<"table_name">>, BodyJson, <<>>))),
%% -     Columns = binary_to_list(proplists:get_value(<<"table_cols">>, BodyJson, <<>>)),
%% -     dderl_session:log(SrvPid, "create table ~p cols ~p~n", [Table, Columns]),
%% -     %iprocess({build_table, Table, Columns}, MPort),
%% -     {MPort, "{\"create_table\":\"ok\"}"};
%% - process_cmd({"imem_nodes", _BodyJson}, SrvPid, MPort) ->
%% -     Nodes = iprocess({imem_nodes}, MPort),
%% -     Row = [lists:flatten(io_lib:format("\"~p\"", [N])) || N <- Nodes],
%% -     Rs = "[" ++ string:join(Row, ",") ++ "]",
%% -     dderl_session:log(SrvPid, "imem_nodes ~p~n", [Rs]),
%% -     {MPort, "{\"nodes\":"++Rs++"}"};

%% - iprocess({connect,IpAddr,Port}, _) ->
%% -     {ok, Ip} = inet:getaddr(IpAddr, inet),
%% -     {ok, MPort} = gen_tcp:connect(Ip, Port, []),
%% -     inet:setopts(MPort, [{active, false}, binary, {packet, 0}, {nodelay, true}]),
%% -     MPort;
%% - iprocess({imem_nodes}, MPort) ->
%% -     gen_tcp:send(MPort, term_to_binary({imem_nodes})),
%% -     ImemNodes = recv_term(MPort, <<>>),
%% -     ImemNodes;
%% - iprocess({tables}, MPort) ->
%% -     gen_tcp:send(MPort, term_to_binary({tables})),
%% -     Tables = recv_term(MPort, <<>>),
%% -     Tables;
%% - iprocess({columns, Table}, MPort) ->
%% -     gen_tcp:send(MPort, term_to_binary({table, Table})),
%% -     recv_term(MPort, <<>>);
%% - iprocess({row, Table}, MPort) ->
%% -     gen_tcp:send(MPort, term_to_binary({row, Table})),
%% -     recv_term(MPort, <<>>);
%% - iprocess({build_table, TableName, Columns}, MPort) ->
%% -     gen_tcp:send(MPort, term_to_binary({build_table, TableName, Columns})),
%% -     recv_term(MPort, <<>>);
%% - iprocess(Unknown, _MPort) ->
%% -     io:format(user, "command unimplimented ~p~n", [Unknown]).

%% - recv_term(Sock, Bin) ->
%% -     {ok, Pkt} = gen_tcp:recv(Sock, 0),
%% -     NewBin = << Bin/binary, Pkt/binary >>,
%% -     case (catch binary_to_term(NewBin)) of
%% -         {'EXIT', Reason} ->
%% -             io:format(user, "term incomplete (~p), received ~p bytes~n", [Reason, byte_size(Pkt)]),
%% -             recv_term(Sock, NewBin);
%% -         Data -> Data
%% -     end.
