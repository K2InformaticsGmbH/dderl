-module(imem_adapter).

-export([ process_cmd/3
        , iprocess/2
        ]).

process_cmd({"connect", BodyJson}, _SrvPid, _) ->
    IpAddr   = binary_to_list(proplists:get_value(<<"ip">>, BodyJson, <<>>)),
    Port     = list_to_integer(binary_to_list(proplists:get_value(<<"port">>, BodyJson, <<>>))),
    %dderl_session:log(Srv, "Params ~p~n", [{Ip, Port}]),
    MPort = iprocess({connect,IpAddr,Port},[]),
    {MPort, "{\"connect\":\"ok\"}"};
process_cmd({"users", _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "users ~p~n", [MPort]),
    iprocess({users},MPort),
    {MPort, "{\"rows\":[]}"};
process_cmd({"tables", _BodyJson}, _SrvPid, MPort) ->
    Tables = iprocess({tables},MPort),
    Tabs = dderl_session:string_list_to_json([atom_to_list(X) || X <- Tables], []),
    io:format(user, "tables ~p~n", [Tabs]),
    {MPort, "{\"rows\":"++Tabs++"}"};
process_cmd({"views", _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "views~n", []),
    iprocess({views},MPort),
    {MPort, "{\"rows\":[]}"};
process_cmd({"query", BodyJson}, _SrvPid, MPort) ->
    Table = list_to_atom(binary_to_list(proplists:get_value(<<"table">>, BodyJson, <<>>))),
    io:format(user, "query table ~p~n", [Table]),
    Columns = iprocess({columns, Table}, MPort),
    Cols = dderl_session:string_list_to_json([atom_to_list(X) || X <- Columns], []),
    io:format(user, "table ~p columns ~p~n", [Table, Cols]),
    {MPort, "{\"table\":\""++atom_to_list(Table)++"\",\"headers\":"++Cols++",\"statement\":\""++atom_to_list(Table)++"\"}"};
process_cmd({"row", BodyJson}, _SrvPid, MPort) ->
    Table = list_to_atom(binary_to_list(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    RowsTerm = iprocess({row, Table}, MPort),
    Rows = dderl_session:convert_rows_to_string(RowsTerm),
    Rs = dderl_session:convert_rows_to_json(Rows),
    io:format(user, "table ~p rows ~p~n", [Table, Rs]),
    {MPort, "{\"rows\":"++Rs++"}"};
process_cmd({"create_table", BodyJson}, _SrvPid, MPort) ->
    Table = list_to_atom(binary_to_list(proplists:get_value(<<"table_name">>, BodyJson, <<>>))),
    Columns = binary_to_list(proplists:get_value(<<"table_cols">>, BodyJson, <<>>)),
    io:format(user, "create table ~p cols ~p~n", [Table, Columns]),
    %iprocess({build_table, Table, Columns}, MPort),
    {MPort, "{\"create_table\":\"ok\"}"};
process_cmd({Cmd, _BodyJson}, _SrvPid, MPort) ->
    io:format(user, "Cmd ~p~n", [Cmd]),
    {MPort, "{\"rows\":[]}"}.

iprocess({connect,IpAddr,Port}, _) ->
    {ok, Ip} = inet:getaddr(IpAddr, inet),
    {ok, MPort} = gen_tcp:connect(Ip, Port, []),
    inet:setopts(MPort, [{active, false}, binary, {packet, 0}, {nodelay, true}]),
    MPort;
iprocess({tables}, MPort) ->
    gen_tcp:send(MPort, term_to_binary({tables})),
    Tables = recv_term(MPort, <<>>),
    Tables;
iprocess({columns, Table}, MPort) ->
    gen_tcp:send(MPort, term_to_binary({table, Table})),
    recv_term(MPort, <<>>);
iprocess({row, Table}, MPort) ->
    gen_tcp:send(MPort, term_to_binary({row, Table})),
    recv_term(MPort, <<>>);
iprocess({build_table, TableName, Columns}, MPort) ->
    gen_tcp:send(MPort, term_to_binary({build_table, TableName, Columns})),
    recv_term(MPort, <<>>);
iprocess(Unknown, _MPort) ->
    io:format(user, "command unimplimented ~p~n", [Unknown]).

recv_term(Sock, Bin) ->
    {ok, Pkt} = gen_tcp:recv(Sock, 0),
    NewBin = << Bin/binary, Pkt/binary >>,
    case (catch binary_to_term(NewBin)) of
        {'EXIT', Reason} ->
            io:format(user, "term incomplete (~p), received ~p bytes~n", [Reason, byte_size(Pkt)]),
            recv_term(Sock, NewBin);
        Data -> Data
    end.
