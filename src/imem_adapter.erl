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
    io:format(user, "tables 1~n", []),
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
    io:format(user, "table ~p rows ~p~n", [Table, RowsTerm]),
    Rows = dderl_session:convert_rows_to_string(RowsTerm),
    io:format(user, "table ~p rows ~p~n", [Table, Rows]),
    Rs = dderl_session:convert_rows_to_json(Rows, []),
    {MPort, "{\"rows\":["++string:substr(Rs,1,length(Rs)-1)++"]}"};
    %{MPort, "{\"rows\":[]}"};
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
    io:format(user, "tables 2~n", []),
    Tables = recv_term(MPort, <<>>),
    io:format(user, "tables 3~n", []),
    Tables;
iprocess({columns, Table}, MPort) ->
    gen_tcp:send(MPort, term_to_binary({table, Table})),
    recv_term(MPort, <<>>);
iprocess({row, Table}, MPort) ->
    gen_tcp:send(MPort, term_to_binary({row, Table})),
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
