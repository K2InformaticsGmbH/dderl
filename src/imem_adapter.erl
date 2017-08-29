-module(imem_adapter).
-author('Bikram Chatterjee <bikram.chatterjee@k2informatics.ch>').

-include("dderl.hrl").
-include("gres.hrl").

-include_lib("imem/include/imem_sql.hrl").

-export([ init/0
        , process_cmd/6
        , disconnect/1
        , rows/2
        , get_deps/0
        , process_query/4
        , bind_arg_types/0
        , add_conn_info/2
        , connect_map/1
        ]).

-record(priv, {connections = [], conn_info}).

-define(E2B(__T), gen_adapter:encrypt_to_binary(__T)).
-define(D2T(__B), gen_adapter:decrypt_to_term(__B)).

bind_arg_types() -> imem_datatype:bind_arg_types().

-spec init() -> ok.
init() ->
    dderl_dal:add_adapter(imem, <<"IMEM DB">>),
    SystemViews = [
        { <<"Remote Tables">>
        , <<"select
                to_name(qname),
                size as rows,
                memory,
                nodef(expiry) as expires,
                nodef(tte) as expires_after
            from
                all_tables,
                ddSize
            where
                name = element(2, qname) and size <> to_atom('undefined')
            order by 1 asc">>
        , []},
        { <<"All ddViews">>
        , <<"select
                c.owner,
                v.name
            from
                ddView as v,
                ddCmd as c
            where
                c.id = v.cmd
                and (c.conns = to_list('[]') or is_member(:ddConn.id, c.conns))
                and (c.owner = user or c.owner = to_atom('system'))
                and (is_member(:ddAdapter.id, c.adapters))
            order by
                2 asc,
                1 asc">>
        , local}
    ],
    %% TODO: This should be added on the load of the other adapter but we don't know the id...
    AddViewResult = gen_adapter:add_cmds_views(undefined, system, imem, false, SystemViews),
    case lists:member(need_replace, AddViewResult) of
        true ->
            View = dderl_dal:get_view(undefined, <<"All ddViews">>, imem, system),
            dderl_dal:add_adapter_to_cmd(undefined, View#ddView.cmd, oci);
        _ ->
            [_, ViewId] = AddViewResult,
            View = dderl_dal:get_view(undefined, ViewId),
            dderl_dal:add_adapter_to_cmd(undefined, View#ddView.cmd, oci)
    end.

-spec add_conn_info(undefined | #priv{}, map()) -> #priv{}.
add_conn_info(undefined, ConnInfo) ->
    add_conn_info(#priv{connections = []}, ConnInfo);
add_conn_info(#priv{} = Priv, ConnInfo) when is_map(ConnInfo) ->
    Priv#priv{conn_info = ConnInfo}.

-spec connect_map(#ddConn{}) -> map().
connect_map(#ddConn{adapter = imem} = C) ->
    add_conn_extra(C, #{id => C#ddConn.id,
                        name => C#ddConn.name,
                        adapter => <<"imem">>,
                        owner => dderl_dal:user_name(C#ddConn.owner),
                        schema => atom_to_binary(C#ddConn.schm, utf8)}).

add_conn_extra(#ddConn{access = Access}, Conn)
  when is_map(Access), is_map(Conn) ->
        maps:merge(Conn, maps:remove(owner,maps:remove(<<"owner">>,Access)));
add_conn_extra(#ddConn{access = Access}, Conn) when is_list(Access), is_map(Conn) ->
    case proplists:get_value(type, Access, proplists:get_value(<<"method">>, Access, tcp)) of
        Local when Local == local; Local == <<"local">> ->
            Conn#{method => <<"local">>};
        LocalSec when LocalSec == local_sec; LocalSec == <<"local_sec">> ->
            Conn#{method => <<"local_sec">>,
                           user => proplists:get_value(user, Access, <<>>)};
        Rpc when Rpc == rpc; Rpc == <<"rpc">> ->
            Conn#{method => <<"rpc">>,
                  node => proplists:get_value(node, Access, <<>>),
                  user => proplists:get_value(user, Access, <<>>)};
        Tcp when Tcp == tcp; Tcp == <<"tcp">> ->
            Conn#{method => <<"tcp">>,
                  host => proplists:get_value(ip, Access, <<>>),
                  port => proplists:get_value(port, Access, <<>>),
                  user => proplists:get_value(user, Access, <<>>),
                  secure => proplists:get_value(secure, Access, false)}
    end.

-spec conn_method([{binary(),binary()}]|binary()) -> tcp | rpc | local.
conn_method(<<"rpc">>)          -> rpc;
conn_method(<<"local">>)        -> local;
conn_method(Other)
  when Other == <<"tcp">>;
       Other == '$not_defined'  -> tcp;
conn_method([{K,V}|_] = PropList) when is_binary(K), is_binary(V) ->
    conn_method(proplists:get_value(<<"method">>, PropList, '$not_defined')).

-spec connect_erlimem(tcp | rpc | local | local_sec, {atom(), pid()},
                      binary(), list({binary(),binary()}), map()) ->
    {ok, {atom(),pid()}, any()}.
connect_erlimem(tcp, _Sess, SessionId, Params, ConnInfo) ->
    Opts = case proplists:get_value(<<"secure">>, Params, false) of
               false -> [];
               true -> [ssl]
           end,
    IpAddr = binary_to_list(proplists:get_value(<<"host">>, Params)),
    Port = binary_to_integer(proplists:get_value(<<"port">>, Params)),
    Schema = try binary_to_existing_atom(
                 proplists:get_value(<<"schema">>, Params, '$bad_schema'),
                 utf8) of
               AtomSchema -> AtomSchema
           catch _:_ -> error("Invalid schema for tcp")
           end,
    connect_erlimem_password({tcp, IpAddr, Port, Opts}, Schema, SessionId, ConnInfo, Params);
connect_erlimem(rpc, _Sess, SessionId, Params, ConnInfo) ->
    Node = try binary_to_existing_atom(
                 proplists:get_value(<<"node">>, Params, '$bad_node'),
                 utf8) of
               AtomPort -> AtomPort
           catch _:_ -> error("Invalid node for rpc")
           end,
    Schema = try binary_to_existing_atom(
                 proplists:get_value(<<"schema">>, Params, '$bad_schema'),
                 utf8) of
               AtomSchema -> AtomSchema
           catch _:_ -> error("Invalid schema for rpc")
           end,
    connect_erlimem_password({rpc, Node}, Schema, SessionId, ConnInfo, Params);
connect_erlimem(local, Sess, SessionId, Params, ConnInfo) ->
    case proplists:get_value(<<"secure">>, Params, false) of
        true ->
            connect_erlimem(local_sec, Sess, SessionId, Params, ConnInfo);
        false ->
            case dderl_dal:can_connect_locally(Sess) of
                true ->
                    {ok, ErlImemSess} = erlimem:open(local, imem_meta:schema()),
                    {ok, ErlImemSess, #{node => list_to_binary(imem_meta:node_shard())}};
                _ -> error(<<"Local connection unauthorized">>)
            end
    end;
connect_erlimem(local_sec, ErlImemSess, _SessionId, _Params, _ConnInfo) ->
    {ok, ErlImemSess, #{node => list_to_binary(imem_meta:node_shard())}}.

connect_erlimem_password(Connect, Schema, SessionId, ConnInfo, Params) ->
    {ok, ErlImemSess}
    = case erlimem:open(Connect, Schema) of
          {ok, _} = E -> E;
          Error ->
              error(list_to_binary(io_lib:format("~p", [Error])))
      end,
    case ErlImemSess:auth(dderl,SessionId,{access,ConnInfo#{type => internal}}) of
        {ok, [{pwdmd5,_}|_]} ->
            User = proplists:get_value(<<"user">>, Params, <<>>),
            Password = proplists:get_value(<<"password">>, Params, []),
            case ErlImemSess:auth(dderl,SessionId,{pwdmd5,{User,list_to_binary(Password)}}) of
                Ok when Ok == {ok,[]}; Ok == ok ->
                    case ErlImemSess:run_cmd(login,[]) of
                        {error,{{'SecurityException',{?PasswordChangeNeeded,_}},ST}} ->
                            ?Warn("Password expired ~s~n~p", [User, ST]),
                            {ok, ErlImemSess, #{changePass=>User}};
                        _ ->
                            {ok, ErlImemSess, '$no_extra'}
                    end;
                {ok, [{smsott,Data}|_]} ->
                    {ok, ErlImemSess, maps:remove(accountName,Data)}
            end
    end.

-spec process_cmd({[binary()], term()}, {atom(), pid()}, ddEntityId(), pid(), #priv{}, pid()) -> #priv{}.
process_cmd({[<<"connect">>], BodyJson, SessionId}, Sess, UserId, From,
            #priv{connections = Connections, conn_info = ConnInfo} = Priv, _SessPid) ->
    {value, {<<"id">>, Id}, BodyJson1} = lists:keytake(<<"id">>, 1, BodyJson),
    {value, {<<"name">>, Name}, BodyJson2} = lists:keytake(<<"name">>, 1, BodyJson1),
    {value, {<<"schema">>, Schema}, BodyJson3} = lists:keytake(<<"schema">>, 1, BodyJson2),
    {value, {<<"adapter">>, <<"imem">>}, BodyJson4} = lists:keytake(<<"adapter">>, 1, BodyJson3),
    BodyJson5 = case lists:keytake(<<"password">>, 1, BodyJson4) of
                    {value, {<<"password">>, _}, BJ} -> BJ;
                    false -> BodyJson4
                end,
    {value, {<<"owner">>, _Owner}, BodyJson6} = lists:keytake(<<"owner">>, 1, BodyJson5),
    SchemaAtom = binary_to_existing_atom(Schema, utf8),
    case catch connect_erlimem(conn_method(BodyJson), Sess, SessionId, BodyJson, ConnInfo) of
        {ok, ErlImemSess, Extra} ->
            %% Id undefined if we are creating a new connection.
            case dderl_dal:add_connect(Sess, #ddConn{adapter = imem, id = Id, name = Name,
                          owner = UserId, schm = SchemaAtom,
                          access = jsx:decode(jsx:encode(BodyJson6), [return_maps])}) of
                {error, Msg} ->
                    ErlImemSess:close(),
                    From ! {reply, jsx:encode(#{connect=>#{error=>Msg}})};
                #ddConn{owner = Owner} = NewConn ->
                    ConnReply = #{conn_id=>NewConn#ddConn.id,
                                  owner=>Owner, conn=>?E2B(ErlImemSess)},
                    From ! {reply,
                            jsx:encode(
                              #{connect =>
                                if '$no_extra' == Extra -> ConnReply;
                                   true -> maps:put(extra, Extra, ConnReply)
                                end}
                             )}
            end,
            Priv#priv{connections = [ErlImemSess|Connections]};
        {{E,M},ST} when is_list(M); is_binary(M) ->
            ?Error("~p:~s~n~p", [E,M,ST]),
            From ! {reply, jsx:encode(#{connect=>
                                        #{error=>
                                          list_to_binary(
                                            io_lib:format(
                                              "~p:~s", [E,M]))}
                                       })},
            Priv;
        {'EXIT', {M, ST}} when is_binary(M) ->
            ?Error("~s~n~p", [M,ST]),
            From ! {reply, jsx:encode(#{connect=>#{error=>M}})},
            Priv;
        {'EXIT', Error} ->
            From ! {reply, jsx:encode(#{connect=>
                                        #{error=>
                                          list_to_binary(
                                            io_lib:format(
                                              "~p", [Error]))}
                                       })},
            Priv
    end;
process_cmd({[<<"smstoken">>], BodyJson}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    ErlImemSess = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(ErlImemSess, Connections) of
        true ->
            Token = proplists:get_value(<<"smstoken">>, BodyJson, <<>>),
            case ErlImemSess:auth(dderl,<<>>,{smsott,Token}) of
                Ok when Ok == {ok,[]}; Ok == ok ->
                    case ErlImemSess:run_cmd(login,[]) of
                        {error,{{'SecurityException',{?PasswordChangeNeeded,_}},ST}} ->
                            User = proplists:get_value(<<"user">>, BodyJson, <<>>),
                            ?Warn("Password expired ~s~n~p", [User, ST]),
                            From ! {reply, jsx:encode(#{smstoken=>#{changePass=>User}})};
                        _ ->
                            From ! {reply, jsx:encode(#{smstoken=>ok})}
                    end,
                    Priv;
                Unexpected ->
                    ?Error("Not expected ~p", [Unexpected]),
                    From ! {reply, jsx:encode(#{smstoken=>#{error=>"More steps"}})}
            end,
            Priv;
        false ->
            From ! {reply, jsx:encode([{<<"error">>, <<"Connection not found">>}])},
            Priv
    end;
process_cmd({[<<"change_conn_pswd">>], BodyJson}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    ErlImemSess = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    OldPassword = list_to_binary(proplists:get_value(<<"password">>, BodyJson, [])),
    NewPassword = proplists:get_value(<<"new_password">>, BodyJson, []),
    case lists:member(ErlImemSess, Connections) of
        true ->
            case (imem_seco:password_strength_fun())(NewPassword) of
                strong ->
                    case ErlImemSess:run_cmd(
                           change_credentials,
                           [{pwdmd5, OldPassword}, {pwdmd5, erlang:md5(NewPassword)}]
                          ) of
                        SeKey when is_integer(SeKey) ->
                            ?Debug("change password successful"),
                            From ! {reply, jsx:encode(#{change_conn_pswd=><<"ok">>})};
                        {error, {error, {E, M}}} ->
                            ?Error("change password failed result ~n~p", [{E, M}]),
                            From ! {reply, jsx:encode(#{change_conn_pswd=>
                                                        #{error=>
                                                          list_to_binary(io_lib:format("~p: ~p", [E,M]))
                                                         }})};
                        {error, {{E, M}, ST}} ->
                            ?Error("change password failed for result ~p~n~p", [{E, M}, ST]),
                            From ! {reply, jsx:encode(#{change_conn_pswd=>
                                                        #{error=>
                                                          list_to_binary(io_lib:format("~p: ~p", [E,M]))
                                                         }})}
                    end,
                    Priv;
                _ -> From ! {reply, jsx:encode(#{change_conn_pswd=>#{error => <<"Password is not strong">>}})}
            end;
        false ->
            From ! {reply, jsx:encode(#{change_conn_pswd=>
                                        #{error=><<"Connection not found">>}})},
            Priv
    end;
process_cmd({[<<"disconnect">>], ReqBody, _SessionId}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"disconnect">>, BodyJson}] = ReqBody,
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            Connection:close(),
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
    Query = proplists:get_value(<<"qstr">>, BodyJson, <<>>),
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>), %% This should be change to params...
    ?Debug("query ~p", [Query]),
    case lists:member(Connection, Connections) of
        true ->
            Params = make_binds(proplists:get_value(<<"binds">>, BodyJson, null)),
            R = case dderl_dal:is_local_query(Query) of
                    true -> process_query(Query, Sess, {ConnId, imem}, Params, SessPid);
                    _ -> process_query(Query, Connection, {ConnId, imem}, Params, SessPid)
                end,
            From ! {reply, jsx:encode([{<<"query">>, [{<<"qstr">>, Query} | R]}])};
        false ->
            From ! {reply, error_invalid_conn(Connection, Connections)}
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
    ?Debug("Row with key ~p",[R]),
    Tables = [element(1,T) || T <- tuple_to_list(element(3, R)), size(T) > 0],
    IsView = lists:any(fun(E) -> E =:= ddCmd end, Tables) andalso
        case element(3, R) of
            {_,#ddView{},#ddCmd{}} -> true;
            _ -> false
        end,
    ?Debug("browse_data (view ~p) ~p - ~p", [IsView, Tables, {R, Col}]),
    if
        IsView ->
            {_,#ddView{name=Name,owner=Owner},#ddCmd{}=OldC} = element(3, R),
            V = dderl_dal:get_view(Sess, Name, imem, Owner),
            C = dderl_dal:get_command(Sess, OldC#ddCmd.id),
            ?Debug("Cmd ~p Name ~p", [C#ddCmd.command, Name]),
            case C#ddCmd.conns of
                'local' ->
                    Resp = process_query(C#ddCmd.command, Sess, {ConnId, imem}, SessPid),
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
                            case {gen_adapter:opt_bind_json_obj(C#ddCmd.command, imem),
                                  make_binds(proplists:get_value(<<"binds">>, BodyJson, null))} of
                                {[], _} ->
                                    Resp = process_query(C#ddCmd.command, Connection, {ConnId, imem}, SessPid),
                                    RespJson = jsx:encode([{<<"browse_data">>,
                                        [{<<"content">>, C#ddCmd.command}
                                         ,{<<"name">>, Name}
                                         ,{<<"table_layout">>, (V#ddView.state)#viewstate.table_layout}
                                         ,{<<"column_layout">>, (V#ddView.state)#viewstate.column_layout}
                                         ,{<<"view_id">>, V#ddView.id}] ++ Resp}]),
                                    ?Debug("loading ~p at ~p", [Name, (V#ddView.state)#viewstate.table_layout]);
                                {JsonBindInfo, []} ->
                                    RespJson = jsx:encode([{<<"browse_data">>,
                                        [{<<"content">>, C#ddCmd.command}
                                        ,{<<"name">>, Name}
                                        ,{<<"table_layout">>, (V#ddView.state)#viewstate.table_layout}
                                        ,{<<"column_layout">>, (V#ddView.state)#viewstate.column_layout}
                                        ,{<<"view_id">>, V#ddView.id} | JsonBindInfo]}]);
                                {_, Binds} ->
                                    Resp = process_query(C#ddCmd.command, Connection, {ConnId, imem}, Binds, SessPid),
                                    RespJson = jsx:encode([{<<"browse_data">>,
                                        [{<<"content">>, C#ddCmd.command}
                                         ,{<<"name">>, Name}
                                         ,{<<"table_layout">>, (V#ddView.state)#viewstate.table_layout}
                                         ,{<<"column_layout">>, (V#ddView.state)#viewstate.column_layout}
                                         ,{<<"view_id">>, V#ddView.id}] ++ Resp}])
                            end;
                        false ->
                            RespJson = error_invalid_conn(Connection, Connections)
                    end
            end,
            From ! {reply, RespJson};
        true ->
            case lists:member(Connection, Connections) of
                true ->
                    Name = sql_name(element(3 + Col, R)),
                    Query = <<"select * from ", Name/binary>>,
                    Resp = process_query(Query, Connection, {ConnId, imem}, SessPid),
                    RespJson = jsx:encode([{<<"browse_data">>,
                        [{<<"content">>, Query}
                         ,{<<"name">>, Name}] ++ Resp }]),
                    From ! {reply, RespJson};
                false ->
                    From ! {reply, error_invalid_conn(Connection, Connections)}
            end
    end,
    Priv;
process_cmd({[<<"views">>], ReqBody}, Sess, UserId, From, Priv, SessPid) ->
    [{<<"views">>,BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
    %% TODO: This should be replaced by dashboard.
    case dderl_dal:get_view(Sess, <<"All ddViews">>, imem, UserId) of
        {error, _} = Error->
            F = Error;
        undefined ->
            ?Debug("Using system view All ddViews"),
            F = dderl_dal:get_view(Sess, <<"All ddViews">>, imem, system);
        UserView ->
            ?Debug("Using a personalized view All ddViews"),
            F = UserView
    end,
    case F of
        {error, Reason} ->
            RespJson = jsx:encode([{<<"error">>, Reason}]);
        _ ->
            C = dderl_dal:get_command(Sess, F#ddView.cmd),
            Resp = process_query(C#ddCmd.command, Sess, {ConnId, imem}, SessPid),
            ?Debug("ddViews ~p~n~p", [C#ddCmd.command, Resp]),
            RespJson = jsx:encode([{<<"views">>,
                [{<<"content">>, C#ddCmd.command}
                ,{<<"name">>, <<"All ddViews">>}
                ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                ,{<<"view_id">>, F#ddView.id}]
                ++ Resp
            }])
    end,
    From ! {reply, RespJson},
    Priv;
process_cmd({[<<"system_views">>], ReqBody}, Sess, _UserId, From, Priv, SessPid) ->
    [{<<"system_views">>,BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
    case dderl_dal:get_view(Sess, <<"All ddViews">>, imem, system) of
        {error, Reason} ->
            RespJson = jsx:encode([{<<"error">>, Reason}]);
        F ->
            C = dderl_dal:get_command(Sess, F#ddView.cmd),
            Resp = process_query(C#ddCmd.command, Sess, {ConnId, imem}, SessPid),
            ?Debug("ddViews ~p~n~p", [C#ddCmd.command, Resp]),
            RespJson = jsx:encode([{<<"system_views">>,
                [{<<"content">>, C#ddCmd.command}
                ,{<<"name">>, <<"All ddViews">>}
                ,{<<"table_layout">>, (F#ddView.state)#viewstate.table_layout}
                ,{<<"column_layout">>, (F#ddView.state)#viewstate.column_layout}
                ,{<<"view_id">>, F#ddView.id}]
                ++ Resp
            }])
    end,
    From ! {reply, RespJson},
    Priv;
process_cmd({[<<"open_view">>], ReqBody}, Sess, _UserId, From, #priv{connections = Connections} = Priv, SessPid) ->
    [{<<"open_view">>, BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            Binds = make_binds(proplists:get_value(<<"binds">>, BodyJson, null)),
            View = dderl_dal:get_view(Sess, ViewId),
            Res = open_view(Sess, Connection, SessPid, ConnId, Binds, View),
            From ! {reply, jsx:encode(#{<<"open_view">> => Res})};
        false ->
            From ! {reply, error_invalid_conn(Connection, Connections)}
    end,
    Priv;
process_cmd({[<<"open_graph_view">>], ReqBody}, Sess, UserId, From,
            #priv{connections = Connections} = Priv, SessPid) ->
    [{<<"open_graph_view">>, BodyJson}] = ReqBody,
    ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
    ViewName = proplists:get_value(<<"view_name">>, BodyJson),
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            %% We need to check first for owner views, and then for the rest...
            View = case dderl_dal:get_view(Sess, ViewName, imem, UserId) of
                undefined ->
                    dderl_dal:get_view(Sess, ViewName, imem, '_');
                VRes -> VRes
            end,
            Binds = make_binds(proplists:get_value(<<"binds">>, BodyJson, null)),
            Res = open_view(Sess, Connection, SessPid, ConnId, Binds, View),
            %% We have to add the supported types so edit sql can be prefilled with the parameters.
            Result = [{<<"bind_types">>, bind_arg_types()} | Res],
            From ! {reply, jsx:encode(#{<<"open_graph_view">> => Result})};
        false ->
            From ! {reply, error_invalid_conn(Connection, Connections)}
    end,
    Priv;
process_cmd({[<<"update_focus_stmt">>], BodyJson}, Sess, UserId, From, Priv, SessPid) ->
    Statement = ?D2T(proplists:get_value(<<"statement">>, BodyJson, <<>>)),
    Key = proplists:get_value(<<"key">>, BodyJson, <<>>),
    DashView = proplists:get_value(<<"view_name">>, BodyJson, <<>>),
    Suffix = proplists:get_value(<<"suffix">>, BodyJson, <<>>),
    ViewName = case Key of
        <<>> -> DashView;
        _ -> <<DashView/binary, Suffix/binary>>
    end,
    View = case dderl_dal:get_view(Sess, ViewName, imem, UserId) of
        undefined -> dderl_dal:get_view(Sess, ViewName, imem, '_');
        VRes -> VRes
    end,
    case View of
        undefined ->
            From ! {reply, jsx:encode([{<<"update_focus_stmt">>,[{<<"error">>, <<"unable to find the view">>}]}])};
        _ ->
            Binds = make_binds(proplists:get_value(<<"binds">>, BodyJson, null)),
            Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
            ConnId = proplists:get_value(<<"conn_id">>, BodyJson, <<>>),
            Result = open_view(Sess, Connection, SessPid, ConnId, Binds, View),
            case proplists:get_value(<<"error">>, Result, undefined) of
                undefined ->
                    Statement:close(),
                    From ! {reply, jsx:encode(#{<<"update_focus_stmt">> => Result})};
                Error ->
                    From ! {reply, jsx:encode([{<<"update_focus_stmt">>,[{<<"error">>, Error}]}])}
            end
    end,
    Priv;
process_cmd({[<<"graph_subscribe">>], BodyJson}, _Sess, _UserId, From, Priv, _SessPid) ->
    Statement = ?D2T(proplists:get_value(<<"statement">>, BodyJson, <<>>)),
    Key = proplists:get_value(<<"key">>, BodyJson, <<>>),
    Topic = proplists:get_value(<<"topic">>, BodyJson, <<>>),
    Statement:gui_req(subscribe, {Topic, Key}, gui_resp_cb_fun(<<"graph_subscribe">>, Statement, From)),
    Priv;
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
    TableNames = [sql_name(N) || N <- proplists:get_value(<<"table_names">>, BodyJson, [])],
    Results = [process_table_cmd(drop_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"drop_table">>, Results),
    Priv;
process_cmd({[<<"truncate_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"truncate_table">>, BodyJson}] = ReqBody,
    TableNames = [sql_name(N) || N <- proplists:get_value(<<"table_names">>, BodyJson, [])],
    Results = [process_table_cmd(truncate_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"truncate_table">>, Results),
    Priv;
process_cmd({[<<"snapshot_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"snapshot_table">>, BodyJson}] = ReqBody,
    TableNames = [sql_name(N) || N <- proplists:get_value(<<"table_names">>, BodyJson, [])],
    Results = [process_table_cmd(snapshot_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"snapshot_table">>, Results),
    Priv;
process_cmd({[<<"restore_table">>], ReqBody}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    [{<<"restore_table">>, BodyJson}] = ReqBody,
    TableNames = [sql_name(N) || N <- proplists:get_value(<<"table_names">>, BodyJson, [])],
    Results = [process_table_cmd(restore_table, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"restore_table">>, Results),
    Priv;
process_cmd({[<<"button">>], ReqBody}, _Sess, _UserId, From, Priv, _SessPid) ->
    [{<<"button">>,BodyJson}] = ReqBody,
    Statement = binary_to_term(base64:decode(proplists:get_value(<<"statement">>, BodyJson, <<>>))),
    Button = case proplists:get_value(<<"btn">>, BodyJson, <<">">>) of
        ButtonInt when is_integer(ButtonInt) -> ButtonInt;
        ButtonBin when is_binary(ButtonBin) ->
            case string:to_integer(binary_to_list(ButtonBin)) of
                {Target, []} -> Target;
                _ -> ButtonBin
            end
    end,
    Statement:gui_req(button, Button, gui_resp_cb_fun(<<"button">>, Statement, From)),
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
process_cmd({[<<"download_query">>], ReqBody}, _Sess, UserId, From, Priv, _SessPid) ->
    [{<<"download_query">>, BodyJson}] = ReqBody,
    FileName = proplists:get_value(<<"fileToDownload">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"queryToDownload">>, BodyJson, <<>>),
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case check_funs(Connection:exec(Query, ?DEFAULT_ROW_SIZE, [])) of
        ok ->
            ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
            From ! {reply_csv, FileName, <<>>, single};
        {ok, #stmtResult{stmtCols = Clms, stmtRef = StmtRef, rowFun = RowFun}} ->
            Columns = gen_adapter:build_column_csv(UserId, imem, Clms),
            From ! {reply_csv, FileName, Columns, first},
            ProducerPid = spawn(fun() ->
                produce_csv_rows(UserId, Connection, From, StmtRef, RowFun)
            end),
            Connection:add_stmt_fsm(StmtRef, {?MODULE, ProducerPid}),
            Connection:run_cmd(fetch_recs_async, [[{fetch_mode,push}], StmtRef]),
            ?Debug("process_query created statement ~p for ~p", [ProducerPid, Query]);
        {error, {{Ex, M}, Stacktrace} = Error} ->
            ?Error("query error ~p", [Error], Stacktrace),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply_csv, FileName, Err, single};
        {error, {Ex,M}} ->
            ?Error("query error ~p", [{Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            From ! {reply_csv, FileName, Err, single};
        Error ->
            ?Error("query error ~p", [Error]),
            Error = if is_binary(Error) -> Error;
                true -> list_to_binary(lists:flatten(io_lib:format("~p", [Error])))
            end,
            From ! {reply_csv, FileName, Error, single}
    end,
    Priv;
process_cmd({[<<"restore_tables_as">>], BodyJson}, _Sess, _UserId, From, #priv{connections = Connections} = Priv, _SessPid) ->
    TableNames = proplists:get_value(<<"restore_tables_as">>, BodyJson, <<>>),
    Results = [process_table_cmd(restore_table_as, TableName, BodyJson, Connections) || TableName <- TableNames],
    send_result_table_cmd(From, <<"restore_tables_as">>, Results),
    Priv;
% unsupported gui actions
process_cmd({Cmd, BodyJson}, _Sess, _UserId, From, Priv, _SessPid) ->
    ?Error("unsupported command ~p content ~p and priv ~p", [Cmd, BodyJson, Priv]),
    CmdBin = lists:last(Cmd),
    From ! {reply, jsx:encode([{CmdBin,[{<<"error">>, <<"command '", CmdBin/binary, "' is unsupported">>}]}])},
    Priv.

sql_name(Name) ->
    %% accept {Schema,Name} and convert to Schema.Name
    case re:split(Name,"[,]") of
        [<<${,S/binary>>,N] ->
            NN0=re:replace(N,"}$","",[{return,binary}]),
            NN1=re:replace(NN0,"^'","",[{return,binary}]),
            NN2=re:replace(NN1,"'$","",[{return,binary}]),
            % convert "{Schema,Name}"" to "Schema.Name"
            <<S/binary,$.,NN2/binary>>;
        _ ->
            Name
    end.

% dderl_fsm like row receive interface for compatibility
rows(Rows, {?MODULE, Pid}) -> Pid ! Rows.
produce_csv_rows(UserId, Connection, From, StmtRef, RowFun)
  when is_function(RowFun), is_pid(From) ->
    receive
        Data ->
            case erlang:process_info(From) of
                undefined -> ?Error("Request aborted (response pid ~p invalid)", [From]);
                _ ->
                    produce_csv_rows_result(Data, UserId, Connection, From, StmtRef, RowFun)
            end
    end.

produce_csv_rows_result({error, Error}, _UserId, Connection, From, StmtRef, _RowFun) ->
    From ! {reply_csv, <<>>, list_to_binary(io_lib:format("Error: ~p", [Error])), last},
    Connection:run_cmd(close, [StmtRef]);
produce_csv_rows_result({Rows,false}, UserId, Connection, From, StmtRef, RowFun) when is_list(Rows) ->
    if length(Rows) > 0 ->
           CsvRows = gen_adapter:make_csv_rows(UserId, Rows, RowFun, imem),
           ?Debug("Rows intermediate ~p", [CsvRows]),
           From ! {reply_csv, <<>>, CsvRows, continue};
       true -> ok
    end,
    produce_csv_rows(UserId, Connection, From, StmtRef, RowFun);
produce_csv_rows_result({Rows,true}, UserId, Connection, From, StmtRef, RowFun) when is_list(Rows) ->
    CsvRows = gen_adapter:make_csv_rows(UserId, Rows, RowFun, imem),
    ?Debug("Rows last ~p", [CsvRows]),
    From ! {reply_csv, <<>>, CsvRows, last},
    Connection:run_cmd(close, [StmtRef]).

-spec disconnect(#priv{}) -> #priv{}.
disconnect(#priv{connections = []} = Priv) -> Priv;
disconnect(#priv{connections = [Connection | Rest]} = Priv) ->
    ?Debug("closing the connection ~p", [Connection]),
    try Connection:close()
    catch Class:Error ->
            ?Error("Error trying to close the connection ~p ~p:~p~n",
                   [Connection, Class, Error], erlang:get_stacktrace())
    end,
    disconnect(Priv#priv{connections = Rest}).

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

-spec make_binds(null | [{binary(), [{binary(), binary()}]}]) -> [tuple()].
make_binds(null) -> [];
make_binds(Binds) ->
    [{B, binary_to_existing_atom(proplists:get_value(<<"typ">>, TV, <<>>), utf8), 0, [proplists:get_value(<<"val">>, TV, <<>>)]} || {B, TV} <- Binds].

-spec add_param(boolean(), [tuple()], tuple()) -> [tuple()].
add_param(false, Params, _ParamToAdd) -> Params;
add_param(true, Params, ParamToAdd) -> [ParamToAdd | Params].

-spec process_query(binary(), tuple(), {binary(), atom()}, [tuple()], pid()) -> list().
process_query(Query, Connection, {ConnId, Adapter}, Params, SessPid) ->
    QueryParams = get_params(Query),
    Params0 = add_param(lists:member(<<":ddConn.id">>, QueryParams), Params, {<<":ddConn.id">>, integer, 0, [ConnId]}),
    Params1 = add_param(lists:member(<<":ddAdapter.id">>, QueryParams), Params0, {<<":ddAdapter.id">>, atom, undefined,[atom_to_binary(Adapter, utf8)]}),
    process_query(Query, Connection, Params1, SessPid).

-spec process_query(binary(), tuple(), {binary(), atom()} | [tuple()], pid()) -> list().
process_query(Query, Connection, {ConnId, Adapter}, SessPid) ->
    process_query(Query, Connection, {ConnId, Adapter}, [], SessPid);
process_query(Query, {_,_ConPid}=Connection, Params, SessPid) ->
    case check_funs(Connection:exec(Query, ?DEFAULT_ROW_SIZE, Params)) of
        ok ->
            ?Debug([{session, Connection}], "query ~p -> ok", [Query]),
            [{<<"result">>, <<"ok">>}];
        {ok, #stmtResult{ stmtCols = Clms
                        , rowFun   = RowFun
                        , stmtRef  = StmtRef
                        , sortFun  = SortFun
                        , sortSpec = SortSpec} = _StmtRslt} ->
            TableName = extract_table_name(Query),
            StmtFsm = dderl_fsm:start(
                                #fsmctx{ id                         = "what is it?"
                                       , stmtCols                   = Clms
                                       , rowFun                     = RowFun
                                       , sortFun                    = SortFun
                                       , sortSpec                   = SortSpec
                                       , orig_qry                   = Query
                                       , bind_vals                  = Params
                                       , table_name                 = TableName
                                       , block_length               = ?DEFAULT_ROW_SIZE
                                       , fetch_recs_async_fun       = imem_adapter_funs:fetch_recs_async(Connection, StmtRef)
                                       , fetch_close_fun            = imem_adapter_funs:fetch_close(Connection, StmtRef)
                                       , stmt_close_fun             = imem_adapter_funs:stmt_close(Connection, StmtRef)
                                       , filter_and_sort_fun        = imem_adapter_funs:filter_and_sort(Connection, StmtRef)
                                       , update_cursor_prepare_fun  = imem_adapter_funs:update_cursor_prepare(Connection, StmtRef)
                                       , update_cursor_execute_fun  = imem_adapter_funs:update_cursor_execute(Connection, StmtRef)
                                       }, SessPid),
            Connection:add_stmt_fsm(StmtRef, StmtFsm),
            ?Debug("StmtRslt ~p ~p", [Clms, SortSpec]),
            Columns = gen_adapter:build_column_json(lists:reverse(Clms)),
            JSortSpec = build_srtspec_json(SortSpec),
            ?Debug("JColumns~n ~s~n JSortSpec~n~s", [jsx:prettify(jsx:encode(Columns)), jsx:prettify(jsx:encode(JSortSpec))]),
            ?Debug("process_query created statement ~p for ~p", [StmtFsm, Query]),
            [{<<"columns">>, Columns},
             {<<"sort_spec">>, JSortSpec},
             {<<"statement">>, base64:encode(term_to_binary(StmtFsm))},
             {<<"connection">>, ?E2B(Connection)}];
        {error, {{Ex, M}, Stacktrace} = Error} ->
            ?Error("Error on query ~p: ~p", [Query, Error], Stacktrace),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            [{<<"error">>, Err}];
        {error, {Ex,M}} ->
            ?Error("Error on query ~p: ~p", [Query, {Ex,M}]),
            Err = list_to_binary(atom_to_list(Ex) ++ ": " ++
                                     lists:flatten(io_lib:format("~p", [M]))),
            [{<<"error">>, Err}];
        <<"Unsupported target database version">> = Msg ->
            ?Error("Unsupported target database version"),
            [{<<"error">>, Msg}];
        Result ->
            ?Debug("query result ~p", [Result]),
            %% Todo: client can't handle this result yet, so we send ok.
            [{<<"result">>, <<"ok">>}]
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
            TableErrorsBin = lists:foldl(
                fun({{Table, _}, Error}, Acc) ->
                        ErrorBin = imem_datatype:term_to_io(Error),
                        <<Acc/binary, Table/binary, " - ", ErrorBin/binary, ", ">>;
                   ({Table, Error}, Acc) ->
                        ErrorBin = imem_datatype:term_to_io(Error),
                        <<Acc/binary, Table/binary, " - ", ErrorBin/binary, ", ">>;
                   (Table, Acc) ->  <<Acc/binary, Table/binary, ", ">>
                end, <<>>, TableErrors),
            FTableErrorsBin = re:replace(TableErrorsBin, ", $", "", [{return, binary}]),
            [CmdSplit|_] = binary:split(BinCmd, <<"_">>),
            Err = <<"Unable to ", CmdSplit/binary, " the following tables: ", FTableErrorsBin/binary>>,
            ?Error("Error: ~p",  [Err]),
            From ! {reply, jsx:encode([{BinCmd, [{<<"error">>, Err}]}])}
    end,
    ok.

-spec process_table_cmd(atom(), binary(), term(), [{atom(), pid()}]) -> term().
process_table_cmd(Cmd, TableName, BodyJson, Connections) ->
    Connection = ?D2T(proplists:get_value(<<"connection">>, BodyJson, <<>>)),
    case lists:member(Connection, Connections) of
        true ->
            case Connection:run_cmd(Cmd, [TableName]) of
                ok ->
                    ok;
                {error, {{_Ex, {_M, E}}, Stacktrace} = Error} ->
                    ?Error("query error ~p", [Error], Stacktrace),
                    {error, {TableName, E}};
                {error, {{_Ex, _M}, Stacktrace} = Error} ->
                    ?Error("query error ~p", [Error], Stacktrace),
                    {error, TableName};
                {error, {Ex, M}} ->
                    ?Error("query error ~p", [{Ex,M}]),
                    {error, TableName};
                Error ->
                    ?Error("query error ~p", [Error]),
                    {error, TableName}
            end;
        false ->
            {error, invalid_connection}
    end.

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

%% -spec connect_to_erlimem(atom(), {atom(), pid()}, list(), binary(), atom(), binary(), tuple(), map()) ->
%%     {ok, {atom(), pid()}} | {error, term()}.
%% connect_to_erlimem(rpc, _Sess, _Ip, Port, _Secure, Schema, Credentials, ConnInfo)
%%   when is_map(ConnInfo) ->
%%     try binary_to_existing_atom(Port, utf8) of
%%         AtomPort ->
%%             {ok, ErlImemSess} = erlimem:open({rpc, AtomPort}, Schema),
%%             ?Info("Credentials ~p", [Credentials]),
%%             ErlImemSess:run_cmd(login,[]),
%%             {ok, ErlImemSess}
%%     catch _:_ -> {error, "Invalid port for connection type rpc"}
%%     end;
%% connect_to_erlimem(tcp, _Sess, Ip, Port, Secure, Schema, Credentials, ConnInfo)
%%   when is_map(ConnInfo) ->
%%     SSL = if Secure =:= true -> [ssl]; true -> [] end,
%%     try binary_to_integer(Port) of
%%         IntPort ->
%%             {ok, ErlImemSess} = erlimem:open({tcp, Ip, IntPort, SSL}, Schema),
%%             ?Info("Credentials ~p", [Credentials]),
%%             ErlImemSess:run_cmd(login,[]),
%%             {ok, ErlImemSess}
%%     catch _:_ -> {error, "Invalid port for connection type tcp"}
%%     end;
%% connect_to_erlimem(local, Sess, _Ip, _Port, _Secure, Schema, _Credentials, ConnInfo)
%%   when is_map(ConnInfo) ->
%%     ?Info("Got conn info ~p", [ConnInfo]),
%%     case dderl_dal:is_admin(Sess) of
%%         true ->
%%             erlimem:open(local, Schema);
%%         _ -> {error, "Local connection unauthorized"}
%%     end.

-spec error_invalid_conn({atom(), pid()}, [{atom(), pid()}]) -> binary().
error_invalid_conn(Connection, Connections) ->
    Err = <<"Trying to process a query with an unowned connection">>,
    ?Error("~s: ~p~n connections list: ~p", [Err, Connection, Connections]),
    jsx:encode([{<<"error">>, Err}]).

-spec error_invalid_conn() -> binary().
error_invalid_conn() ->
    Err = <<"Trying to process a query with an unowned connection">>,
    jsx:encode([{<<"error">>, Err}]).

-spec check_fun_vsn(fun()) -> boolean().
check_fun_vsn(Fun) when is_function(Fun)->
    {module, Mod} = erlang:fun_info(Fun, module),
    [ModVsn] = proplists:get_value(vsn, Mod:module_info(attributes)),
    ?Debug("The Module version: ~p~n", [ModVsn]),
    {new_uniq, <<FunVsn:16/unit:8>>} = erlang:fun_info(Fun, new_uniq),
    ?Debug("The function version: ~p~n", [FunVsn]),
    ModVsn =:= FunVsn;
check_fun_vsn(_) ->
    false.

-spec check_funs(term()) -> term().
check_funs({ok, #stmtResult{rowFun = RowFun, sortFun = SortFun} = StmtRslt}) ->
    ValidFuns = check_fun_vsn(RowFun) andalso check_fun_vsn(SortFun),
    if
        ValidFuns -> {ok, StmtRslt};
        true -> <<"Unsupported target database version">>
    end;
check_funs(Error) ->
    Error.

-spec extract_table_name(binary()) -> binary().
extract_table_name(Query) ->
    case sqlparse:parsetree(Query) of
        {ok,[{{select, SelectSections},_}]} ->
            {from, [FirstTable|_]} = lists:keyfind(from, 1, SelectSections),
            case FirstTable of
                {as, Tab, _Alias} -> Tab;
                {{as, Tab, _Alias}, _} -> Tab;
                {Tab, _} -> Tab;
                Tab when is_binary(Tab) -> Tab;
                _ -> <<>>
            end;
        _ ->
            <<>>
    end.

-spec open_view({atom(), pid()}, {atom(), pid()}, pid(), binary(), [tuple()], undefined | {error, binary()}) -> list().
open_view(_Sess, _Connection, _SessPid, _ConnId, _Binds, undefined) -> [{<<"error">>, <<"view not found">>}];
open_view(_Sess, _Connection, _SessPid, _ConnId, _Binds, {error, Reason}) -> [{<<"error">>, Reason}];
open_view(Sess, Connection, SessPid, ConnId, Binds, #ddView{id = Id, name = Name, cmd = CmdId, state = ViewState}) ->
    C = dderl_dal:get_command(Sess, CmdId),
    ConnUsed = case C#ddCmd.conns of
        local -> Sess;
        _ -> Connection
    end,
    Resp = case {gen_adapter:opt_bind_json_obj(C#ddCmd.command, imem), Binds} of
        {[], _} -> process_query(C#ddCmd.command, ConnUsed, {ConnId, imem}, SessPid);
        {JsonBindInfo, []} -> JsonBindInfo;
        {_, Binds} -> process_query(C#ddCmd.command, Connection, {ConnId, imem}, Binds, SessPid)
    end,
    [{<<"content">>, C#ddCmd.command}
    ,{<<"name">>, Name}
    ,{<<"table_layout">>,ViewState#viewstate.table_layout}
    ,{<<"column_layout">>, ViewState#viewstate.column_layout}
    ,{<<"view_id">>, Id} | Resp].

-spec get_params(binary()) -> [binary()].
get_params(Sql) ->
    case sqlparse:parsetree(Sql) of
        {ok,[{ParseTree,_}]} ->
            Pred = fun(P,Ctx) ->
                           case P of
                               {param, Param} -> [Param|Ctx];
                               _ -> Ctx
                           end
                   end,
            sqlparse:foldtd(Pred,[],ParseTree);
        _ ->
            []
    end.

-spec get_deps() -> [atom()].
get_deps() -> [erlimem, imem].
