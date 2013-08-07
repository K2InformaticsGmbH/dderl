-module(gen_adapter).

-include("dderl.hrl").
-include("gres.hrl").

-include_lib("imem/include/imem_sql.hrl").
-include_lib("sqlparse/src/sql_box.hrl").

-export([ process_cmd/5
        , init/0
        , add_cmds_views/4
        , gui_resp/2
        , build_resp_fun/3
        ]).

init() -> ok.

add_cmds_views(_, _, _, []) -> ok;
add_cmds_views(Sess, UserId, A, [{N,C,Con}|Rest]) ->
    add_cmds_views(Sess, UserId, A, [{N,C,Con,#viewstate{}}|Rest]);
add_cmds_views(Sess, UserId, A, [{N,C,Con,#viewstate{}=V}|Rest]) ->
    case dderl_dal:get_view(Sess, N, A, UserId) of
        undefined ->
            Id = dderl_dal:add_command(Sess, UserId, A, N, C, Con, []),
            dderl_dal:add_view(Sess, UserId, N, Id, V);
        View ->
            dderl_dal:update_command(Sess, View#ddView.cmd, UserId, A, N, C, Con, []),
            dderl_dal:add_view(Sess, UserId, N, View#ddView.cmd, V)
    end,
    add_cmds_views(Sess, UserId, A, Rest).

box_to_json(Box) ->
    [ {<<"ind">>, Box#box.ind}
    , {<<"name">>, any_to_bin(Box#box.name)}
    , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
    , {<<"collapsed">>, Box#box.collapsed}
    , {<<"error">>, Box#box.error}
    , {<<"color">>, Box#box.color}
    , {<<"pick">>, Box#box.pick}].

any_to_bin(C) when is_list(C) -> list_to_binary(C);
any_to_bin(C) when is_binary(C) -> C;
any_to_bin(C) -> list_to_binary(lists:nth(1, io_lib:format("~p", [C]))).
    
process_cmd({[<<"parse_stmt">>], ReqBody}, _Sess, _UserId, From, _Priv) ->
    [{<<"parse_stmt">>,BodyJson}] = ReqBody,
    Sql = string:strip(binary_to_list(proplists:get_value(<<"qstr">>, BodyJson, <<>>))),
    ?Info("parsing ~p", [Sql]),
    case (catch jsx:encode([{<<"parse_stmt">>, [
        try
            Box = sql_box:boxed(Sql),
            ?Debug("--- Box --- ~n~p", [Box]),
            {<<"sqlbox">>, box_to_json(Box)}
        catch
            _:ErrorBox -> {<<"boxerror">>, list_to_binary(lists:flatten(io_lib:format("~p", [ErrorBox])))}
        end,
        try
            Pretty = sql_box:pretty(Sql),
            {<<"pretty">>, list_to_binary(Pretty)}
        catch
            _:ErrorPretty -> {<<"prettyerror">>, list_to_binary(lists:flatten(io_lib:format("~p", [ErrorPretty])))}
        end,
        try
            Flat = sql_box:flat(Sql),
            {<<"flat">>, list_to_binary(Flat)}
        catch
            _:ErrorFlat -> {<<"flaterror">>, list_to_binary(lists:flatten(io_lib:format("~p", [ErrorFlat])))}
        end
    ]}])) of
        ParseStmt when is_binary(ParseStmt) ->
            ?Debug("Json -- ~s", [jsx:prettify(ParseStmt)]),
            From ! {reply, ParseStmt};
        Error ->
            ?Error("parse_stmt error ~p~n", [Error]),
            ReasonBin = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            From ! {reply, jsx:encode([{<<"parse_stmt">>, [{<<"error">>, ReasonBin}]}])}
    end;
process_cmd({[<<"get_query">>], ReqBody}, _Sess, _UserId, From, _Priv) ->
    [{<<"get_query">>,BodyJson}] = ReqBody,
    Table = proplists:get_value(<<"table">>, BodyJson, <<>>),
    Query = "SELECT * FROM " ++ binary_to_list(Table),
    ?Debug("get query ~p~n", [Query]),
    Res = jsx:encode([{<<"qry">>,[{<<"name">>,Table},{<<"content">>,list_to_binary(Query)}]}]),
    From ! {reply, Res};
process_cmd({[<<"save_view">>], ReqBody}, Sess, UserId, From, _Priv) ->
    [{<<"save_view">>,BodyJson}] = ReqBody,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, <<>>),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, <<>>),
    ?Info("save_view for ~p layout ~p", [Name, TableLay]),
    add_cmds_views(Sess, UserId, imem, [{Name, Query, undefined, #viewstate{table_layout=TableLay, column_layout=ColumLay}}]),
    Res = jsx:encode([{<<"save_view">>,<<"ok">>}]),
    From ! {reply, Res};
process_cmd({[<<"update_view">>], ReqBody}, Sess, _UserId, From, _Priv) ->
    [{<<"update_view">>,BodyJson}] = ReqBody,
    Name = proplists:get_value(<<"name">>, BodyJson, <<>>),
    Query = proplists:get_value(<<"content">>, BodyJson, <<>>),
    TableLay = proplists:get_value(<<"table_layout">>, BodyJson, <<>>),
    ColumLay = proplists:get_value(<<"column_layout">>, BodyJson, <<>>),
    ViewId = proplists:get_value(<<"view_id">>, BodyJson),
    ?Info("update view ~s with id ~p layout ~p", [Name, ViewId, TableLay]),
    ViewState = #viewstate{table_layout=TableLay, column_layout=ColumLay},
    %% TODO: We need to pass the userid to provide authorization.
    case dderl_dal:update_view(Sess, ViewId, ViewState, Query) of
        {error, Reason} ->
            Res = jsx:encode([{<<"update_view">>, [{<<"error">>, Reason}]}]);
        _ ->
            Res = jsx:encode([{<<"update_view">>, <<"ok">>}])
    end,
    From ! {reply, Res};
process_cmd({Cmd, _BodyJson}, _Sess, _UserId, From, _Priv) ->
    io:format(user, "Unknown cmd ~p ~p~n", [Cmd, _BodyJson]),
    From ! {reply, jsx:encode([{<<"error">>, <<"unknown command">>}])}.

%%%%%%%%%%%%%%%

col2json(Cols) -> col2json(lists:reverse(Cols), [], length(Cols)).
col2json([], JCols, _Counter) -> [<<"id">>,<<"op">>|JCols];
col2json([C|Cols], JCols, Counter) ->
    Nm = C#stmtCol.alias,
    BinCounter = integer_to_binary(Counter),
    Nm1 = <<Nm/binary, $_, BinCounter/binary>>,
    col2json(Cols, [Nm1 | JCols], Counter - 1).

gui_resp(#gres{} = Gres, Columns) ->
    JCols = col2json(Columns),
    ?Debug("processing resp ~p cols ~p jcols ~p", [Gres, Columns, JCols]),
    % refer to erlimem/src/gres.hrl for the descriptions of the record fields
    [{<<"op">>,         Gres#gres.operation}
    ,{<<"cnt">>,        Gres#gres.cnt}
    ,{<<"toolTip">>,    Gres#gres.toolTip}
    ,{<<"message">>,    Gres#gres.message}
    ,{<<"beep">>,       Gres#gres.beep}
    ,{<<"state">>,      Gres#gres.state}
    ,{<<"loop">>,       Gres#gres.loop}
    ,{<<"rows">>,       r2jsn(Gres#gres.rows, JCols)}
    ,{<<"keep">>,       Gres#gres.keep}
    ,{<<"focus">>,      Gres#gres.focus}
    ,{<<"sql">>,        Gres#gres.sql}
    ,{<<"disable">>,    Gres#gres.disable}
    ,{<<"promote">>,    Gres#gres.promote}
    ,{<<"max_width_vec">>, lists:flatten(r2jsn([widest_cell_per_clm(Gres#gres.rows)], JCols))}
    ].

widest_cell_per_clm([]) -> [];
widest_cell_per_clm([R|_] = Rows) ->
    widest_cell_per_clm(Rows, lists:duplicate(length(R), <<>>)).
widest_cell_per_clm([],V) -> V;
widest_cell_per_clm([R|Rows],V) ->
    NewV = 
    [case {Re, Ve} of
        {Re, Ve} ->
            ReS = if
                is_atom(Re)     -> atom_to_binary(Re, utf8);
                is_integer(Re)  -> integer_to_binary(Re);
                %is_list(Re)     -> list_to_binary(Re);
                true            -> Re
            end,
            ReL = byte_size(ReS),
            VeL = byte_size(Ve),
            if ReL > VeL -> ReS; true -> Ve end
     end
    || {Re, Ve} <- lists:zip(R,V)],
    widest_cell_per_clm(Rows,NewV).

r2jsn(Rows, JCols) -> r2jsn(Rows, JCols, []).
r2jsn([], _, NewRows) -> lists:reverse(NewRows);
r2jsn([[]], _, NewRows) -> lists:reverse(NewRows);
r2jsn([Row|Rows], JCols, NewRows) ->
    r2jsn(Rows, JCols, [
        [{C, case R of
                R when is_integer(R) -> R;
                R when is_atom(R)    -> atom_to_binary(R, utf8);
                R when is_binary(R)  ->
                     %% check if it is a valid utf8
                     %% else we convert it from latin1
                     case unicode:characters_to_binary(R) of
                         Invalid when is_tuple(Invalid) -> 
                             unicode:characters_to_binary(R, latin1, utf8);
                         UnicodeBin ->
                             UnicodeBin
                     end
             end
         }
        || {C, R} <- lists:zip(JCols, Row)]
    | NewRows]).

build_resp_fun(Cmd, Clms, From) ->
    fun(#gres{} = GuiResp) ->
        GuiRespJson = gui_resp(GuiResp, Clms),
        case (catch jsx:encode([{Cmd,GuiRespJson}])) of
            {'EXIT', Error} -> ?Error("Encoding problem ~p ~p~n~p~n~p", [Cmd, Error, GuiResp, GuiRespJson]);
            Resp -> From ! {reply, Resp}
        end
    end.
