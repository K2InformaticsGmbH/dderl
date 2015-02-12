-module(dderl_sqlbox).

-export([ pretty_from_pt/1
        , boxed_from_pt/1
        , flat_from_box/1
        , pretty_from_box/1
        , box_to_json/1
        , foldb/3
        , wrap_multiple_json/1
        ]).

-include("dderl.hrl").
-include("dderl_sqlbox.hrl").

-define(DefCollInd,6).  % First indentation level which will be collapsed by default
-define(DefCollLen,40). % Max length of flattened subboxes which is collapsed

-spec validate_children([#box{}]) -> ok | {error, binary()}.
validate_children([]) -> ok;
validate_children([#box{}]) -> ok;
validate_children(Children) ->
    case length(lists:usort([C#box.ind || C <- Children])) of
        1 -> ok;
        _ -> {error, <<"Mixed identation error">>}
    end.

-spec flat_from_box([#box{}] | #box{}) -> binary().
flat_from_box([]) -> <<>>;
flat_from_box(#box{name= <<",">> ,children=[]}) -> <<", ">>;
flat_from_box(#box{name=Name,children=[]}) ->
    iolist_to_binary([Name, " "]);
flat_from_box(#box{name= <<>>,children=CH}) ->
    iolist_to_binary([[flat_from_box(C) || C <-CH]]);
flat_from_box(#box{name=Name,children=CH}) ->
    iolist_to_binary([Name," ",[flat_from_box(C) || C <-CH]]);
flat_from_box([Box|Boxes]) ->
    iolist_to_binary([flat_from_box(Box),flat_from_box(Boxes)]).

-spec pretty_from_pt(term()) -> binary() | {error, binary()}.
pretty_from_pt(ParseTree) ->
    case foldb(ParseTree) of
        {error, Reason} -> {error, Reason};
        SqlBox -> pretty_from_box(SqlBox)
    end.

-spec pretty_from_box([#box{}] | #box{}) -> binary().
pretty_from_box([]) -> [];
pretty_from_box(#box{name= <<>>,children=[]}) -> <<>>;
pretty_from_box(#box{ind=Ind,name=Name,children=[]}) ->
    iolist_to_binary([indent(Ind), Name, "\r\n"]);
pretty_from_box(#box{ind=Ind,name= <<>>,children=[#box{children=[]} = L, #box{name= OP}, #box{children=[]} = R]}) when OP== <<"#">>;OP== <<":">>;OP== <<"{">>;OP== <<"}">>;OP== <<"[">>;OP== <<"]">> ->
    iolist_to_binary([indent(Ind+1), L#box.name, OP, R#box.name, "\r\n"]);
pretty_from_box(#box{ind=Ind,name= <<>>,children=CH}) ->
    if
        (hd(CH))#box.collapsed ->
            % ?Info("collapsed box ~p ~p",[<<>>,CH]),                
            iolist_to_binary([indent(Ind+1),[flat_from_box(C) || C <-CH],"\r\n"]);
        true ->
            % ?Info("expanded box ~p ~p",[<<>>,CH]),                
            iolist_to_binary([[pretty_from_box(C) || C <-CH]])
    end;
pretty_from_box(#box{collapsed=true}=Box) -> flat_from_box(Box);  % moved down from top to be revisited
pretty_from_box(#box{ind=Ind,name=Name,children=CH}) ->
    if
        (hd(CH))#box.collapsed ->
            % ?Info("collapsed box ~p ~p",[Name,CH]),                
            iolist_to_binary([indent(Ind),binary_to_list(Name),"\r\n",
                indent(Ind+1),[flat_from_box(C) || C <-CH],"\r\n"]);
        true ->
            % ?Info("expanded box ~p ~p",[Name,CH]),                
            iolist_to_binary([indent(Ind), Name,"\r\n",[pretty_from_box(C) || C <-CH]])
    end;
pretty_from_box([Box|Boxes]) ->
    iolist_to_binary([pretty_from_box(Box), pretty_from_box(Boxes)]).

indent(Ind) -> lists:duplicate(Ind*2,32).

boxed_from_pt(ParseTree) ->
    try
        case foldb(ParseTree) of
            {error, _} = Error -> Error;
            Res -> {ok, Res}
        end
    catch
        _:R -> {error, {R, erlang:get_stacktrace()}}
    end.

%% Operator Binding Power -------------------------------

binding(A) when is_binary(A) -> 190;
binding('param') -> 190;
binding('prior') -> 185;
binding('fun') -> 180;
binding('||') -> 175;
binding('#') -> 175;
binding('*') -> 170;
binding('/') -> 170;
binding('+') -> 160;
binding('-') -> 160;
binding('=') -> 150;
binding('<=') -> 150;
binding('>=') -> 150;
binding('<>') -> 150;
binding('<') -> 150;
binding('>') -> 150;
binding('like') -> 150;
binding('is') -> 150;
binding('between') -> 150;
% binding('lists') -> 145;
binding('in') -> 140;
% binding('list') -> 135;
binding('not') -> 130;
binding('and') -> 120;
binding('or') -> 100;
binding('as') -> 90;
binding('where') -> 88;
binding('hierarchical query') -> 88;
binding('start with') -> 88;
binding('connect by') -> 88;
binding('list') -> 85;
binding('fields') -> 80;
binding('into') -> 80;
binding('hints') -> 80;
binding('opt') -> 80;
binding('from') -> 80;
% binding('where') -> 80;
binding('order by') -> 80;
binding('group by') -> 80;
binding('having') -> 80;
binding('select') -> 70;
binding('union') -> 30;
binding('union all') -> 30;
binding('all') -> 30;           %% needed here?
binding('minus') -> 30;
binding('intersect') -> 30;
binding({A,_}) -> binding(A);
binding({A,_,_}) -> binding(A);
binding({A,_,_,_}) -> binding(A);
binding(undefined) -> -1;
binding(_) -> 0.

neItems(List) ->    %% non-empty items of as list
    lists:filter(fun(X)-> X /= empty end,List).

-spec bStr(atom() | binary()) -> binary().
bStr(A) when is_atom(A) -> atom_to_binary(A, utf8);
bStr(B) when is_binary(B) -> B;
bStr(C) ->
    % ?Error("Expected atom or binary, got ~p", [C]),
    throw({error, iolist_to_binary(io_lib:format("Expected atom or binary, got ~p", [C]))}).

foldb(ParseTree) ->
    foldb(0, undefined, ParseTree).

foldb(Ind, P, {select, List}) when is_list(List) ->
    Res = [foldb(Ind+1, P, Sli) || Sli <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ -> mk_box(Ind, neItems(Res), select)
    end;
foldb(_, _P, {hints,<<>>})               -> empty;
foldb(_, _P, {opt,  <<>>})               -> empty;
foldb(_, _P, {into,   _ })               -> empty;
foldb(_, _P, {where,  {}})               -> empty;
foldb(_, _P, {having, {}})               -> empty;
foldb(_, _P, {'hierarchical query', {}}) -> empty;

foldb(Ind, _P, T) when is_binary(T); is_atom(T) -> mk_box(Ind, [], T);
foldb(Ind, _P, {param, T}) -> mk_box(Ind-1, [], T);

%%TODO: Improve these ugly nested cases to check for errors.
foldb(Ind, P, {'as', {'fun',Fun,List}, Alias}) ->
    case foldb(Ind, P, {'fun',Fun,List}) of
        {error, Reason} ->
            {error, Reason};
        B ->
            [BChild] = B#box.children,
            GChildren = BChild#box.children,
            {CF,[CL]} = lists:split(length(GChildren)-1,GChildren),
            NewGChildren = CF ++ [CL#box{name=list_to_binary([CL#box.name," as ",Alias])}],
            NewChild = BChild#box{children=NewGChildren},
            B#box{children=[NewChild]}
    end;

foldb(Ind, P, {'as', Item, Alias}) ->
    case foldb(Ind, P, Item) of
        {error, Reason} ->
            {error, Reason};
        B ->
            append_alias(B, Alias)
    end;
foldb(Ind, _P, {hints, Hint}) -> mk_box(Ind, [], Hint);
foldb(Ind, _P, {opt, Opt}) -> mk_box(Ind, [], Opt);

foldb(Ind, _P, {where, WC}) ->
    case foldb(Ind+1, where, WC) of
        {error, Reason} ->
            {error, Reason};
        #box{name= <<>>, children=Children} ->
            mk_box(Ind, Children, where);
        Fold ->
            mk_box(Ind, Fold, where)
    end;

foldb(Ind, 'hierarchical query', {'start with', SW}) ->
    case foldb(Ind+1, 'hierarchical query', SW) of
        {error, Reason} ->
            {error, Reason};
        #box{name= <<>>, children=Children} ->
            mk_box(Ind, Children, 'start with');
        Fold ->
            mk_box(Ind, Fold, 'start with')
    end;
foldb(Ind, _P, {prior, L}) ->
    case foldb(Ind, 'prior', L) of
        {error, Reason} ->
            {error, Reason};
        #box{} = Fold ->
            mk_box(Ind-1, [Fold], 'prior')
    end;
foldb(Ind, 'hierarchical query', {'connect by', OptNoCycle, CB}) when is_binary(OptNoCycle) ->
    ConnectBy = case OptNoCycle of
        <<>>    -> 'connect by';
        _       -> 'connect by nocycle'
    end,
    case foldb(Ind+1, 'connect by', CB) of
        {error, Reason} ->
            {error, Reason};
        #box{name= <<>>, children=Children} ->
            mk_box(Ind, Children, ConnectBy);
        Fold ->
            mk_box(Ind, Fold, ConnectBy)
    end;
foldb(Ind, _P, {'hierarchical query', {HQ1, HQ2}}) ->
    HQ1Box = foldb(Ind, 'hierarchical query', HQ1),
    HQ2Box = foldb(Ind, 'hierarchical query', HQ2),
    mk_box(Ind, [HQ1Box, HQ2Box], <<"">>);

foldb(Ind, _P, {having, HC}) ->
    case foldb(Ind+1, having, HC) of
        {error, Reason} -> {error, Reason};
        B -> mk_box(Ind, B, having)
    end;

foldb(Ind, P, {'fun', Fun, [B]}) when is_binary(B) ->
    case (binding(P) =< binding('list')) of
        true ->
            Ch = [mk_clspd_box(Ind+3, [], B)],
            B0 = mk_clspd_box(Ind+2, [], <<"(">>),
            B1 = mk_clspd_box(Ind+2, Ch, <<>>),
            B2 = mk_clspd_box(Ind+2, [], <<")">>),
            mk_box(Ind, mk_clspd_box(Ind+1, [B0,B1,B2], Fun),<<>>);
        false ->
            Ch = [mk_clspd_box(Ind+2, [], B)],
            B0 = mk_clspd_box(Ind+1, [], <<"(">>),
            B1 = mk_clspd_box(Ind+1, Ch, <<>>),
            B2 = mk_clspd_box(Ind+1, [], <<")">>),
            mk_clspd_box(Ind, [B0,B1,B2], Fun)
    end;
foldb(Ind, P, {'fun', Fun, [A,B]}) when is_binary(A), is_binary(B) ->
    case (binding(P) =< binding('list')) of
        true ->
            case foldb_commas([mk_clspd_box(Ind+3, [], A),mk_clspd_box(Ind+3, [], B)]) of
                {error, Reason} -> {error, Reason};
                Ch ->
                    B0 = mk_clspd_box(Ind+2, [], <<"(">>),
                    B1 = mk_clspd_box(Ind+2, Ch, <<>>),
                    B2 = mk_clspd_box(Ind+2, [], <<")">>),
                    mk_box(Ind, mk_clspd_box(Ind+1, [B0,B1,B2], Fun),<<>>)
            end;
        false ->
            case foldb_commas([mk_clspd_box(Ind+2, [], A),mk_clspd_box(Ind+2, [], B)]) of
                {error, Reason} -> {error, Reason};
                Ch ->
                    B0 = mk_clspd_box(Ind+1, [], <<"(">>),
                    B1 = mk_clspd_box(Ind+1, Ch, <<>>),
                    B2 = mk_clspd_box(Ind+1, [], <<")">>),
                    mk_clspd_box(Ind, [B0,B1,B2], Fun)
            end
    end;
foldb(Ind, P, {'fun', Fun, List}) ->
    case (binding(P) =< binding('list')) of
        true ->
            Res = [foldb(Ind+3, 'list', Li) || Li <- List],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ ->
                    case foldb_commas(Res) of
                        {error, Reason} -> {error, Reason};
                        Ch ->
                            B0 = mk_box(Ind+2, [], <<"(">>),
                            B1 = mk_box(Ind+2, Ch, <<>>),
                            B2 = mk_box(Ind+2, [], <<")">>),
                            mk_box(Ind, mk_box(Ind+1, [B0,B1,B2], Fun),<<>>)
                    end
            end;
        false ->
            Res = [foldb(Ind+2, 'list', Li) || Li <- List],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ ->
                    case foldb_commas(Res) of
                        {error, Reason} -> {error, Reason};
                        Ch ->
                            B0 = mk_box(Ind+1, [], <<"(">>),
                            B1 = mk_box(Ind+1, Ch, <<>>),
                            B2 = mk_box(Ind+1, [], <<")">>),
                            mk_box(Ind, [B0,B1,B2], Fun)
                    end
            end
    end;
foldb(Ind, P, {OP, Fun, List}) when OP=='{}';OP=='[]' ->
    case (binding(P) =< binding('list')) of
        true ->
            Res = [foldb(Ind+3, 'list', Li) || Li <- List],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ ->
                    case foldb_commas(Res) of
                        {error, Reason} -> {error, Reason};
                        Ch ->
                            B0 = mk_box(Ind+2, [], char1(OP)),
                            B1 = mk_box(Ind+2, Ch, <<>>),
                            B2 = mk_box(Ind+2, [], char2(OP)),
                            mk_box(Ind, mk_box(Ind+1, [B0,B1,B2], Fun),<<>>)
                    end
            end;
        false ->
            Res = [foldb(Ind+2, 'list', Li) || Li <- List],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ ->
                    case foldb_commas(Res) of
                        {error, Reason} -> {error, Reason};
                        Ch ->
                            B0 = mk_box(Ind+1, [], char1(OP)),
                            B1 = mk_box(Ind+1, Ch, <<>>),
                            B2 = mk_box(Ind+1, [], char2(OP)),
                            mk_box(Ind, [B0,B1,B2], Fun)
                    end
            end
    end;
foldb(Ind, P, {'#', R, L}) when is_binary(L), is_binary(R) ->
    case (binding(P) =< binding('list')) of
        true ->
            mk_box(Ind, [mk_clspd_box(Ind+1, [], L), mk_clspd_box(Ind+1, [], '#'), mk_clspd_box(Ind+1, [], R)], <<>>);
        false ->
            [mk_clspd_box(Ind, [], L), mk_clspd_box(Ind, [], '#'), mk_clspd_box(Ind, [], R)]
    end;
foldb(Ind, P, {':', R, L}) when is_binary(L), is_binary(R) ->
    case (binding(P) =< binding('list')) of
        true ->
            mk_box(Ind, [mk_clspd_box(Ind+1, [], L), mk_clspd_box(Ind+1, [], ':'), mk_clspd_box(Ind+1, [], R)], <<>>);
        false ->
            [mk_clspd_box(Ind, [], L), mk_clspd_box(Ind, [], ':'), mk_clspd_box(Ind, [], R)]
    end;
foldb(Ind, _P, {fields, List}) when is_list(List) ->    %% Sli from, 'group by', 'order by'
    Res = [foldb(Ind+1, fields, Li) || Li <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case foldb_commas(Res) of
                {error, Reason} -> {error, Reason};
                Ch -> mk_box(Ind, Ch, bStr(<<>>))
            end
    end;
foldb(Ind, _P, {list, List}) when is_list(List) ->
    Res = [foldb(Ind, list, Li) || Li <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case foldb_commas(Res) of
                {error, Reason} -> {error, Reason};
                Ch -> mk_box(Ind, Ch, <<>>)
            end
    end;
foldb(Ind, _P, {'||', List}) when is_list(List) ->
    Res = [foldb(Ind, '||', Li) || Li <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case foldb_concat(Res) of
                {error, Reason} -> {error, Reason};
                Ch -> mk_box(Ind, Ch, <<>>)
            end
    end;
foldb(Ind, _P, {{'fun', Fun, List}, Dir}) when is_binary(Dir) ->
    case foldb(Ind, 'order by', {'fun',Fun,List}) of
        {error, Reason} -> {error, Reason};
        B ->
            [BChild] = B#box.children,
            GChildren = BChild#box.children,
            {CF,[CL]} = lists:split(length(GChildren)-1,GChildren),
            NewGChildren = CF ++ [CL#box{name=list_to_binary([CL#box.name," ",Dir])}],
            NewChild = BChild#box{children=NewGChildren},
            B#box{children=[NewChild]}
    end;
foldb(Ind, _P, {Item, Dir}) when is_binary(Dir) ->
    case foldb(Ind, 'order by', Item) of
        {error, Reason} -> {error, Reason};
        B ->
            case B#box.children of
                [] ->   B#box{name=list_to_binary([B#box.name," ",Dir])};
                Ch ->   {CF,[CL]} = lists:split(length(Ch)-1,Ch),
                        EChildren = CF ++ [CL#box{name=list_to_binary([CL#box.name," ",Dir])}],
                        B#box{children=EChildren}
            end
    end;
foldb(Ind, _P, {Sli, List}) when is_list(List) ->       %% Sli from, 'group by', 'order by'
    Res = lists:flatten([foldb(Ind+1, Sli, Li) || Li <- List]),
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case neItems(Res) of
                [] -> empty;
                Ch ->
                    case foldb_commas(Ch) of
                        {error, Reason} -> {error, Reason};
                        ChCommas -> mk_box(Ind, ChCommas, Sli)
                    end
            end
    end;
foldb(Ind, P, {Op, R}) when is_atom(Op), is_tuple(R) ->
    Bo = binding(Op),
    Br = binding(R),
    case (binding(P) =< binding('list')) of
        true ->
            case foldb(Ind+1, P, Op) of
                {error, Reason} -> {error, Reason};
                Fl ->
                    if
                        Bo > Br ->
                            case foldb(Ind+3, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    R0 = mk_box(Ind+2, [], <<"(">>),
                                    R1 = mk_box(Ind+2, FR1, <<>>),
                                    R2 = mk_box(Ind+2, [], <<")">>),
                                    Fr = mk_box(Ind+1, [R0,R1,R2], <<>>),
                                    mk_box(Ind, lists:flatten([Fl, Fr]), <<>>)
                            end;
                        Bo == Br ->
                            case foldb(Ind+1, Op, R) of
                                {error, Reason} -> {error, Reason};
                                Fr -> mk_box(Ind, lists:flatten([Fl, Fr]), <<>>)
                            end;
                        true ->
                            case foldb(Ind+2, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    Fr = mk_box(Ind+1, FR1, <<>>),
                                    mk_box(Ind, lists:flatten([Fl, Fr]), <<>>)
                            end
                    end
            end;
        false ->
            case foldb(Ind, P, Op) of
                {error, Reason} -> {error, Reason};
                Fl ->
                    if
                        Bo > Br ->
                            case foldb(Ind+2, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    R0 = mk_box(Ind+1, [], <<"(">>),
                                    R1 = mk_box(Ind+1, FR1, <<>>),
                                    R2 = mk_box(Ind+1, [], <<")">>),
                                    Fr = mk_box(Ind, [R0,R1,R2], <<>>),
                                    lists:flatten([Fl, Fr])
                            end;
                        Bo == Br ->
                            case foldb(Ind, Op, R) of
                                {error, Reason} -> {error, Reason};
                                Fr -> lists:flatten([Fl, Fr])
                            end;
                        true ->
                            case foldb(Ind+1, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    Fr = mk_box(Ind, FR1, <<>>),
                                    lists:flatten([Fl, Fr])
                            end
                    end
            end
    end;

foldb(Ind, P, {Op, R}) when is_atom(Op), is_binary(R) ->
    case (binding(P) =< binding('list')) of
        true ->
            case foldb(Ind+1, P, Op) of
                {error, Reason} -> {error, Reason};
                Fl ->
                    case foldb(Ind+1, Op, R) of
                        {error, Reason} -> {error, Reason};
                        Fr ->
                            mk_box(Ind, [Fl, Fr], <<>>)
                    end
            end;
        false ->
            Res = [foldb(Ind, P, Op), foldb(Ind, Op, R)],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ -> Res
            end
    end;

foldb(Ind, P, {Op, L, R}) when is_atom(Op), is_tuple(L), is_tuple(R) ->
    Bo = binding(Op),
    Bl = binding(L),
    Br = binding(R),
    case (binding(P) =< binding('list')) of
        true ->
            case foldb(Ind+1, P, Op) of
                {error, Reason} -> {error, Reason};
                Fm ->
                    if
                        Bo > Bl ->
                            case foldb(Ind+3, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    L0 = mk_box(Ind+2, [], <<"(">>),
                                    L1 = mk_box(Ind+2, FL1, <<>>),
                                    L2 = mk_box(Ind+2, [], <<")">>),
                                    Fl = mk_box(Ind+1, lists:flatten([L0,L1,L2]), <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+3, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = mk_box(Ind+2, [], <<"(">>),
                                                    R1 = mk_box(Ind+2, FR1, <<>>),
                                                    R2 = mk_box(Ind+2, [], <<")">>),
                                                    Fr = mk_box(Ind+1, [R0,R1,R2], <<>>),
                                                    mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        true ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = mk_box(Ind+1, FR1, <<>>),
                                                    mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end
                                    end
                            end;
                        Bo == Bl ->
                            case foldb(Ind+1, Op, L) of
                                {error, Reason} -> {error, Reason};
                                Fl ->
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+3, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = mk_box(Ind+2, [], <<"(">>),
                                                    R1 = mk_box(Ind+2, FR1, <<>>),
                                                    R2 = mk_box(Ind+2, [], <<")">>),
                                                    Fr = mk_box(Ind+1, [R0,R1,R2], <<>>),
                                                    mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        true ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = mk_box(Ind+1, FR1, <<>>),
                                                    mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end
                                    end
                            end;
                        true ->
                            case foldb(Ind+2, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FlM ->
                                    Fl = mk_box(Ind+1, FlM, <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+3, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = mk_box(Ind+2, [], <<"(">>),
                                                    R1 = mk_box(Ind+2, FR1, <<>>),
                                                    R2 = mk_box(Ind+2, [], <<")">>),
                                                    Fr = mk_box(Ind+1, [R0,R1,R2], <<>>),
                                                    mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        true ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = mk_box(Ind+1, FR1, <<>>),
                                                    mk_box(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end
                                    end
                            end
                    end
            end;
        false ->
            case foldb(Ind, P, Op) of
                {error, Reason} -> {error, Reason};
                FPOP ->
                    if
                        Bo > Bl ->
                            case foldb(Ind+2, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    L0 = mk_box(Ind+1, [], <<"(">>),
                                    L1 = mk_box(Ind+1, FL1, <<>>),
                                    L2 = mk_box(Ind+1, [], <<")">>),
                                    Fl = mk_box(Ind, [L0,L1,L2], <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = mk_box(Ind+1, [], <<"(">>),
                                                    R1 = mk_box(Ind+1, FR1, <<>>),
                                                    R2 = mk_box(Ind+1, [], <<")">>),
                                                    Fr = mk_box(Ind, [R0,R1,R2], <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> lists:flatten([Fl, FPOP, Fr])
                                            end;
                                        true ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = mk_box(Ind, FR1, <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end
                                    end
                            end;
                        Bo == Bl ->
                            case foldb(Ind, Op, L) of
                                {error, Reason} -> {error, Reason};
                                Fl ->
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = mk_box(Ind+1, [], <<"(">>),
                                                    R1 = mk_box(Ind+1, FR1, <<>>),
                                                    R2 = mk_box(Ind+1, [], <<")">>),
                                                    Fr = mk_box(Ind, [R0,R1,R2], <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> lists:flatten([Fl, FPOP, Fr])
                                            end;
                                        true ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = mk_box(Ind, FR1, <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end
                                    end
                            end;
                        true ->
                            case foldb(Ind+1, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FlM ->
                                    Fl = mk_box(Ind, FlM, <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = mk_box(Ind+1, [], <<"(">>),
                                                    R1 = mk_box(Ind+1, FR1, <<>>),
                                                    R2 = mk_box(Ind+1, [], <<")">>),
                                                    Fr = mk_box(Ind, [R0,R1,R2], <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> lists:flatten([Fl, FPOP, Fr])
                                            end;
                                        true ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = mk_box(Ind, FR1, <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end
                                    end
                            end
                    end
            end
    end;

foldb(Ind, P, {Op, L, R}) when is_atom(Op), is_binary(L), is_tuple(R) ->
    Bo = binding(Op),
    Br = binding(R),
    case (binding(P) =< binding('list')) of
        true ->
            Res = [foldb(Ind+1, Op, L), foldb(Ind+1, P, Op)],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ ->
                    if
                        Bo > Br ->
                            case foldb(Ind+3, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    R0 = mk_box(Ind+2, [], <<"(">>),
                                    R1 = mk_box(Ind+2, FR1, <<>>),
                                    R2 = mk_box(Ind+2, [], <<")">>),
                                    Fr = mk_box(Ind+1, [R0,R1,R2], <<>>),
                                    mk_box(Ind, lists:flatten([Res, Fr]), <<>>)
                            end;
                        Bo == Br ->
                            case foldb(Ind+1, Op, R) of
                                {error, Reason} -> {error, Reason};
                                Fr -> mk_box(Ind, lists:flatten([Res, Fr]), <<>>)
                            end;
                        true ->
                            case foldb(Ind+2, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    Fr = mk_box(Ind+1, FR1, <<>>),
                                    mk_box(Ind, lists:flatten([Res, Fr]), <<>>)
                            end
                    end
            end;
        false ->
            Res = [foldb(Ind, Op, L), foldb(Ind, P, Op)],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _ ->
                    if
                        Bo > Br ->
                            case foldb(Ind+2, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    R0 = mk_box(Ind+1, [], <<"(">>),
                                    R1 = mk_box(Ind+1, FR1, <<>>),
                                    R2 = mk_box(Ind+1, [], <<")">>),
                                    Fr = mk_box(Ind, [R0,R1,R2], <<>>),
                                    lists:flatten([Res, Fr])
                            end;
                        Bo == Br ->
                            case foldb(Ind, Op, R) of
                                {error, Reason} -> {error, Reason};
                                Fr -> lists:flatten([Res, Fr])
                            end;
                        true ->
                            case foldb(Ind+1, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    Fr = mk_box(Ind, FR1, <<>>),
                                    lists:flatten([Res, Fr])
                            end
                    end
            end
    end;

foldb(Ind, P, {Op, L, R}) when is_atom(Op), is_tuple(L), is_binary(R) ->
    Bo = binding(Op),
    Bl = binding(L),
    case (binding(P) =< binding('list')) of
        true ->
            Res = [foldb(Ind+1, P, Op), foldb(Ind+1, Op, R)],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _->
                    if
                        Bo > Bl ->
                            case foldb(Ind+3, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    L0 = mk_box(Ind+2, [], <<"(">>),
                                    L1 = mk_box(Ind+2, FL1, <<>>),
                                    L2 = mk_box(Ind+2, [], <<")">>),
                                    Fl = mk_box(Ind+1, [L0,L1,L2], <<>>),
                                    mk_box(Ind, lists:flatten([Fl, Res]), <<>>)
                            end;
                        Bo == Bl ->
                            case foldb(Ind+1, Op, L) of
                                {error, Reason} -> {error, Reason};
                                Fl -> mk_box(Ind, lists:flatten([Fl, Res]), <<>>)
                            end;
                        true ->
                            case foldb(Ind+2, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    Fl = mk_box(Ind+1, FL1, <<>>),
                                    mk_box(Ind, lists:flatten([Fl, Res]), <<>>)
                            end
                    end
            end;
        false ->
            Res = [foldb(Ind, P, Op),foldb(Ind, Op, R)],
            case check_error(Res) of
                {error, Reason} -> {error, Reason};
                _->
                    if
                        Bo > Bl ->
                            case foldb(Ind+2, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    L0 = mk_box(Ind+1, [], <<"(">>),
                                    L1 = mk_box(Ind+1, FL1, <<>>),
                                    L2 = mk_box(Ind+1, [], <<")">>),
                                    Fl = mk_box(Ind, [L0,L1,L2], <<>>),
                                    lists:flatten([Fl, Res])
                            end;
                        Bo == Bl ->
                            case foldb(Ind, Op, L) of
                                {error, Reason} -> {error, Reason};
                                Fl -> lists:flatten([Fl, Res])
                            end;
                        true ->
                            case foldb(Ind+1, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    Fl = mk_box(Ind, FL1, <<>>),
                                    lists:flatten([Fl, Res])
                            end
                    end
            end
    end;

foldb(Ind, P, {Op, L, R}) when is_atom(Op), is_binary(L), is_binary(R) ->
    case (binding(P) =< binding('list')) of
        true ->     mk_box(Ind, [mk_clspd_box(Ind+1, [], L), mk_clspd_box(Ind+1, [], Op), mk_clspd_box(Ind+1, [], R)], <<>>);
        false ->    [mk_clspd_box(Ind, [], L), mk_clspd_box(Ind, [], Op), mk_clspd_box(Ind, [], R)]
    end;

% REVISIT: Indentation level reduced for heirarchy
foldb(Ind, P, {like, L, R, Escape}) when is_binary(L) ->
    RBoxed = if is_binary(R) -> mk_clspd_box(Ind, [], R); true -> foldb(Ind, P, R) end,
    case (binding(P) =< binding(like)) of
        true ->
            mk_box(Ind, [mk_clspd_box(Ind, [], L), mk_clspd_box(Ind, [], like), RBoxed |
                case Escape of
                    <<>> -> [];
                    _ -> [mk_box(Ind, [], 'escape'), mk_box(Ind, [], Escape)]
                end
            ], <<>>);
        false ->
            [mk_clspd_box(Ind, [], L), mk_clspd_box(Ind, [], like), RBoxed |
                case Escape of
                    <<>> -> [];
                    _ -> [mk_box(Ind, [], 'escape'), mk_box(Ind, [], Escape)]
                end
            ]
    end;

foldb(Ind, P, {like, L, R, _Escape})  ->
    foldb(Ind, P, {like, L, R});            %% ToDo: support for like with escape 

foldb(Ind, P, {between, A, B, C}) ->
    Childern = case {A, B, C} of
        {A,B,C} when is_binary(A), is_binary(B), is_binary(C) ->
            [ mk_clspd_box(Ind, [], A)
            , mk_clspd_box(Ind, [], between)
            , mk_clspd_box(Ind, [], B)
            , mk_clspd_box(Ind, [], 'and')
            , mk_clspd_box(Ind, [], C)];
        _->
            [ foldb(Ind, P, A)
            , mk_clspd_box(Ind, [], between)
            , foldb(Ind, P, B)
            , mk_clspd_box(Ind, [], 'and')
            , foldb(Ind, P, C)]
    end,
    case (binding(P) =< binding(between)) of
        true    -> mk_box(Ind, Childern, <<>>);
        false   -> Childern
    end;

foldb(_Ind, _P, Term) ->    
    % ?Debug("Unrecognized parse tree term ~p in foldb under parent ~p~n", [Term, _P]),
    {error, iolist_to_binary(io_lib:format("Unrecognized parse tree term ~p in foldb", [Term]))}.

-spec mk_clspd_box(integer(), tuple() | list(), binary()) -> #box{}.
mk_clspd_box(Ind, Child, Name) when is_record(Child, box) ->
    mk_clspd_box(Ind, [Child], Name);
mk_clspd_box(Ind, Children, Name) ->
    #box{ind = Ind, collapsed = true, children = Children, name = bStr(Name)}.

-spec mk_box(integer(), tuple() | list(), binary() | atom()) -> #box{}.
mk_box(Ind, Child, Name) when is_record(Child, box) ->
    mk_box(Ind, [Child], Name);
mk_box(Ind, Children, Name) when (Ind >= ?DefCollInd) ->
    #box{ind = Ind, collapsed = true, children = Children, name = bStr(Name)};
mk_box(Ind, [L,Op,R], Name) ->
    ON = bStr(Op#box.name), 
    Coll = case lists:member(ON,[<<"=">>,<<"<>">>,<<">">>,<<"<">>,<<">=">>,<<"<=">>,<<"in">>,<<"is">>,<<"like">>]) of
        false ->    false;
        true ->     false % (lists:sum([byte_size(flat_from_box(Ch)) || Ch <- [L,Op,R]]) < ?DefCollLen)
    end,
    #box{ind = Ind, collapsed = Coll, children = [L,Op,R], name = bStr(Name)};
mk_box(Ind, Children, Name) ->
    #box{ind = Ind, collapsed = false, children = Children, name = bStr(Name)}.

-spec foldb_commas([#box{}]) -> list() | {error, binary()}.
foldb_commas(Boxes) when is_list(Boxes) ->
    validate_children(Boxes),
    Comma = (hd(Boxes))#box{children=[],name=bStr(<<",">>)},
    [_|Result] = lists:flatten([[Comma,B] || B <- Boxes]),
    Result;
foldb_commas(Boxes) ->
    ?Error("Invalid list for folding in commas ~p", [Boxes]),
    {error, <<"Invalid list for folding in commas">>}.

-spec foldb_concat([#box{}]) -> list() | {error, binary()}.
foldb_concat(Boxes) when is_list(Boxes) ->
    validate_children(Boxes),
    Comma = (hd(Boxes))#box{children=[],name=bStr(<<"||">>)},
    [_|Result] = lists:flatten([[Comma,B] || B <- Boxes]),
    Result;
foldb_concat(Boxes) ->
    ?Error("Invalid list for folding in '||' ~p", [Boxes]),
    {error, <<"Invalid list for folding in '||'">>}.

-spec append_alias(#box{}, binary()) -> #box{}.
append_alias(#box{children = []} = B, Alias) ->
    B#box{name=list_to_binary([B#box.name," as ",Alias])};
append_alias(#box{children = Ch} = B, Alias) ->
    {CF,[CL]} = lists:split(length(Ch)-1,Ch),
    EChildren = CF ++ [append_alias(CL, Alias)],
    B#box{children=EChildren}.

-spec check_error(list()) -> {error, binary()} | no_errors.
check_error([]) -> no_errors;
check_error([{error, Reason} | _Rest]) -> {error, Reason};
check_error([_ | Rest]) -> check_error(Rest).

char1('{}') -> <<"{">>;
char1('[]') -> <<"[">>.

char2('{}') -> <<"}">>;
char2('[]') -> <<"]">>.

-spec box_to_json(#box{}) -> [{binary(), term()}].
box_to_json(Box) ->
    [ {<<"ind">>, Box#box.ind}
    , {<<"name">>, any_to_bin(Box#box.name)}
    , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
    , {<<"collapsed">>, Box#box.collapsed}
    , {<<"error">>, Box#box.error}
    , {<<"color">>, Box#box.color}
    , {<<"pick">>, Box#box.pick}].

-spec wrap_multiple_json(list()) -> list().
wrap_multiple_json(Children) ->
    Default = #box{},
    [ {<<"ind">>, 0}
    , {<<"name">>, any_to_bin(Default#box.name)}
    , {<<"children">>, Children}
    , {<<"collapsed">>, false}
    , {<<"error">>, Default#box.error}
    , {<<"color">>, Default#box.color}
    , {<<"pick">>, Default#box.pick}].

-spec any_to_bin(term()) -> binary().
any_to_bin(C) when is_list(C) -> list_to_binary(C);
any_to_bin(C) when is_binary(C) -> C;
any_to_bin(C) -> list_to_binary(lists:nth(1, io_lib:format("~p", [C]))).

-ifdef(TEST).

-ifdef(DefCollInd).
-undef(DefCollInd).
-define(DefCollInd,6).
-endif.

%% TESTS ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

setup() -> ok.

teardown(_) -> ok.

sql_box_test_() ->
    {timeout, 30000,
        {
            setup,
            fun setup/0,
            fun teardown/1,
            {with,
                [
                fun test_sqlb/1
                ]
            }
        }
    }.

wait_processes(LoggerPid, Pids) ->
    case [P || P <- Pids, erlang:process_info(P) =/= undefined] of
        [] ->
            LoggerPid ! done,
            timer:sleep(10),
            case erlang:process_info(LoggerPid) of
                undefined -> ok;
                _ ->
                    timer:sleep(10),
                    wait_processes(LoggerPid, [])
            end;
        RestPids ->
            timer:sleep(1),
            wait_processes(LoggerPid, RestPids)
    end.

test_sqlb(_) ->
    io:format(user, "=================================~n", []),
    io:format(user, "|     S Q L  B O X  T E S T     |~n", []),
    io:format(user, "=================================~n", []),
    LoggerPid = spawn(fun() -> log_process([]) end),
    Pids = [spawn(fun() ->
        Log = sqlb_loop(false, Sql),
        LoggerPid ! {log, Idx, Log}
    end) || {Idx, Sql} <- lists:zip(lists:seq(1, length(?TEST_SELECT_QUERIES)), ?TEST_SELECT_QUERIES)],
    io:format(user, "Number of testing processes ~p~n", [length(Pids)]),
    wait_processes(LoggerPid, Pids).

log_process(Logs) ->
    receive
        {log, Idx, Log} ->
            log_process([{Idx, Log}|Logs]);
        done ->
            lists:map(fun({Idx, Log}) ->
                        io:format(user, "[~p]===============================~n", [Idx]),
                        io:format(user, "~s~n", [lists:flatten(Log)])
                      end
                      , lists:sort(Logs))
    end.

sqlb_loop(PrintParseTree, Sql) ->
    case re:run(Sql, "select", [global, {capture, all, list}, caseless]) of
        nomatch ->  "";
        _ ->
            try
                Log1 = io_lib:format("Orig:~n~s~n", [Sql]),
                {ok, [{ParseTree,_}|_]} = sqlparse:parsetree(Sql),
                Log2 = Log1 ++
                if PrintParseTree =:= true ->
                    print_parse_tree(ParseTree);
                true -> ""
                end,
                SqlBox = foldb(ParseTree),
                ?assertMatch(#box{ind=0},SqlBox),
                ?assert(is_list(validate_box(SqlBox))),
                FlatSql = (catch flat_from_box(SqlBox)),
                Log3 = Log2 ++ io_lib:format("Flat:~n~s~n", [FlatSql]),
                {ok, [{FlatSqlParseTree,_}|_]} = sqlparse:parsetree(FlatSql),
                ?assertEqual(ParseTree, FlatSqlParseTree),
                PrettySqlExp = (catch pretty_from_box_exp(SqlBox)),
                Log4 = Log3 ++ io_lib:format("Pretty:~n~s~n", [PrettySqlExp]),
                {ok, [{PrettySqlParseTree,_}|_]} = sqlparse:parsetree(PrettySqlExp),
                ?assertEqual(ParseTree, PrettySqlParseTree),
                CleanSql = clean(Sql),
                CleanPrettySqlExp = clean(PrettySqlExp),
                Log5 = case str_diff(CleanSql,CleanPrettySqlExp) of
                    same -> Log4;
                    Diff ->
                        Log4 ++ io_lib:format("Sql Difference: ~p~n", [Diff])
                end,
                ?assertEqual(CleanSql,CleanPrettySqlExp),
                Log5
                %print_box(SqlBox),
                %io:format(user, "~s~n", [jsx:prettify(jsx:encode(box_to_json(SqlBox)))])
            catch
                Class:Reason ->
                    io_lib:format("Exception ~p:~p~n~p~n", [Class, Reason, erlang:get_stacktrace()])
            end
    end.

validate_box([]) -> ok;
validate_box(#box{children=[]}) -> ok;
validate_box(#box{children=CH}) ->
    validate_children(CH),
    [validate_box(C) || C <-CH];
validate_box([Box|Boxes]) ->
    validate_box(Box),
    validate_box(Boxes).

pretty_from_box_exp([]) -> "";
pretty_from_box_exp(#box{ind=Ind,name=Name,children=[]}) ->
    lists:flatten(["\r\n",lists:duplicate(Ind,9),binary_to_list(Name)]);
pretty_from_box_exp(#box{name= <<>>,children=CH}) ->
    lists:flatten([[pretty_from_box_exp(C) || C <-CH]]);
pretty_from_box_exp(#box{ind=Ind,name=Name,children=CH}) ->
    lists:flatten(["\r\n",lists:duplicate(Ind,9),binary_to_list(Name),[pretty_from_box_exp(C) || C <-CH]]);
pretty_from_box_exp([Box|Boxes]) ->
    lists:flatten([pretty_from_box_exp(Box),pretty_from_box_exp(Boxes)]).

print_parse_tree(ParseTree) -> io_lib:format("ParseTree:~n~p~n~n", [ParseTree]).

% print_box(_Box) ->
%     io:format(user, "~p~n~n", [_Box]),
%     ok.

-define(REG_COL, [
    {"(--.*[\n\r]+)",                             " "}    % comments                      -> removed
  , {"([\n\r\t ]+)",                              " "}    % \r,\n or spaces               -> single space
  , {"(^[ ]+)|([ ]+$)",                           ""}     % leading or trailing spaces    -> removed
  , {"([ ]*)([\(\),])([ ]*)",                     "\\2"}  % spaces before or after ( or ) -> removed
  , {"([ ]*)([\\/\\*\\+\\-\\=\\<\\>]+)([ ]*)",    "\\2"}  % spaces around math operators  -> removed
% , {"([\)])([ ]*)",                              "\\1 "} % no space after )              -> added one space
]).

-define(REG_CR, [
    {"(\r)",                        ""}     % all carriage returns      -> removed
]).

-define(REG_NL, [
    {"(^[\r\n]+)",                  ""}     % leading newline           -> removed
  , {"([\r\n]+$)",                  ""}     % trailing newline          -> removed
]).

str_diff([], [])                                            -> same;
str_diff(String1, []) when length(String1) > 0              -> {String1, ""};
str_diff([], String2) when length(String2) > 0              -> {"", String2};
str_diff([S0|_] = String1, [S1|_] = String2) when S0 =/= S1 -> {String1, String2};
str_diff([_|String1], [_|String2])                          -> str_diff(String1, String2).

%% Child Type Label ------------------------------------------
% ct([]) -> [];
% ct(L) when is_list(L) -> '[_]';
% ct(<<>>) -> <<>>;
% ct({}) -> {};
% ct({_A}) -> '{_}';
% ct({_A,_B}) -> '{_,_}';
% ct({_A,_B,_C}) -> '{_,_,_}';
% ct(A) when is_atom(A) -> A;
% ct(B) when is_binary(B) -> B;
% ct(X) -> X.

% collapse(Sql) ->
%     lists:foldl(
%         fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
%         Sql,
%         ?REG_COL).

clean(Sql) when is_binary(Sql) ->
    trim_nl(clean_cr(string:to_lower(binary_to_list(Sql))));
clean(Sql) ->
    trim_nl(clean_cr(string:to_lower(Sql))).

clean_cr(Sql) ->
    lists:foldl(
        fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
        Sql,
        ?REG_CR).

trim_nl(Sql) ->
    lists:foldl(
        fun({Re,R}, S) -> re:replace(S, Re, R, [{return, list}, global]) end,
        Sql,
        ?REG_NL).
-endif. % TEST
