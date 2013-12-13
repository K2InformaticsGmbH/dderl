-module(dderl_sqlbox).

-export([ pretty_from_pt/1
        , boxed_from_pt/1
        , flat_from_box/1
        , pretty_from_box/1
        , box_to_json/1
        ]).

-include("dderl.hrl").
-include("dderl_sqlbox.hrl").

-ifdef(TEST).
-export([ sqlb_loop/5   %% needed for test interface (sql iterator)
        ]).
-endif.

-define(DefCollInd,3).  % First indentation level which will be collapsed by default

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
    iolist_to_binary([binary_to_list(Name), " "]);
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
pretty_from_box(#box{collapsed=true}=Box) -> flat_from_box(Box);
pretty_from_box(#box{name= <<>>,children=[]}) -> <<>>;
pretty_from_box(#box{ind=Ind,name=Name,children=[]}) ->
    iolist_to_binary([indent(Ind), Name, "\r\n"]);
pretty_from_box(#box{ind=Ind,name= <<>>,children=CH}) ->
    if
        (hd(CH))#box.collapsed ->
            iolist_to_binary([indent(Ind+1),[flat_from_box(C) || C <-CH],"\r\n"]);
        true ->
            iolist_to_binary([[pretty_from_box(C) || C <-CH]])
    end;
pretty_from_box(#box{ind=Ind,name=Name,children=CH}) ->
    if
        (hd(CH))#box.collapsed ->
            iolist_to_binary([indent(Ind),binary_to_list(Name),"\r\n",
                indent(Ind+1),[flat_from_box(C) || C <-CH],"\r\n"]);
        true ->
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
binding('fun') -> 180;
binding('||') -> 175;
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
    ?Error("Expected atom or binary, got ~p", [C]),
    {error, <<"Expected atom or binary">>}.

foldb(ParseTree) ->
    foldb(0, undefined, ParseTree).

foldb(Ind, P, {select, List}) when is_list(List) ->
    Res = [foldb(Ind+1, P, Sli) || Sli <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ -> fb(Ind, neItems(Res), select)
    end;
foldb(_, _P, {hints,<<>>})               -> empty;
foldb(_, _P, {opt,  <<>>})               -> empty;
foldb(_, _P, {into,   _ })               -> empty;
foldb(_, _P, {where,  {}})               -> empty;
foldb(_, _P, {having, {}})               -> empty;
foldb(_, _P, {'hierarchical query', {}}) -> empty;

foldb(Ind, _P, T) when is_binary(T); is_atom(T) -> fb(Ind, [], T);

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
            case B#box.children of
                [] ->   B#box{name=list_to_binary([B#box.name," as ",Alias])};
                Ch ->   {CF,[CL]} = lists:split(length(Ch)-1,Ch),
                        EChildren = CF ++ [CL#box{name=list_to_binary([CL#box.name," as ",Alias])}],
                        B#box{children=EChildren}
            end
    end;
foldb(Ind, _P, {hints, Hint}) -> fb(Ind, [], Hint);
foldb(Ind, _P, {opt, Opt}) -> fb(Ind, [], Opt);

foldb(Ind, _P, {where, WC}) ->
    case foldb(Ind+1, where, WC) of
        {error, Reason} ->
            {error, Reason};
        #box{name= <<>>, children=Children} ->
            fb(Ind, Children, where);
        Fold ->
            fb(Ind, Fold, where)
    end;

foldb(Ind, _P, {having, HC}) ->
    case foldb(Ind+1, having, HC) of
        {error, Reason} -> {error, Reason};
        B -> fb(Ind, B, having)
    end;

foldb(Ind, P, {'fun', Fun, [B]}) when is_binary(B) ->
    case (binding(P) =< binding('list')) of
        true ->
            Ch = [fb_coll(Ind+3, [], B)],
            B0 = fb_coll(Ind+2, [], <<"(">>),
            B1 = fb_coll(Ind+2, Ch, <<>>),
            B2 = fb_coll(Ind+2, [], <<")">>),
            fb(Ind, fb_coll(Ind+1, [B0,B1,B2], Fun),<<>>);
        false ->
            Ch = [fb_coll(Ind+2, [], B)],
            B0 = fb_coll(Ind+1, [], <<"(">>),
            B1 = fb_coll(Ind+1, Ch, <<>>),
            B2 = fb_coll(Ind+1, [], <<")">>),
            fb_coll(Ind, [B0,B1,B2], Fun)
    end;
foldb(Ind, P, {'fun', Fun, [A,B]}) when is_binary(A), is_binary(B) ->
    case (binding(P) =< binding('list')) of
        true ->
            case foldb_commas([fb_coll(Ind+3, [], A),fb_coll(Ind+3, [], B)]) of
                {error, Reason} -> {error, Reason};
                Ch ->
                    B0 = fb_coll(Ind+2, [], <<"(">>),
                    B1 = fb_coll(Ind+2, Ch, <<>>),
                    B2 = fb_coll(Ind+2, [], <<")">>),
                    fb(Ind, fb_coll(Ind+1, [B0,B1,B2], Fun),<<>>)
            end;
        false ->
            case foldb_commas([fb_coll(Ind+2, [], A),fb_coll(Ind+2, [], B)]) of
                {error, Reason} -> {error, Reason};
                Ch ->
                    B0 = fb_coll(Ind+1, [], <<"(">>),
                    B1 = fb_coll(Ind+1, Ch, <<>>),
                    B2 = fb_coll(Ind+1, [], <<")">>),
                    fb_coll(Ind, [B0,B1,B2], Fun)
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
                            B0 = fb(Ind+2, [], <<"(">>),
                            B1 = fb(Ind+2, Ch, <<>>),
                            B2 = fb(Ind+2, [], <<")">>),
                            fb(Ind, fb(Ind+1, [B0,B1,B2], Fun),<<>>)
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
                            B0 = fb(Ind+1, [], <<"(">>),
                            B1 = fb(Ind+1, Ch, <<>>),
                            B2 = fb(Ind+1, [], <<")">>),
                            fb(Ind, [B0,B1,B2], Fun)
                    end
            end
    end;
foldb(Ind, _P, {fields, List}) when is_list(List) ->    %% Sli from, 'group by', 'order by'
    Res = [foldb(Ind+1, fields, Li) || Li <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case foldb_commas(Res) of
                {error, Reason} -> {error, Reason};
                Ch -> fb(Ind, Ch, bStr(<<>>))
            end
    end;
foldb(Ind, _P, {list, List}) when is_list(List) ->
    Res = [foldb(Ind, list, Li) || Li <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case foldb_commas(Res) of
                {error, Reason} -> {error, Reason};
                Ch -> fb(Ind, Ch, <<>>)
            end
    end;
foldb(Ind, _P, {'||', List}) when is_list(List) ->
    Res = [foldb(Ind, '||', Li) || Li <- List],
    case check_error(Res) of
        {error, Reason} -> {error, Reason};
        _ ->
            case foldb_concat(Res) of
                {error, Reason} -> {error, Reason};
                Ch -> fb(Ind, Ch, <<>>)
            end
    end;
foldb(Ind, _P, {Item, Dir}) when is_binary(Dir) ->
    case foldb(Ind, 'order by', Item) of
        {error, Reason} -> {error, Reason};
        B ->
            case B#box.children of
                [] ->   B#box{name=list_to_binary([B#box.name," ",Dir])};
                Ch ->   {CF,[CL]} = lists:split(length(Ch)-1,Ch),
                        EChildren = [CF|[CL#box{name=list_to_binary([CL#box.name," ",Dir])}]],
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
                        ChCommas -> fb(Ind, ChCommas, Sli)
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
                                    R0 = fb(Ind+2, [], <<"(">>),
                                    R1 = fb(Ind+2, FR1, <<>>),
                                    R2 = fb(Ind+2, [], <<")">>),
                                    Fr = fb(Ind+1, [R0,R1,R2], <<>>),
                                    fb(Ind, lists:flatten([Fl, Fr]), <<>>)
                            end;
                        Bo == Br ->
                            case foldb(Ind+1, Op, R) of
                                {error, Reason} -> {error, Reason};
                                Fr -> fb(Ind, lists:flatten([Fl, Fr]), <<>>)
                            end;
                        true ->
                            case foldb(Ind+2, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    Fr = fb(Ind+1, FR1, <<>>),
                                    fb(Ind, lists:flatten([Fl, Fr]), <<>>)
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
                                    R0 = fb(Ind+1, [], <<"(">>),
                                    R1 = fb(Ind+1, FR1, <<>>),
                                    R2 = fb(Ind+1, [], <<")">>),
                                    Fr = fb(Ind, [R0,R1,R2], <<>>),
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
                                    Fr = fb(Ind, FR1, <<>>),
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
                            fb(Ind, [Fl, Fr], <<>>)
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
                                    L0 = fb(Ind+2, [], <<"(">>),
                                    L1 = fb(Ind+2, FL1, <<>>),
                                    L2 = fb(Ind+2, [], <<")">>),
                                    Fl = fb(Ind+1, lists:flatten([L0,L1,L2]), <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+3, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = fb(Ind+2, [], <<"(">>),
                                                    R1 = fb(Ind+2, FR1, <<>>),
                                                    R2 = fb(Ind+2, [], <<")">>),
                                                    Fr = fb(Ind+1, [R0,R1,R2], <<>>),
                                                    fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        true ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = fb(Ind+1, FR1, <<>>),
                                                    fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
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
                                                    R0 = fb(Ind+2, [], <<"(">>),
                                                    R1 = fb(Ind+2, FR1, <<>>),
                                                    R2 = fb(Ind+2, [], <<")">>),
                                                    Fr = fb(Ind+1, [R0,R1,R2], <<>>),
                                                    fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        true ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = fb(Ind+1, FR1, <<>>),
                                                    fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end
                                    end
                            end;
                        true ->
                            case foldb(Ind+2, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FlM ->
                                    Fl = fb(Ind+1, FlM, <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+3, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = fb(Ind+2, [], <<"(">>),
                                                    R1 = fb(Ind+2, FR1, <<>>),
                                                    R2 = fb(Ind+2, [], <<")">>),
                                                    Fr = fb(Ind+1, [R0,R1,R2], <<>>),
                                                    fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        Bo == Br ->
                                            case foldb(Ind+1, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                Fr -> fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
                                            end;
                                        true ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    Fr = fb(Ind+1, FR1, <<>>),
                                                    fb(Ind, lists:flatten([Fl,Fm,Fr]), <<>>)
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
                                    L0 = fb(Ind+1, [], <<"(">>),
                                    L1 = fb(Ind+1, FL1, <<>>),
                                    L2 = fb(Ind+1, [], <<")">>),
                                    Fl = fb(Ind, [L0,L1,L2], <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = fb(Ind+1, [], <<"(">>),
                                                    R1 = fb(Ind+1, FR1, <<>>),
                                                    R2 = fb(Ind+1, [], <<")">>),
                                                    Fr = fb(Ind, [R0,R1,R2], <<>>),
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
                                                    Fr = fb(Ind, FR1, <<>>),
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
                                                    R0 = fb(Ind+1, [], <<"(">>),
                                                    R1 = fb(Ind+1, FR1, <<>>),
                                                    R2 = fb(Ind+1, [], <<")">>),
                                                    Fr = fb(Ind, [R0,R1,R2], <<>>),
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
                                                    Fr = fb(Ind, FR1, <<>>),
                                                    lists:flatten([Fl, FPOP, Fr])
                                            end
                                    end
                            end;
                        true ->
                            case foldb(Ind+1, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FlM ->
                                    Fl = fb(Ind, FlM, <<>>),
                                    if
                                        Bo > Br ->
                                            case foldb(Ind+2, Op, R) of
                                                {error, Reason} -> {error, Reason};
                                                FR1 ->
                                                    R0 = fb(Ind+1, [], <<"(">>),
                                                    R1 = fb(Ind+1, FR1, <<>>),
                                                    R2 = fb(Ind+1, [], <<")">>),
                                                    Fr = fb(Ind, [R0,R1,R2], <<>>),
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
                                                    Fr = fb(Ind, FR1, <<>>),
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
                                    R0 = fb(Ind+2, [], <<"(">>),
                                    R1 = fb(Ind+2, FR1, <<>>),
                                    R2 = fb(Ind+2, [], <<")">>),
                                    Fr = fb(Ind+1, [R0,R1,R2], <<>>),
                                    fb(Ind, lists:flatten([Res, Fr]), <<>>)
                            end;
                        Bo == Br ->
                            case foldb(Ind+1, Op, R) of
                                {error, Reason} -> {error, Reason};
                                Fr -> fb(Ind, lists:flatten([Res, Fr]), <<>>)
                            end;
                        true ->
                            case foldb(Ind+2, Op, R) of
                                {error, Reason} -> {error, Reason};
                                FR1 ->
                                    Fr = fb(Ind+1, FR1, <<>>),
                                    fb(Ind, lists:flatten([Res, Fr]), <<>>)
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
                                    R0 = fb(Ind+1, [], <<"(">>),
                                    R1 = fb(Ind+1, FR1, <<>>),
                                    R2 = fb(Ind+1, [], <<")">>),
                                    Fr = fb(Ind, [R0,R1,R2], <<>>),
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
                                    Fr = fb(Ind, FR1, <<>>),
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
                                    L0 = fb(Ind+2, [], <<"(">>),
                                    L1 = fb(Ind+2, FL1, <<>>),
                                    L2 = fb(Ind+2, [], <<")">>),
                                    Fl = fb(Ind+1, [L0,L1,L2], <<>>),
                                    fb(Ind, lists:flatten([Fl, Res]), <<>>)
                            end;
                        Bo == Bl ->
                            case foldb(Ind+1, Op, L) of
                                {error, Reason} -> {error, Reason};
                                Fl -> fb(Ind, lists:flatten([Fl, Res]), <<>>)
                            end;
                        true ->
                            case foldb(Ind+2, Op, L) of
                                {error, Reason} -> {error, Reason};
                                FL1 ->
                                    Fl = fb(Ind+1, FL1, <<>>),
                                    fb(Ind, lists:flatten([Fl, Res]), <<>>)
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
                                    L0 = fb(Ind+1, [], <<"(">>),
                                    L1 = fb(Ind+1, FL1, <<>>),
                                    L2 = fb(Ind+1, [], <<")">>),
                                    Fl = fb(Ind, [L0,L1,L2], <<>>),
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
                                    Fl = fb(Ind, FL1, <<>>),
                                    lists:flatten([Fl, Res])
                            end
                    end
            end
    end;

foldb(Ind, P, {Op, L, R}) when is_atom(Op), is_binary(L), is_binary(R) ->
    case (binding(P) =< binding('list')) of
        true ->     fb(Ind, [fb_coll(Ind+1, [], L), fb_coll(Ind+1, [], Op), fb_coll(Ind+1, [], R)], <<>>);
        false ->    [fb_coll(Ind, [], L), fb_coll(Ind, [], Op), fb_coll(Ind, [], R)]
    end;

foldb(Ind, P, {like, L, R, Escape}) ->
    case (binding(P) =< binding(like)) of
        true ->     fb(Ind, [fb_coll(Ind+1, [], L), fb_coll(Ind+1, [], like), fb_coll(Ind+1, [], R)], <<>>);
        false ->    [fb_coll(Ind, [], L), fb_coll(Ind, [], like), fb_coll(Ind, [], R)]
    end;

foldb(_Ind, P, Term) ->    
    ?Error("Unrecognized parse tree term ~p in foldb under parent ~p~n", [Term, P]),
    {error, iolist_to_binary(io_lib:format("Unrecognized parse tree term ~p in foldb", [Term]))}.

-spec fb_coll(integer(), tuple() | list(), binary()) -> #box{}.
fb_coll(Ind, Child, Name) when is_tuple(Child) ->
    fb_coll(Ind, [Child], Name);
fb_coll(Ind, Children, Name) ->
    #box{ind = Ind, collapsed = true, children = Children, name = bStr(Name)}.

-spec fb(integer(), tuple() | list(), binary() | atom()) -> #box{}.
fb(Ind, Child, Name) when is_tuple(Child) ->
    fb(Ind, [Child], Name);
fb(Ind, Children, Name) ->
    #box{ind = Ind, collapsed = (Ind >= ?DefCollInd), children = Children, name = bStr(Name)}.

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

-spec check_error(list()) -> {error, binary()} | no_errors.
check_error([]) -> no_errors;
check_error([{error, Reason} | _Rest]) -> {error, Reason};
check_error([_ | Rest]) -> check_error(Rest).

-spec box_to_json(#box{}) -> [{binary(), term()}].
box_to_json(Box) ->
    [ {<<"ind">>, Box#box.ind}
    , {<<"name">>, any_to_bin(Box#box.name)}
    , {<<"children">>, [box_to_json(CB) || CB <- Box#box.children]}
    , {<<"collapsed">>, Box#box.collapsed}
    , {<<"error">>, Box#box.error}
    , {<<"color">>, Box#box.color}
    , {<<"pick">>, Box#box.pick}].

-spec any_to_bin(term()) -> binary().
any_to_bin(C) when is_list(C) -> list_to_binary(C);
any_to_bin(C) when is_binary(C) -> C;
any_to_bin(C) -> list_to_binary(lists:nth(1, io_lib:format("~p", [C]))).

-ifdef(TEST).
-include_lib("sqlparse/src/sql_tests.hrl").
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

test_sqlb(_) ->
    io:format(user, "=================================~n", []),
    io:format(user, "|     S Q L  B O X  T E S T     |~n", []),
    io:format(user, "=================================~n", []),
    sql_test:parse_groups(fun ?MODULE:sqlb_loop/5, false).

sqlb_loop(_, [], _, _, Private) -> Private;
sqlb_loop(PrintParseTree, [Sql|Rest], N, Limit, Private) ->
    case re:run(Sql, "select", [global, {capture, all, list}, caseless]) of
        nomatch ->
            sqlb_loop(PrintParseTree, Rest, N+1, Limit, Private);
        _ ->
            io:format(user, "[~p]===============================~n", [N]),
                NewPrivate = try
                    io:format(user, "~p~n", [Sql]),
                    ParseTree = sqlparse:parsetree(Sql),
                    print_parse_tree(ParseTree),
                    % SqlBox = fold_tree(ParseTree, fun sqlb/6, []),
                    SqlBox = foldb(ParseTree),
                    print_box(SqlBox),
                    ?assertMatch(#box{ind=0},SqlBox),
                    ?assert(is_list(validate_box(SqlBox))),
                    FlatSql = (catch flat_from_box(SqlBox)),
                    io:format(user, FlatSql ++ "~n", []),
                    ?assertEqual(ParseTree, sqlparse:parsetree(FlatSql)),
                    PrettySql = (catch pretty_from_box(SqlBox)),
                    io:format(user, PrettySql ++ "~n", []),
                    PrettySqlExp = (catch pretty_from_box_exp(SqlBox)),
                    ?assertEqual(ParseTree, sqlparse:parsetree(PrettySqlExp)),
                    CleanSql = clean(Sql),
                    CleanPrettySqlExp = clean(PrettySqlExp),
                    case str_diff(CleanSql,CleanPrettySqlExp) of
                        same -> ok;
                        Diff ->
                            io:format(user, "Sql Difference: ~p~n", [Diff])
                    end,
                    ?assertEqual(CleanSql,CleanPrettySqlExp),
                    sql_test:update_counters(ParseTree, Private)
                catch
                    Class:Reason ->  io:format(user,"Exception ~p:~p~n~p~n", [Class, Reason, erlang:get_stacktrace()]),
                    ?assert( true == "all tests completed")
                end,
                if
                    (Limit =:= 1) ->
                        NewPrivate;
                    true ->
                        sqlb_loop(PrintParseTree, Rest, N+1, Limit-1, NewPrivate)
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

print_parse_tree(_ParseTree) ->
    io:format(user, "~p~n~n", [_ParseTree]),
    ok.

print_box(_Box) ->
    io:format(user, "~p~n~n", [_Box]),
    ok.

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
