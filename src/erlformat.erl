-module(erlformat).
-author("bikram.chatterjee@k2informatics.ch").

-export([format/3]).

-define(TAB_SIZE, 4).
-define(NL(__E), if __E > 0 -> "\n"; true -> "" end).
-define(NT(__E,__T), if __E > 0 -> lists:duplicate(?TAB_SIZE*(__T),32); true -> "" end).
-define(MAX_COMPRESS, 50).

-spec format(binary(), integer() | auto, boolean()) -> binary() | {error, term()}.
format(<<>>, _Expand, _Force) -> <<>>;
format(String, Expand, false) ->
    case re:run(String, [$\n]) of
        nomatch ->
            %% If we dont have formatting already.
            format(String, Expand);
        _ ->
            %% Else we preserve it.
            String
    end;
format(String, Expand, true) ->
    format(String, Expand).

-spec format(binary(), integer() | auto) -> binary() | {error, term()}.
format(String, Expand) ->
    case erl_scan:string(add_dot(binary_to_list(String))) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {error, _TermError} ->
                    %% Try to format it using pretty_pr in case of expresion.
                    Escaped = escape_quotes(binary_to_list(String)),
                    NewString = lists:flatten(["<<\"", Escaped, "\">>"]),
                    case erl_scan:string(add_dot(NewString)) of
                        {ok, NewTokens, _} ->
                            case erl_parse:parse_term(NewTokens) of
                                {ok, NewBin} ->
                                    case expand_expression(NewBin) of
                                        {ok, Result} ->
                                            iolist_to_binary(Result);
                                        Error ->
                                            Error
                                    end;
                                Error ->
                                    Error
                            end;
                        {error, ErrorInfo, _} ->
                            {error, ErrorInfo}
                    end;
                {ok, <<>>} -> <<"<<>>">>;
                {ok, Term} ->
                    case Expand of
                        0 ->
                            %% In level 0 raw data is returned.
                            iolist_to_binary(io_lib:format("~w", [Term]));
                        _ ->
                            if
                                is_binary(Term) ->
                                    case expand_expression(Term) of
                                        {ok, Result} ->
                                            Escaped = escape_quotes(Result),
                                            iolist_to_binary(["<<\"\n", Escaped, "\n\">>"]);
                                        {error, Bin} ->
                                            Bin
                                    end;
                                true ->
                                    iolist_to_binary(expand(Term, Expand))
                            end
                    end
            end;
        {error, ErrorInfo, _} ->
            {error, ErrorInfo}
    end.

-spec expand_expression(binary()) -> {ok, string()} | {error, binary()}.
expand_expression(BinStr) ->
    AsString = add_dot(binary_to_list(BinStr)),
    case erl_scan:string(AsString) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, ExprList} ->
                    NoComments = erl_syntax:form_list(ExprList),
                    Comments = erl_comment_scan:string(AsString),
                    WithComments = erl_recomment:recomment_forms(NoComments, Comments),
                    {ok, erl_prettypr:format(WithComments)};
                _Error ->
                    {error, iolist_to_binary(io_lib:format("~p", [BinStr]))}
            end;
        _ ->
            {error, iolist_to_binary(io_lib:format("~p", [BinStr]))}
    end.

-spec expand(term(), integer() | auto) -> iolist().
expand(Term, auto) ->
    {_Expanded, Result} = expand_auto(Term, 0),
    Result;
expand(Term, Expand) -> expand(Term, Expand, 0).

-spec expand_auto(term(), integer()) -> {boolean(), iolist()}.
expand_auto(Term, _Col) when is_atom(Term);
                             is_integer(Term);
                             is_float(Term)  -> {false, io_lib:format("~w", [Term])};
expand_auto(Term, _Col) when is_binary(Term) -> {false, io_lib:format("~p", [Term])};
expand_auto(Term, _Col) when is_map(Term)    -> {false, io_lib:format("~p", [Term])};
expand_auto({First, Second}, Col)            ->
    case expand_auto(First, Col+1) of
        {true, FirstResult} ->
            {true, ["{", FirstResult, "\n", add_spaces(Col),
                    ",", get_result(expand_auto(Second, Col+1)),
                    "\n", add_spaces(Col), "}"]};
        {false, FirstResult} ->
            FirstFlat = lists:flatten(FirstResult),
            {WasExpanded, SecondResult} = expand_auto(Second, Col + length(FirstFlat) + 2),
            if
                WasExpanded -> NL = ["\n", add_spaces(Col)];
                true -> NL = ""
            end,
            {WasExpanded, ["{", FirstResult, ",", SecondResult, NL, ["}"]]}
    end;
expand_auto(Term, Col) when is_tuple(Term)  -> expand_elements(tuple_to_list(Term), "{}", Col);
expand_auto(Term, Col) when is_list(Term)   -> 
    case io_lib:printable_list(Term) of
        true -> {false, io_lib:format("~p", [Term])};
        false -> expand_elements(Term, "[]", Col)
    end.

-spec expand_elements(list(), list(), integer()) -> {boolean(), iolist()}.
expand_elements([], Brackets, _Col) -> {false, [Brackets]};
expand_elements([Element | Rest] = Term, [LB, RB], Col) ->
    TextWidth = length(lists:flatten(io_lib:format("~w", [Term]))),
    Result = lists:flatten(get_result(expand_auto(Element, Col+1))),
    if
        TextWidth =< ?MAX_COMPRESS ->
            {false, [[LB], Result, add_commas(Rest, Col + length(Result) + 1), [RB]]};
        true ->
            {true,
            [[LB], Result,
             [["\n", add_spaces(Col), ",", get_result(expand_auto(T, Col+1))] || T <- Rest],
             "\n", add_spaces(Col), [RB]]}
    end.

-spec add_commas(iolist(), integer()) -> iolist().
add_commas([], _Col) -> [];
add_commas(Element, Col) when not is_list(Element) ->
    Result = lists:flatten(get_result(expand_auto(Element, Col+1))),
    ["|", Result];
add_commas([Element | Rest], Col) ->
    Result = lists:flatten(get_result(expand_auto(Element, Col+1))),
    [",", Result, add_commas(Rest, Col + length(Result) + 1)].

-spec add_spaces(integer()) -> [32].
add_spaces(Count) ->
   lists:duplicate(Count, 32).

-spec get_result({boolean(), iolist()}) -> iolist().
get_result({_WasExpanded, Result}) -> Result.

-spec expand(term(), integer(), integer()) -> iolist().
expand(Term, _Expand, _Tab) when is_atom(Term);
                                 is_integer(Term);
                                 is_float(Term)  -> io_lib:format("~w", [Term]);
expand(Term, _Expand, _Tab) when is_binary(Term) -> io_lib:format("~p", [Term]);
expand(Term, Expand, Tab) when is_tuple(Term)    -> expand_cons(tuple_to_list(Term), "{}", Expand, Tab);
expand(Term, Expand, Tab) when is_list(Term)     -> 
    case io_lib:printable_list(Term) of
        true -> io_lib:format("~p", [Term]);
        false -> expand_cons(Term, "[]", Expand, Tab)
    end.

-spec expand_cons(list(), list(), integer(), integer()) -> iolist().
expand_cons([], Brackets, _Expand, _Tab) -> [Brackets];
expand_cons([Element | Rest], [LB, RB], Expand, Tab) ->
    [[LB], ?NL(Expand), ?NT(Expand,Tab+1),
    [expand(Element,Expand-1,Tab+1)],
    [[?NL(Expand), ?NT(Expand,Tab+1), ",", expand(T,Expand-1,Tab+1)] || T <- Rest],
    ?NL(Expand), ?NT(Expand,Tab), [RB]].

-spec add_dot(list()) -> list().
add_dot(Val) ->
    case [lists:last(trim_whitespace(Val))] of
        "." -> Val;
        _ -> Val ++ "."
    end.

-spec escape_quotes(list()) -> list().
escape_quotes([]) -> [];
escape_quotes([$\\, $" | Rest]) ->
    [$\\, $\\, $\\, $" | escape_quotes(Rest)];
escape_quotes([$\\, $\\ | Rest]) ->
    [$\\, $\\, $\\, $\\ | escape_quotes(Rest)];
escape_quotes([$"|Rest]) ->
    [$\\, $" | escape_quotes(Rest)];
escape_quotes([Char|Rest]) ->
    [Char | escape_quotes(Rest)].

-spec trim_whitespace(list()) -> list().
trim_whitespace(Input) ->
    LS = re:replace(Input, "^\\s*", "", [unicode, {return, list}]),
    RS = re:replace(LS, "\\s*$", "", [unicode, {return, list}]),
    RS.
