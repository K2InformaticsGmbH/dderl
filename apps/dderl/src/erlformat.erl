-module(erlformat).
-author("bikram.chatterjee@k2informatics.ch").

-export([format/2]).

-define(TAB_SIZE, 4).
-define(NL(__E), if __E > 0 -> "\n"; true -> "" end).
-define(NT(__E,__T), if __E > 0 -> lists:duplicate(?TAB_SIZE*(__T),32); true -> "" end).
-define(MAX_COMPRESS, 50).

format(String, Expand) ->
    case erl_scan:string(add_dot(binary_to_list(String))) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {error, ErrorInfo} ->
                    {error, ErrorInfo};
                {ok, Term} ->
                    case Expand of
                        auto ->
                            {_Expanded, Result} = expand_auto(Term, 0),
                            iolist_to_binary(Result);
                        0 ->
                            %% In level raw data is returned.
                            iolist_to_binary(io_lib:format("~w", [Term]));
                        _ ->
                            iolist_to_binary(expand(Term, Expand))
                    end
            end;
        {error, ErrorInfo, _} ->
            {error, ErrorInfo}
    end.

expand_auto(Term, _Col) when is_atom(Term);
                             is_integer(Term);
                             is_float(Term)  -> {false, io_lib:format("~w", [Term])};
expand_auto(Term, _Col) when is_binary(Term) -> {false, io_lib:format("~p", [Term])};
expand_auto({First, Second}, Col)            ->
    %TODO: If the first is expanded the second one needs a new line...
    {IsFirstExp, FirstResult} = expand_auto(First, Col + 1),
    FirstFlat = lists:flatten(FirstResult),
    {IsSecondExp, SecondResult} = expand_auto(Second, Col + length(FirstFlat) + 2),
    WasExpanded = IsFirstExp orelse IsSecondExp,
    if
        WasExpanded -> NL = ["\n", add_spaces(Col)];
        true -> NL = ""
    end,
    {WasExpanded, ["{", FirstResult,
     ",", SecondResult,
     NL, ["}"]]};
expand_auto(Term, Col) when is_tuple(Term)  -> expand_elements(tuple_to_list(Term), "{}", Col);
expand_auto(Term, Col) when is_list(Term)   -> 
    case io_lib:printable_list(Term) of
        true -> {false, io_lib:format("~p", [Term])};
        false -> expand_elements(Term, "[]", Col)
    end.

expand_elements([], Brackets, _Col) -> [Brackets];
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

add_commas([], _Col) -> [];
add_commas([Element | Rest], Col) ->
    Result = lists:flatten(get_result(expand_auto(Element, Col+1))),
    [",", Result, add_commas(Rest, Col + length(Result) + 1)].

add_spaces(Count) ->
   lists:duplicate(Count, 32).

get_result({_WasExpanded, Result}) -> Result.

expand(Term, Expand) -> expand(Term, Expand, 0).
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

expand_cons([], Brackets, _Expand, _Tab) -> [Brackets];
expand_cons([Element | Rest], [LB, RB], Expand, Tab) ->
    [[LB], ?NL(Expand), ?NT(Expand,Tab+1),
    [expand(Element,Expand-1,Tab+1)],
    [[?NL(Expand), ?NT(Expand,Tab+1), ",", expand(T,Expand-1,Tab+1)] || T <- Rest]
    ++ ?NL(Expand) ++ ?NT(Expand,Tab), [RB]].

add_dot(Val) ->
    case [lists:last(string:strip(Val))] of
        "." -> Val;
        _ -> Val ++ "."
    end.
