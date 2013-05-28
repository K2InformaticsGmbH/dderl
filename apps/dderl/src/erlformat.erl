-module(erlformat).
-author("bikram.chatterjee@k2informatics.ch").

-export([format/2]).

-define(NL(__E), if __E > 0 -> "\n"; true -> "" end).
-define(NT(__E,__T), if __E > 0 -> lists:duplicate(__T,"\t"); true -> "" end).

format(String, Expand) ->
    case erl_scan:string(add_dot(String)) of
        {ok, Tokens, _} ->
			case erl_parse:parse_term(Tokens) of
                {error, ErrorInfo} ->
                    {error, ErrorInfo};
                {ok, Term} ->
                    Parse = erl_parse:abstract(Term),
                    list_to_binary(expand(Parse, Expand))
            end;
		{error, ErrorInfo, _} ->
			{error, ErrorInfo}
	end.

expand(P,E) -> expand(P,E,0).
expand({nil,0},_Expand,_Tab) -> lists:flatten(io_lib:format("~w", [[]]));
expand({atom,_,V},_Expand,_Tab) -> lists:flatten(io_lib:format("~w", [V]));
expand({integer,_,V},_Expand,_Tab) -> lists:flatten(io_lib:format("~w", [V]));
expand({float,_,V},_Expand,_Tab) -> lists:flatten(io_lib:format("~w", [V]));
expand({string,_,V},_Expand,_Tab) -> lists:flatten(io_lib:format("~p", [V]));
expand({bin,_, BinList},Expand,Tab) ->
    "<<" ++
    string:join([expand(BinV,Expand,Tab) || {bin_element,_,BinV,_,_} <- BinList], ",")
    ++ ">>";
expand({cons,_,_,_} = List,Expand,Tab) ->
    "[" ++ ?NL(Expand) ++ ?NT(Expand,Tab+1) ++
    expand_cons(List,Expand,Tab+1)
    ++ ?NL(Expand) ++ ?NT(Expand,Tab) ++ "]";
expand({tuple,_,TupleElms},Expand,Tab) ->
    "{" ++ ?NL(Expand) ++ ?NT(Expand,Tab+1) ++
    string:join([expand(T,Expand-1,Tab+1) || T <- TupleElms], ?NL(Expand) ++ ?NT(Expand,Tab+1) ++ ",")
    ++ ?NL(Expand) ++ ?NT(Expand,Tab) ++ "}";
expand([],_,_) -> "";
expand([P|Parse],Expand,Tab) ->
    expand(P,Expand-1,Tab) ++
    expand(Parse,Expand,Tab).

expand_cons({cons,_,Elm,{nil,_}},Expand,Tab) ->
    expand(Elm,Expand-1,Tab);
expand_cons({cons,_,Elm,Rest},Expand,Tab) ->
    expand(Elm,Expand-1,Tab) ++ ?NL(Expand) ++ ?NT(Expand,Tab) ++ "," ++
    expand_cons(Rest,Expand,Tab).

add_dot(Val) ->
	case [lists:last(string:strip(Val))] of
		"." -> Val;
		_ -> Val ++ "."
	end.
