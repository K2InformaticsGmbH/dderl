Header
"%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email bikram.chatterjee@k2informatics.ch".

Nonterminals
% tuplelist
 tuple
 expr
 op.

Terminals COMPARISON NAME '{' '}' ',' 'and' 'or' 'not'.

%Rootsymbol tuplelist.
Rootsymbol tuple.

%tuplelist -> tuple           : ['$1'].
%tuplelist -> tuple tuplelist : ['$1'|'$2'].
%
%tuple -> '{' op ',' tuple ',' tuple '}'
% : ["{\"cond\":\""++'$2'++ ", \"r_prn\":\"\", "++'$4'++", \"e_prn\":\"\"},"|'$6'].

tuple -> '{' op ',' tuple ',' tuple '}'
: "(" ++ '$4' ++ " " ++ '$2' ++ " " ++ '$6' ++ ")".
%%: "{\"cond\":\""++'$2'++ ", \"r_prn\":\"\", "++'$4'++", \"e_prn\":\"\"},".
% : '$4'++'$6'.

tuple -> '{' op ',' expr            '}'
 : "(" ++ '$2' ++ " "  ++ '$4' ++ ")".
%: ["{\"cond\":\""++'$2'++ ", \"r_prn\":\"\", "++'$4'++", \"e_prn\":\"\"}"].

tuple -> expr
: '$1'.
%: ["{\"cond\":\""++", \"r_prn\":\"\", "++'$1'++", \"e_prn\":\"\"}"].
%: '$1'.

op -> 'and' : "\"and\"".
op -> 'or'  : "\"or\"".
op -> 'not' : "\"not\"".

expr -> '{' COMPARISON ',' NAME ',' NAME '}'
: unwrap('$4') ++ " " ++ unwrap('$2') ++ " " ++ unwrap('$6').
%: "\"exprn\":{\"fld_expr\":\"" ++ unwrap('$4') ++ "\", \"op\":\"" ++ unwrap('$2') ++ "\", \"val_expr\":\"" ++ unwrap('$6') ++ "\"}".

Erlang code.
unwrap({_,_,X}) -> X.
