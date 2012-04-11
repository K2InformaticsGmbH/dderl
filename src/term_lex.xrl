Definitions.

Rules.

('or'|'and'|'not')        : {token, {list_to_atom(string:strip(TokenChars,both,$')), TokenLine}}.
[\{\}\,]                  : {token, {list_to_atom(TokenChars), TokenLine}}.
'(=|<>|<|>|<=|>=)'        : {token, {'COMPARISON', TokenLine, string:strip(TokenChars,both,$')}}.
"[A-Za-z][A-Za-z0-9_]*"   : {token, {'NAME', TokenLen, string:strip(TokenChars,both,$")}}.
([\s\t\r\n]+)             : skip_token.

Erlang code.
