-record(accounts, { user
                  , password
                  , db_connections
                  , db_files
                  }
       ).

-record(db_connection, { name = ""
                       , adapter
                       , ip
                       , port
                       , service
                       , type
                       , user
                       , password
                       , tns
                       }
       ).

-record(common,   { adapter
                  , db_files
                  }
       ).

-record(file,     {name = ""
                  , content = ""
                  , posX = -1
                  , posY = -1
                  , width = -1
                  , height = -1
                  }
       ).

-define(DEFAULT_ROW_SIZE, 150).

%% rr("src/dderl.hrl").
%% F = fun(User) ->
%%     [Acc|_]=imem_if:read(accounts, User),
%%     {struct, ConsList} = mochijson:decode(Acc#accounts.db_connections),
%%     NewAcc = Acc#accounts{db_connections =
%%         [#db_connection {
%%             name       = N
%%             , adapter  = proplists:get_value("adapter", P, "")
%%             , ip       = proplists:get_value("ip", P, "")
%%             , port     = list_to_integer(proplists:get_value("port", P, "0"))
%%             , service  = proplists:get_value("service", P, "")
%%             , type     = proplists:get_value("type", P, "")
%%             , user     = proplists:get_value("user", P, "")
%%             , password = proplists:get_value("password", P, "")
%%         } || {N, {struct, P}} <- ConsList]},
%%     imem_if:insert_into_table(accounts, NewAcc)
%% end.

