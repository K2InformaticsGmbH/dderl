
-type ddTimestamp() :: 'undefined' | {{integer(), integer(), integer()},{integer(), integer(), integer()}}.

-type ddCredential() :: {pwdmd5, binary(), binary()}.           %% {pwdmd5, ddAccount.id, md5(password)} for now

-type ddEntityId() :: reference() | atom().                    

-record(ddAdapter,                          %% DDerl adapter (connect to databases)              
                  { id                      :: atom()           %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , fullName                :: string()         %% displayed in drop down box
                  }
       ). 

-record(ddInterface,                        %% DDerl client interface (connect to ui / applications)               
                  { id                      :: atom()           %% ddjson
                  , fullName                :: string()         %% displayed in drop down box
                  }
       ).

-record(ddAccount,                          %% DDerl account  (as opposed to database accounts)
                  { id                      ::ddEntityId() 
                  , name                    ::binary()          %% unique login id (mutable)
                  , type='user'             ::atom()            %% user | driver | deamon | application 
                  , credentials             ::[ddCredential()]  
                  , fullName                ::binary()                    
                  , lastLoginTime           ::ddTimestamp()     %% erlang time of last login success
                  , lastFailureTime         ::ddTimestamp()     %% erlang time of last login failure (for existing account name)
                  , lastPasswordChangeTime  ::ddTimestamp()     %% change time (undefined or too old  => must change it now and reconnect)
                  , isLocked='false'        ::'true' | 'false'
                  }
       ).

-record(ddRole,                             %% DDerl hierarchy of roles with permissions and access privileges to connections and commands  
                  { id                      ::ddEntityId()            %% lookup starts with ddAccount.id, other roles are atoms
                  , roles=[]                ::[atom()]                %% granted roles
                  , permissions=[]          ::[atom()]                %% granted permissions
                  , dbConns=[]              ::'all' | [ddEntityId()]  %% granted db connections
                  , dbCmds=[]               ::'all' | [ddEntityId()]  %% granted db commands
                  }
       ). 

-record(dbConn,                             %% DB connection    
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% connection name (mutable)
                  , owner                   ::binary()          %% binary account.name of creator / owner
                  , adapter                 ::atom()            %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , access                  ::any()             %% erlang term depending on adapter (e.g. ip+service or tns)
                  , schema                  ::any()             %% erlang term depending on adapter (e.g. name or uri or data root path)
                  }
       ).
      
-record(dbCmd,                              %% DB command     
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% command template name (mutable)
                  , owner                   ::binary()          %% ddAccount.id
                  , adapters                ::[atom()]          %% can be used for this list of ddAdap
                  , conns                   ::[ddEntityId()]    %% can be used for this list of dbConn references
                  , command                 ::string()          %% erlang term depending on adapter (e.g. SQL text)
                  , opts                    ::any()             %% command options ()
                  }
       ).

-record(ddView,                             %% user representation of a db command including rendering parameters
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% dbAccount.id
                  , name                    ::binary()          %% should default to command name
                  , dbCmdId                 ::ddEntityId()      %% db command id
                  , viewState               ::any()             %% transparent viewstate (managed by client application)
                  }
       ).

-record(ddDash,                             %% user representation of a dashboard (collection of views)
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% dbAccount.id
                  , name                    ::binary()          %% should default to command name
                  , ddViews                 ::[ddEntityId()]    %% ddView.ids
                  }
       ).

-record(ddSession,                          %% user representation of a db command including rendering parameters
                  { id                      ::integer()
                  , pointer                 ::{atom(),pid()}    %% for parameterized access to session process
                  , connectTime             ::ddTimestamp()     %% first http request time
                  , loginTime               ::ddTimestamp()     
                  , accountId               ::ddEntityId()      
                  }
       ).


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

-define(DEFAULT_ROW_SIZE, 100).

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

