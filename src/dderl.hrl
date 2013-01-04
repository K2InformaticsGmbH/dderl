-include_lib("imem/include/imem_meta.hrl").

-define(DEFAULT_ROW_SIZE, 100).
-record(viewstate, { posX   = 5
                   , posY   = 5
                   , width  = 100
                   , height = 100
       }).

-record(ddAdapter,                          %% DDerl adapter (connect to databases)              
                  { id                      :: atom()             %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , fullName                :: string()           %% displayed in drop down box
                  }
       ). 
-define(ddAdapter, [atom, string]).

-record(ddInterface,                        %% DDerl client interface (connect to ui / applications)               
                  { id                      :: atom()             %% ddjson
                  , fullName                :: string()           %% displayed in drop down box
                  }
       ).
-define(ddInterface, [atom, string]).

-record(ddConn,                             %% DB connection    
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% connection name (mutable)
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , adapter                 ::atom()            %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , access                  ::any()             %% erlang term depending on adapter (e.g. ip+service or tns)
                  , schema                  ::any()             %% erlang term depending on adapter (e.g. name or uri or data root path)
                  }
       ).
-define(ddConn, [integer, string, userid, atom, term, term]).
      
-record(ddCmd,                              %% DB command     
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% command template name (mutable)
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , adapters                ::[atom()]          %% can be used for this list of ddAdap
                  , conns                   ::[ddEntityId()]    %% can be used for this list of dbConn references
                  , command                 ::string()          %% erlang term depending on adapter (e.g. SQL text)
                  , opts                    ::any()             %% command options ()
                  }
       ).
-define(ddCmd, [integer, string, userid, list, list, string, term]).

-record(ddView,                             %% user representation of a db command including rendering parameters
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , name                    ::binary()          %% should default to command name
                  , cmd                     ::ddEntityId()      %% db command id
                  , state                   ::#viewstate{}       %% transparent viewstate (managed by client application)
                  }
       ).
-define(ddView, [integer, atom, userid, string, integer, term]).

-record(ddDash,                             %% user representation of a dashboard (collection of views)
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , name                    ::binary()          %% should default to command name
                  , views                   ::[ddEntityId()]    %% ddView.ids
                  }
       ).
-define(ddDash, [integer, atom, userid, string, list]).
