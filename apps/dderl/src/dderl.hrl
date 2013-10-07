-include_lib("imem/include/imem_meta.hrl").

-define(DEFAULT_ROW_SIZE, 100).
-record(viewstate, { table_layout = []
                   , column_layout = []
       }).

-record(ddAdapter,                          %% DDerl adapter (connect to databases)              
                  { id                      :: atom()             %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , fullName                :: binary()           %% displayed in drop down box
                  }
       ). 
-define(ddAdapter, [atom, binstr]).

-record(ddInterface,                        %% DDerl client interface (connect to ui / applications)               
                  { id                      :: atom()             %% ddjson
                  , fullName                :: binary()           %% displayed in drop down box
                  }
       ).
-define(ddInterface, [atom, binstr]).

-record(ddConn,                             %% DB connection    
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% connection name (mutable)
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , adapter                 ::atom()            %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , access                  ::any()             %% erlang term depending on adapter (e.g. ip+service or tns)
                  , schm                    ::any()             %% erlang term depending on adapter (e.g. name or uri or data root path)
                  }
       ).
-define(ddConn, [integer, binstr, userid, atom, term, term]).
      
-record(ddCmd,                              %% DB command     
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% command template name (mutable)
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , adapters                ::[atom()]          %% can be used for this list of ddAdap
                  , conns                   ::local|remote      %% can be used for local dderl connections or for remote connections
                  , command                 ::binary()          %% erlang term depending on adapter (e.g. SQL text)
                  , opts                    ::any()             %% command options ()
                  }
       ).
-define(ddCmd, [integer, binstr, userid, list, atom, binstr, term]).

-record(ddView,                             %% user representation of a db command including rendering parameters
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , name                    ::binary()          %% should default to command name
                  , cmd                     ::ddEntityId()      %% db command id
                  , state                   ::#viewstate{}      %% transparent viewstate (managed by client application)
                  }
       ).
-define(ddView, [integer, atom, userid, binstr, integer, term]).

-record(ddDash,                             %% user representation of a dashboard (collection of views)
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , name                    ::binary()          %% should default to command name
                  , views                   ::list()            %% array of proplists with view layout
                  }
       ).



-define(ddDash, [integer, atom, userid, binstr, list]).

-define(LOG_TAG, "_DDRL_").

-define(Debug(__M,__F,__A), lager:debug(__M, "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}]++__A)).
-define(Debug(__F,__A),     lager:debug(     "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}]++__A)).
-define(Debug(__F),         lager:debug(     "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}])).

-define(Info(__M,__F,__A),  lager:info(__M,  "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}]++__A)).
-define(Info(__F,__A),      lager:info(      "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}]++__A)).
-define(Info(__F),          lager:info(      "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}])).

-define(Error(__M,__F,__A), lager:error(__M, "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}]++__A)).
-define(Error(__F,__A),     lager:error(     "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}]++__A)).
-define(Error(__F),         lager:error(     "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}])).

% Function shortcuts
-define(EncryptPid(__M, __P), dderl:encrypt_pid(__M, __P)).
-define(DecryptPid(__M, __P), dderl:decrypt_pid(__M, __P)).
