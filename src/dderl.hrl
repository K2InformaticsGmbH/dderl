-record(accounts, { user
                  , password
                  , db_connections
                  , db_files
                  }
       ).

-record(common,   { adapter
                  , db_files
                  }
       ).
