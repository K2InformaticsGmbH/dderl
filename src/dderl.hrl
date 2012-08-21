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

-record(file,     {content = ""
                  , posX = 0
                  , posY = 0
                  , width = 0
                  , height = 0
                  }
       ).

-define(DEFAULT_ROW_SIZE, 150).
