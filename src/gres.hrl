-record(gres,   { %% response sent back to gui
                  operation           %% rpl (replace) | app (append) | prp (prepend) | nop | close
                , cnt = 0             %% current buffer size (raw table or index table size)
                , toolTip = <<"">>    %% current buffer sizes RawCnt/IndCnt plus status information
                , message = <<"">>    %% error message
                , beep = false        %% alert with a beep if true
                , state = <<"empty">> %% determines color of buffer size indicator
                , loop = <<"">>       %% gui should come back with this command -- empty string is 'undefined'
                , rows = []           %% rows to show (append / prepend / merge)
                , keep = 0            %% row count to be kept
                , focus = 0           %% 0 -> default scroll depending on operation (rpl = no scroll)
                , sql = <<"">>        %% new sql string (only present if it changes)
                , disable = []        %% list of {<<"button name">>, <<"Comment">>} to be disabled
                , promote = []        %% list of {<<"button name">>, <<"Comment">>} promoted to the user
                }).

-record(fsmctx, { % fsm interface
                  id
                , stmtCols
                , rowFun
                , sortFun
                , sortSpec
                , block_length
                , fetch_recs_async_fun
                , fetch_close_fun
                , stmt_close_fun
                , filter_and_sort_fun
                , update_cursor_prepare_fun
                , update_cursor_execute_fun
                , orig_qry
                , bind_vals
                , table_name
                }).
