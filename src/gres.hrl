-ifndef(GRES_HRL).
-define(GRES_HRL, true).

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
                , stmtClass = <<>>    %% statement class "L" = local
                }).

-record(fsmctxs,{ % fsm interfaces to multiple statements
                  stmtRefs                   %% one per statement
                , stmtTables                 %% one per statement
                , fetch_recs_async_funs      %% one per statement
                , fetch_close_funs           %% one per statement
                , stmt_close_funs            %% one per statement
                , filter_and_sort_funs       %% one per statement
                , update_cursor_prepare_funs %% one per statement
                , update_cursor_execute_funs %% one per statement
                , orig_qry                   %% same for all statements
                , rowCols                    %% same for all statements
                , rowFun                     %% same for all statements
                , sortFun                    %% same for all statements
                , sortSpec                   %% same for all statements
                , block_length               %% same for all statements
                , bind_vals                  %% same for all statements
                , stmtClass = <<>>           %% same for all statements
                }).

-endif.
