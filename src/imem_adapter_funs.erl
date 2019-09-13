-module(imem_adapter_funs).

-include("dderl.hrl").

-export([fetch_recs_async/2
        ,fetch_close/2
        ,stmt_close/2
        ,filter_and_sort/2
        ,update_cursor_prepare/2
        ,update_cursor_execute/2]).

fetch_recs_async(Connection, StmtRefs) when is_list(StmtRefs) ->
    [fetch_recs_async(Connection, SR) || SR <- StmtRefs];
fetch_recs_async(Connection, StmtRef) ->
    fun(Opts, _) -> Connection:run_cmd(fetch_recs_async, [Opts, StmtRef]) end.

fetch_close(Connection, StmtRefs) when is_list(StmtRefs) ->
    [fetch_close(Connection, SR) || SR <- StmtRefs];
fetch_close(Connection, StmtRef) ->
    fun() -> Connection:run_cmd(fetch_close, [StmtRef]) end.

stmt_close(Connection, StmtRefs) when is_list(StmtRefs) ->
    [stmt_close(Connection, SR) || SR <- StmtRefs];
stmt_close(Connection, StmtRef) ->
    fun() ->
            try Connection:run_cmd(close, [StmtRef])
            catch
                exit:{noproc,_} ->
                    ?Debug("Fsm terminated after the connection was closed");
                Class:Error ->
                    ?Error("Error trying to terminate the statement ~p:~p",
                           [Class, Error], erlang:get_stacktrace())
            end
    end.

filter_and_sort(Connection, StmtRefs) when is_list(StmtRefs) ->
    [filter_and_sort(Connection, SR) || SR <- StmtRefs];
filter_and_sort(Connection, StmtRef) ->
    fun(FilterSpec, SrtSpec, Cols) ->
        Connection:run_cmd(filter_and_sort, [StmtRef, FilterSpec, SrtSpec, Cols])
    end.

update_cursor_prepare(Connection, StmtRefs) when is_list(StmtRefs) ->
    [update_cursor_prepare(Connection, SR) || SR <- StmtRefs];
update_cursor_prepare(Connection, StmtRef) ->
    fun(ChangeList) ->
        Connection:run_cmd(update_cursor_prepare, [StmtRef, ChangeList])
    end.

update_cursor_execute(Connection, StmtRefs) when is_list(StmtRefs) ->
    [update_cursor_execute(Connection, SR) || SR <- StmtRefs];
update_cursor_execute(Connection, StmtRef) ->
    fun(Lock) ->
        Connection:run_cmd(update_cursor_execute, [StmtRef, Lock])
    end.
