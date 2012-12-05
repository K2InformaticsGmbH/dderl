@ECHO OFF

SET NODE=dderl@10.66.148.80

SET BASE=D:/Projects/Git/dderl
SET PAs= -pa %BASE%/ebin
SET PAs=%PAs% -pa ./deps/imem/ebin
SET PAs=%PAs% -pa ./deps/erloci/ebin
SET PAs=%PAs% -pa ./deps/sqlparse/ebin
SET PAs=%PAs% -pa ./deps/mochiweb/ebin
SET PAs=%PAs% -pa ./deps/webmachine/ebin

erlsrv remove "dderl"
erlsrv add "dderl" -stopaction "init:stop()." -workdir %BASE% -name %NODE% -debugtype reuse -args "-kernel error_logger {file,\\""D:/Projects/Git/dderl/kernel.txt\\""} %PAs% -setcookie dderl712abc -s reloader -s dderl"
pause

