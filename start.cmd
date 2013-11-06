@echo off
setlocal EnableDelayedExpansion
for /d %%d in (./deps/*) do set deps=!deps! %CD%/deps/%%d/ebin

set exe= erl.exe
set erlpaths= -pa %CD%/apps/dderl/ebin -pa %deps%

if "%1" == "add" (
    @echo Adding dderl service
    erlsrv.exe add dderl -c "DDErl Service" -args "%erlpaths% -kernel inet_dist_listen_min 7000 -kernel inet_dist_listen_max 7020 -setcookie dderl -dderl port 443 -imem tcp_port 8125 -imem mnesia_schema_name dderlstag -s dderl"
) else if "%1" == "remove" (
    @echo Removing dderl service
   erlsrv.exe remove dderl
) else if "%1" == "start" (
    @echo Starting dderl service
    erlsrv.exe start dderl
) else if "%1" == "stop" (
    @echo Stoping dderl service
    erlsrv.exe stop dderl
) else if "%1" == "list" (
    erlsrv.exe list dderl
) else if "%1" == "gui" (
    @echo Starting dderl service
    start /MAX werl.exe %erlpaths% -args_file vm.args -s dderl
) else if "%1" == "txt" (
    @echo Starting dderl
    erl.exe %erlpaths% -args_file vm.args -s dderl
) else (
    @echo "usage: cmd //C start.cmd add|remove|start|stop|list|gui|txt"
)
