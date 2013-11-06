@echo off
setlocal EnableDelayedExpansion
for /d %%d in (./deps/*) do set deps=!deps! %CD%/deps/%%d/ebin

set exe= erl.exe
set erlpaths= -pa %CD%/apps/dderl/ebin -pa %deps%

if "%1" == "add" (
    @echo "Adding dderl service"
    erlsrv.exe add dderl -c "DDErl Service" -args "%erlpaths% -setcookie dderl -dderl port 8443 -imem tcp_port 8125 -imem mnesia_schema_name dderlstag -s dderl"
) else if "%1" == "remove" (
    @echo "Removing dderl service"
   erlsrv.exe remove dderl
) else if "%1" == "start" (
    @echo "Starting dderl service"
    erlsrv.exe start dderl
) else if "%1" == "stop" (
    @echo "Stoping dderl service"
    erlsrv.exe stop dderl
) else if "%1" == "gui" (
    @echo "Starting dderl service"
    start /MAX werl.exe %erlpaths% -args_file vm.args -s dderl
) else (
    @echo "Starting dderl"
    erl.exe %erlpaths% -args_file vm.args -s dderl
)
