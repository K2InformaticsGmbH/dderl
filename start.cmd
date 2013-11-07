@echo off
setlocal EnableDelayedExpansion
for /d %%d in (./deps/*) do set deps=!deps! %CD%/deps/%%d/ebin

set exe= erl.exe
set erlpaths=-pa %CD%/apps/dderl/ebin -pa %deps%
set kernelconfig=-kernel inet_dist_listen_min 7000 -kernel inet_dist_listen_max 7020
set kernelconfigsrv=%kernelconfig% -kernel error_logger {file,\\\""%CD%/log/kernel.txt\\\""}
set commonparams=%erlpaths% -emu_args -setcookie dderl -dderl port 443 -imem tcp_port 8125 -imem mnesia_schema_name dderlstag -s dderl

set name=-name dderl@%2
set extra=-imem erl_cluster_mgr 'dderl@%3'
if not "%~2%~3" == "%~3%~2" (
    @echo Cmd '%1' name '%name%' extra '%extra%'
    if "%1" == "add" (
        @echo Adding dderl service
        erlsrv.exe add dderl -c "DDErl Service" -stopaction "init:stop()." -debugtype new -w %CD% %name% -args "%kernelconfigsrv% %commonparams% %extra%"
    ) else if "%1" == "gui" (
        @echo Starting dderl service
        start /MAX werl.exe %name% %kernelconfig% %erlpaths% %commonparams% %extra%
    ) else if "%1" == "txt" (
        @echo Starting dderl
        erl.exe %name% %kernelconfig% %erlpaths% %commonparams% %extra%
    ) else (
        @echo "Bad Argument '%1' '%2' '%3'"
        @echo "usage: cmd //C start.cmd [add node_host cluster_host | remove | start | stop | list | gui node_host cluster_host | txt node_host cluster_host]"
    )
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
) else (
    @echo "Bad Argument '%1'"
    @echo "usage: cmd //C start.cmd (add node_host cluster_host | remove | start | stop | list | gui node_host cluster_host | txt node_host cluster_host)"
)
