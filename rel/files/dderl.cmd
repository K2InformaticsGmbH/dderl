@setlocal

@for %%A in ("%0") do @set script_name_with_ext=%%~nxA
@for /F "delims=." %%a in ("%script_name_with_ext%") do @set app_name=%%a

@echo Script name with extention '%script_name_with_ext%'
@echo Set app name from script '%app_name%'

@rem Get the absolute path to the parent directory,
@rem which is assumed to be the node root.
@for /F "delims=" %%I in ("%~dp0..") do @set node_root=%%~fI

@set releases_dir=%node_root%\releases

@rem Parse ERTS version and release version from start_erl.data
@for /F "usebackq tokens=1,2" %%I in ("%releases_dir%\start_erl.data") do @(
    @call :set_trim erts_version %%I
    @call :set_trim release_version %%J
)

@set vm_args=%releases_dir%\%release_version%\vm.args
@set sys_config=%releases_dir%\%release_version%\sys.config
@set node_boot_script=%releases_dir%\%release_version%\%app_name%
@set clean_boot_script=%releases_dir%\%release_version%\start_clean

@rem extract erlang cookie from vm.args
@for /f "usebackq tokens=1-2" %%I in (`findstr /b \-setcookie "%vm_args%"`) do @set erlang_cookie=%%J

@rem extract node name from vm.args
@for /f "usebackq tokens=1-2" %%I in (`findstr /b \-name "%vm_args%"`) do @set name=%%J
@if %name%=="" (
    @set nodenamearg=-sname %app_name%
) else (
    @set nodenamearg=-name %name%
)

@set vm_t_args=%releases_dir%\%release_version%\vm_t.args
@set vm_t1_args=%releases_dir%\%release_version%\vm_t1.args
@echo.> "%vm_t1_args%"
@for /f "usebackq tokens=*" %%I in (`findstr /v [#] "%vm_args%"`) do @echo %%I >> "%vm_t1_args%"
@echo.> "%vm_t_args%"
@for /f "usebackq tokens=*" %%I in (`findstr /v \-name "%vm_t1_args%"`) do @echo %%I >> "%vm_t_args%"
@del /f /q "%vm_t1_args%"

@set erts_bin=%node_root%\erts-%erts_version%\bin

@set service_name=%app_name%_%release_version%

@set erlsrv="%erts_bin%\erlsrv.exe"
@set epmd="%erts_bin%\epmd.exe"
@set escript="%erts_bin%\escript.exe"
@set werl="%erts_bin%\werl.exe"

@if "%1"=="usage" @goto usage
@if "%1"=="install" @goto install
@if "%1"=="uninstall" @goto uninstall
@if "%1"=="start" @goto start
@if "%1"=="stop" @goto stop
@if "%1"=="restart" @call :stop && @goto start
@if "%1"=="console" @goto console
@if "%1"=="query" @goto query
@if "%1"=="attach" @goto attach
@if "%1"=="upgrade" @goto upgrade
@echo Unknown command: "%1"

:usage
@echo Usage: %~n0 [install^|uninstall^|start^|stop^|restart^|console^|query^|attach^|upgrade]
@goto :EOF

:install
@set description=Application %app_name% in %node_root%
@set start_erl=%node_root%\bin\start_erl.cmd
@set args= ++ %app_name% ++ %node_root%
@%erlsrv% add %service_name% -c "%description%" %nodenamearg% -w "%node_root%" -m "%start_erl%" -args "%args%" -stopaction "init:stop()."
@goto :EOF

:uninstall
@%erlsrv% remove %service_name%
@%epmd% -kill
@goto :EOF

:start
@%erlsrv% start %service_name%
@goto :EOF

:stop
@%erlsrv% stop %service_name%
@goto :EOF

:console
@start "%app_name% console" %werl% -boot "%node_boot_script%" -config "%sys_config%" -args_file "%vm_args%"
@goto :EOF

:query
@%erlsrv% list %service_name%
@exit %ERRORLEVEL%
@goto :EOF

:attach
@for /f "delims=@ tokens=1,2" %%I in ("%name%") do @set host=%%J
start "%app_name% attach" %werl% -boot "%clean_boot_script%" -remsh %name% -name console@%host% -setcookie %erlang_cookie%
@goto :EOF

:upgrade
@if "%2"=="" (
    @echo Missing upgrade package argument
    @echo Usage: %~n0 upgrade {package base name}
    @echo NOTE {package base name} MUST NOT include the .tar.gz suffix
    @goto :EOF
)
@%escript% %node_root%\bin\install_upgrade.escript %app_name% %erlang_cookie% %2
@goto :EOF

:set_trim
@set %1=%2
@goto :EOF
