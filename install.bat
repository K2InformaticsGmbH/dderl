FOR /F "usebackq" %%i IN (`hostname`) DO SET HOST=%%i
::echo "-kernel error_logger {file,\\""D:/Projects/Git/dderl/kernel.txt\\""} -pa D:/Projects/Git/dderl/ebin -pa D:/Projects/Git/dderl/deps/erloci/ebin -pa D:/Projects/Git/dderl/deps/imem/ebin -pa D:/Projects/Git/dderl/deps/mochiweb/ebin -pa D:/Projects/Git/dderl/deps/sqlparse/ebin -pa D:/Projects/Git/dderl/deps/webmachine/ebin -name dderl@%HOST%.it.bwns.ch -setcookie dderl -s reloader -s dderl"
erlsrv remove "dderl"
erlsrv add "dderl" -stopaction "init:stop()." -debugtype reuse -args "-kernel error_logger {file,\\""D:/Projects/Git/dderl/kernel.txt\\""} -pa D:/Projects/Git/dderl/ebin -pa D:/Projects/Git/dderl/deps/erloci/ebin -pa D:/Projects/Git/dderl/deps/imem/ebin -pa D:/Projects/Git/dderl/deps/mochiweb/ebin -pa D:/Projects/Git/dderl/deps/sqlparse/ebin -pa D:/Projects/Git/dderl/deps/webmachine/ebin -name dderl@%HOST%.it.bwns.ch -setcookie dderl -s reloader -s dderl"
pause
