erlsrv remove "dderl"
erlsrv add "dderl" -stopaction "init:stop()." -debugtype reuse -args "-kernel error_logger {file,\\""D:/Projects/Git/dderl/kernel.txt\\""} -pa D:/Projects/Git/dderl/ebin -pa D:/Projects/Git/dderl/deps/*/ebin -name dderl@oltapmop-sbsm01.it.bwns.ch -setcookie dderl -s reloader -s dderl"
pause
