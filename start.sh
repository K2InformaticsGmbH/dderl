#!/bin/sh
cd `dirname $0`
#exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s dderl
HOST=`hostname`
exec werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@$HOST.it.bwns.ch -setcookie dderl -s reloader -s dderl
#exec werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@$HOST.it.bwns.ch -setcookie dderl -s reloader -s dderl -detached
#exec werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@localhost -setcookie dderl -s reloader -s dderl
