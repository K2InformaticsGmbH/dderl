#!/bin/sh
cd `dirname $0`
#exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s dderl
exec werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@oltapmop-sbsm01.it.bwns.ch -setcookie dderl -s reloader -s dderl
#exec erl -pa $PWD/ebin $PWD/deps/*/ebin -s reloader -s dderl -detached
#exec erl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@oltapmop-sbsm01.it.bwns.ch -setcookie dderl -s reloader -s dderl -detached
