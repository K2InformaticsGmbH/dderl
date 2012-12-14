#!/bin/sh
# cd `dirname $0`
# HOST=`hostname`
#werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@$HOST.it.bwns.ch -setcookie imem -s reloader -s dderl
#werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@127.0.0.1 -setcookie imem -s reloader -s dderl -imem erl_cluster_mgr \'CM@127.0.0.1\'
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@127.0.0.1 -setcookie imem -s reloader -s dderl -imem mnesia_node_type disc
