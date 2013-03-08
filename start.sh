#!/bin/sh
#unamestr=`uname`
exename=werl.exe
#exename=erl
if [[ "$unamestr" == 'Linux' ]]; then
     exename=erl
fi
echo "exe $exename unamestr $unamestr"
if [[ "$unamestr" == 'Linux' ]]; then
    $exename -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@127.0.0.1 -setcookie imem -s dderl -imem mnesia_node_type disc
else
    start //MAX $exename -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@127.0.0.1 -setcookie imem -s dderl -imem mnesia_node_type disc
 #   $exename -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@127.0.0.1 -setcookie imem -s dderl -imem mnesia_node_type disc
fi
