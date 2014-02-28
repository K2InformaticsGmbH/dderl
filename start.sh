#!/bin/bash
cd apps/dderl

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
else
    #exename=erl.exe
    exename='start //MAX werl.exe'
fi
$exename -pa ebin -pa ../../deps/*/ebin -name dderl$1@127.0.0.1 -setcookie zdss4d2_6tP -dderl port ${3}443 -imem tcp_port ${3}125 -imem erl_cluster_mgrs [\'dderl$2@127.0.0.1\'] -imem mnesia_schema_name dderlstag -s dderl 
