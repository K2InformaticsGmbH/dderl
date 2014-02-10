#!/bin/bash
cd apps/dderl

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
else
    #exename=erl.exe
    exename='start //MAX werl.exe'
fi
$exename -pa ebin -pa ../../deps/*/ebin -name dderl@WKS01$1.k2informatics.ch -setcookie zdss4d2_6tP -dderl port 8443 -imem tcp_port 8125 -imem erl_cluster_mgr \'dderl@WKS01$2.k2informatics.ch\' -imem mnesia_schema_name dderlstag -s dderl 
