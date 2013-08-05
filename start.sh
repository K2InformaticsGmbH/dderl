#!/bin/bash
cd apps/dderl

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
else
    #exename=erl.exe
    exename='start //MAX werl.exe'
fi
$exename -pa ebin -pa ../../deps/*/ebin -name dderl@127.0.0.1 -setcookie dderl -s dderl -dderl port 8443 -imem tcp_port 8125
