#!/bin/bash
cd apps/dderl

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    erl -pa ebin -pa ../../deps/*/ebin -s "dderl" -dderl port 8443 -imem tcp_port 8125
else
    start //MAX werl.exe -pa ebin -pa ../../deps/*/ebin -s "dderl" -dderl port 8443 -imem tcp_port 8125
fi
