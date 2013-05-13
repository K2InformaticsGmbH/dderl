#!/bin/sh
cd apps/dderl
start //MAX werl.exe -pa ebin -pa ../../deps/*/ebin -s "dderl" -dderl port 8443 -imem tcp_port 8125
