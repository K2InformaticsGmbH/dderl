#!/bin/sh
# cd `dirname $0`
# HOST=`hostname`
werl.exe -pa $PWD/ebin -pa $PWD/deps/*/ebin -name dderl@10.66.148.80 -setcookie dderl712abc -s reloader -s dderl 
