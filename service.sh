#!/bin/bash

# Change below to use specific hosts
node_host=127.0.0.1
cluster_host=127.0.0.1

cmd=$0
argscount=$#
function usage {
    echo "usage: $1 add"
    echo "       $1 remove"
    echo "       $1 start"
    echo "       $1 stop"
    echo "       $1 list"
    echo "       $1 attach"
    echo "       $1 gui"
    echo "       $1 txt"
    echo "default arguments node_host = $node_host"
    echo "                  cluster_host = $cluster_host"
}

if [ "$1" == "gui" ] ||
   [ "$1" == "txt" ] ||
   [ "$1" == "add" ]; then
    service_generic.sh $1 $node_host $cluster_host
elif [ "$1" == "remove" ] ||
     [ "$1" == "start" ] ||
     [ "$1" == "stop" ] ||
     [ "$1" == "list" ] ||
     [ "$1" == "attach" ]; then
    service_generic.sh $1
else
    usage $cmd
    echo ---------------
    echo Generic usage :
    echo
    service_generic.sh
fi
