#!/bin/bash

cmd=$0
argscount=$#

function usage {
    echo "usage: $1 add node_host cluster_host"
    echo "       $1 remove"
    echo "       $1 start"
    echo "       $1 stop"
    echo "       $1 list"
    echo "       $1 attach"
    echo "       $1 gui"
    echo "       $1 txt"
    echo "       $1 guit"
    echo "       $1 txtt"
}

if [ "$1" == "gui" ] ||
   [ "$1" == "txt" ] ||
   [ "$1" == "add" ]; then
    service_generic.sh $1 ZHHAPMOP-SBSM01.it.bwns.ch OLTAPMOP-SBSM01.it.bwns.ch
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
