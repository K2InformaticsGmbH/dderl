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
    echo "       $1 gui node_host cluster_host"
    echo "       $1 txt node_host cluster_host"
    echo "       $1 guit node_host"
    echo "       $1 txtt node_host"
}

function path2win {
    eval $1=$(echo "${!1}" | sed 's/^\(\/\)\(.\)\(\/\)/\2:\//')
}

function check_arg_count {
    if [ $argscount -le $1 ]; then
        echo "Missing arguments; got $argscount"
        usage $cmd
        exit 0
    fi
}

deps=''
for dir in $PWD/deps/*/
do
    dir=${dir%*/}
    dr=$PWD/deps/${dir##*/}/ebin
    path2win dr
    deps="$deps $dr"
done

# augment pwd to the front of all paths
dderlebin=$PWD/apps/dderl/ebin
path2win dderlebin
erlpaths="-pa $dderlebin -pa $deps"
kernellogfile=$PWD/log/kernel.txt
path2win kernellogfile
kernelconfig="-kernel inet_dist_listen_min 7000 -kernel inet_dist_listen_max 7020"
kernelconfigsrv="$kernelconfig -kernel error_logger {file,\\\"""$kernellogfile\\\"""}"
commonparams="$erlpaths -emu_args -setcookie dderl -dderl port 443 -imem tcp_port 8125 -imem mnesia_schema_name dderlstag -s dderl"
commonparamsnoapp="$erlpaths -emu_args -setcookie dderl"

name="-name dderl@$2"
extra="-imem erl_cluster_mgrs ['dderl@$3']"

case $1 in
    "gui" )
        check_arg_count 1
        echo "Starting dderl local GUI with 'start /MAX werl.exe -name dderlt@$2 $kernelconfig $commonparams $extra'"
        start //MAX werl.exe -name dderlg@$2 $kernelconfig $commonparams $extra
        ;;
    "txt" )
        check_arg_count 1
        echo "Starting dderl local TEXT with 'erl.exe -name dderlt@$2 $kernelconfig $commonparams $extra'"
        erl.exe -name dderlt@$2 $kernelconfig $commonparams $extra
        ;;
    "add" )
        check_arg_count 2
        echo "Adding dderl service erlsrv.exe add dderl -c \"DDErl Service\" -stopaction \"init:stop().\" -debugtype new -w $PWD $name -args \"$kernelconfigsrv $commonparams $extra\""
        erlsrv.exe add dderl -c "DDErl Service" -stopaction "init:stop()." -debugtype new -w $PWD $name -args "$kernelconfigsrv $commonparams $extra"
        ;;
    "remove" )
        echo Removing dderl service
        erlsrv.exe remove dderl
        ;;
    "start" )
        echo Starting dderl service
        erlsrv.exe start dderl
        ;;
    "stop" )
        echo Stoping dderl service
        erlsrv.exe stop dderl
        ;;
    "list" )
        erlsrv.exe list dderl
        ;;
    "attach" )
        node=$(erlsrv.exe list dderl | grep -re "Name:" | awk '{print $2}')
        node=$(echo $node | awk '{print $1}')
        host=(${node//\@/ })
        host=${host[1]}
        args=$(erlsrv.exe list dderl | grep -re "Args:")
        args=(${args// / })
        idx=0
        cookie=''
        for i in "${args[@]}"; do
            if [ "${args[$idx]}" == "-setcookie" ]; then
                cookie=${args[$[idx+1]]}
                break
            fi
            idx=$[idx+1]
        done
        echo 
        echo "Connecting local node start //MAX werl.exe -name remoteattach@$host $kernelconfig -setcookie $cookie -eval \"net_adm:ping('$node')\""
        start //MAX werl.exe -name remoteattach@$host $kernelconfig -setcookie $cookie -eval "net_adm:ping('$node')"
        ;;
    *)
        erlsrv.exe list dderl
        echo
        if [ ! -z "$1" -a "$1" != " " ]; then
            echo Bad Argument : "'"$1"'"
        fi
        usage $cmd
        ;;
esac
