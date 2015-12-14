#!/bin/bash

cmd=$0
argscount=$#

function usage {
    echo "usage: $1 add app_name node_name node_host cluster_name cluster_host cookie dderlport imemport imemtype imemschema"
    echo "       $1 remove app_name"
    echo "       $1 start app_name"
    echo "       $1 stop app_name"
    echo "       $1 list app_name"
    echo "       $1 attach app_name"
    echo "       $1 gui app_name node_name node_host cluster_name cluster_host cookie dderlport imemport imemtype imemschema"
    echo "       $1 txt app_name node_name node_host cluster_name cluster_host cookie dderlport imemport imemtype imemschema"
    echo "       $1 check node_name node_host cookie"
    echo "       $1 escript script_file_path_name node_name node_host cookie"
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
appebin=$PWD/ebin
path2win appebin
erlpaths="-pa $appebin -pa $deps"
kernellogfile=$PWD/log/kernel.txt
path2win kernellogfile
kernelconfig="-kernel inet_dist_listen_min 7000 -kernel inet_dist_listen_max 7020 -proto_dist imem_inet_tcp"
kernelconfigsrv="$kernelconfig -kernel error_logger {file,\\\"""$kernellogfile\\\"""}"
app_name=$2
node_name=$3
node_host=$4
cluster_name=$5
cluster_host=$6
cookie=$7
dderlport=$8
imemport=$9
imemtype=${10}
imemschema=${11}
commonparams="$erlpaths -emu_args -setcookie $cookie -dderl port $dderlport -imem tcp_port $imemport -imem mnesia_node_type $imemtype -imem mnesia_schema_name $imemschema -s $app_name"

name="-name $node_name@$node_host"
extra="-imem erl_cluster_mgrs ['$cluster_name@$cluster_host']"

case $1 in
    "gui" )
        check_arg_count 6
        unamestr=`uname`
        if [[ "$unamestr" == 'Linux' ]]; then
            exename=erl
        else
            exename="start //MAX werl.exe"
        fi
        echo "Starting $app_name local GUI with '$exename $name $kernelconfig $commonparams $extra'"
        $exename $name $kernelconfig $commonparams $extra
        ;;
    "txt" )
        check_arg_count 6
        echo "Starting $app_name local TEXT with 'erl.exe -name $name $kernelconfig $commonparams $extra'"
        unamestr=`uname`
        if [[ "$unamestr" == 'Linux' ]]; then
            exename=erl
        else
            exename=erl.exe
        fi
        $exename $name $kernelconfig $commonparams $extra
        ;;
    "add" )
        check_arg_count 6
        echo "Adding $app_name service erlsrv.exe add $app_name -c \"$app_name Service\" -stopaction \"init:stop().\" -debugtype new -w $PWD $name -args \"$kernelconfigsrv $commonparams $extra\""
        erlsrv.exe add $app_name -c "$app_name Service" -stopaction "init:stop()." -debugtype new -w $PWD $name -args "$kernelconfigsrv $commonparams $extra"
        ;;
    "remove" )
	    check_arg_count 1
        echo Removing $app_name service
        erlsrv.exe remove $app_name
        ;;
    "start" )
	    check_arg_count 1
        echo Starting $app_name service
        erlsrv.exe start $app_name
        ;;
    "stop" )
	    check_arg_count 1
        echo Stopping $app_name service
        erlsrv.exe stop $app_name
        ;;
    "list" )
	    check_arg_count 1
        erlsrv.exe list $app_name
        ;;
    "escript" )
	    check_arg_count 4
        if [ -f "$2" ]; then
            $2 $3 $4 $5
        else
            echo escript not found $2
        fi
        ;;
    "check" )
	    check_arg_count 3
        if [ -f "./check.escript" ]; then
            ./check.escript $2 $3 $4
        elif [ -f "./deps/dderl/check.escript" ]; then
            ./deps/dderl/check.escript $2 $3 $4
        else
            echo file not found : check.escript
        fi
        ;;
    "attach" )
	    check_arg_count 1
        node=$(erlsrv.exe list $app_name | grep -re "Name:" | awk '{print $2}')
        node=$(echo $node | awk '{print $1}')
        host=(${node//\@/ })
        host=${host[1]}
        args=$(erlsrv.exe list $app_name | grep -re "Args:")
        args=(${args// / })
        idx=0
        cookie_service=''
        for i in "${args[@]}"; do
            if [ "${args[$idx]}" == "-setcookie" ]; then
                cookie_service=${args[$[idx+1]]}
                break
            fi
            idx=$[idx+1]
        done
        echo 
        echo "Connecting local node start //MAX werl.exe -name remoteattach@$host $kernelconfig -setcookie $cookie_service -eval \"net_adm:ping('$node')\""
        start //MAX werl.exe -name remoteattach@$host $kernelconfig -setcookie $cookie_service -eval "net_adm:ping('$node')"
        ;;
    *)
        erlsrv.exe list $app_name
        echo
        if [ ! -z "$1" -a "$1" != " " ]; then
            echo Bad Argument : "'"$1"'"
        fi
        usage $cmd
        ;;
esac
