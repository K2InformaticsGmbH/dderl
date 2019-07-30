#!/bin/bash

cmd=$0
argscount=$#

function usage {
    echo "usage: $1 add service_name app_name node_name node_host cluster_name cluster_host cookie dderlip dderlport imemip imemport imemtype imemschema"
    echo "       $1 remove service_name"
    echo "       $1 start service_name"
    echo "       $1 stop service_name"
    echo "       $1 list service_name"
    echo "       $1 attach node_name node_host cookie"
    echo "       $1 gui service_name app_name node_name node_host cluster_name cluster_host cookie dderlip dderlport imemip imemport imemtype imemschema"
    echo "       $1 txt service_name app_name node_name node_host cluster_name cluster_host cookie dderlip dderlport imemip imemport imemtype imemschema"
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
for dir in $PWD/_build/default/lib/*/
do
    dir=${dir%*/}
    dr=$PWD/_build/default/lib/${dir##*/}/ebin
    path2win dr
    deps="$deps $dr"
done

# augment pwd to the front of all paths
erlpaths="-pa $deps"
kernellogfile=$PWD/log/kernel.txt
path2win kernellogfile
kernelconfig="-kernel inet_dist_listen_min 7000 -kernel inet_dist_listen_max 7020 -proto_dist imem_inet_tcp"
kernelconfigsrv="$kernelconfig -kernel error_logger {file,\\\"""$kernellogfile\\\"""}"
service_name=$2
app_name=$3
node_name=$4
node_host=$5
cluster_name=$6
cluster_host=$7
cookie=$8
dderlip=$9
dderlport=${10}
imemip=${11}
imemport=${12}
imemtype=${13}
imemschema=${14}
lagerconfig=${15}

# dderl opts
dderl_opts="-dderl"
dderl_srv_opts=$dderl_opts" interface \\\"$dderlip\\\" port $dderlport"
dderl_opts=$dderl_opts" interface \"$dderlip\" port $dderlport"

# imem opts
imem_opts="-imem"
imem_opts=$imem_opts" tcp_port $imemport"
imem_opts=$imem_opts" mnesia_node_type $imemtype"
imem_opts=$imem_opts" mnesia_schema_name $imemschema"
imem_opts=$imem_opts" erl_cluster_mgrs ['$cluster_name@$cluster_host']"
imem_srv_opts=$imem_opts" tcp_ip \\\"$imemip\\\""
imem_opts=$imem_opts" tcp_ip \"$imemip\""

# sasl opts
sasl_opts="-sasl"
sasl_opts=$sasl_opts"  sasl_error_logger false" 

# lager config
config=""
if [ -n "$lagerconfig" ]; then
    path2win lagerconfig
    config="-config $lagerconfig"
fi

commonparams="$erlpaths -emu_args -setcookie $cookie $dderl_opts $imem_opts $sasl_opts $config -s $app_name"
common_srv_params="$erlpaths -emu_args -setcookie $cookie $dderl_srv_opts $imem_srv_opts $config -s $app_name"

name="-name $node_name@$node_host"

case $1 in
    "gui" )
        check_arg_count 6
        unamestr=`uname`
        if [[ "$unamestr" == 'Linux' ]]; then
            exename=erl
        else
            exename="start //MAX werl.exe"
        fi
        echo "Starting $service_name local GUI with '$exename $name $kernelconfig $commonparams'"
        $exename $name $kernelconfig $commonparams
        ;;
    "txt" )
        check_arg_count 6
        echo "Starting $service_name local TEXT with 'erl.exe -name $name $kernelconfig $commonparams'"
        unamestr=`uname`
        if [[ "$unamestr" == 'Linux' ]]; then
            exename=erl
        else
            exename=erl.exe
        fi
        $exename $name $kernelconfig $commonparams
        ;;
    "add" )
        check_arg_count 6
        echo "Adding $service_name service erlsrv.exe add $service_name -c \"$service_name Service\" -stopaction \"init:stop().\" -debugtype new -w $PWD $name -args \"$kernelconfigsrv $commonparams\""
        erlsrv.exe add $service_name -c "$service_name Service" -stopaction "init:stop()." -debugtype new -w $PWD $name -args "$kernelconfigsrv $common_srv_params"
        ;;
    "remove" )
	    check_arg_count 1
        echo Removing $service_name service
        erlsrv.exe remove $service_name
        ;;
    "start" )
	    check_arg_count 1
        echo Starting $service_name service
        erlsrv.exe start $service_name
        ;;
    "stop" )
	    check_arg_count 1
        echo Stopping $service_name service
        erlsrv.exe stop $service_name
        ;;
    "list" )
	    check_arg_count 1
        erlsrv.exe list $service_name
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
        elif [ -f "./_build/default/lib/dderl/check.escript" ]; then
            ./_build/default/lib/dderl/check.escript $2 $3 $4
        else
            echo file not found : check.escript
        fi
        ;;
    "attach" )
	    check_arg_count 3
        node=$2@$3
        cookie_service=$4
        echo "Connecting local node start //MAX werl.exe -remsh $node -name remsh_$node -setcookie $cookie_service"
        start //MAX werl.exe -remsh $node -name remsh_$node -setcookie $cookie_service
        ;;
    *)
        erlsrv.exe list $service_name
        echo
        if [ ! -z "$1" -a "$1" != " " ]; then
            echo Bad Argument : "'"$1"'"
        fi
        usage $cmd
        ;;
esac
