#!/bin/bash

cmd=$0
argscount=$#

function usage {
    echo "usage: $1 add app_name node_name node_host cluster_name cluster_host cookie dderlip dderlport imemip imemport imemtype imemschema"
    echo "       $1 remove app_name"
    echo "       $1 start app_name"
    echo "       $1 stop app_name"
    echo "       $1 list app_name"
    echo "       $1 attach node_name node_host cookie"
    echo "       $1 gui app_name node_name node_host cluster_name cluster_host cookie dderlip dderlport imemip imemport imemtype imemschema"
    echo "       $1 txt app_name node_name node_host cluster_name cluster_host cookie dderlip dderlport imemip imemport imemtype imemschema"
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
app_name=$2
node_name=$3
node_host=$4
cluster_name=$5
cluster_host=$6
cookie=$7
dderlip=$8
dderlport=$9
imemip=${10}
imemport=${11}
imemtype=${12}
imemschema=${13}
lagerconfig=${14}

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
        echo "Starting $app_name local GUI with '$exename $name $kernelconfig $commonparams'"
        $exename $name $kernelconfig $commonparams
        ;;
    "txt" )
        check_arg_count 6
        echo "Starting $app_name local TEXT with 'erl.exe -name $name $kernelconfig $commonparams'"
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
        echo "Adding $app_name service erlsrv.exe add $app_name -c \"$app_name Service\" -stopaction \"init:stop().\" -debugtype new -w $PWD $name -args \"$kernelconfigsrv $commonparams\""
        erlsrv.exe add $app_name -c "$app_name Service" -stopaction "init:stop()." -debugtype new -w $PWD $name -args "$kernelconfigsrv $common_srv_params"
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
        erlsrv.exe list $app_name
        echo
        if [ ! -z "$1" -a "$1" != " " ]; then
            echo Bad Argument : "'"$1"'"
        fi
        usage $cmd
        ;;
esac
