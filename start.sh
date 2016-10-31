#!/bin/bash

# Parameters:  NodeID ClusterID IMEMPort DDERLPort

nid=$1
cid=$2
port=$3
dderlport=$4
if [ "$#" -ne 4 ]; then
    nid=1
    cid=2
    port=1236
    dderlport=443
fi

unamestr=`uname`
host=`hostname`
hostArrayIn=(${host//./ })
host=${hostArrayIn[0]}
host=127.0.0.1
name=dderl$nid@$host
cmname=dderl$cid@$host
imemtyp=disc
ck=dderl
if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi

# Node name
node_name="-name $name"

# Cookie
cookie="-setcookie $ck"

# PATHS
paths="-pa"
paths=$paths" ebin"
paths=$paths" deps/*/ebin"

# Proto dist module
dist_opts="-proto_dist"
dist_opts=$dist_opts" imem_inet_tcp"

# Kernel Opts
kernel_opts="-kernel"
kernel_opts=$kernel_opts" inet_dist_listen_min 7000"
kernel_opts=$kernel_opts" inet_dist_listen_max 7020"

# Imem Opts
imem_opts="-imem"
imem_opts=$imem_opts" mnesia_node_type $imemtyp"
imem_opts=$imem_opts" erl_cluster_mgrs ['$cmname']"
imem_opts=$imem_opts" mnesia_schema_name dderl"
imem_opts=$imem_opts" tcp_port $port"

# dderl opts
dderl_opts="-dderl"
dderl_opts=$dderl_opts" port $dderlport" 

start_opts="$paths $cookie $node_name $dist_opts $kernel_opts $imem_opts $dderl_opts"

# DDERL start options
echo "------------------------------------------"
echo "Starting DDERL (Opts)"
echo "------------------------------------------"
echo "Node Name : $node_name"
echo "Cookie    : $cookie"
echo "EBIN Path : $paths"
echo "Dist      : $dist_opts"
echo "Kernel    : $kernel_opts"
echo "IMEM      : $imem_opts"
echo "DDERL     : $dderl_opts"
echo "------------------------------------------"

# Starting dderl
$exename $start_opts -s dderl
