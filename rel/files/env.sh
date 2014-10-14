#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

RUNNER_USER={{runner_user}}
WHOAMI=$(whoami)
RUNNER_SCRIPT_DIR={{runner_script_dir}}
RUNNER_BASE_DIR={{runner_base_dir}}
WAIT_FOR_PROCESS={{runner_wait_process}}
RUNNER_SCRIPT=${0##*/}

# Variables needed to support creation of .pid files
# PID directory and pid file name of this app
# ex: /var/run/riak & /var/run/riak/riak.pid
RUN_DIR="/var/run"
PID_DIR=$RUN_DIR/$RUNNER_SCRIPT
PID_FILE=$PID_DIR/$RUNNER_SCRIPT.pid

# Function to su into correct user
check_user() {
    # Validate that the user running the script is the owner of the
    # RUN_DIR.
    if ([ "$RUNNER_USER" ] && [ "x$WHOAMI" != "x$RUNNER_USER" ]); then
        type sudo > /dev/null 2>&1
        if [ "$?" -ne 0 ]; then
            echoerr "sudo doesn't appear to be installed and your EUID isn't $RUNNER_USER" 1>&2
            exit 1
        fi
        exec sudo -H -u $RUNNER_USER -i $RUNNER_SCRIPT_DIR/$RUNNER_SCRIPT $@
    fi
}

# Parse out release and erts info
START_ERL=`cat $RUNNER_BASE_DIR/releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
#APP_VSN=${START_ERL#* }

# Check the first argument for instructions
ERTS_PATH=$RUNNER_BASE_DIR/erts-$ERTS_VSN/bin
VMARGS_FILE=$RUNNER_BASE_DIR/releases/{{rel_vsn}}/vm.args
NAME_ARG=`egrep '^\-s?name' $VMARGS_FILE`
if [ -z "$NAME_ARG" ]; then
    echoerr "vm.args needs to have either -name or -sname parameter."
    exit 1
fi
NODE_NAME=${NAME_ARG#* }

COOKIE_ARG=`grep '^\-setcookie' $VMARGS_FILE`
if [ -z "$COOKIE_ARG" ]; then
    echoerr "vm.args needs to have a -setcookie parameter."
    exit 1
fi
NODE_COOKIE=`echo $COOKIE_ARG | awk '{print $2}'`

NODETOOL="$ERTS_PATH/escript $ERTS_PATH/nodetool $NAME_ARG $COOKIE_ARG"

# Ping node without stealing stdin
ping_node() {
    $NODETOOL ping < /dev/null
}

# Attempt to create a pid file for the process
# This function assumes the process is already up and running and can
#    respond to a getpid call.  It also assumes that two processes
#    with the same name will not be run on the machine
# Do not print any error messages as failure to create a pid file because
#    pid files are strictly optional
# This function should really only be called in a "start" function
#    you have been warned
create_pid_file() {
    # Validate a pid directory even exists
    if [ -w $PID_DIR ]; then
        # Grab the proper pid from getpid
        get_pid
        ES=$?
        if [ "$ES" -ne 0 ]; then
            return $ES
        else
            # Remove pid file if it already exists since we do not
            # plan for multiple identical runners on a single machine
            rm -f $PID_FILE
            echo $PID > $PID_FILE
            return 0
        fi
    else
        return 1
    fi
}

# Set the PID global variable, return 1 on error
get_pid() {
    PID=`$NODETOOL getpid < /dev/null`
    if [ "$?" -ne 0 ]; then
        echo "Node is not running!"
        return 1
    fi

    # don't allow empty or init pid's
    if [ -z $PID ] || [ "$PID" -le 1 ]; then
        return 1
    fi

    return 0
}
