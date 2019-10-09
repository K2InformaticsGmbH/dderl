#!/bin/sh
cd ~/dderl && bash start.sh 1 2 1326 8443 
sleep 10
tail -f ~/dderl/log/console.log
