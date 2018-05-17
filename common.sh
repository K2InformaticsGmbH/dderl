#!/bin/sh

log() {
    if [ $# -ne "2" ]
    then
        echo -e "===> $1"
    else
        case $1 in
            black)       color="0;30";;
            red)         color="0;31";;
            green)       color="0;32";;
            brown)       color="0;33";;
            blue)        color="0;34";;
            purple)      color="0;35";;
            cyan)        color="0;36";;
            lightgrey)   color="0;37";;
            darkgray)    color="1;30";; 
            lightred)    color="1;31";;
            lightgreen)  color="1;32";;
            yellow)      color="1;33";;
            lightblue)   color="1;34";;
            lightpurple) color="1;35";;
            lightcyan)   color="1;36";;
            white)       color="1;37";;
            *)           color="0;30";;
        esac
        echo -e "\033[${color}m===> $2\033[0m"
    fi
}
