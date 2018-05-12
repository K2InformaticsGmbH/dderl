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

dderlPriv=$(readlink -f _build/default/lib/dderl/priv)
dderlPrivPublic=$dderlPriv/public

log green "post_compile mpro $(pwd)"
log lightgrey "dderlPriv ${dderlPriv}"

if [ -d $dderlPrivPublic ]; then
    log blue "dderl front-end already built!"
    exit 0
else
    log green "building dderl front-end..."

    cd "$dderlPriv/dev"
    if [ -d node_modules ]; then
        log purple "deleting 'dev/node_modules' ..."
        rm -rf node_modules
        log purple "'dev/node_modules' deleted"
    fi

    log green "npm install (dev) $(pwd)"
    npm install

    cd "$dderlPriv/swagger"
    if [ -d node_modules ]; then
        log purple "deleting 'swagger/node_modules' ..."
        rm -rf node_modules
        log purple "'swagger/node_modules' deleted"
    fi
    
    log green "npm install (swagger) at $(pwd)"
    npm install

    # builds both dderl and swagger
    cd "$dderlPriv/dev"
    log green "npm run build
     $(pwd)"
    npm run build-prod

    log green "dderl front-end built!"
fi