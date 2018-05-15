#!/bin/sh
. $(dirname $0)/common.sh

dderlRoot=$(readlink -f _build)
if [ -d $dderlRoot/default/lib/dderl/priv ]; then
    dderlPriv=$dderlRoot/default/lib/dderl/priv
elif [ -d $dderlRoot/prod/lib/dderl/priv ]; then
    dderlPriv=$dderlRoot/prod/lib/dderl/priv
else
    dderlPriv=$(readlink -f priv)
fi

dderlPrivPublic=$dderlPriv/public

log green "post_compile dderl $(pwd)"
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
    npm run build

    log green "dderl front-end built!"
fi
