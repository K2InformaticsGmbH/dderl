. $(dirname $0)/common.sh

app=${1:-dderl}
log green "post_release $app $(pwd)"

dderlPriv=$(readlink -f _build/prod/rel/$1/lib/dderl-*/priv/)

if [ -d "$dderlPriv/public" ]; then
    log blue "dderl(dev+swagger) already built!"
else
    if [ -d "$dderlPriv/dev" ]; then
        cd $dderlPriv/dev
        rm -rf node_modules
        log purple "'dev/node_modules' deleted"
    
        log green "npm install (dev)"
        npm install
    else
        log red "unable to build dderl(dev), missing $dderlPriv/dev"
        exit 1
    fi
    
    if [ -d "$dderlPriv/swagger" ]; then
        cd $dderlPriv/swagger
        rm -rf node_modules
        log purple "'swagger/node_modules' deleted"
    
        log green "npm install (swagger)"
        npm install
    else
        log red "unable to build dderl(swagger), missing $dderlPriv/swagger"
        exit 1
    fi

    log green "npm run build-prod"
    cd $dderlPriv/dev
    npm run build-prod

    # cleanup
    rm -rf $dderlPriv/dev
    log green "dir 'dev' deleted"
    
    rm -rf $dderlPriv/swagger
    log green "dir 'swagger' deleted"
fi
