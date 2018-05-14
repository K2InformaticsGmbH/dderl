. $(dirname $0)/common.sh

app=${1:-dderl}
log green "post_release $app $(pwd)"

dderlPriv=$(readlink -f _build/prod/rel/$app/lib/dderl-*/priv/)
log lightgrey "path: $dderlPriv"

if [ ! -d "$dderlPriv/dev/node_modules" ]; then
    log red "unable to build dderl(dev), missing $dderlPriv/dev/node_modules"
    exit 1
fi

if [ ! -d "$dderlPriv/swagger/node_modules" ]; then
    log red "unable to build dderl(swagger), missing $dderlPriv/swagger/node_modules"
    exit 1
fi

if [ -d "$dderlPriv/public" ]; then
    log blue "found dderl(dev+swagger) debug build"
    rm -rf $dderlPriv/public
    log purple "found $dderlPriv/public"
fi

log green "npm run build-prod"
cd $dderlPriv/dev
npm run build-prod

# # cleanup
# rm -rf $dderlPriv/dev
# log green "dir 'dev' deleted"
# 
# rm -rf $dderlPriv/swagger
# log green "dir 'swagger' deleted"
