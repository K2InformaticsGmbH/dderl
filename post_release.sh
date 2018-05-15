. $(dirname $0)/common.sh

app=${1:-dderl}
log green "post_release $app $(pwd)"

dderlPriv=$(readlink -f _build/prod/rel/$app/lib/dderl-*/priv/)
log lightgrey "path: $dderlPriv"

if [ -d "$dderlPriv/dev/node_modules" ]; then
    log blue "$dderlPriv/dev/node_modules already exists"
    log blue "this needs to be deleted and re-installed for lint to work..."
    rm -rf $dderlPriv/dev/node_modules
    log purple "deleted $dderlPriv/dev/node_modules"
fi
cd $dderlPriv/dev
log green "npm install @ $(pwd)"
npm install

if [ ! -d "$dderlPriv/swagger/node_modules" ]; then
    log red "unable to build dderl(swagger), missing $dderlPriv/swagger/node_modules"
    exit 1
fi

if [ -d "$dderlPriv/public" ]; then
    log blue "found dderl(dev+swagger) debug build"
    rm -rf $dderlPriv/public
    log purple "deleted $dderlPriv/public"
fi

cd $dderlPriv/dev
log green "npm run build-prod @ $(pwd)"
npm run build-prod

# cleanup
rm -rf $dderlPriv/dev
log green "dir 'dev' deleted"

rm -rf $dderlPriv/swagger
log green "dir 'swagger' deleted"
