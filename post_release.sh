. $(dirname $0)/common.sh

log green "-------------------------------------------------------------------------"
app=${1:-dderl}
log green "post_release $app @ $(pwd)"

dderlPriv=$(readlink -f _build/prod/rel/$app/lib/dderl-*/priv/)
log lightgrey "building dderl @ $dderlPriv"

if [ -d "$dderlPriv/dev/node_modules" ]; then
    log red "$dderlPriv/dev/node_modules already exists"
	exit 1
fi

if [ -d "$dderlPriv/swagger/node_modules" ]; then
	log red "$dderlPriv/swagger/node_modules already exists"
    exit 1
fi

if [ -d "$dderlPriv/public" ]; then
    log red "$dderlPriv/public already exists"
    exit 1
fi

cd $dderlPriv/swagger
log green "yarn @ $(pwd)"
yarn

cd $dderlPriv/dev
log green "yarn @ $(pwd)"
yarn
log green "yarn run build-prod @ $(pwd)"
yarn run build-prod

# cleanup
cd $dderlPriv
rm -rf $dderlPriv/dev
log green "dir $dderlPriv/dev deleted"

rm -rf $dderlPriv/swagger
log green "dir $dderlPriv/swagger deleted"
log green "------------------------------------------------------------ post_release"