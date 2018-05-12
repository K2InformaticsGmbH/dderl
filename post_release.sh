#!/bin/sh
if [ -z "$1" ]
then
    app="dderl"
else
    app="$1"
fi
echo "===> post_release $1"

dderlPriv=$(readlink -f _build/prod/rel/$1/lib/dderl-*/priv/)
if [ -d "$dderlPriv/dev" ]; then
    cd $dderlPriv/dev
    rm -rf node_modules
    echo "===> dir 'dev/node_modules' deleted"
else
    echo "===> dderl-dev already built!"
    exit 0
fi

echo "===> npm install"
npm install

if [ -d "$dderlPriv/swagger" ]; then
    cd $dderlPriv/swagger
    rm -rf node_modules
    echo "===> dir 'swagger/node_modules' deleted"
else
    echo "===> dderl-swagger already built!"
    exit 0
fi

echo "===> npm run build-prod"
cd $dderlPriv/dev
npm run build-prod

rm -rf $dderlPriv/dev
echo "===> dir 'dev' deleted"

rm -rf $dderlPriv/swagger
echo "===> dir 'swagger' deleted"