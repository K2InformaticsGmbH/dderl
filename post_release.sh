#!/bin/sh
echo "===> dderl post_release"

app=${1:-dderl}
dderlDev=_build/prod/rel/${app}/lib/dderl-*/priv/dev
if [ -d $dderlDev ]; then
    cd $dderlDev
    rm -rf node_modules
    echo "===> dir 'node_modules' deleted"
else
    echo "===> dderl-npm already built!"
    exit 0
fi

echo "===> npm install"
npm install

echo "===> npm run build-prod"
npm run build

cd ..
echo "===> dir 'dev' deleted"
rm -rf dev
