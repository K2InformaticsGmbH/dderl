#!/bin/sh
echo "===> dderl post_release"

prefix=_build/prod/rel/$1/lib
src=$prefix/imem-*/ebin/filename.beam
dst=$prefix/stdlib-*/ebin
dstfile=$dst/filename.beam

if [ -f $src ]; then
    mv $src $dst
    echo "===> replaced $dstfile"
elif [ -f $dstfile ]; then
    echo "===> already replaced $dstfile"
else
    echo "===> not found $src"
fi

dderlDev=$prefix/dderl-*/priv/dev
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

echo "===> npm run build"
npm run build

cd ..
echo "===> dir 'dev' deleted"
rm -rf dev
