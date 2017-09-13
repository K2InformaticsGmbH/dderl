#!/bin/sh
echo "===> dderl post_release"

libDir=_build/prod/rel/$1/lib
filename=$libDir/imem-*/ebin/filename.beam
imemApp=`echo $libDir/imem-*/ebin/imem.app`

if [ -f $filename ]; then
    rm -rf $filename
    sed -i 's/filename,//g' $imemApp
    echo "===> deleted $filename"
else
    echo "===> not found $src"
fi

dderlDev=$libDir/dderl-*/priv/dev
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
