#!/bin/sh
cd _build/prod/rel/$1/lib
mv imem-*/ebin/filename.beam stdlib-*/ebin/
echo "===> replaced stdlib-*/ebin/filename.beam"

cd dderl-*/priv/dev
rm -rf node_modules
echo "===> dir 'node_modules' deleted"

echo "===> npm install"
npm install

echo "===> npm run build"
npm run build

cd ..
echo "===> dir 'dev' deleted"
rm -rf dev
