#!/bin/sh
cd _build/prod/rel/dderl/lib
mv imem-*/ebin/filename.beam stdlib-*/ebin/

cd dderl-*/priv/dev
rm -rf node_modules
npm install
npm run build

cd ..
rm -rf dev
