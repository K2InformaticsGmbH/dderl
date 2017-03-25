cd _build/default/rel/dderl/lib/dderl-*/priv/
Write-Host "npm install"
Remove-Item node_modules -Force -Recurse
npm install

Write-Host "npm run build"
Remove-Item public -Force -Recurse
npm run build

Write-Host "clean up"
Remove-Item d3_* -Force -Recurse
Remove-Item node_modules -Force -Recurse
Remove-Item static -Force -Recurse
Remove-Item test -Force -Recurse
Remove-Item karma.config.js -Force -Recurse
Remove-Item .jshintrc -Force -Recurse
Remove-Item package.config -Force -Recurse
