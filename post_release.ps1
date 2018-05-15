Param([string]$app="dderl")
Write-Host "===> -------------------------------------------------------------------------"
Write-Host "===> post_release $app @ $pwd" -foregroundcolor green

$root = (Resolve-Path "_build/prod/rel/$app/").Path
cd $root
$erts = Get-ChildItem -Filter erts-* |
        Select-Object -First 1 -Expand FullName
$ertsIni = "$erts\bin\erl.ini"
If (Test-Path $ertsIni) {
    Remove-Item $ertsIni
    Write-Host "===> deleted $ertsIni" -foregroundcolor magenta
}
Else {
    Write-Host "===> not found $ertsIni" -foregroundcolor red
}

$dderlPriv = (Resolve-Path "$root\lib\dderl-*\priv").Path
Write-Host "===> building dderl @ $dderlPriv ..." -foregroundcolor gray

If (!(Test-Path "$dderlPriv\dev\node_modules")) {
    throw "unable to build dderl (dev), $dderlPriv\dev\node_modules doesn't exist"
}

If (!(Test-Path "$dderlPriv\swagger\node_modules")) {
    throw "unable to build dderl (swager), $dderlPriv\swagger\node_modules doesn't exist"
}

If (Test-Path "$dderlPriv\public") {
    Write-Host "===> found dderl(dev+swagger) debug build" -foregroundcolor blue
    Remove-Item "$dderlPriv\public" -Force -Recurse
    Write-Host "===> deleted $dderlPriv/public" -foregroundcolor magenta
}

cd "$dderlPriv\dev"
Write-Host "===> npm run build-prod @ $pwd" -foregroundcolor green
npm run build-prod

# Cleanup
cd ..
Remove-Item "$dderlPriv\swagger" -Force -Recurse
Write-Host "===> $dderlPriv/swagger deleted" -foregroundcolor green

Remove-Item "$dderlPriv\dev" -Force -Recurse
Write-Host "===> $dderlPriv/dev deleted" -foregroundcolor green

Write-Host "===> ------------------------------------------------------------ post_release"