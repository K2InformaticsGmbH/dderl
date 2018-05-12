Param([string]$app="dderl")

$root = (Resolve-Path "_build/prod/rel/$app/").Path
cd $root
$erts = Get-ChildItem -Filter erts-* |
        Select-Object -First 1 -Expand FullName
$ertsIni = "$erts\bin\erl.ini"
If (Test-Path $ertsIni) {
    Remove-Item $ertsIni
    Write-Host "===> deleted $ertsIni" -foregroundcolor "magenta"
}
Else {
    Write-Host "===> not found $ertsIni" -foregroundcolor "red"
}

$dderlPriv = (Resolve-Path "$root\lib\dderl-*\priv").Path
Write-Host "===> building dderl @ $dderlPriv ..." -foregroundcolor "magenta"

If (Test-Path "$dderlPriv\dev") {
    cd "$dderlPriv\dev"
    Write-Host "===> building dderl (dev)@ $dderlPriv ..." -foregroundcolor "magenta"
} Else {
    Write-Host "===> dderl (dev) already built!" -foregroundcolor "magenta"
    exit
}

If (Test-Path node_modules) {
    # Remove-Item node_modules -Force -Recurse
    Write-Host "===> directory 'dev\node_modules' deleted" -foregroundcolor "magenta"
}

Write-Host "===> npm install (dev)" -foregroundcolor "magenta"
# npm install

If (Test-Path "$dderlPriv\swagger") {
    cd "$dderlPriv\swagger"
    Write-Host "===> building dderl (swagger) @ $dderlPriv ..." -foregroundcolor "magenta"
} Else {
    Write-Host "===> dderl (swagger) already built!" -foregroundcolor "magenta"
    exit
}

If (Test-Path node_modules) {
    Remove-Item node_modules -Force -Recurse
    Write-Host "===> directory 'swagger\node_modules' deleted" -foregroundcolor "magenta"
}

Write-Host "===> npm install (swagger)" -foregroundcolor "magenta"
npm install

cd "$dderlPriv\dev"
Write-Host "===> npm run build-prod" -foregroundcolor "magenta"
npm run build-prod

Write-Host "===> clean up dev"
If (Test-Path "$dderlPriv\dev") {
    If (!(Get-ItemProperty "$dderlPriv\dev").Target) {
        Remove-Item "$dderlPriv\dev" -Force -Recurse
        Write-Host "===> directory $dderlPriv\dev deleted" -foregroundcolor "magenta"
    } Else {
        Write-Host "===> directory $dderlPriv\dev is a symbolic link - no cleanup" -foregroundcolor "magenta"
        If (Test-Path "$dderlPriv\dev\node_modules") {
            Remove-Item "$dderlPriv\dev\node_modules" -Force -Recurse
            Write-Host "===> directory 'dev\node_modules' deleted" -foregroundcolor "magenta"
        }
    }
} Else {
        Write-Host "===> directory $dderlPriv\dev not found" -foregroundcolor "magenta"
}

Write-Host "===> clean up swagger"
If (Test-Path "$dderlPriv\swagger") {
    If (!(Get-ItemProperty "$dderlPriv\swagger").Target) {
        Remove-Item "$dderlPriv\swagger" -Force -Recurse
        Write-Host "===> directory $dderlPriv\swagger deleted" -foregroundcolor "magenta"
    } Else {
        Write-Host "===> directory $dderlPriv\swagger is a symbolic link - no cleanup" -foregroundcolor "magenta"
        If (Test-Path "$dderlPriv\swagger\node_modules") {
            Remove-Item "$dderlPriv\swagger\node_modules" -Force -Recurse
            Write-Host "===> directory 'swagger\node_modules' deleted" -foregroundcolor "magenta"
        }
    }
} Else {
        Write-Host "===> directory $dderlPriv\swagger not found" -foregroundcolor "magenta"
}