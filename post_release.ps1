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

function Remove-Recursive-Force([string]$Root, [string]$Dir) {
    Try {
        Remove-Item "$Root\$Dir" -Force -Recurse -ea Stop
        Write-Host "===> $Root\$Dir deleted" -foregroundcolor green
    }
    Catch {
        Write-Host "===> failed to delete $Root\$Dir" -foregroundcolor red
        md -Force C:\Temp
        Write-Host "===> C:\Temp\ created (if didn't exist)" -foregroundcolor green
        Remove-Item "C:\Temp\$Dir" -Force -Recurse
        Write-Host "===> any existing C:\Temp\$Dir deleted" -foregroundcolor green
        Move-Item -Path "$Root\$Dir" -Force -Destination C:\Temp
        Write-Host "===> $Root\$Dir moved to C:\Temp\$Dir" -foregroundcolor green
        Remove-Item "C:\Temp\$Dir" -Force -Recurse
        Write-Host "===> C:\Temp\$Dir deleted" -foregroundcolor green
    }
}

# Cleanup
cd ..
Remove-Recursive-Force $dderlPriv "swagger"
Remove-Recursive-Force $dderlPriv "dev"

Write-Host "===> ------------------------------------------------------------ post_release"
