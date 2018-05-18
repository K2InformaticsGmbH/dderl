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

If (Test-Path "$dderlPriv\dev\node_modules") {
    throw "$dderlPriv\dev\node_modules already exists"
}

If (Test-Path "$dderlPriv\swagger\node_modules") {
	throw "$dderlPriv\swagger\node_modules already exists"
}

If (Test-Path "$dderlPriv\public") {
	throw "$dderlPriv\public already exists"
}

cd "$dderlPriv/swagger"
Write-Host "===> yarn @ $pwd" -foregroundcolor green
yarn

cd "$dderlPriv/dev"
Write-Host "===> yarn @ $pwd" -foregroundcolor green
yarn
Write-Host "===> yarn run build-prod @ $pwd" -foregroundcolor green
yarn run build-prod

function Remove-Recursive-Force([string]$Root, [string]$Dir) {
    Try {
        Remove-Item "$Root\$Dir" -Force -Recurse -ea Stop
        Write-Host "===> $Root\$Dir deleted" -foregroundcolor green
    }
    Catch {
        Write-Host "===> failed to delete $Root\$Dir" -foregroundcolor red
        md -Force C:\Temp
        Write-Host "===> C:\Temp\ created (if didn't exist)" -foregroundcolor green
        Remove-Item "C:\Temp\$Dir" -Force -Recurse -ErrorAction SilentlyContinue
        Write-Host "===> any existing C:\Temp\$Dir deleted" -foregroundcolor green
        Move-Item -Path "$Root\$Dir" -Force -Destination C:\Temp
        Write-Host "===> $Root\$Dir moved to C:\Temp\$Dir" -foregroundcolor green
        Remove-Item "C:\Temp\$Dir" -Force -Recurse
        Write-Host "===> C:\Temp\$Dir deleted" -foregroundcolor green
    }
}

# Cleanup
cd $dderlPriv
Remove-Recursive-Force $dderlPriv "swagger"
Remove-Recursive-Force $dderlPriv "dev"

Write-Host "===> ------------------------------------------------------------ post_release"
