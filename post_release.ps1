cd _build/default/rel/dderl/lib/dderl-*/priv/
If (Test-Path node_modules) {
    Remove-Item node_modules -Force -Recurse
    Write-Host "===> node_modules dir deleted"
}
Write-Host "===> npm install"
npm install

If (Test-Path public) {
    Remove-Item public -Force -Recurse
    Write-Host "===> public dir deleted"
}
Write-Host "===> npm run build"
npm run build

Write-Host "===> clean up" -foregroundcolor "green"
$delFiles = "d3_*","node_modules","static","test",`
            "karma.conf.js",".jshintrc*","package.json*"
For($i = 0; $i -lt $delFiles.Length; $i++) {
    If (Test-Path $delFiles[$i]) {
        Remove-Item $delFiles[$i] -Force -Recurse
        Write-Host "===> "$delFiles[$i]" deleted"
    } Else {
        Write-Host "===> "$delFiles[$i]" not found!"
    }
}
