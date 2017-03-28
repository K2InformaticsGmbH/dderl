cd _build/default/rel/dderl/lib

$source = Get-ChildItem -Filter imem-* |
          Select-Object -First 1 -Expand FullName
$target = Get-ChildItem -Filter stdlib-* |
          Select-Object -First 1 -Expand FullName
Move-Item $source\ebin\filename.beam -Destination $target\ebin -Force

cd dderl-*/priv/dev

If (Test-Path node_modules) {
    Remove-Item node_modules -Force -Recurse
    Write-Host "===> node_modules dir deleted"
}
Write-Host "===> npm install"
npm install

Write-Host "===> npm run build"
npm run build

cd ..
Write-Host "===> clean up"
If (Test-Path "dev") {
    Remove-Item "dev" -Force -Recurse
    Write-Host "===> dir 'dev' deleted"
} Else {
    Write-Host "===> dir 'dev' not found!"
}
