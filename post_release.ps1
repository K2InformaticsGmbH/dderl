cd _build/default/rel/dderl/lib

$source = Get-ChildItem -Filter imem-* |
          Select-Object -First 1 -Expand FullName
$target = Get-ChildItem -Filter stdlib-* |
          Select-Object -First 1 -Expand FullName
Move-Item $source\ebin\filename.beam -Destination $target\ebin -Force

cd dderl-*/priv/dev

If (Test-Path node_modules) {
    Remove-Item node_modules -Force -Recurse
    Write-Host "===> directory 'node_modules' deleted"
}

Write-Host "===> npm install"
npm install

Write-Host "===> npm run build"
npm run build

cd ..
Write-Host "===> clean up"
$Path = "dev"
If (Test-Path $Path) {
    If (!(Get-ItemProperty $Path).Target) {
        Remove-Item $Path -Force -Recurse
        Write-Host "===> directory '" -nonewline
        Write-Host $Path -nonewline
        Write-Host "' deleted"
    } Else {
        Write-Host "===> directory '" -nonewline
        Write-Host $Path -nonewline
        Write-Host "' is a symbolic link - no cleanup"
        If (Test-Path dev/node_modules) {
            Remove-Item dev/node_modules -Force -Recurse
            Write-Host "===> directory 'dev/node_modules' deleted"
        }
    }
} Else {
        Write-Host "===> directory '" -nonewline
        Write-Host $Path -nonewline
        Write-Host "' not found"
}
