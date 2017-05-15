cd _build/prod/rel/dderl/lib

$source = Get-ChildItem -Filter imem-* |
          Select-Object -First 1 -Expand FullName
$target = Get-ChildItem -Filter stdlib-* |
          Select-Object -First 1 -Expand FullName
Move-Item $source\ebin\filename.beam -Destination $target\ebin -Force

cd dderl-*/priv/dev

If (Test-Path node_modules) {
    Remove-Item node_modules -Force -Recurse
    Write-Host "===> directory 'node_modules' deleted" -foregroundcolor "magenta"
}

Write-Host "===> npm install" -foregroundcolor "magenta"
npm install

Write-Host "===> npm run build" -foregroundcolor "magenta"
npm run build

cd ..
Write-Host "===> clean up"
$Path = "dev"
If (Test-Path $Path) {
    If (!(Get-ItemProperty $Path).Target) {
        Remove-Item $Path -Force -Recurse
        Write-Host "===> directory '" -nonewline -foregroundcolor "magenta"
        Write-Host $Path -nonewline -foregroundcolor "magenta"
        Write-Host "' deleted" -foregroundcolor "magenta"
    } Else {
        Write-Host "===> directory '" -nonewline -foregroundcolor "magenta"
        Write-Host $Path -nonewline -foregroundcolor "magenta"
        Write-Host "' is a symbolic link - no cleanup" -foregroundcolor "magenta"
        If (Test-Path dev/node_modules) {
            Remove-Item dev/node_modules -Force -Recurse
            Write-Host "===> directory 'dev/node_modules' deleted" -foregroundcolor "magenta"
        }
    }
} Else {
        Write-Host "===> directory '" -nonewline -foregroundcolor "magenta"
        Write-Host $Path -nonewline -foregroundcolor "magenta"
        Write-Host "' not found" -foregroundcolor "magenta"
}
