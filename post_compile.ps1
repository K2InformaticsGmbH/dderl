Write-Host "===> post_compile dderl" -foregroundcolor "magenta"

$dderlPriv = (Resolve-Path "_build/default/lib/dderl/priv").Path
$dderlPrivPublic = "$dderlPriv/public"
If (Test-Path $dderlPrivPublic) {
	Write-Host "===> dderl front-end already built!" -foregroundcolor "magenta"
	exit
}
Else {
	Write-Host "===> building dderl front-end..." -foregroundcolor "magenta"
	cd "$dderlPriv/dev"
	If (Test-Path node_modules) {
		Write-Host "===> deleting 'dev/node_modules' ..." -foregroundcolor "magenta"
		Remove-Item node_modules -Force -Recurse
		Write-Host "===> 'dev/node_modules' deleted" -foregroundcolor "magenta"
	}

	Write-Host "===> npm install (dev)" -foregroundcolor "magenta"
	npm install

	cd "$dderlPriv/swagger"
	If (Test-Path node_modules) {
		Write-Host "===> deleting 'swagger/node_modules' ..." -foregroundcolor "magenta"
		Remove-Item node_modules -Force -Recurse
		Write-Host "===> 'swagger/node_modules' deleted" -foregroundcolor "magenta"
	}
    
	Write-Host "===> npm install (swagger)" -foregroundcolor "magenta"
	npm install

	# builds both dderl and swagger
	cd "$dderlPriv/dev"
	Write-Host "===> npm run build" -foregroundcolor "magenta"
	npm run build

	Write-Host "===> dderl front-end built!" -foregroundcolor "magenta"
}
