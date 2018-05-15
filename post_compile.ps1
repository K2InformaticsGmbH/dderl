Write-Host "===> -------------------------------------------------------------------------"
$dderlRoot = (Resolve-Path "_build").Path
If (Test-Path "$dderlRoot/default/lib/dderl/priv") {
	$dderlPriv="$dderlRoot/default/lib/dderl/priv"
}
ElseIf (Test-Path "$dderlRoot/prod/lib/dderl/priv") {
	$dderlPriv="$dderlRoot/prod/lib/dderl/priv"
}
Else {
    $dderlPriv= (Resolve-Path "priv").Path
}

$dderlPrivPublic = "$dderlPriv/public"

Write-Host "===> post_compile dderl $pwd" -foregroundcolor green
Write-Host "===> dderlPriv $dderlPriv" -foregroundcolor gray
If (Test-Path $dderlPrivPublic) {
	Write-Host "===> dderl front-end already built!" -foregroundcolor magenta
	exit
}
Else {
	Write-Host "===> building dderl front-end..." -foregroundcolor green
	cd "$dderlPriv/dev"
	If (Test-Path node_modules) {
		Write-Host "===> deleting 'dev/node_modules' ..." -foregroundcolor magenta
		Remove-Item node_modules -Force -Recurse
		Write-Host "===> 'dev/node_modules' deleted" -foregroundcolor magenta
	}

	Write-Host "===> npm install (dev) @ $pwd" -foregroundcolor green
	npm install

	cd "$dderlPriv/swagger"
	If (Test-Path node_modules) {
		Write-Host "===> deleting 'swagger/node_modules' ..." -foregroundcolor magenta
		Remove-Item node_modules -Force -Recurse
		Write-Host "===> 'swagger/node_modules' deleted" -foregroundcolor magenta
	}
    
	Write-Host "===> npm install (swagger) @ $pwd" -foregroundcolor green
	npm install

	# builds both dderl and swagger
	cd "$dderlPriv/dev"
	Write-Host "===> npm run build @ $pwd" -foregroundcolor green
	npm run build

	Write-Host "===> dderl front-end built!" -foregroundcolor green
}

Write-Host "===> ------------------------------------------------------------ post_compile"