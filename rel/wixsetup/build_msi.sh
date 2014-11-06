#!/bin/bash

ScriptDir=$(cd ${0%/*} && pwd)
ProjDir=$ScriptDir/../../
Proj=dderl
ReleaseDir=$ScriptDir
"$ScriptDir/build_msi.escript" "$Proj" "$ProjDir" "$ReleaseDir" $1
