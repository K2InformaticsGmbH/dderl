dderl
=====

WEB DataBase Browser Application.

###Port forwarding to run as non root user
* install rinetd
* chkconfig rinetd on
* add /etc/rinet.d
	0.0.0.0 80 0.0.0.0 8080
	0.0.0.0 443 0.0.0.0 8443
* service rinetd start

###Building with WIX
1. Create environment variable `WINSDKBIN` pointing to latest version of Microsoft Windows SDK bin path (for example `C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Bin\x64`) ensure `Uuidgen.Exe` exists in the path
2. Append `%WINSDKBIN%` to `%PATH%` environment variable
3. [Install Wix](http://wix.codeplex.com/downloads/get/762937)
4. Create Environment variable `%WIXBIN%` pointing to  WIX installation binary path (e.g. `C:\Program Files (x86)\WiX Toolset v3.8\bin`)
5. Append `%WIXBIN%` to `%PATH%` environment variable
6. Navigate to `dderl/rel/wixsetup` and execute `./build_msi.escript`
7. Installer MSI will be generated in `dderl/rel/wixsetup`

###Hacks
Unlock account
```erlang
rr(imem_seco).
{[[A]], _} = imem_meta:select(ddAccount, [{#ddAccount{id=system,_='_'}, [], [['$_']]}]).
imem_meta:write(ddAccount, A#ddAccount{locked = false}).
```
