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
* Create environment variable `WINSDKBIN` pointing to latest version of Microsoft Windows SDK bin path (for example C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Bin\x64) ensure `Uuidgen.Exe` exists in the path
* Append `%WINSDKBIN%` to `%PATH%` environment variable
* [Install Wix](http://wix.codeplex.com/downloads/get/762937)
* Create Environment variable `%WIXBIN%` pointing to  WIX installation binary path (e.g. C:\Program Files (x86)\WiX Toolset v3.8\bin)
* Append `%WIXBIN%` to `%PATH%` environment variable
* Navigate to `dderl/rel/wixsetup` and execute `./build_msi.escript`
* Installer MSI will be generated in `dderl/rel/wixsetup`
