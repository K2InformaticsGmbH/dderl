dderl
=====

[![Build Status](https://travis-ci.org/K2InformaticsGmbH/dderl.svg?branch=master)](https://travis-ci.org/K2InformaticsGmbH/dderl) [![Coverage Status](https://coveralls.io/repos/github/K2InformaticsGmbH/dderl/badge.svg?branch=master)](https://coveralls.io/github/K2InformaticsGmbH/dderl?branch=master)

WEB DataBase Browser Application.

### Port forwarding to run as non root user
* install rinetd
* chkconfig rinetd on
* add /etc/rinet.d
	0.0.0.0 80 0.0.0.0 8080
	0.0.0.0 443 0.0.0.0 8443
* service rinetd start

### Building with WIX
1. Create environment variable `WINSDKBIN` pointing to latest version of Microsoft Windows SDK bin path (for example `C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Bin\x64`) ensure `Uuidgen.Exe` exists in the path
1. Append `%WINSDKBIN%` to `%PATH%` environment variable
1. [Install Wix](http://wix.codeplex.com/downloads/get/762937)
1. Create Environment variable `%WIXBIN%` pointing to  WIX installation binary path (e.g. `C:\Program Files (x86)\WiX Toolset v3.8\bin`)
1. Append `%WIXBIN%` to `%PATH%` environment variable
1. Execute `rebar3 as prod release`
1. Execute `rebar3 as prod erlpkg`
1. Installer MSI will be generated in `_build/prod/rel/erlpkg`

### Hacks
Unlock account
```erlang
rr(imem_seco).
{[[A]], _} = imem_meta:select(ddAccount, [{#ddAccount{id=system,_='_'}, [], [['$_']]}]).
imem_meta:write(ddAccount, A#ddAccount{locked = false}).
```

### Windows Bash for Jenkins (Visual studio 2017 comunity)
```
call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
echo "--------------------------------------------------------"
echo "	PULL DEPENDENCIES"
echo "--------------------------------------------------------"
call rebar3 as prod get-deps
echo "--------------------------------------------------------"
echo "	BUILD RELEASE"
echo "--------------------------------------------------------" 
call rebar3 as prod release
echo "--------------------------------------------------------"
echo "	BUILD PACKAGE (MSI)"
echo "--------------------------------------------------------" 
call rebar3 as prod erlpkg
```

### shell script for Jenkins on CentOS building rpms
```
#!/bin/sh -e
source ~/.bashrc
echo "--------------------------------------------------------"
echo "	PULL DEPENDENCIES"
echo "--------------------------------------------------------"
rebar3 as prod get-deps
echo "--------------------------------------------------------"
echo "	BUILD RELEASE"
echo "--------------------------------------------------------" 
rebar3 as prod release
echo "--------------------------------------------------------"
echo "	BUILD PACKAGE (RPM)"
echo "--------------------------------------------------------" 
rebar3 as prod erlpkg
```

with ``.bashrc`` as:
```
# cat /var/lib/jenkins/.bashrc 
export PATH=$PATH:/usr/local/bin
export INSTANT_CLIENT_INCLUDE_PATH=/usr/include/oracle/12.2/client64
export INSTANT_CLIENT_LIB_PATH=/usr/lib/oracle/12.2/client64/lib/
export ERL_INTERFACE_DIR=/usr/lib/erlang/lib/erl_interface-3.10.1
```

### Certificates
DDErl runs on SSL. A default certificate/key pair is [supplied](https://github.com/k2informatics/dderl/tree/master/priv/certs). This, however can be changed either by replacing these files atinstallation or modifying configuration in `ddConfig` table (`[{dderl,dderl,dderlSslOpts}]`). A sample configuration is given below:
```erlang
[{cert,<<48,...,107>>},
 {key,{'RSAPrivateKey',<<48,...,192>>}},
 {versions,['tlsv1.2','tlsv1.1',tlsv1]}]
```
[`erlang:ssl`](http://erlang.org/doc/man/ssl.html) describes all possible options above.
To convert a PEM crt/key files to DER (accepted by erlang SSL binary certificate/key option above) [`public_key:pem_decode/1`](http://www.erlang.org/doc/man/public_key.html#pem_decode-1) may be used as follows to obtain the DER binary of the PEM certificate files:
```erlang
> {ok, PemCrt} = file:read_file("server.crt").
{ok,<<"-----BEGIN CERTIFICATE-----\nMIICyTC"...>>}
> public_key:pem_decode(PemCrt).
[{'Certificate',<<48,130,2,201,48,130,2,50,2,9,0,241,25,...>>,not_encrypted}]
> {ok, PemKey} = file:read_file("server.key").
{ok,<<"-----BEGIN RSA PRIVATE KEY-----\nMIICXAI"...>>}
> public_key:pem_decode(PemKey).              
[{'RSAPrivateKey',<<48,130,2,92,2,1,0,2,129,129,0,160,95,...>>,not_encrypted}]
```

#### For changing the partition time of rolling tables

1. partition time in the dderl tables are saved in seconds. So 86400 corresponds to a day
2. On [line 65 in dderl.erl](https://github.com/K2InformaticsGmbH/dderl/blob/master/src/dderl.erl#L65) change the number at the end of the table name to the partion time that you want to set. For example setting the partition time from day to a minute you have replace 
  ```erlang
  -define(cproLogTable,"dderlLog_86400@_").
  ```
  with
  
  ```erlang 
  -define(cproLogTable,"dderlLog_60@_").
  ```
3. Compile and load the code hot update the code into the node.
4. In dderl on table ddAlias change the row with qname as 

  ```erlang
  {sbsgui,dderlLog_86400@}
  ```
  to 
  ```erlang
  {sbsgui,dderlLog_60@}
  ```
for partitioning the table every minute.
5. Also make the similar change in table ddConfig for the record with hkl value [{dderl,dderl,dderlLogTable}]. Edit the column val.
