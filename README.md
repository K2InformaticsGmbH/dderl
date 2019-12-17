dderl
=====

![Travis (.org)](https://img.shields.io/travis/K2InformaticsGmbH/dderl.svg)
![Coveralls github](https://img.shields.io/coveralls/github/K2InformaticsGmbH/dderl.svg)
![GitHub](https://img.shields.io/github/license/K2InformaticsGmbH/dderl.svg)
![GitHub release](https://img.shields.io/github/release/K2InformaticsGmbH/dderl.svg)
![GitHub Release Date](https://img.shields.io/github/release-date/K2InformaticsGmbH/dderl.svg)
![GitHub commits since latest release](https://img.shields.io/github/commits-since/K2InformaticsGmbH/dderl/3.7.2.svg)

WEB DataBase Browser Application.

### Build (Supported erlang OTP version - 20.2)

1. `git clone https://github.com/K2InformaticsGmbH/dderl` in `$ROOT`
1. cd `$ROOT/dderl`
1. `NO_OCI=true rebar3 compile`
1. To compile with erloci follow setup instruction at https://github.com/K2InformaticsGmbH/erloci and execute the above two commands without `NO_OCI=true
1. cd `priv/dev`
1. `yarn install-build-prod`
1. cd `$ROOT/dderl`
1. `./start.sh`
1. go to https://127.0.0.1:8443/dderl in your browser

### Features

1. Browse mnesia and oracle tables in the browser
2. Add and update data
3. Import and Export data
4. Send and receive data from one desitination to other on the same session
5. SQL support for queries
6. Filter, Sort, Distinct and Statistics on data
7. Multifactor authentication support (SMS, SAML and username/password)
8. JSON parsing with SQL 
9. Tailing of tables 
10. Log table rotation and purging
11. Snapshot and restore table
12. Cluster backup and restore
13. Configuration encryption for ssl certificates and passwords
14. D3 graph support to plot graphs
15. Save views of tables 
16. Query history support
17. Connect to other imem server over TCP with SSL
18. CSV file parsing

![screenshot](https://github.com/K2InformaticsGmbH/dderl/blob/master/docs/dderl_screenshot.png)


### Certificates
DDErl runs on SSL. A default certificate/key pair is [supplied](https://github.com/k2informatics/dderl/tree/master/priv/certs). This, however can be changed either by replacing these files at installation or modifying configuration in `ddConfig` table (`[{dderl,dderl,dderlSslOpts}]`). A sample configuration is given below:
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


