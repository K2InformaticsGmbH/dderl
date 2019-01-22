dderl
=====

[![Build Status](https://travis-ci.org/K2InformaticsGmbH/dderl.svg?branch=master)](https://travis-ci.org/K2InformaticsGmbH/dderl) [![Coverage Status](https://coveralls.io/repos/github/K2InformaticsGmbH/dderl/badge.svg?branch=master)](https://coveralls.io/github/K2InformaticsGmbH/dderl?branch=master)

WEB DataBase Browser Application.

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


