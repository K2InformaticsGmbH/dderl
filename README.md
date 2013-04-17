dderl
=====

WEB Application DataBase Browser.

Port forwarding to run as non root user
* install rinetd
* chkconfig rinetd on
* add /etc/rinet.d
	0.0.0.0 80 0.0.0.0 8080
	0.0.0.0 443 0.0.0.0 8443
* service rinetd start
