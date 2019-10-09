Commands to run (build, run and connect to it):
-----------------------------------------------
```
docker build --tag "dderl:latest" .
```
```
docker run -it -p 8443:8443 -p 9443:9443 dderl
```
connect from the host's browser with https://localhost:8443/dderl
    
  
Note:
-----
Dockerfile created by adapting the old Vagrant file in dderl/vag.  
  
  
