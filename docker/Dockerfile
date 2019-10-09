#Dockerfile for DDERL only
FROM centos:7

RUN yum group install -y "Development Tools" 

#ERLANG and a few other things
RUN yum install -y epel-release wget git bash coreutils
RUN wget https://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm && rpm -Uvh erlang-solutions-1.0-1.noarch.rpm  && yum install -y esl-erlang-20.2.2-1
RUN yum install -y libevent libevent-devel libaio
RUN wget https://nodejs.org/download/release/v10.15.1/node-v10.15.1-linux-x64.tar.xz && VERSION=v10.15.1 && DISTRO=linux-x64 && mkdir -p /usr/local/lib/nodejs && tar -xJvf node-$VERSION-$DISTRO.tar.xz -C /usr/local/lib/nodejs 
RUN curl --silent --location https://dl.yarnpkg.com/rpm/yarn.repo | tee /etc/yum.repos.d/yarn.repo
RUN yum install -y yarn

RUN wget https://s3.amazonaws.com/rebar3/rebar3 && mv rebar3 /usr/local/bin/ && chmod +x /usr/local/bin/rebar3

ADD runStart.sh /root

RUN cd ~ && git clone https://github.com/K2InformaticsGmbH/dderl

# removing Oracle dependency (erloci) by manipulating rebar.config.script
RUN echo -e "begin\n\
    Deps = proplists:get_value(deps, CONFIG),\n\
    NewDeps = lists:keydelete(erloci, 1, Deps),\n\
    {ok, [{application, dderl, Props}]}\n\
    = file:consult(\"src/dderl.app.src\"),\n\
    Apps = proplists:get_value(applications, Props) -- [erloci],\n\
    NewAppSrc = {application, dderl, lists:keyreplace(applications, 1, Props, {applications, Apps})},\n\
    file:write_file(\"src/dderl.app.src\", list_to_binary(io_lib:format(\"~p.\",[NewAppSrc]))),\n\
    lists:keyreplace(deps, 1, CONFIG, {deps, NewDeps})\n\
end." > ~/dderl/rebar.config.script

#replace the 127.0.0.1 with the container's own IP address (found using "awk 'END{print $1}' /etc/hosts"), so we can forward a port on the docker host to the container holding the DDERL
RUN MYIP=`awk 'END{print $1}' /etc/hosts` && sed -i "s/127.0.0.1/${MYIP}/g" ~/dderl/start.sh ~/dderl/service.sh ~/dderl/rebar.config ~/dderl/src/dderl.app.src ~/dderl/config/sys_shell.config ~/dderl/config/vars.config 
RUN MYIP=`awk 'END{print $1}' /etc/hosts | sed -e 's/\./,/g'` && sed -i "s/127,0,0,1/${MYIP}/g" ~/dderl/config/sys_shell.config ~/dderl/src/dderl_rest.erl ~/dderl/src/dderl.hrl

#DDERL compilation
RUN cd ~/dderl/ && rebar3 get-deps && rebar3 compile
RUN cd ~/dderl/priv/dev && npm install && npm run build 

EXPOSE 8443/tcp
EXPOSE 9443/tcp

ENTRYPOINT ["sh", "/root/runStart.sh"]
