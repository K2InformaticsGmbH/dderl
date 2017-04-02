#!/usr/bin/env bash

apt-get update

apt-get -y install git
apt-get -y install npm
apt-get -y install nodejs

ln -s /usr/bin/nodejs /usr/bin/node

apt-get -y install g++
apt-get -y install make

apt-get -y install libevent-dev

wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb

apt-get update
apt-get -y install esl-erlang=1:18.3.4
apt-get -f -y install
apt-get -y install erlang-doc=1:18.3.4.4+dfsg-1ubuntu2
apt-get -f -y install
