#!/usr/bin/env sh
sudo apt-get -qq update
sudo apt-get -qq -y install wget xz-utils
wget http://files.metasepi.org/ajhc/sat_ubuntu1204_64bit.tar.xz
tar xf sat_ubuntu1204_64bit.tar.xz
