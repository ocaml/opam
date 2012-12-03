#!/bin/sh -ex

rm -f nlopt-2.3.tar.gz
curl -OL http://ab-initio.mit.edu/nlopt/nlopt-2.3.tar.gz
tar -zxvf nlopt-2.3.tar.gz
cd nlopt-2.3
env CFLAGS="-fPIC" ./configure && make
sudo make install
cd ..
rm -rf nlopt-2.3
