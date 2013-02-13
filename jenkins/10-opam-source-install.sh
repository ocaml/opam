#!/bin/sh -ex

rm -f nlopt-2.3.tar.gz
curl -OL http://ab-initio.mit.edu/nlopt/nlopt-2.3.tar.gz
tar -zxvf nlopt-2.3.tar.gz
cd nlopt-2.3
env CFLAGS="-fPIC" ./configure && make
sudo make install
cd ..
rm -rf nlopt-2.3

rm -f libsrs2-1.0.18.tar.gz
curl -OL http://www.libsrs2.org/srs/libsrs2-1.0.18.tar.gz
tar -zxvf libsrs2-1.0.18.tar.gz
cd libsrs2-1.0.18
./configure && make
sudo make install
cd ..
rm -rf libsrs2-1.0.18
