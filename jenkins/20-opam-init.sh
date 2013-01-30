#!/bin/sh -ex
PREFIX=$1
if [ "$PREFIX" = "" ]; then echo Need prefix as first arg; exit 1; fi
cd src_ext
make distclean
cd ..
./configure --prefix=$HOME/opam-bin/$PREFIX
make
make install
make tests
