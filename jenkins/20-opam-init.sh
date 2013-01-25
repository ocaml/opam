#!/bin/sh -ex
PREFIX=$1
if [ "$PREFIX" = "" ]; then echo Need prefix as first arg; exit 1; fi
make distclean
./configure --prefix=$HOME/opam-bin/$PREFIX
make
make install
