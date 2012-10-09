#!/bin/sh -ex
PREFIX=$1
if [ "$PREFIX" = "" ]; then echo Need prefix as first arg; exit 1; fi
./configure --prefix=$HOME/opam-bin/$PREFIX
make
make install
