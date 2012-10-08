#!/bin/sh -e
rm -rf .opam
./configure --prefix=$HOME/opam-bin
make
make install
