#!/bin/sh -ex

rm -rf bootstrap
mkdir bootstrap
cd bootstrap
curl -OL http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz
tar -zxvf ocaml-4.01.0.tar.gz
cd ocaml-4.01.0
./configure -prefix `pwd`/../ocaml
make world opt
make install
