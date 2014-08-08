#!/bin/sh -ex

rm -rf bootstrap
mkdir bootstrap
cd bootstrap
curl -OL http://caml.inria.fr/pub/distrib/ocaml-4.02/ocaml-4.02.0+beta1.tar.gz
tar -zxvf ocaml-4.02.0+beta1.tar.gz
cd ocaml-4.02.0+beta1
./configure -prefix `pwd`/../ocaml
make world opt
make install
