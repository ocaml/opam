#!/bin/sh -ex

V=ocaml-4.02.1
URL=http://caml.inria.fr/pub/distrib/ocaml-4.02/${V}.tar.gz
mkdir -p bootstrap
cd bootstrap
if [ ! -e ${V}.tar.gz ]; then
  curl -OL ${URL}
fi
tar -zxvf ${V}.tar.gz
cd ${V}
./configure -prefix `pwd`/../ocaml
make world opt
make install
