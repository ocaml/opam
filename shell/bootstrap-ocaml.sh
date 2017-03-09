#!/bin/sh -ex

V=ocaml-4.04.0
URL=http://caml.inria.fr/pub/distrib/ocaml-4.04/${V}.tar.gz
mkdir -p bootstrap
cd bootstrap
if [ ! -e ${V}.tar.gz ]; then
  if command -v curl > /dev/null; then
    curl -OL ${URL}
  elif command -v wget > /dev/null; then
    wget ${URL}
  else
    echo "This script requires curl or wget"
    exit 1
  fi
fi
tar -zxvf ${V}.tar.gz
cd ${V}
./configure -prefix "`pwd`/../ocaml"
make world opt.opt
make install
