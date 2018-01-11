#!/bin/sh -e

V=ocaml-4.06.0
URL=http://caml.inria.fr/pub/distrib/ocaml-4.06/${V}.tar.gz
mkdir -p bootstrap
cd bootstrap
if [ ! -e ${V}.tar.gz ]; then
  if command -v curl > /dev/null; then
    curl -OLSs ${URL}
  elif command -v wget > /dev/null; then
    wget ${URL}
  else
    echo "This script requires curl or wget"
    exit 1
  fi
fi
tar -zxf ${V}.tar.gz
cd ${V}
./configure -prefix "`pwd`/../ocaml"
${MAKE:-make} world opt.opt
${MAKE:-make} install
