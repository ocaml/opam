#!/bin/sh -ex

if [ ! -e ocaml-4.00.1.tar.bz2 ]; then 
  wget http://caml.inria.fr/pub/distrib/ocaml-4.00/ocaml-4.00.1.tar.bz2
fi
rm -rf ocaml-4.00.1
tar -jxvf ocaml-4.00.1.tar.bz2
cd ocaml-4.00.1
./configure -prefix /x/ocaml-4.00.1
make world world.opt install
