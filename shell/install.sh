#!/bin/bash

# TODO: make this script BSD-compatible

git clone --depth 1 git://github.com/OCamlPro/opam.git
cd opam
./configure --prefix=/usr/local
make
sudo make install
