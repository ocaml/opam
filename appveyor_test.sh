#!/bin/bash

opam init -y -a --compiler=ocaml.4.09.1
eval $(opam config env)
opam install -y -v ocamlfind
