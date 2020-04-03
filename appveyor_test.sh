#!/bin/bash

opam init -y -a --compiler=ocaml.4.07.1
eval $(opam config env)
opam install -y -v ocamlfind
