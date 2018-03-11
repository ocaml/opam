#!/bin/bash

opam init -y -a --compiler=ocaml.4.11.1 'git+https://github.com/dra27/opam-repository.git#windows'
eval $(opam config env)
opam install -y -v ocamlfind
