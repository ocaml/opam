#!/bin/bash

opam init -y -a --compiler=ocaml.4.09.1 git+https://github.com/ocaml/opam-repository#$OPAM_REPO_SHA
eval $(opam config env)
opam install -y -v ocamlfind
