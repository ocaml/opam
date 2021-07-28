#!/bin/bash

opam init -y -a --compiler=ocaml.4.12.0 git+$OPAM_REPO#$OPAM_TEST_REPO_SHA
eval $(opam config env)
opam install -y -v ocamlfind
