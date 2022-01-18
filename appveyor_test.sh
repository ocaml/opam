#!/bin/bash

COLD_OCAML="$(sed -ne 's/URL_ocaml = .*ocaml-.*ocaml-\(.*\)\.tar\.gz/ocaml.\1/p' src_ext/Makefile)"
# This is supposed to be able to select ocaml-system using the bootstrap compiler
opam init -y -a --compiler=$COLD_OCAML git+$OPAM_REPO#$OPAM_TEST_REPO_SHA
eval $(opam config env)
opam install -y -v ocamlfind
