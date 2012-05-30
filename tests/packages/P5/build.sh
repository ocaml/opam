#!/bin/bash

OCPGET="opam --root ${OPAM_ROOT}"
FLAGS="`${OCPGET} config -I P1`"

echo "Bytecode Compilation"
ocamlopt ${FLAGS} -a p5.ml -o p5.cmxa

echo "Native Compilation"
ocamlc ${FLAGS} -a p5.ml -o p5.cma
