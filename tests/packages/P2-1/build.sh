#!/bin/bash

OCPGET="ocp-get --root /tmp/OPAM.TEST"
FLAGS="`${OCPGET} config Include P1`"

echo "Bytecode Compilation"
ocamlopt ${FLAGS} -a p2.ml -o p2.cmxa

echo "Native Compilation"
ocamlc ${FLAGS} -a p2.ml -o p2.cma
