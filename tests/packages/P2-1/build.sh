#!/bin/bash

OCPGET="ocp-get --root /tmp/OPAM.TEST"
FLAGS="`${OCPGET} config Include P1`"

echo ${FLAGS}

echo "Bytecode Compilation"
ocamlopt -c ${FLAGS} p2.ml

echo "Native Compilation"
ocamlc -c ${FLAGS} p2.ml
