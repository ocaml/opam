#!/bin/bash
OCPGET="./ocp-get --root /tmp/OPAM.TEST"

echo "Bytecode Compilation"
ocamlopt -c `${OCPGET} config dir P1` p2.ml

echo "Native Compilation"
ocamlc -c `${OCPGET} config dir P1` p2.ml