#!/bin/bash

echo "Bytecode Compilation"
ocamlopt -c -I `$(OCPGET) -dir P1` p2.ml

echo "Native Compilation"
ocamlc -c -I `$(OCPGET) -dir P1` p2.ml