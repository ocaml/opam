#! /bin/sh -eu

FLAGS="`${OPAM} config includes P1`"

echo "Bytecode Compilation"
ocamlopt ${FLAGS} -a p2.ml -o p2.cmxa

echo "Native Compilation"
ocamlc ${FLAGS} -a p2.ml -o p2.cma
