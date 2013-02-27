#! /bin/sh -eu

FLAGS="`${OPAM} config includes P1`"

echo "Bytecode Compilation"
ocamlopt ${FLAGS} -a p5.ml -o p5.cmxa

echo "Native Compilation"
ocamlc ${FLAGS} -a p5.ml -o p5.cma
