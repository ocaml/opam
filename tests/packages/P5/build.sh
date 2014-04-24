#! /bin/sh -eu

FLAGS="-I `${OPAM} config var P1:lib`"

echo "Bytecode Compilation"
ocamlopt ${FLAGS} -a p5.ml -o p5.cmxa

echo "Native Compilation"
ocamlc ${FLAGS} -a p5.ml -o p5.cma
