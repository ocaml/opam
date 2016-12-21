#! /bin/sh -eu

FLAGS="-I `${OPAM} config var P1:lib`"

echo "Bytecode Compilation"
ocamlc ${FLAGS} -a p5.ml -o p5.cma

if which ocamlopt >/dev/null 2>&1; then
    echo "Native Compilation"
    ocamlopt ${FLAGS} -a p5.ml -o p5.cmxa
fi
