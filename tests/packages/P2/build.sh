#! /bin/sh -eu

OFLAGS="`${OPAM} config var P1:asmcomp`"
CFLAGS="`${OPAM} config var P1:bytecomp`"

echo "Bytecode Compilation"
ocamlopt ${OFLAGS} -a p2.ml -o p2.cmxa

echo "Native Compilation"
ocamlc ${CFLAGS} -a p2.ml -o p2.cma
