#! /bin/sh -eu

OFLAGS="`${OPAM} config var P1:asmcomp | tr -d '\r'`"
CFLAGS="`${OPAM} config var P1:bytecomp | tr -d '\r'`"

echo "Bytecode Compilation"
ocamlc ${CFLAGS} -a p2.ml -o p2.cma

if which ocamlopt >/dev/null 2>&1; then
    echo "Native Compilation"
    ocamlopt ${OFLAGS} -a p2.ml -o p2.cmxa
fi

