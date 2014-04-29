#! /bin/sh -e

echo "Building P4 with ${OPAM}"
LIBDIR="`${OPAM} config var lib`"
COMP="-I ${LIBDIR}/P1 -I ${LIBDIR}/P2 -I ${LIBDIR}/P3"
LINK="p1.cmxa p2.cmxa p3.cmxa p3_bar.cmxa"

ocamlopt ${COMP} ${LINK} p4.ml -o p4.foo

echo "TEST=${TEST}"
