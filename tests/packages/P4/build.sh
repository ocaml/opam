#! /bin/sh -e

echo "Building P4 with ${OPAM}"
COMP0="`${OPAM} config -R asmcomp P2 P3`"
LINK0="`${OPAM} config -R asmlink P2 P3`"

COMP=$(echo ${COMP0} | sed -e 's| |,|g')
LINK=$(echo ${LINK0} | sed -e 's| |,|g')

ocamlbuild -cflags ${COMP} -lflags ${LINK} p4.native

echo "TEST=${TEST}"
