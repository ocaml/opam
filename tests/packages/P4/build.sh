#!/bin/bash -e

echo "Building P4 with ${OPAM}"
COMP0="`${OPAM} config -R asmcomp P2 P3`"
LINK0="`${OPAM} config -R asmlink P2 P3`"

COMP=${COMP0// /,}
LINK=${LINK0// /,}

ocamlbuild -cflags ${COMP} -lflags ${LINK} p4.native

echo "TEST=${TEST}"