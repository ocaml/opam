#!/bin/bash

OCPGET="ocp-get --root /tmp/OPAM.TEST"
INCLUDES="`${OCPGET} config Include P2 P3`"
INCLUDES=${INCLUDES// /,}

echo ${INCLUDES}

ocamlbuild -cflags ${INCLUDES} p4.native