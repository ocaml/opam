#!/bin/bash

OCPGET="ocp-get --root /tmp/OPAM.TEST"
INCLUDES0="`${OCPGET} config R Include P2 P3`"
INCLUDES=${INCLUDES0// /,}

echo ${INCLUDES0}
echo ${INCLUDES}

ocamlbuild -cflags ${INCLUDES} p4.native