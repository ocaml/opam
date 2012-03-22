#!/bin/bash

OCPGET="ocp-get --root /tmp/OPAM.TEST"
INCLUDES0="`${OCPGET} config -r -I P2 P3`"
LINK0="`${OCPGET} config -r -asmlink P2 P3`"

INCLUDES=${INCLUDES0// /,}
LINK=${LINK0// /,}

ocamlbuild -cflags ${INCLUDES} -lflags ${LINK} p4.native