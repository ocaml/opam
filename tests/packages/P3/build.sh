#! /bin/sh -eu

echo "Building P3 version ${OPAM_PACKAGE_VERSION}"

if [ "x${OPAM_PACKAGE_NAME}" = "xP3" ]; then
    LIB=$(${OPAM} config var lib)
    ocamlc -a -I $LIB/P1 -I $LIB/P2 p3.ml -o p3.cma
    ocamlopt -a -I $LIB/P1 -I $LIB/P2 p3.ml -o p3.cmxa
    ocamlc -a -I $LIB/P1 -I $LIB/P2 p3_bar.ml -o p3_bar.cma
    ocamlopt -a -I $LIB/P1 -I $LIB/P2 p3_bar.ml -o p3_bar.cmxa
else
   exit 1
fi
