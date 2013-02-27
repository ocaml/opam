#! /bin/sh -eu

echo "Building P3 version ${OPAM_PACKAGE_VERSION}"

if [ "x${OPAM_PACKAGE_NAME}" = "xP3" ]; then
    ocamlbuild p3.cma p3.cmxa p3_bar.cma p3_bar.cmxa
else
   exit 1
fi
