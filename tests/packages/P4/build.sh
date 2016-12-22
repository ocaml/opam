#! /bin/sh -e

if [ $OPAM_PACKAGE_VERSION -eq 2 ]; then
    if [ "X${P1:-}" != "Xversion1" ]; then
        echo "P1 not set to version1 while P1.1 should be installed" >&2
        exit 12
    fi
else
    if [ -z "X${P1:-}" ]; then
        echo "P1 not set while P1 should be installed" >&2
        exit 12
    fi
fi

echo "Building P4 with ${OPAM}"
LIBDIR="`${OPAM} config var lib`"
COMP="-I ${LIBDIR}/P1 -I ${LIBDIR}/P2 -I ${LIBDIR}/P3"
LINK="p1.cmxa p2.cmxa p3.cmxa p3_bar.cmxa"

OCAMLC=ocamlc
if which ocamlopt >/dev/null 2>&1; then OCAMLC=ocamlopt; fi

$OCAMLC ${COMP} ${LINK} p4.ml -o p4.foo

echo "TEST=${TEST}"
