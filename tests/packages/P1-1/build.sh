#! /bin/sh -eu

if [ -n "${P1:-}" ]; then
    echo "P1 ('$P1') should not be set yet" >&2
    exit 12
fi

ocamlbuild p1.cma p1.cmxa
