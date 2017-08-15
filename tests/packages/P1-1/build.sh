#! /bin/sh -eu

if [ -n "${P1:-}" ]; then
    echo "P1 ('$P1') should not be set yet" >&2
    exit 12
fi

ocamlc -a p1.ml -o p1.cma
ocamlopt -a p1.ml -o p1.cmxa
