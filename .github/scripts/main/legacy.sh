#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

eval $(opam env)
OPAMCONFIRMLEVEL= OPAMCLI=2.0 opam upgrade --unlock-base --yes

for package in core format installer ; do
  opam pin add --yes --no-action opam-$package .
done
opam install  --deps-only --yes opam-core opam-format opam-installer

export OCAMLRUNPARAM=b

dune build --profile=dev --only-packages opam-core,opam-format,opam-installer @install
