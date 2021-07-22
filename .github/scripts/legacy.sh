#!/bin/bash -xue

. .github/scripts/preamble.sh

export OPAMYES=1
export OCAMLRUNPARAM=b

opam init default git+$OPAM_REPO#$OPAM_REPO_SHA
eval $(opam env)
OPAMCLI=2.0 opam reinstall ocaml-system --unlock-base
opam upgrade
for package in core format installer ; do
  opam pin add -yn opam-$package .
done
opam install opam-core opam-format opam-installer --deps-only
dune build --profile=dev --only-packages opam-core,opam-format,opam-installer @install
