#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b
# XXX This should be matching up with $PREFIX in main
export PATH=~/local/bin:$PATH
export OPAMKEEPLOGS=1

if [[ $OPAM_COLD -eq 1 ]] ; then
  export PATH=$PWD/bootstrap/ocaml/bin:$PATH
fi

# Test basic actions
# The SHA is fixed so that upstream changes shouldn't affect CI. The SHA needs
# to be moved forwards when a new version of OCaml is added to ensure that the
# ocaml-system package is available at the correct version.
opam init --bare default git+$OPAM_REPO_CACHE#$OPAM_TEST_REPO_SHA
cat >> $(opam var root --global 2>/dev/null)/config <<EOF
archive-mirrors: "https://opam.ocaml.org/cache"
EOF
opam switch create default ocaml-system
eval $(opam env)
opam install lwt
opam list
opam config report
