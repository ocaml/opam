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
# TODO: Temporary: revert back to « opam install lwt » once the following tickets are fixed
#       https://github.com/ocaml/ocamlfind/pull/112
#       https://github.com/ocsigen/lwt/issues/1081
#       https://github.com/ocsigen/lwt/issues/1082
opam pin add -yn git+https://github.com/dra27/ocamlfind.git#c9efeea72743b2ff59ef67d354e0a88a08804a2c
opam pin add lwt 5.9.1
opam list
opam config report

# Make sure opam init (including the sandbox script) works in the absence of OPAMROOT
unset OPAMROOT
rm -r ~/.opam
opam init --bare -vvv
rm -r ~/.opam
