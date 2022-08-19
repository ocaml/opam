#!/bin/bash -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b
# XXX This should be matching up with $PREFIX in main
export PATH=~/local/bin:$PATH
export OPAMKEEPLOGS=1

# TODO: Make opam-rt compatible with non-master initial branches
git config --global init.defaultBranch master

cd $CACHE/opam-rt
make KINDS="local git" run
