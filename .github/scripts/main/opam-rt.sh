#!/bin/bash -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b
export PATH=~/local/bin:$PATH
export OPAMKEEPLOGS=1

cd $CACHE/opam-rt
make KINDS="local git" run
