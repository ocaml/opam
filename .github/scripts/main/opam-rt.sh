#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b
# XXX This should be matching up with $PREFIX in main
export PATH=~/local/bin:$PATH
export OPAMKEEPLOGS=1

# TODO: Make opam-rt compatible with non-master initial branches
sudo git config --system init.defaultBranch master
# TODO: Make opam-rt compatible with a hermetic git configuration
sudo git config --system user.name 'OPAM test environment'
sudo git config --system user.email 'noreply@ocaml.org'

cd $CACHE/opam-rt
make KINDS="local git" run
