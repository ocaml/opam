#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

rm -rf src_ext/archives
make -C src_ext cache-archives
ls -al src_ext/archives
rm -rf ~/opam-repository
git clone $OPAM_REPO_MAIN ~/opam-repository --bare
