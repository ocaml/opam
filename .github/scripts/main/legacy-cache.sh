#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

rm -rf ~/.opam
opam init --yes default git+$OPAM_REPO_CACHE#$OPAM_REPO_SHA --bare
OPAMVAR_sys_ocaml_version=4.02.3 opam switch create --yes build ocaml-system.4.02.3 --fake
opam install --yes ocaml-secondary-compiler
