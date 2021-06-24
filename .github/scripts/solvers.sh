#!/bin/bash -xue

. .github/scripts/preamble.sh

export OPAMYES=1
export OCAMLRUNPARAM=b

# All environment variable are overwritten in job description
# One cache per solver, $CACHE/opam.<solver>.cached
export OPAMROOT=$OPAMBSROOT
echo $OPAMROOT

case "$SOLVER" in
  z3)
    PKGS=$SOLVER
    ;;
  0install)
    PKGS="$SOLVER opam-0install-cudf"
    ;;
  *)
    echo -e "\e[31mSolver $SOLVER not handled\e[0m";
    exit 3
    ;;
esac

opam switch create $SOLVER ocaml-system || true
opam install $PKGS
opam install . --deps
opam clean --logs --switch-cleanup
eval $(opam env)
./configure
make
