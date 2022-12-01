#!/bin/bash -xue

OCAML_BRANCH="$1"
PREFIX="$2"
EXE="$3"
OCAML_LOCAL="$4"
PLATFORM="$5"

if [[ $OCAML_BRANCH -gt 407 ]]; then
  if [[ -n $GITHUB_BASE_REF ]]; then
    git tag combak
    git fetch origin $GITHUB_BASE_REF
    git checkout origin/$GITHUB_BASE_REF
  fi
  make -C src_ext dune-local.stamp
  cd src_ext/dune-local
  ocaml bootstrap.ml
  cp dune.exe "$PREFIX/bin/dune$EXE"
  cd ../..

  ./configure
  make
  cp -a _build "$OCAML_LOCAL/"
  rm -f "$OCAML_LOCAL/_build/log"
  mv "$OCAML_LOCAL/_build/default/src_ext" "$OCAML_LOCAL/_build/"
  rm -rf "$OCAML_LOCAL/_build/default"/* "$OCAML_LOCAL/_build/install"
  mv "$OCAML_LOCAL/_build/src_ext" "$OCAML_LOCAL/_build/default/" 
  git clean -dfX
  if [[ -n $GITHUB_BASE_REF ]]; then
    git checkout combak
  fi
fi

# The Windows BSD tar can't cope with symlinks, so we pre-tar the archive and cache that!
if [[ $PLATFORM = 'Windows' ]]; then
  tar -C "$OCAML_LOCAL" -pcf "$OCAML_LOCAL.tar" .
fi
