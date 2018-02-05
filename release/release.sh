#!/bin/bash -uex

# This script is expected to run on Linux with docker available, and to have two
# remotes "some-osx" and "some-openbsd", with the corresponding OSes, ocaml deps
# installed

DIR=$(dirname $0)
cd "$DIR"
if [[ $# -eq 0 || "x$1" =~ "x-" ]]; then
    echo "Usage: $0 TAG [archive|builds]"
    exit 1
fi

TAG="$1"; shift

if [[ $# -eq 0 || " $* " =~ " archive " ]]; then
  make TAG="$TAG" GIT_URL="https://github.com/ocaml/opam.git" "out/opam-full-$TAG.tar.gz"
  ( cd out && git-upload-release ocaml opam "$TAG" "opam-full-$TAG.tar.gz"; )
fi

if [[ $# -eq 0 || " $* " =~ " builds " ]]; then
  make TAG="$TAG" all &
  make TAG="$TAG" remote REMOTE=some-osx REMOTE_DIR=opam-release &
  make TAG="$TAG" remote REMOTE=some-openbsd REMOTE_MAKE=gmake REMOTE_DIR=opam-release &
  wait
  cd out && for f in opam-$TAG-*; do
      git-upload-release ocaml opam "$TAG" $f
  done
fi
