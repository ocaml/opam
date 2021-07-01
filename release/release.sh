#!/usr/bin/env bash
set -uex

# This script is expected to run on Linux with docker available, and to have
# three remotes "some-osx-x86", "some-osx-arm" and "some-openbsd", with the
# corresponding OSes, ocaml deps installed

LC_ALL=C
DIR=$(dirname $0)
cd "$DIR"
if [[ $# -eq 0 || "x$1" =~ "x-" ]]; then
    echo "Usage: $0 TAG [archive|builds]"
    exit 1
fi

TAG="$1"; shift

OPAM_DEV_KEY='92C526AE50DF39470EB2911BED4CF1CA67CBAA92'

sign() {
    if ! [ -f "$1.sig" ] || ! gpg -u "$OPAM_DEV_KEY" --verify "$1.sig" "$1"; then
        gpg -u "$OPAM_DEV_KEY" --detach-sign "$1"
    fi
}

if [[ $# -eq 0 || " $* " =~ " archive " ]]; then
    make TAG="$TAG" GIT_URL="https://github.com/ocaml/opam.git" "out/opam-full-$TAG.tar.gz"
    ( cd out &&
          sign "opam-full-$TAG.tar.gz" &&
          git-upload-release ocaml opam "$TAG" "opam-full-$TAG.tar.gz.sig" &&
          git-upload-release ocaml opam "$TAG" "opam-full-$TAG.tar.gz"; )
fi

if [[ $# -eq 0 || " $* " =~ " builds " ]]; then
  make TAG="$TAG" all &
  [ -f out/opam-$TAG-x86_64-macos ] || make TAG="$TAG" remote REMOTE=some-osx-x86 REMOTE_DIR=opam-release-$TAG &
  [ -f out/opam-$TAG-arm64-macos ] || make TAG="$TAG" remote REMOTE=some-osx-arm REMOTE_DIR=opam-release-$TAG &
  [ -f out/opam-$TAG-x86_64-openbsd ] || make TAG="$TAG" remote REMOTE=some-openbsd REMOTE_MAKE=gmake REMOTE_DIR=opam-release-$TAG &
  wait
  cd out && for f in opam-$TAG-*; do
      sign "$f"
      git-upload-release ocaml opam "$TAG" "$f.sig"
      git-upload-release ocaml opam "$TAG" "$f"
  done
fi
