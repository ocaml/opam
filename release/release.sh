#!/usr/bin/env bash
set -uex

# This script is expected to run on Linux with docker available, and to have
# three remotes "some-osx-x86", "some-osx-arm" and "some-openbsd", with the
# corresponding OSes, ocaml deps installed

# Change this to your github user if you only want to test the script
GH_USER=ocaml

LC_ALL=C
DIR=$(dirname $0)
cd "$DIR"
if [[ $# -eq 0 || "x$1" =~ "x-" ]]; then
    echo "Usage: $0 TAG [archive|builds]"
    exit 1
fi

TAG="$1"; shift
OUTDIR="out/$TAG"

OPAM_DEV_KEY='92C526AE50DF39470EB2911BED4CF1CA67CBAA92'

sign() {
    if ! [ -f "$1.sig" ] || ! gpg -u "$OPAM_DEV_KEY" --verify "$1.sig" "$1"; then
        gpg -u "$OPAM_DEV_KEY" --detach-sign "$1"
    fi
}

mkdir -p "$OUTDIR"

if [[ $# -eq 0 || " $* " =~ " archive " ]]; then
    make GH_USER="${GH_USER}" TAG="$TAG" GIT_URL="https://github.com/${GH_USER}/opam.git" "${OUTDIR}/opam-full-$TAG.tar.gz"
    ( cd ${OUTDIR} &&
          sign "opam-full-$TAG.tar.gz" &&
          git-upload-release "${GH_USER}" opam "$TAG" "opam-full-$TAG.tar.gz.sig" &&
          git-upload-release "${GH_USER}" opam "$TAG" "opam-full-$TAG.tar.gz"; )
fi

if [[ $# -eq 0 || " $* " =~ " builds " ]]; then
  make GH_USER="${GH_USER}" TAG="$TAG" all &
  [ -f ${OUTDIR}/opam-$TAG-x86_64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" remote REMOTE=some-osx-x86 REMOTE_DIR=opam-release-$TAG &
  [ -f ${OUTDIR}/opam-$TAG-arm64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" remote REMOTE=some-osx-arm REMOTE_DIR=opam-release-$TAG &
  [ -f ${OUTDIR}/opam-$TAG-x86_64-openbsd ] || make GH_USER="${GH_USER}" TAG="$TAG" remote REMOTE=some-openbsd REMOTE_MAKE=gmake REMOTE_DIR=opam-release-$TAG &
  wait
  upload_failed=
  cd ${OUTDIR} && for f in opam-$TAG-*; do
      if [ "${f%.sig}" != "$f" ]; then continue; fi
      sign "$f"
      git-upload-release "${GH_USER}" opam "$TAG" "$f.sig" || upload_failed="$upload_failed $f.sig"
      git-upload-release "${GH_USER}" opam "$TAG" "$f" || upload_failed="$upload_failed $f"
  done
fi
if [ -n "$upload_failed" ]; then
    echo "Failed uploads: $upload_failed"
    exit 10
fi
