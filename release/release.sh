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
  make GH_USER="${GH_USER}" TAG="$TAG" x86_64-linux &
  make GH_USER="${GH_USER}" TAG="$TAG" i686-linux &
  make GH_USER="${GH_USER}" TAG="$TAG" armhf-linux &
  make GH_USER="${GH_USER}" TAG="$TAG" arm64-linux &
  [ -f ${OUTDIR}/opam-$TAG-x86_64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" remote REMOTE=some-osx-x86 REMOTE_DIR=opam-release-$TAG &
  [ -f ${OUTDIR}/opam-$TAG-arm64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" remote REMOTE=some-osx-arm REMOTE_DIR=opam-release-$TAG &
  [ -f ${OUTDIR}/opam-$TAG-x86_64-openbsd ] || \
    ( ([ -f ./qemu-base-images ] || (git clone https://github.com/kit-ty-kate/qemu-base-images.git && qemu-img convert -O raw ./qemu-base-images/OpenBSD-7.0-amd64.qcow2 ./qemu-base-images/OpenBSD-7.0-amd64.raw)) &&
      (ssh -p 9999 root@localhost true || (qemu-system-x86_64 -drive "file=./qemu-base-images/OpenBSD-7.0-amd64.raw,format=raw" -nic "user,hostfwd=tcp::9999-:22" -m 1G & sleep 60)) &&
      ssh -p 9999 root@localhost "pkg_add gmake curl bzip2" &&
      make GH_USER="${GH_USER}" TAG="$TAG" qemu QEMU_PORT=9999 REMOTE_MAKE=gmake REMOTE_DIR=opam-release-$TAG &&
      ssh -p 9999 root@localhost "shutdown -p now" ) &
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
