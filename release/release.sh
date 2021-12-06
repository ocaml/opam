#!/usr/bin/env bash
set -uex

# Change this to your github user if you only want to test the script
GH_USER=ocaml

if [[ $# -eq 0 || "x$1" =~ "x-" ]]; then
    echo "Usage: $0 TAG [archive|builds]"
    exit 1
fi

if test "$(uname -s)" != Darwin -o "$(uname -m)" != arm64; then
  echo "This script is required to be run on macOS/arm64"
  exit 1
fi

LC_ALL=C
CWD=$(pwd)
JOBS=$(sysctl -n hw.ncpu)
DIR=$(dirname $0)
cd "$DIR"

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
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" x86_64-linux
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" i686-linux
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" armhf-linux
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" arm64-linux
  [ -f ${OUTDIR}/opam-$TAG-x86_64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" JOBS=$(JOBS) macos-local MACOS_ARCH=x86_64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
  [ -f ${OUTDIR}/opam-$TAG-arm64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" JOBS=$(JOBS) macos-local MACOS_ARCH=arm64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
  [ -f ${OUTDIR}/opam-$TAG-x86_64-openbsd ] || \
    ( ([ -d ./qemu-base-images ] || (git clone https://github.com/kit-ty-kate/qemu-base-images.git)) &&
      (ssh -p 9999 root@localhost true ||
       (qemu-img convert -O raw ./qemu-base-images/OpenBSD-7.0-amd64.qcow2 ./qemu-base-images/OpenBSD-7.0-amd64.raw &&
        qemu-system-x86_64 -drive "file=./qemu-base-images/OpenBSD-7.0-amd64.raw,format=raw" -nic "user,hostfwd=tcp::9999-:22" -m 2G &
        sleep 60)) &&
      ssh -p 9999 root@localhost "pkg_add gmake curl bzip2" &&
      make GH_USER="${GH_USER}" TAG="$TAG" JOBS=$(JOBS) qemu QEMU_PORT=9999 REMOTE_MAKE=gmake REMOTE_DIR=opam-release-$TAG &&
      ssh -p 9999 root@localhost "shutdown -p now" )
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
