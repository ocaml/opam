#!/usr/bin/env bash
set -uex

# Change this to your github user if you only want to test the script
GH_USER=ocaml

usage() {
    echo "Usage: $0 TAG [archive|builds] [--without-signatures]"
    exit 1
}

if [[ $# -lt 1 || $# -gt 3 || "x$1" =~ "x-" ]]; then
  usage
fi

TAG="$1"
shift

case "$1" in
"archive") ACTION_ARCHIVE=true; ACTION_BUILDS=false;;
"builds") ACTION_ARCHIVE=false; ACTION_BUILDS=true;;
"") ACTION_ARCHIVE=true; ACTION_BUILDS=true;;
*) usage;;
esac
shift

case "$1" in
"--without-signatures") WITH_SIGS=false;;
"") WITH_SIGS=true;;
*) usage;;
esac
shift

if test "$(uname -s)" != Darwin -o "$(uname -m)" != arm64; then
  echo "This script is required to be run on macOS/arm64"
  exit 1
fi

LC_ALL=C
CWD=$(pwd)
JOBS=$(sysctl -n hw.ncpu)
DIR=$(dirname $0)
cd "$DIR"

OUTDIR="out/$TAG"

OPAM_DEV_KEY='92C526AE50DF39470EB2911BED4CF1CA67CBAA92'

sign() {
    if ! [ -f "$1.sig" ] || ! gpg -u "$OPAM_DEV_KEY" --verify "$1.sig" "$1"; then
        gpg -u "$OPAM_DEV_KEY" --detach-sign "$1"
    fi
}

mkdir -p "$OUTDIR"

if test "$ACTION_ARCHIVE" = true; then
    make GH_USER="${GH_USER}" TAG="$TAG" GIT_URL="https://github.com/${GH_USER}/opam.git" "${OUTDIR}/opam-full-$TAG.tar.gz"
    cd "${OUTDIR}"
    if test "$WITH_SIGS" = true; then
        sign "opam-full-$TAG.tar.gz"
        git-upload-release "${GH_USER}" opam "$TAG" "opam-full-$TAG.tar.gz.sig"
    fi
    git-upload-release "${GH_USER}" opam "$TAG" "opam-full-$TAG.tar.gz"
fi

qemu_build() {
  local port=$1
  local image=$2
  local install=$3
  local make=$4
  local arch=$5

  if ! ssh -p "${port}" root@localhost true; then
      qemu-img convert -O raw "./qemu-base-images/${image}.qcow2" "./qemu-base-images/${image}.raw"
      "qemu-system-${arch}" -drive "file=./qemu-base-images/${image}.raw,format=raw" -nic "user,hostfwd=tcp::${port}-:22" -m 2G &
      sleep 60
  fi
  ssh -p "${port}" root@localhost "${install}"
  make GH_USER="${GH_USER}" TAG="$TAG" JOBS=$(JOBS) qemu QEMU_PORT="${port}" REMOTE_MAKE="${make}" REMOTE_DIR=opam-release-$TAG
  ssh -p "${port}" root@localhost "shutdown -p now"
}

if test "$ACTION_BUILDS" = true; then
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" x86_64-linux
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" i686-linux
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" armhf-linux
  make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" arm64-linux
  [ -f ${OUTDIR}/opam-$TAG-x86_64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" JOBS=$(JOBS) macos-local MACOS_ARCH=x86_64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
  [ -f ${OUTDIR}/opam-$TAG-arm64-macos ] || make GH_USER="${GH_USER}" TAG="$TAG" JOBS=$(JOBS) macos-local MACOS_ARCH=arm64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
  [ -d ./qemu-base-images ] || git clone https://gitlab.com/kit-ty-kate/qemu-base-images.git
  [ -f ${OUTDIR}/opam-$TAG-x86_64-openbsd ] || qemu_build 9999 OpenBSD-7.0-amd64 "pkg_add gmake curl bzip" gmake x86_64 &
  [ -f ${OUTDIR}/opam-$TAG-x86_64-freebsd ] || qemu_build 9998 FreeBSD-13.0-RELEASE-amd64 "pkg install -y gmake curl bzip2" gmake x86_64 &
  wait
  upload_failed=
  cd "${OUTDIR}"
  for f in opam-$TAG-*; do
      if [ "${f%.sig}" != "$f" ]; then continue; fi
      if test "$WITH_SIGS" = true; then
        sign "$f"
        git-upload-release "${GH_USER}" opam "$TAG" "$f.sig" || upload_failed="$upload_failed $f.sig"
      fi
      git-upload-release "${GH_USER}" opam "$TAG" "$f" || upload_failed="$upload_failed $f"
  done
fi
if [ -n "$upload_failed" ]; then
    echo "Failed uploads: $upload_failed"
    exit 10
fi
