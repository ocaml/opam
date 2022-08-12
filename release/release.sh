#!/usr/bin/env bash
set -uex

# Change this to your github user if you only want to test the script
GH_USER=ocaml

usage() {
    echo "Usage: $0 TAG"
    exit 1
}

if [[ $# -lt 1 || $# -gt 1 || "x$1" =~ "x-" ]]; then
  usage
fi

TAG="$1"
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
SSH="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

OUTDIR="out/$TAG"
mkdir -p "$OUTDIR"

qemu_build() {
  local port=$1
  local image=$2
  local install=$3
  local make=$4
  local arch=$5

  if ! ${SSH} -p "${port}" root@localhost true; then
      qemu-img convert -O raw "./qemu-base-images/${image}.qcow2" "./qemu-base-images/${image}.raw"
      "qemu-system-${arch}" -drive "file=./qemu-base-images/${image}.raw,format=raw" -nic "user,hostfwd=tcp::${port}-:22" -m 2G -smp "${JOBS}" &
      sleep 60
  fi
  ${SSH} -p "${port}" root@localhost "${install}"
  make GH_USER="${GH_USER}" TAG="$TAG" JOBS="${JOBS}" qemu QEMU_PORT="${port}" REMOTE_MAKE="${make}" REMOTE_DIR=opam-release-$TAG
  ${SSH} -p "${port}" root@localhost "shutdown -p now"
}

make GH_USER="${GH_USER}" TAG="$TAG" GIT_URL="https://github.com/${GH_USER}/opam.git" "${OUTDIR}/opam-full-$TAG.tar.gz"
make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" x86_64-linux
make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" i686-linux
make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" armhf-linux
make "-j${JOBS}" GH_USER="${GH_USER}" TAG="$TAG" arm64-linux
[ -f "${OUTDIR}/opam-$TAG-x86_64-macos" ] || make GH_USER="${GH_USER}" TAG="$TAG" JOBS="${JOBS}" macos-local MACOS_ARCH=x86_64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
[ -f "${OUTDIR}/opam-$TAG-arm64-macos" ] || make GH_USER="${GH_USER}" TAG="$TAG" JOBS="${JOBS}" macos-local MACOS_ARCH=arm64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
[ -d ./qemu-base-images ] || git clone https://gitlab.com/kit-ty-kate/qemu-base-images.git
[ -f "${OUTDIR}/opam-$TAG-x86_64-openbsd" ] || qemu_build 9999 OpenBSD-7.0-amd64 "pkg_add gmake curl bzip2" gmake x86_64 &
[ -f "${OUTDIR}/opam-$TAG-x86_64-freebsd" ] || qemu_build 9998 FreeBSD-13.0-RELEASE-amd64 "pkg install -y gmake curl bzip2" gmake x86_64 &
wait
