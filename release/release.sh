#!/usr/bin/env bash
set -uex

unset $(env | cut -d= -f1 | grep -Fvx HOME | grep -Fvx PWD | grep -Fvx USER | grep -Fvx SHELL | grep -Fvx TERM)
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin:/usr/local/bin

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

DIR=$(dirname $0)
cd "$DIR"

LC_ALL=C
CWD=$(pwd)
JOBS=$(sysctl -n hw.ncpu)
JOBS=$(echo "${JOBS} / 1.5" | bc)
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
      "qemu-system-${arch}" -drive "file=./qemu-base-images/${image}.raw,format=raw" -nic "user,hostfwd=tcp::${port}-:22" -m 2G &
      sleep 60
  fi
  ${SSH} -p "${port}" root@localhost "${install}"
  # NOTE: JOBS=1 because qemu does not support proper multithreading from arm64 to x86_64 yet because of memory model differences.
  # See https://wiki.qemu.org/Features/tcg-multithread
  make TAG="$TAG" JOBS=1 qemu QEMU_PORT="${port}" REMOTE_MAKE="${make}" REMOTE_DIR=opam-release-$TAG
  ${SSH} -p "${port}" root@localhost "shutdown -p now"
}

make JOBS="${JOBS}" TAG="$TAG" "${OUTDIR}/opam-full-$TAG.tar.gz"
make JOBS="${JOBS}" TAG="$TAG" x86_64-linux
make JOBS="${JOBS}" TAG="$TAG" i686-linux
make JOBS="${JOBS}" TAG="$TAG" armhf-linux
make JOBS="${JOBS}" TAG="$TAG" arm64-linux
[ -f "${OUTDIR}/opam-$TAG-x86_64-macos" ] || make TAG="$TAG" JOBS="${JOBS}" macos-local MACOS_ARCH=x86_64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
[ -f "${OUTDIR}/opam-$TAG-arm64-macos" ] || make TAG="$TAG" JOBS="${JOBS}" macos-local MACOS_ARCH=arm64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
[ -d ./qemu-base-images ] || git clone https://gitlab.com/kit-ty-kate/qemu-base-images.git
[ -f "${OUTDIR}/opam-$TAG-x86_64-openbsd" ] || qemu_build 9999 OpenBSD-7.0-amd64 "pkg_add gmake curl bzip2" gmake x86_64 &
[ -f "${OUTDIR}/opam-$TAG-x86_64-freebsd" ] || qemu_build 9998 FreeBSD-13.0-RELEASE-amd64 "env IGNORE_OSVERSION=yes pkg install -y gmake curl bzip2" gmake x86_64 &
wait
