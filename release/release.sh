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
SSH="sshpass -ppassword ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

OUTDIR="out/$TAG"
mkdir -p "$OUTDIR"

windows_build() {
  local port=$1
  local image=$2

  if ! ${SSH} -p "${port}" opam@localhost cd; then
    qemu-img convert -O raw "./${image}.qcow2" "./${image}.raw"
    # NOTE: -machine q35 seems to be required to avoid random but recurring crashes
    "qemu-system-x86_64" -drive "file=./${image}.raw,format=raw" -nic "user,hostfwd=tcp::${port}-:22" -m 6G -smp "${JOBS}" -machine q35 &
    sleep 240
  fi

  # Disable Windows Defender before anything else (makes the build process faster)
  # TODO: ... doesn't work
  #       as well as the following line to add to the readme.md: Open the Windows Security app and turn off "Tamper Protection" and Real-time protection" from "Virus & threat protection -> Virus & threat protection settings -> Manage settings"
  #${SSH} -p "${port}" opam@localhost "powershell -c 'Set-MpPreference -Force \
  #                                    -DisableBehaviorMonitoring \$True \
  #                                    -DisableRealtimeMonitoring \$True \
  #                                    -DisableScriptScanning \$True \
  #                                    -EnableLowCpuPriority \$True'"

  ${SSH} -p "${port}" opam@localhost "curl -LO https://cygwin.com/setup-x86_64.exe"
  ${SSH} -p "${port}" opam@localhost '.\setup-x86_64.exe -q -O -s https://cygwin.mirror.uk.sargasso.net -P make,diffutils,mingw64-x86_64-gcc-g++,mingw64-i686-gcc-g++,rsync,patch,curl,unzip,git,binutils'

  make TAG="$TAG" JOBS="${JOBS}" qemu QEMU_PORT="${port}" REMOTE_MAKE=make REMOTE_DIR="opam-release-$TAG" 'REMOTE_SHELL=C:\Cygwin64\bin\bash.exe -l' SSH_USER=opam ULIMIT=""

  ${SSH} -p "${port}" opam@localhost "shutdown /s /f"

  wait
}

qemu_build() {
  local port=$1
  local image=$2
  local install=$3
  local make=$4
  local arch=$5

  if ! ${SSH} -p "${port}" root@localhost true; then
      qemu-img convert -O raw "./qemu-base-images/${image}.qcow2" "./qemu-base-images/${image}.raw"
      "qemu-system-${arch}" -drive "file=./qemu-base-images/${image}.raw,format=raw" -nic "user,hostfwd=tcp::${port}-:22" -machine q35 -m 2G -smp "${JOBS}" &
      sleep 60
  fi
  ${SSH} -p "${port}" root@localhost "${install}"
  make TAG="$TAG" JOBS="${JOBS}" qemu QEMU_PORT="${port}" REMOTE_MAKE="${make}" REMOTE_DIR="opam-release-$TAG"
  ${SSH} -p "${port}" root@localhost "/sbin/shutdown -p now"
}

make JOBS="${JOBS}" TAG="$TAG" "${OUTDIR}/opam-full-$TAG.tar.gz"
make JOBS="${JOBS}" TAG="$TAG" x86_64-linux
make JOBS="${JOBS}" TAG="$TAG" i686-linux
make JOBS="${JOBS}" TAG="$TAG" armhf-linux
make JOBS="${JOBS}" TAG="$TAG" arm64-linux
make JOBS="${JOBS}" TAG="$TAG" ppc64le-linux
make JOBS="${JOBS}" TAG="$TAG" s390x-linux
make JOBS="${JOBS}" TAG="$TAG" riscv64-linux
[ -f "${OUTDIR}/opam-$TAG-x86_64-macos" ] || make TAG="$TAG" JOBS="${JOBS}" macos-local MACOS_ARCH=x86_64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
[ -f "${OUTDIR}/opam-$TAG-arm64-macos" ] || make TAG="$TAG" JOBS="${JOBS}" macos-local MACOS_ARCH=arm64 REMOTE_DIR=opam-release-$TAG GIT_URL="$CWD/.."
[ -d ./qemu-base-images ] || git clone https://gitlab.com/kit-ty-kate/qemu-base-images.git
[ -f "${OUTDIR}/opam-$TAG-x86_64-netbsd" ] || qemu_build 9996 NetBSD-10.0-amd64 "pkgin -y install gmake curl bzip2" gmake x86_64
[ -f "${OUTDIR}/opam-$TAG-x86_64-openbsd" ] || qemu_build 9999 OpenBSD-7.6-amd64 "pkg_add gmake curl bzip2" gmake x86_64
[ -f "${OUTDIR}/opam-$TAG-x86_64-freebsd" ] || qemu_build 9998 FreeBSD-14.1-RELEASE-amd64 "env IGNORE_OSVERSION=yes pkg install -y gmake curl bzip2" gmake x86_64
[ -f "${OUTDIR}/opam-$TAG-x86_64-windows" ] || windows_build 9997 Windows-10-x86_64
