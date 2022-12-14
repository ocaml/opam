#!/usr/bin/env bash
set -uex

usage() {
    echo "Usage: $0 TAG"
    exit 1
}

if [[ $# -lt 1 || $# -gt 1 || "x$1" =~ "x-" ]]; then
  usage
fi

TAG="$1"
shift

OPAM_DEV_KEY='92C526AE50DF39470EB2911BED4CF1CA67CBAA92'

sign() {
    if ! [ -f "$1.sig" ] || ! gpg -u "$OPAM_DEV_KEY" --verify "$1.sig" "$1"; then
        gpg -u "$OPAM_DEV_KEY" --detach-sign "$1"
    fi
}

DIR=$(dirname $0)
cd "$DIR"

OUTDIR="out/$TAG"
cd "${OUTDIR}"

for f in opam-$TAG-*; do
    sign "$f"
done
