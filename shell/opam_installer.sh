#!/bin/sh

set -ue

# (c) Copyright Fabrice Le Fessant INRIA/OCamlPro 2013
# (c) Copyright Louis Gesbert OCamlPro 2014-2016

VERSION='2.0-alpha4'

default_ocaml=4.02.1

usage() {
cat <<EOF

Usage:
    ./opam_install BINDIR

    Download and installs the latest binary version of OPAM

    BINDIR is the directory where it should be installed, e.g. /usr/local/bin
    (it should be in your PATH).
EOF
    exit 1
}

#
#       Report an error and exit
#
PROGNAME=$0
error() {
    echo -n "`basename $PROGNAME`: " >&2
    for s in "$@"; do echo $s; done
    exit 1
}


TMP=${TMPDIR:-/tmp}

dlerror () {
    error "Couldn't download $url" \
        "There may not yet be a binary release for your architecture or OS, sorry."
}

getopam() {
    opamfile=$2
    url=$1/$opamfile

    if which wget >/dev/null; then
        wget -q -O "$TMP/$opamfile" "$url" || dlerror
    else
        curl -s -L -o "$TMP/$opamfile" "$url" || dlerror
    fi
}

if [ $# -lt 1 ] || [ $# -gt 2 ] || [ "${1#-}" != "$1" ]; then
    echo "OPAM binary installer v. $VERSION"
    usage
fi

BINDIR=$1

file="opam-$VERSION-$(uname -m || echo unknown)-$(uname -s || echo unknown)"

echo Downloading OPAM...
getopam "https://github.com/ocaml/opam/releases/download/$VERSION" $file

mkdir -p "$BINDIR" 2>/dev/null || true
if [ ! -w "$BINDIR" ]; then
    echo "You don't have write access to $BINDIR: sudo may ask for your password"
    if [ ! -d "$BINDIR" ]; then sudo mkdir -p "$BINDIR"; fi
    sudo install -g root -o root -m 755 $TMP/$file $BINDIR/opam
else
    install -m 755 $TMP/$file $BINDIR/opam
fi
rm -f $TMP/$file

OPAM=$(which opam || echo "$BINDIR/opam")
if [ "$OPAM" != "$BINDIR/opam" ]; then
    echo "WARNING: you have a different version of OPAM installed at $OPAM"
    echo "It is highly recommended that you remove it."
    echo
    OPAM="$BINDIR/opam"
fi

echo "Installation done. If you need to uninstall, simply remove $BINDIR/opam"
echo "and ~/.opam"
echo
echo "Now run 'opam init' to setup opam for the current user."
