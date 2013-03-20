#!/bin/sh

# (c) Copyright Fabrice Le Fessant INRIA/OCamlPro 2013
# (c) Copyright Thomas Gazagnaire        OCamlPro 2013

set -e 

usage() {
cat <<EOF

Usage:
    ./make_opam_binary VERSION

    Script to create a binary version of opam.
    VERSION is the version of OPAM you want to build.
EOF
    exit 1
}

#
#       Report an error and exit
#
PROGNAME=$0
error() {
	echo "`basename $PROGNAME`: $1" >&2
	exit 1
}

if [ $# = 0 ]; then
    echo "OPAM binary builder."
    usage
fi

VERSION=$1
BUILDDIR=tmp-opam-$VERSION
CURRENTDIR=`pwd`
UNAME_MACHINE=`(uname -m) 2>/dev/null` || UNAME_MACHINE=unknown
UNAME_RELEASE=`(uname -r) 2>/dev/null` || UNAME_RELEASE=unknown
UNAME_SYSTEM=`(uname -s) 2>/dev/null`  || UNAME_SYSTEM=unknown
UNAME_VERSION=`(uname -v) 2>/dev/null` || UNAME_VERSION=unknown
file="opam-${VERSION}-${UNAME_MACHINE}-${UNAME_SYSTEM}"

if which wget >/dev/null; then
    WGETOPTS="--passive-ftp -q -O"
else
    WGETOPTS="-OL -o"
    wget() {
	shift
	curl -OL $*
    }
fi

# Download the source archive from the web
download() {
    url="$1"
    mkdir -p $BUILDDIR
    cd $BUILDDIR
    file=opam-full-$VERSION.tar.gz
    echo "Downloading ${url}/${file} ..."
    wget $WGETOPTS $file "$url/$file" ||
	error "Couldn't download $url/$file"
}

# Extract the source archive
extract() {
    tar xfvz opam-full-$VERSION.tar.gz
    cd opam-full-$VERSION
}

# Build the binary
build() {

    ./configure
    pwd
    make
    cd $CURRENTDIR
    ln -sf $BUILDDIR/opam-full-$VERSION/_obuild/opam/opam.asm $file
}

{
download "http://www.ocamlpro.com/pub"
extract
build
} 1>&2
echo $file
