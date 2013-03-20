#!/bin/sh

set -e 

# (c) Copyright Fabrice Le Fessant INRIA/OCamlPro 2013

VERSION='1.0.0'

default_ocaml=4.00.1

usage() {
cat <<EOF

Usage:
    ./opam_install BINDIR [COMP]

    Script to download and install binary versions of opam 
    BINDIR is the directory where you want to install OPAM
    (BINDIR must be in your PATH). 
    COMP is an optional argument, specifying the default
    version of OCaml you want to use ($default_ocaml by default).
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


SYSTEM=`uname -s`
if which wget >/dev/null; then
    WGETOPTS="--passive-ftp -q -O"
else
    WGETOPTS="-OL -o"
    wget() {
	shift
	curl -OL $*
    }
fi

#
#	Download a file from the web, unzip it, and extract the
#	files we want
getopam() {
    url="$1"
    opamfile="$2"
    
    wget $WGETOPTS $opamfile "$url/$opamfile" ||
	error "Couldn't download $url/$opamfile"
    chmod +x $opamfile    
}

if [ $# = 0 ]; then
    echo "OPAM binary installer v. $VERSION"
    usage
fi

BINDIR=$1
COMP=$2

UNAME_MACHINE=`(uname -m) 2>/dev/null` || UNAME_MACHINE=unknown
UNAME_RELEASE=`(uname -r) 2>/dev/null` || UNAME_RELEASE=unknown
UNAME_SYSTEM=`(uname -s) 2>/dev/null`  || UNAME_SYSTEM=unknown
UNAME_VERSION=`(uname -v) 2>/dev/null` || UNAME_VERSION=unknown

# Note: order is significant - the case branches are not exclusive.
case "${UNAME_MACHINE}:${UNAME_SYSTEM}:${UNAME_RELEASE}:${UNAME_VERSION}" in
  x86_64:Linux:*:*)
    file="opam-${VERSION}-${UNAME_MACHINE}-${UNAME_SYSTEM}"
  ;;
  x86_64:Darwin:*:*)
    file="opam-${VERSION}-${UNAME_MACHINE}-${UNAME_SYSTEM}"
  ;;
  i686:Linux:*:*)
    file="opam-${VERSION}-${UNAME_MACHINE}-${UNAME_SYSTEM}"
  ;;
  *)
    echo "No file yet for ${UNAME_MACHINE}:${UNAME_SYSTEM}"
    exit 1
 ;;
esac


echo Downloading OPAM...
getopam "http://www.ocamlpro.com/pub" $file

if [ $(id -ru) -ne 0 ]; then
  echo "Do you need to use sudo to write to $BINDIR (y/N) ?"
  read rep
  if test "$rep" = "y"; then
    echo "[sudo is probably going to ask for your password]"
    sudo mv $file $BINDIR/opam
  else
    mv $file $BINDIR/opam
  fi
else
    mv $file $BINDIR/opam
fi

PATH=$BINDIR:$PATH
export PATH

if test "$COMP" = ""; then
  COMPOPT="--comp ${default_ocaml}"
else
  if test "$COMP" = "system"; then
    COMPOPT=
  else
    COMPOPT="--comp ${COMP}"
  fi
fi

if test -d $HOME/.opam; then
  echo 'You already have a ~/.opam directory'
  echo 'If you have problems, you should consider removing it'
  echo '  and run:'
  echo "opam init $COMPOPT"
else
  echo Initializing with compiler $COMP
  opam init $COMPOPT
fi

echo 'To use OCaml installed by OPAM, use'
echo 'eval `opam config env`'
