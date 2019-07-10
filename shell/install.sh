#!/bin/sh

set -ue

# (c) Copyright Fabrice Le Fessant INRIA/OCamlPro 2013
# (c) Copyright Louis Gesbert OCamlPro 2014-2017

VERSION='2.0.4'
TAG=$(echo "$VERSION" | tr '~' '-')
DEFAULT_BINDIR=/usr/local/bin

usage() {
    echo "opam binary installer v.$VERSION"
    echo "Downloads and installs a pre-compiled binary of opam $VERSION to the system."
    echo "This can also be used to switch between opam versions"
    echo
    echo "Options:"
    echo "    --no-backup            Don't attempt to backup the current opam root"
    echo "    --backup               Force the backup the current opam root (even if it"
    echo "                           is from the 2.0 branch already)"
    echo "    --fresh                Create the opam $VERSION root from scratch"
    echo "    --restore   VERSION    Restore a backed up opam binary and root"
    echo
    echo "The default is to backup if the current version of opam is 1.*, or when"
    echo "using '--fresh'"
}

RESTORE=
NOBACKUP=
FRESH=

while [ $# -gt 0 ]; do
    case "$1" in
        --restore)
            if [ $# -lt 2 ]; then echo "Option $1 requires an argument"; exit 2; fi
            shift;
            RESTORE=$1;;
        --no-backup)
            NOBACKUP=1;;
        --backup)
            NOBACKUP=0;;
        --fresh)
            FRESH=1;;
        --help|-h)
            usage; exit 0;;
        *)
            usage; exit 2;;
    esac
    shift
done

EXISTING_OPAM=$(command -v opam || echo)
EXISTING_OPAMV=
if [ -n "$EXISTING_OPAM" ]; then
   EXISTING_OPAMV=$("$EXISTING_OPAM" --version || echo "unknown")
fi

FRESH=${FRESH:-0}

OPAMROOT=${OPAMROOT:-$HOME/.opam}

if [ ! -d "$OPAMROOT" ]; then FRESH=1; fi

if [ -z "$NOBACKUP" ] && [ ! "$FRESH" = 1 ] && [ -z "$RESTORE" ]; then
    case "$EXISTING_OPAMV" in
        2.*) NOBACKUP=1;;
        *) NOBACKUP=0;;
    esac
fi

xsudo() {
    local CMD=$1; shift
    local DST
    for DST in "$@"; do : ; done

    local DSTDIR=$(dirname "$DST")
    if [ ! -w "$DSTDIR" ]; then
        echo "Write access to $DSTDIR required, using 'sudo'."
        echo "Command: $CMD $@"
        if [ "$CMD" = "install" ]; then
            sudo "$CMD" -g 0 -o root "$@"
        else
            sudo "$CMD" "$@"
        fi
    else
        "$CMD" "$@"
    fi
}

if [ -n "$RESTORE" ]; then
    OPAM=$(command -v opam)
    OPAMV=$("$OPAM" --version)
    OPAM_BAK="$OPAM.$RESTORE"
    OPAMROOT_BAK="$OPAMROOT.$RESTORE"
    if [ ! -e "$OPAM_BAK" ] || [ ! -d "$OPAMROOT_BAK" ]; then
        echo "No backup of opam $RESTORE was found"
        exit 1
    fi
    if [ "$NOBACKUP" = 1 ]; then
        printf "## This will clear $OPAM and $OPAMROOT. Continue ? [Y/n] "
        read R
        case "$R" in
            ""|"y"|"Y"|"yes")
                xsudo rm -f "$OPAM"
                rm -rf "$OPAMROOT";;
            *) exit 1
        esac
    else
        xsudo mv "$OPAM" "$OPAM.$OPAMV"
        mv "$OPAMROOT" "$OPAMROOT.$OPAMV"
    fi
    xsudo mv "$OPAM_BAK" "$OPAM"
    mv "$OPAMROOT_BAK" "$OPAMROOT"
    printf "## Opam $RESTORE and its root were restored."
    if [ "$NOBACKUP" = 1 ]; then echo
    else echo " Opam $OPAMV was backed up."
    fi
    exit 0
fi

TMP=${TMPDIR:-/tmp}

ARCH=$(uname -m || echo unknown)
case "$ARCH" in
    x86|i?86) ARCH="i686";;
    x86_64|amd64) ARCH="x86_64";;
    ppc|powerpc|ppcle) ARCH="ppc";;
    aarch64_be|aarch64|armv8b|armv8l) ARCH="arm64";;
    armv5*|armv6*|earmv6*|armv7*|earmv7*) ARCH="armhf";;
    *) ARCH=$(echo "$ARCH" | awk '{print tolower($0)}')
esac

OS=$( (uname -s || echo unknown) | awk '{print tolower($0)}')

if [ "$OS" = "darwin" ] ; then
  OS=macos
fi

OPAM_BIN="opam-${TAG}-${ARCH}-${OS}"
OPAM_BIN_URL="https://github.com/ocaml/opam/releases/download/${TAG}/${OPAM_BIN}"

download() {
    if command -v wget >/dev/null; then wget -q -O "$@"
    else curl -s -L -o "$@"
    fi
}

bin_sha512() {
  case "$OPAM_BIN" in
    opam-2.0.5-arm64-linux)     echo "1ce4aa66a390c84dd56771a7aa9bda53913ed1d551fe24a5f54dc0a3305f53f6be440222bb9fc491474b6008b1cbbb4dcf6a8d9796c744651e40cbe02fde34d1";;
    opam-2.0.5-armhf-linux)     echo "85e6f9a44a0f86a2695fa5ecf1ef31c2f439298ce67a564b735272d9c5d735d93428b0d47cd53e4774027893366717c6a03f85c4cbefed3c720448406cbdc12f";;
    opam-2.0.5-i686-linux)      echo "df1493bb0a692374482b3b59d08e421eac307330c9eb8303c5a0e0118739eea2a4d7f9167bbcb57e2896c366f8923a3d06bd135aafa95fe48117a5e926ad014c";;
    opam-2.0.5-x86_64-linux)    echo "df994c53792cad806287f56717fb515524a9e7d074e6f782c8e6d3b364930ac2c891e40b1c0adb81449a2324088f0ce0b66c863124ddacaead88fad282c080a8";;
    opam-2.0.5-x86_64-macos)    echo "0c2d9be085643dbb549d4f56663b0f3909c59d49ec402b4c72aab33b3ee1c51ceac0fcaa9ba4e87edb9da49e97bafe6dbe3d793955d9fa626851d54ac34a81ab";;
    opam-2.0.5-x86_64-openbsd)  echo "cc40d4b031a054c07758330e4dd56b387b1955c66b0b1d4981f316198e9b1863ba2c57b1f802ddb5e10395942ef7f2f4165f10add8eb0bc50aa134e77c149d8e";;
    *) echo "no sha";;
  esac
}

check_sha512() {
  if command -v openssl > /dev/null; then
    sha512_devnull="cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
    sha512_check=`openssl sha512 2>&1 < /dev/null | cut -f 2 -d ' '`
    if [ "x$sha512_devnull" = "x$sha512_check" ]; then
      sha512=`openssl sha512 "$TMP/$OPAM_BIN" 2> /dev/null | cut -f 2 -d ' '`
      check=`bin_sha512`
      test "x$sha512" = "x$check"
    else
      echo "openssl 512 option not handled, binary integrity check can't be performed."
      return 0
    fi
  else
    echo "openssl not found, binary integrity check can't be performed."
    return 0
  fi
}

if [ -e "$TMP/$OPAM_BIN" ] && ! check_sha512 || [ ! -e "$TMP/$OPAM_BIN" ]; then
    echo "## Downloading opam $VERSION for $OS on $ARCH..."

    if ! download "$TMP/$OPAM_BIN" "$OPAM_BIN_URL"; then
        echo "There may not yet be a binary release for your architecture or OS, sorry."
        echo "See https://github.com/ocaml/opam/releases/tag/$TAG for pre-compiled binaries,"
        echo "or run 'make cold' from https://github.com/ocaml/opam/archive/$TAG.tar.gz"
        echo "to build from scratch"
        exit 10
    else
        if check_sha512; then
            echo "## Downloaded."
        else
            echo "Checksum mismatch, a problem occurred during download."
            exit 10
        fi
    fi
else
    echo "## Using already downloaded \"$TMP/$OPAM_BIN\""
fi

if [ -n "$EXISTING_OPAM" ]; then
    DEFAULT_BINDIR=$(dirname "$EXISTING_OPAM")
fi

while true; do
    printf "## Where should it be installed ? [$DEFAULT_BINDIR] "
    read BINDIR
    if [ -z "$BINDIR" ]; then BINDIR="$DEFAULT_BINDIR"; fi

    if [ -d "$BINDIR" ]; then break
    else
        printf "## $BINDIR does not exist. Create ? [Y/n] "
        read R
        case "$R" in
            ""|"y"|"Y"|"yes")
            xsudo mkdir -p $BINDIR
            break;;
        esac
    fi
done

if [ -e "$EXISTING_OPAM" ]; then
    if [ "$NOBACKUP" = 1 ]; then
        xsudo rm -f "$EXISTING_OPAM"
    else
        xsudo mv "$EXISTING_OPAM" "$EXISTING_OPAM.$EXISTING_OPAMV"
        echo "## $EXISTING_OPAM backed up as $(basename $EXISTING_OPAM).$EXISTING_OPAMV"
    fi
fi

if [ -d "$OPAMROOT" ]; then
    if [ "$FRESH" = 1 ]; then
        if [ "$NOBACKUP" = 1 ]; then
            printf "## This will clear $OPAMROOT. Continue ? [Y/n] "
            read R
            case "$R" in
                ""|"y"|"Y"|"yes")
                    rm -rf "$OPAMROOT";;
                *) exit 1
            esac
        else
            mv "$OPAMROOT" "$OPAMROOT.$EXISTING_OPAMV"
            echo "## $OPAMROOT backed up as $(basename $OPAMROOT).$EXISTING_OPAMV"
        fi
        echo "## opam $VERSION installed. Please run 'opam init' to get started"
    elif [ ! "$NOBACKUP" = 1 ]; then
        echo "## Backing up $OPAMROOT to $(basename $OPAMROOT).$EXISTING_OPAMV (this may take a while)"
        if [ -e "$OPAMROOT.$EXISTING_OPAMV" ]; then
            echo "ERROR: there is already a backup at $OPAMROOT.$EXISTING_OPAMV"
            echo "Please move it away or run with --no-backup"
        fi
        FREE=$(df -k "$OPAMROOT" | awk 'NR>1 {print $4}')
        NEEDED=$(du -sk "$OPAMROOT" | awk '{print $1}')
        if ! [ $NEEDED -lt $FREE ]; then
            echo "Error: not enough free space to backup. You can retry with --no-backup,"
            echo "--fresh, or remove '$OPAMROOT'"
            exit 1
        fi
        cp -a "$OPAMROOT" "$OPAMROOT.$EXISTING_OPAMV"
        echo "## $OPAMROOT backed up as $(basename $OPAMROOT).$EXISTING_OPAMV"
    fi
    rm -f "$OPAMROOT"/repo/*/*.tar.gz*
fi

xsudo install -m 755 "$TMP/$OPAM_BIN" "$BINDIR/opam"
echo "## opam $VERSION installed to $BINDIR"

if [ ! "$FRESH" = 1 ]; then
    echo "## Converting the opam root format & updating"
    "$BINDIR/opam" init --reinit -ni
fi

WHICH=$(command -v opam || echo notfound)

case "$WHICH" in
    "$BINDIR/opam") ;;
    notfound) echo "## Remember to add $BINDIR to your PATH";;
    *)
        echo "## WARNING: 'opam' command found in PATH does not match the installed one:"
        echo "   - Installed: '$BINDIR/opam'"
        echo "   - Found:     '$WHICH'"
        echo "Make sure to remove the second or fix your PATH to use the new opam"
        echo
esac

if [ ! "$NOBACKUP" = 1 ]; then
    echo "## Run this script again with '--restore $EXISTING_OPAMV' to revert."
fi

rm -f $TMP/$OPAM_BIN
