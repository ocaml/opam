#!/bin/bash -ue

help () {
    echo -e "$@"
    echo
    cat <<EOF
$0: prepare packages for distribution of opam
Options:
  full-archive	Build the inclusive source archive
  binary	Build a binary for your current platform
  publish	Send generated files to github releases page
  -t TAG	Git tag of the release
  -f FILE	Add file to be pushed
  -v VERSION    Make opam advertise as this version (instead of TAG)
EOF
    exit $#
}

ACTIONS=()
UPLOAD_FILES=()
TAG=
MAKE=${MAKE:-make}
VERSION=

while [ $# -gt 0 ]; do
    A=$1
    case "$1" in
        full-archive)
            ACTIONS+=("full-archive");;
        binary)
            ACTIONS+=("binary");;
        publish)
            ACTIONS+=("publish");;
        -t)
            shift; [ $# -gt 0 ] || help "Option $A requires an argument"
            TAG=$1;;
        -f)
            shift; [ $# -gt 0 ] || help "Option $A requires an argument"
            F=$1
            [ "${F#/}" != "$F" ] || VAR="$PWD/$F" # make absolute
            UPLOAD_FILES+=("$F");;
        -v)
            shift; [ $# -gt 0 ] || help "Option $A requires an argument"
            VERSION=$1;;
        *) help "Unknown option $A"
    esac
    shift
done

if [ ${#ACTIONS[@]} -eq 0 ]; then
    ACTIONS=("binary" "publish")
fi

TMP=$(mktemp -d /tmp/opam-release.XXXX)

cd $TMP

if [[ " ${ACTIONS[@]} " =~ " full-archive " ]]; then
    git clone https://github.com/ocaml/opam

    if [ -z "$TAG" ]; then
        TAG=$(cd opam && git tag -l '*.*.*' | tail -n1)
    fi
    if [ -z "$VERSION" ]; then
        VERSION=$TAG
    fi

    echo -e "\033[43;30mBuilding release tarball for $TAG\033[m"

    NAME=opam-full-"$TAG"
    TARBALL="$NAME".tar.gz
    mv opam "$NAME"

    cd $NAME
    git checkout refs/tags/$TAG
    sed -i 's/^AC_INIT(opam,.*)/AC_INIT(opam,'"$VERSION"')/' configure.ac
    ${MAKE} configure
    ./configure
    ${MAKE} download-ext
    cd ..

    FILES=($NAME/src_ext/*.{tar.gz,tbz})
    for f in $(cd $NAME && git ls-tree --name-only -r "$TAG"); do
        FILES=("${FILES[@]}" "$NAME/$f")
    done

    tar cz "${FILES[@]}" -f "$TARBALL"

    echo -e "\033[43;30mSuccessfully packed $TARBALL\033[m"
    UPLOAD_FILES+=("$TARBALL")
fi

if [ -z "$TAG" ]; then
    git clone https://github.com/ocaml/opam
    TAG=$(cd opam && git tag -l '*.*.*' | tail -n1)
    echo -e "\033[33mUsing tag $TAG\033[m"
fi
if [ -z "${NAME:-}" ]; then NAME=opam-full-"$TAG"; fi
if [ -z "${TARBALL:-}" ]; then TARBALL="$NAME".tar.gz; fi

if [[ " ${ACTIONS[@]} " =~ " binary " ]]; then
    if [ "$(ocaml -vnum)" != "4.03.0" ]; then
        echo "Error: you should use OCaml 4.03.0 for building the release"
        exit 1
    fi

    BINARY="opam-$TAG-$(uname -m || echo unknown)-$(uname -s || echo unknown)"
    echo "Building binary $BINARY"

    if [ ! -f "$TARBALL" ]; then
        curl -OL "https://github.com/ocaml/opam/releases/download/$TAG/$TARBALL"
    fi

    tar -xzf "$TARBALL"
    cd opam-full-$TAG
    ./configure
    ${MAKE} lib-ext
    ${MAKE} opam
    cd ..
    ln -s opam-full-$TAG/src/opam $BINARY
    UPLOAD_FILES+=("$BINARY")
fi

if [[ " ${ACTIONS[@]} " =~ " publish " ]]; then
    echo -e "\n\033[43;30mUploading ${UPLOAD_FILES[@]} from $TMP to github...\033[m"
    if type git-upload-release >&/dev/null; then
        for f in "${UPLOAD_FILES[@]}"; do
            echo "Uploading $(basename "$f"), please be patient..."
            (cd "$(dirname "$f")" && git-upload-release ocaml opam "$TAG" "$(basename "$f")")
        done
    else
        echo "'git-upload-release' not found, can't automatically"
        echo "upload to github. You can manually upload the following files to"
        echo "https://github.com/ocaml/opam/releases/tag/$TAG"
        echo
        echo "${UPLOAD_FILES[*]}"
    fi
fi
