#!/bin/bash -ue

help () {
    echo -e "$@"
    echo
    cat <<EOF
$0: prepare packages for distribution of OPAM
Options:
  full-archive	Build the inclusive source archive
  binary	Build a binary for your current platform
  publish	Send generated files to github releases page
  -t TAG	Git tag of the release
  -f FILE	Add file to be pushed
  -n NAME	Your github name (default \$gitname)
EOF
    exit $#
}

ACTIONS=()
UPLOAD_FILES=()
TAG=
MAKE=${MAKE:-make}

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
            shift || help "$A requires an argument"
            TAG=$1;;
        -f)
            shift || help "$A requires an argument"
            F=$1
            [ "${F#/}" != "$F" ] || VAR="$PWD/$F" # make absolute
            UPLOAD_FILES+=("$F");;
        -n)
            shift || help "$A requires an argument"
            gitname=$1;;
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

    echo -e "\033[43;30mBuilding release tarball for $TAG\033[m"

    NAME=opam-full-"$TAG"
    TARBALL="$NAME".tar.gz
    mv opam "$NAME"

    cd $NAME
    git checkout refs/tags/$TAG
    sed -i 's/^AC_INIT(opam,.*)/AC_INIT(opam,'"$TAG"')/' configure.ac
    ${MAKE} configure
    ./configure
    ${MAKE} clone
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
    if [ "$(ocaml -vnum)" != "4.01.0" ]; then
        echo "Error: you should use OCaml 4.01.0 for building the release"
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
    ${MAKE}
    cd ..
    ln -s opam-full-$TAG/_obuild/opam/opam.asm $BINARY
    UPLOAD_FILES+=("$BINARY")
fi

if [[ " ${ACTIONS[@]} " =~ " publish " ]]; then
    echo -e "\n\033[43;30mUploading ${UPLOAD_FILES[@]} from $TMP to github...\033[m"
    if [ -z "${gitname:-}" ]; then
        echo "Please enter your github name: "
        read gitname
    fi
    url=$(curl "https://api.github.com/repos/ocaml/opam/releases" \
        | jq '.[] | select(.tag_name == "'"$TAG"'") | .upload_url' \
        | sed 's%"\([^"{?]*\).*"%\1%')
    for f in "${UPLOAD_FILES[@]}"; do
        base=$(basename "$f")
        echo "Uploading $base, please be patient..."
        curl -u "$gitname" \
             -H "name: $base" \
             -H "Content-Type: application/gzip" \
             --data-binary "@$f" \
             "$url?name=$base" | jq ".message"
    done
fi
