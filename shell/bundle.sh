#!/bin/bash -ue

OCAMLV=4.04.1
OPAMV=2.0.0~beta5
OPAM_REPO=https://opam.ocaml.org/2.0
DEBUG=
MAKESELF=
INSTALL_PACKAGES=()
TARGET=

help() {
    echo "Usage: $0 [OPTIONS] PACKAGES..."
    echo "  OPTIONS:"
    echo "    --help -h         This help"
    echo "    --ocaml VERSION   Select a version of OCaml to include. Must be able to compile opam."
    echo "    --opam VERSION    Select a version of opam to include. Must be at least 2.0.0~beta3"
    echo "    --repo URL        Archive or git repository containing the opam package repository to use"
    echo "    --debug -d        Add debug messages"
    echo "    --makeself        Generate a self-extracting bundle using 'makeself' instead of a tar archive"
    echo "    -o FILE           Output the bundle to the given FILE"
    echo "  PACKAGES:"
    echo "    Select the opam packages to be packed and installed by the bundle install script."
}

while [ $# -gt 0 ]; do
    case $1 in
        --help|-h)
            help; exit 0;;
        --ocaml)
            if [ $# -lt 2 ]; then echo "Missing argument to $1" >&2; exit 2; fi
            shift; OCAMLV=$1;;
        --opam)
            if [ $# -lt 2 ]; then echo "Missing argument to $1" >&2; exit 2; fi
            shift; OPAMV=$1;;
        --repo)
            if [ $# -lt 2 ]; then echo "Missing argument to $1" >&2; exit 2; fi
            OPAM_REPO=$1;;
        --debug)
            DEBUG=1;;
        --makeself)
            MAKESELF=1;;
        -o)
            if [ $# -lt 2 ]; then echo "Missing argument to $1" >&2; exit 2; fi
            TARGET="$(cd $(dirname "$1"); pwd)/$(basename "$1")";;
        -*)
            echo "Unrecognised option $1" >&2
            help; exit 2;;
        *)
            INSTALL_PACKAGES+=("$1")
    esac
    shift
done

OPAMTAG=${OPAMV//\~/-}

BOOTSTRAP_PACKAGES=("depext" "ocaml-base-compiler.$OCAMLV")
PACKAGES=("${INSTALL_PACKAGES[@]}" "${BOOTSTRAP_PACKAGES[@]}")


if [ -z "$TARGET" ]; then
    if [ -n "$MAKESELF" ]; then
        TARGET="$PWD/${INSTALL_PACKAGES[0]}-installer.sh"
    else
        TARGET="$PWD/${INSTALL_PACKAGES[0]}-bundle.tar.gz"
    fi
fi

comma() ( IFS=, ; echo "$*"; )

TMP=$(mktemp -d /tmp/opam-bundle.XXXX)
CONTENT="$TMP/$(basename ${TARGET%%.*})"
REPO="$CONTENT/repo"

mkdir -p "$CONTENT"

if [ -n "$DEBUG" ]; then
    trap "rm -rf /tmp/${TMP#/tmp/}" EXIT
    set -x
fi

title() {
  printf "\n\e[33m===================\e[m %-39s \e[33m===================\e[m\n\n" "$*"
}

title "Getting opam repository"

if [ "X${OPAM_REPO%.git}" != "X$OPAM_REPO" ] || [ "X${OPAM_REPO#git}" != "X$OPAM_REPO" ]; then
    git clone "$OPAM_REPO" "$REPO" --depth 1
    rm -rf "$REPO/.git"
else
    wget "$OPAM_REPO/index.tar.gz" -O "$REPO.tar.gz"
    mkdir -p "$REPO"
    cd "$REPO"
    tar xzf "$REPO.tar.gz"
    rm -f "$REPO.tar.gz"
fi
cd "$REPO"
opam admin upgrade
rm -rf compilers

title "Selecting and downloading packages"

opam admin filter --or \
     --resolve "ocaml-system.$OCAMLV",$(comma "${INSTALL_PACKAGES[@]}") \
     --resolve $(comma  "${BOOTSTRAP_PACKAGES[@]}")
opam admin cache --link archives

title "Downloading bootstrap archives"

cd "$CONTENT"
wget "https://github.com/ocaml/opam/releases/download/$OPAMTAG/opam-full-$OPAMTAG.tar.gz"

cat <<EOF >common.sh
DIR=\$( cd \$(dirname "\$0") && pwd )
PREFIX="\$DIR/bootstrap"
OPAMROOT="\$DIR/opam"
LOG="\$DIR/\$(basename "\$0").log"

title() {
  printf "\n\e[33m================\e[m %-45s \e[33m================\e[m\n\n" "\$*"
}
logged_cmd() {
  printf "\$1... "
  shift
  echo "+ [ \$1 ] \$*" >>\$LOG
  "\$@" >>\$LOG 2>&1
  echo >>\$LOG
  printf "\e[32mDone\e[m\n"
}

trap "if [ \$? -ne 0 ]; then printf '\nSomething went wrong, see log in \$LOG\n'; fi" EXIT

export PATH="\$PREFIX/bin:\$PATH"
export CAML_LD_LIBRARY_PATH="\$PREFIX/lib/ocaml/stublibs"
export OPAMROOT
cd \$DIR
EOF

cat <<EOF >bootstrap.sh
#!/bin/sh -ue

. "\$(dirname "\$0")/common.sh"

if [ -x "\$PREFIX/bin/ocamlc" ]; then
   echo "Already compiled OCaml found"
else
   title "Bootstrap: compiling OCaml"

   echo "This may take a while. Output is in \$LOG"
   logged_cmd "Uncompressing" tar xzf repo/archives/ocaml-base-compiler."$OCAMLV"/*
   cd "ocaml-$OCAMLV"
   logged_cmd "Configuring" ./configure -prefix "\$PREFIX"
   logged_cmd "Compiling" make world world.opt
   logged_cmd "Installing to temp prefix" make install
   cd "\$DIR"
fi

if [ -x "\$PREFIX/bin/opam" ]; then
   echo "Already compiled opam found"
else
   title "Bootstrap: compiling opam"

   echo "This may take a while. Output is in \$LOG"
   logged_cmd "Uncompressing" tar xzf "opam-full-$OPAMTAG.tar.gz"
   cd "opam-full-$OPAMTAG"
   logged_cmd "Configuring" ./configure --prefix "\$PREFIX"
   logged_cmd "Compiling extra dependencies" make lib-ext
   logged_cmd "Compiling" make
   logged_cmd "Installing to temp prefix" make install
   cd "\$DIR"
fi
EOF

cat <<EOF >configure.sh
#!/bin/sh -ue

. \$(dirname \$0)/common.sh

"\$DIR/bootstrap.sh"

if [ -d "\$OPAMROOT/default" ]; then
   echo "Already initialised opam sandbox found"
else
   title "Configure: initialising opam"

   if [ ! -f "\$OPAMROOT/config" ]; then
      logged_cmd "Initialising" opam init --bare --no-setup \$DIR/repo
   fi
   logged_cmd "Creating sandbox" opam switch create default ocaml-system
fi

title "Configure: bootstrapping auxiliary utilities"

logged_cmd "Compiling bootstrap utilities" opam install depext --yes

title "Configure: getting system dependencies"

echo "You may be asked for 'sudo' access to install required system dependencies through your package system"
opam depext ${INSTALL_PACKAGES[@]}

touch has_depexts
EOF

cat <<EOF >compile.sh
#!/bin/sh -ue

. \$(dirname \$0)/common.sh

if [ \$# -ne 1 ] || [ "X\${1#-}" != "X\$1" ] ; then
   echo "Usage: \$0 PREFIX"
   echo "  Bootstraps and compiles ${INSTALL_PACKAGES[*]}, then installs to the given prefix"
   exit 2
fi
DESTDIR="\$1"

if [ ! -e has_depexts ]; then "\$DIR/configure.sh"; fi

title "Compile: installing packages"

opam install --yes --destdir "\$DESTDIR" ${INSTALL_PACKAGES[@]}
EOF

chmod a+x bootstrap.sh configure.sh compile.sh
cd $(dirname "$CONTENT")
if [ -n "$MAKESELF" ]; then
    makeself $(basename "$CONTENT") "$TARGET" "$(basename "${TARGET%%.*}")" ./compile.sh
else
    tar cz $(basename "$CONTENT") -f "$TARGET"
fi
echo "Bundle has been generated as $TARGET"
