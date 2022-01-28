#!/bin/bash -xue

. .github/scripts/main/preamble.sh

PLATFORM="$1"
OCAML_VERSION="$2"
HOST="${3:-}"

if [ "$PLATFORM" = Windows ]; then
  EXE='.exe'
  if [ -e $OCAML_LOCAL.tar ]; then
    mkdir -p "$OCAML_LOCAL"
    tar -C "$OCAML_LOCAL" -pxf "$OCAML_LOCAL.tar"
    exit 0
  fi
else
  EXE=''
fi

# OCaml's build system doesn't support the triple-form for Cygwin building
case "$HOST" in
  *-pc-cygwin)
    HOST='';;
  x86_64-pc-windows)
    eval $(shell/msvs-detect --arch=x64)
    export PATH="$MSVS_PATH$PATH"
    export LIB="$MSVS_LIB${LIB:-}"
    export INCLUDE="$MSVS_INC${INCLUDE:-}"
    echo "Using $MSVS_NAME x64";;
  i686-pc-windows)
    eval $(shell/msvs-detect --arch=x86)
    export PATH="$MSVS_PATH$PATH"
    export LIB="$MSVS_LIB${LIB:-}"
    export INCLUDE="$MSVS_INC${INCLUDE:-}"
    echo "Using $MSVS_NAME x86";;
esac

case "$HOST" in
  *-pc-windows|*-w64-mingw32)
    PREFIX="$(cygpath -m "$OCAML_LOCAL")";;
  *)
    PREFIX="$OCAML_LOCAL";;
esac

FLEXDLL_VERSION=0.40

curl -sLO "https://caml.inria.fr/pub/distrib/ocaml-${OCAML_VERSION%.*}/ocaml-$OCAML_VERSION.tar.gz"
if [[ $PLATFORM = 'Windows' ]] ; then
  curl -sLO "https://github.com/alainfrisch/flexdll/archive/refs/tags/$FLEXDLL_VERSION.tar.gz"
fi

tar -xzf "ocaml-$OCAML_VERSION.tar.gz"

cd "ocaml-$OCAML_VERSION"
if [[ $PLATFORM = 'Windows' ]] ; then
  tar -xzf ../$FLEXDLL_VERSION.tar.gz
  rm -rf flexdll
  mv "flexdll-$FLEXDLL_VERSION" flexdll
fi

if [[ $PLATFORM = 'macOS' ]]; then
  if [[ ! -e configure.ac ]]; then
    # Fix build with XCode 12+ (cf. https://github.com/ocaml/opam/issues/4364)
    sed -ib -e 's/opts=""/opts="-Wno-implicit-function-declaration"/' config/auto-aux/hasgot
  fi
fi

if [[ -n $HOST ]]; then
  HOST=" --host=$HOST"
fi

OCAML_BRANCH="${OCAML_VERSION%.*}"
case "$OCAML_BRANCH" in
  ?.?) OCAML_BRANCH="${OCAML_BRANCH%.*}.0${OCAML_BRANCH#*.}";;
esac
OCAML_BRANCH="${OCAML_BRANCH/./}"

if [[ $OPAM_TEST -ne 1 ]] ; then
  if [[ -e configure.ac ]]; then
    CONFIGURE_SWITCHES="--disable-debugger --disable-debug-runtime --disable-ocamldoc --disable-installing-bytecode-programs --disable-installing-source-artifacts"
    if [[ $OCAML_BRANCH -eq 408 ]]; then
      curl -L https://github.com/ocaml/ocaml/commit/c8ee39b320207717135d88cad67fb65d0901d6b6.patch -o pr8858.patch
      patch -p1 -i pr8858.patch
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES --disable-graph-lib"
    fi
  else
    if [[ -n $HOST ]]; then
      echo "CI doesn't support specifying HOST for OCaml 4.07 and earlier"
      exit 2
    fi
    CONFIGURE_SWITCHES="-no-graph -no-debugger -no-ocamldoc"
    if [[ $OCAML_BRANCH = 408 ]]; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES --disable-graph-lib"
    fi
    if [[ $OCAML_BRANCH -gt 402 ]] ; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES -no-ocamlbuild"
    fi

  fi
fi

if ! ./configure --prefix "$PREFIX"$HOST ${CONFIGURE_SWITCHES:-} ; then
  echo
  echo -e "[\e[31mERROR\e[0m] OCaml's configure script failed"
  (set +x ; echo -en "::group::config.log contents\r") 2>/dev/null
  cat config.log
  (set +x ; echo -en "::endgroup::config.log\r") 2>/dev/null
  exit 2
fi

if [[ $OPAM_TEST -eq 1 ]] ; then
  make -j 4 world.opt
else
  # XXX Technically shouldn't do this for low OCaml versions
  make -j world.opt
fi

make install

cd ..
rm -rf "ocaml-$OCAML_VERSION"

if [[ $PLATFORM != 'Windows' ]]; then
  echo > "$OCAML_LOCAL/bin/ocamldoc" <<"EOF"
#!/bin/sh

echo 'ocamldoc is not supposed to be called'>&2
exit 1
EOF
  chmod +x "$OCAML_LOCAL/bin/ocamldoc"
fi

if [[ $OCAML_BRANCH -gt 407 ]]; then
  make -C src_ext dune-local.stamp
  cd src_ext/dune-local
  ocaml bootstrap.ml
  cp dune.exe "$PREFIX/bin/dune$EXE"
  cd ../..

  ./configure
  make
  cp -a _build "$OCAML_LOCAL/"
  rm -f "$OCAML_LOCAL/_build/log"
  mv "$OCAML_LOCAL/_build/default/src_ext" "$OCAML_LOCAL/_build/"
  rm -rf "$OCAML_LOCAL/_build/default"/* "$OCAML_LOCAL/_build/install"
  mv "$OCAML_LOCAL/_build/src_ext" "$OCAML_LOCAL/_build/default/" 
  git clean -dfX
fi

# The Windows BSD tar can't cope with symlinks, so we pre-tar the archive and cache that!
if [[ $PLATFORM = 'Windows' ]]; then
  tar -C "$OCAML_LOCAL" -pcf "$OCAML_LOCAL.tar" .
fi
