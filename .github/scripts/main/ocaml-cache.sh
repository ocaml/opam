#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

PLATFORM="$1"
OCAML_VERSION="$2"
HOST="${3:-}"

if [ "$PLATFORM" = Windows ]; then
  EXE='.exe'
  if [ -e "$OCAML_LOCAL".tar ]; then
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

FLEXDLL_VERSION=0.43

curl -sLO "https://caml.inria.fr/pub/distrib/ocaml-${OCAML_VERSION%.*}/ocaml-$OCAML_VERSION.tar.gz"
if [[ $PLATFORM = 'Windows' ]] ; then
  curl -sLO "https://github.com/ocaml/flexdll/archive/refs/tags/$FLEXDLL_VERSION.tar.gz"
fi

tar -xzf "ocaml-$OCAML_VERSION.tar.gz"

case "${OCAML_VERSION%.*}" in
  4.08) PATCHES='e322556b0a9097a2eff2117476193b773e1b947f 17df117b4939486d3285031900587afce5262c8c';;
  4.09) PATCHES='8eed2e441222588dc385a98ae8bd6f5820eb0223';;
  4.10) PATCHES='4b4c643d1d5d28738f6d900cd902851ed9dc5364';;
  4.11) PATCHES='dd28ac0cf4365bd0ea1bcc374cbc5e95a6f39bea';;
  4.12) PATCHES='1eeb0e7fe595f5f9e1ea1edbdf785ff3b49feeeb';;
  *) PATCHES='';;
esac

cd "ocaml-$OCAML_VERSION"
for sha in $PATCHES; do
  curl -sL "https://github.com/ocaml/ocaml/commit/$sha.patch" -o "../$sha.patch"
  patch -p1 -i "../$sha.patch"
done

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

if ! ./configure --prefix "$PREFIX$HOST" --with-vendored-deps "${CONFIGURE_SWITCHES:-}" ; then
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
  cat > "$OCAML_LOCAL/bin/ocamldoc" <<"EOF"
#!/bin/sh

echo 'ocamldoc is not supposed to be called'>&2
exit 1
EOF
  chmod +x "$OCAML_LOCAL/bin/ocamldoc"
fi

# Hand-over control to a separate script in case the branch being tested
# updates this script, which will fail on Windows (since the script is "open"
# and can't be overwritten)
cp -pf .github/scripts/main/create-ocaml-cache.sh ../create-ocaml-cache.sh
exec ../create-ocaml-cache.sh "$OCAML_BRANCH" "$PREFIX" "$EXE" "$OCAML_LOCAL" "$PLATFORM"
