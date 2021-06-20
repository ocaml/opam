#!/bin/bash -xue

. .github/scripts/preamble.sh

wget "http://caml.inria.fr/pub/distrib/ocaml-${OCAML_VERSION%.*}/ocaml-$OCAML_VERSION.tar.gz"
tar -xzf "ocaml-$OCAML_VERSION.tar.gz"

cd "ocaml-$OCAML_VERSION"
if [[ $OPAM_TEST -ne 1 ]] ; then
  if [[ -e configure.ac ]]; then
    CONFIGURE_SWITCHES="--disable-debugger --disable-debug-runtime"
    if [ "$SOLVER" != "z3" ]; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES --disable-ocamldoc"
    fi
    if [[ ${OCAML_VERSION%.*} = '4.08' ]]; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES --disable-graph-lib"
    fi
  else
    CONFIGURE_SWITCHES="-no-graph -no-debugger"
    if [ "$SOLVER" != "z3" ]; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES -no-ocamldoc"
    fi
    if [[ ${OCAML_VERSION%.*} = '4.08' ]]; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES --disable-graph-lib"
    fi
    if [[ "$OCAML_VERSION" != "4.02.3" ]] ; then
      CONFIGURE_SWITCHES="$CONFIGURE_SWITCHES -no-ocamlbuild"
    fi

  fi
fi

./configure --prefix $OCAML_LOCAL ${CONFIGURE_SWITCHES:-}

if [[ $OPAM_TEST -eq 1 ]] ; then
  make -j 4 world.opt
else
  make world.opt
fi

make install

cd ..
rm -rf "ocaml-$OCAML_VERSION"
