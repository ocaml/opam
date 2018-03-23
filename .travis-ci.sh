#!/bin/bash -xue

OPAMBSVERSION=2.0.0-rc
OPAMBSROOT=$HOME/.opam.cached
OPAMBSSWITCH=opam-build
PATH=~/local/bin:$PATH; export PATH

TARGET="$1"; shift

COLD=${COLD:-0}
OPAM_TEST=${OPAM_TEST:-0}
EXTERNAL_SOLVER=${EXTERNAL_SOLVER:-}

init-bootstrap () {
  export OPAMROOT=$OPAMBSROOT
  # The system compiler will be picked up
  opam init --yes --no-setup
  eval $(opam env)
  opam update
  CURRENT_SWITCH=$(opam config var switch)
  if [[ $CURRENT_SWITCH != "default" ]] ; then
    opam switch default
    eval $(opam env)
    opam switch remove $CURRENT_SWITCH --yes
  fi

  if [ "$OPAM_TEST" = "1" ]; then
    opam switch create $OPAMBSSWITCH ocaml-system
    eval $(opam env)
    opam pin add jbuilder --kind=version '1.0+beta18.1' --no-action --yes
    opam install cohttp-lwt-unix ssl cmdliner dose3 opam-file-format re 'jbuilder>=1.0+beta18' 'mccs>=1.1+5' --yes
  fi
  rm -f "$OPAMBSROOT"/log/*
}

case "$TARGET" in
  prepare)
    mkdir -p ~/local/bin

    # Git should be configured properly to run the tests
    git config --global user.email "travis@example.com"
    git config --global user.name "Travis CI"

    if [[ $COLD -eq 1 ]] ; then
      if [ ! -x ~/local/bin/make ] ; then
        wget http://ftpmirror.gnu.org/gnu/make/make-4.2.tar.gz
        tar -xzf make-4.2.tar.gz
        mkdir make-4.2-build
        cd make-4.2-build
        ../make-4.2/configure --prefix ~/local
        make
        make install
        cd ..
      fi
    else
      if [[ $TRAVIS_OS_NAME = "osx" && -n $EXTERNAL_SOLVER ]] ; then
        rvm install ruby-2.3.3
        rvm --default use 2.3.3
        brew install "$EXTERNAL_SOLVER"
      fi

      if [[ -e ~/local/versions ]] ; then
        . ~/local/versions
        if [[ $LOCAL_OCAML_VERSION != $OCAML_VERSION ]] ; then
          echo "Cached compiler is $LOCAL_OCAML_VERSION; requested $OCAML_VERSION"
          echo "Resetting local cache"
          rm -rf ~/local
        elif [[ ${LOCAL_OPAMBSVERSION:-$OPAMBSVERSION} != $OPAMBSVERSION ]] ; then
          echo "Cached opam is $LOCAL_OPAMBSVERSION; requested $OPAMBSVERSION"
          echo "Replacement opam will be downloaded"
          rm -f ~/local/bin/opam-bootstrap
        fi
      fi
    fi
    exit 0
    ;;
  install)
    if [[ $COLD -eq 1 ]] ; then
      make compiler
      make lib-pkg
    else
      if [[ ! -x ~/local/bin/ocaml ]] ; then
        echo -en "travis_fold:start:ocaml\r"
        wget http://caml.inria.fr/pub/distrib/ocaml-${OCAML_VERSION%.*}/ocaml-$OCAML_VERSION.tar.gz
        tar -xzf ocaml-$OCAML_VERSION.tar.gz
        cd ocaml-$OCAML_VERSION
        if [[ $OPAM_TEST -ne 1 ]] ; then
          CONFIGURE_SWITCHES="-no-ocamldoc -no-ocamlbuild"
        fi
        ./configure --prefix ~/local -no-graph -no-debugger ${CONFIGURE_SWITCHES:-}
        if [[ $OPAM_TEST -eq 1 ]] ; then
          make -j world.opt
        else
          make world.opt
        fi
        make install
        echo "LOCAL_OCAML_VERSION=$OCAML_VERSION" > ~/local/versions
        echo -en "travis_fold:end:ocaml\r"
      fi

      if [[ $OPAM_TEST -eq 1 ]] ; then
        echo -en "travis_fold:start:opam\r"
        if [[ ! -e ~/local/bin/opam-bootstrap ]] ; then
          wget -q -O ~/local/bin/opam-bootstrap \
               "https://github.com/ocaml/opam/releases/download/$OPAMBSVERSION/opam-$OPAMBSVERSION-$(uname -m)-$(uname -s)"
        fi

        cp -f ~/local/bin/opam-bootstrap ~/local/bin/opam
        chmod a+x ~/local/bin/opam

        if [[ -d $OPAMBSROOT ]] ; then
          init-bootstrap || { rm -rf $OPAMBSROOT; init-bootstrap; }
        else
          init-bootstrap
        fi
        echo -en "travis_fold:end:opam\r"
      fi
    fi
    exit 0
    ;;
  build)
    ;;
  *)
    echo "bad command $TARGET"; exit 1
esac

export OPAMYES=1
export OCAMLRUNPARAM=b

( # Run subshell in bootstrap root env to build
  if [[ $OPAM_TEST -eq 1 ]] ; then
    export OPAMROOT=$OPAMBSROOT
    eval $(opam env)
  fi

  ./configure --prefix ~/local --with-mccs

  if [[ $OPAM_TEST$COLD -eq 0 ]] ; then
    make lib-ext
  fi
  make all

  rm -f ~/local/bin/opam
  make install

  if [ "$OPAM_TEST" = "1" ]; then
    make distclean
    for pin in core format solver repository state client ; do
      opam pin add --kind=path opam-$pin . --yes
    done
    # Compile and run opam-rt
    cd ~/build
    wget https://github.com/ocaml/opam-rt/archive/$TRAVIS_PULL_REQUEST_BRANCH.tar.gz -O opam-rt.tar.gz || \
    wget https://github.com/ocaml/opam-rt/archive/master.tar.gz -O opam-rt.tar.gz
    tar -xzf opam-rt.tar.gz
    cd opam-rt-*
    make

    opam switch default
    opam switch remove $OPAMBSSWITCH --yes
  else
    # Note: these tests require a "system" compiler and will use the one in $OPAMBSROOT
    OPAMEXTERNALSOLVER="$EXTERNAL_SOLVER" make tests ||
      (tail -2000 _build/default/tests/fulltest-*.log; echo "-- TESTS FAILED --"; exit 1)
  fi
)

( # Finally run the tests, in a clean environment
  export OPAMKEEPLOGS=1

  if [[ $OPAM_TEST -eq 1 ]] ; then
    cd ~/build/opam-rt-*
    OPAMEXTERNALSOLVER="$EXTERNAL_SOLVER" make KINDS="local git" run
  else
    if [[ $COLD -eq 1 ]] ; then
      export PATH=$PWD/bootstrap/ocaml/bin:$PATH
    fi

    # Test basic actions
    opam init --bare
    opam switch create default ocaml-system
    eval $(opam env)
    opam install lwt
    opam list
    opam config report
  fi
)

rm -f ~/local/bin/opam
