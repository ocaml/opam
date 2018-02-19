#!/bin/bash -xue

OPAMBSVERSION=1.2.2
OPAMBSROOT=$HOME/.opam.bootstrap
PATH=~/local/bin:$PATH; export PATH

TARGET="$1"; shift

# Install the build requirements into $OPAMBSROOT using the opam binary from the
# prepare step
install-bootstrap () {
    export OPAMROOT="$OPAMBSROOT"
    opam init --yes --no-setup --compiler=$OCAML_VERSION
    eval $(opam config env)
    if [ "$OPAM_TEST" = "1" ]; then
        opam pin add jbuilder --dev-repo --yes
        opam install ocamlfind ocamlbuild cohttp cohttp-lwt-unix 'lwt>=3.1.0' ssl cmdliner dose3 opam-file-format re 'jbuilder>=1.0+beta14' 'mccs>=1.1+4' --yes
        # Allow use of ocamlfind packages in ~/local/lib
        FINDCONF=$(ocamlfind printconf conf)
        sed "s%^path=.*%path=\"$HOME/local/lib:$(opam config var lib)\"%" $FINDCONF >$FINDCONF.1
        mv $FINDCONF.1 $FINDCONF

    fi
    rm -f "$OPAMBSROOT"/log/*
}

case "$TARGET" in
    prepare)
      mkdir -p ~/local/bin

      if [ "$COLD" = "1" ]; then
        wget http://ftpmirror.gnu.org/gnu/make/make-4.2.tar.gz
        tar -xzf make-4.2.tar.gz
        mkdir make-4.2-build
        cd make-4.2-build
        ../make-4.2/configure --prefix ~/local
        make
        make install
        cd ..
        make compiler
      else
        wget -q -O ~/local/bin/opam \
             "https://github.com/ocaml/opam/releases/download/$OPAMBSVERSION/opam-$OPAMBSVERSION-$(uname -m)-$(uname -s)"
        chmod a+x ~/local/bin/opam
        if [ "$TRAVIS_OS_NAME" = "osx" ] && [ -n "$EXTERNAL_SOLVER" ]; then
            rvm install ruby-2.3.3
            rvm --default use 2.3.3
            brew install "$EXTERNAL_SOLVER"
        fi
      fi
      exit 0
      ;;
    install)
      if [ "$COLD" = "1" ]; then
        make lib-pkg
      else
        # Note: this part is cached, and must be idempotent
        # Re-init opam from scratch if the install fails
        if [ -d $OPAMBSROOT ]
        then install-bootstrap || { rm -rf $OPAMBSROOT; install-bootstrap; }
        else install-bootstrap
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

# Git should be configured properly to run the tests
git config --global user.email "travis@example.com"
git config --global user.name "Travis CI"

( # Run subshell in bootstrap root env to build
  if [ "$COLD" != "1" ]; then
    export OPAMROOT="$OPAMBSROOT"
    eval $(opam config env)

    [ "$(ocaml -vnum)" = "$OCAML_VERSION" ] || exit 12
  fi

    ./configure --prefix ~/local --with-mccs

    if [ "$OPAM_TEST" != "1" -a "$COLD" != "1"  ]; then make lib-ext; fi
    make all

    rm -f ~/local/bin/opam
    make install

    if [ "$OPAM_TEST" = "1" ]; then
        make libinstall LIBINSTALL_DIR=$HOME/local/lib
        # Compile and run opam-rt
        cd ~/build
        wget https://github.com/ocaml/opam-rt/archive/$TRAVIS_PULL_REQUEST_BRANCH.tar.gz -O opam-rt.tar.gz || \
        wget https://github.com/ocaml/opam-rt/archive/master.tar.gz -O opam-rt.tar.gz
        tar xvfz opam-rt.tar.gz
        cd opam-rt-*
        make
    else
        # Note: these tests require a "system" compiler and will use the one in $OPAMBSROOT
        OPAMEXTERNALSOLVER="$EXTERNAL_SOLVER" make tests ||
            (tail -2000 _build/default/tests/fulltest-*.log; echo "-- TESTS FAILED --"; exit 1)
    fi
)

( # Finally run the tests, in a clean environment
    export OPAMKEEPLOGS=1

    if [ "$OPAM_TEST" = "1" ]; then
        cd ~/build/opam-rt-*
        OPAMEXTERNALSOLVER="$EXTERNAL_SOLVER" make KINDS="local git" run
    else
        if [ "$COLD" = "1" ]; then
          export PATH=`pwd`/bootstrap/ocaml/bin:$PATH
        else
          export PATH="$OPAMBSROOT/$OCAML_VERSION/bin":$PATH
        fi
        # Test basic actions
        opam init --bare
        opam switch create default ocaml-system
        # eval $(opam config env)
        opam install lwt
        opam list
        opam config report
    fi
)
