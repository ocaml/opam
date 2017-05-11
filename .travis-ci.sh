#!/bin/bash -xue

OPAMBSVERSION=1.2.2
OPAMBSROOT=$HOME/.opam.bootstrap
PATH=~/local/bin:$PATH; export PATH

TARGET="$1"; shift

# Install the build requirements into $OPAMBSROOT using the opam binary from the
# prepare step
install-bootstrap () {
    opam init --root=$OPAMBSROOT --yes --no-setup --compiler=$OCAML_VERSION
    eval $(opam config env --root=$OPAMBSROOT)
    if [ "$OPAM_TEST" = "1" ]; then
        opam pin add opam-file-format "git://github.com/ocaml/opam-file-format.git" --no-action --yes
        opam install ocamlfind lwt.3.0.0 cohttp.0.22.0 ssl cmdliner dose3 jsonm opam-file-format --yes
        # Allow use of ocamlfind packages in ~/local/lib
        FINDCONF=$(ocamlfind printconf conf)
        sed "s%^path=.*%path=\"$HOME/local/lib:$(opam config var lib)\"%" $FINDCONF >$FINDCONF.1
        mv $FINDCONF.1 $FINDCONF
    else
        opam install ocamlbuild --yes
    fi
    rm -f "$OPAMBSROOT"/log/*
}

case "$TARGET" in
    prepare)
        mkdir -p ~/local/bin
        wget -q -O ~/local/bin/opam \
             "https://github.com/ocaml/opam/releases/download/$OPAMBSVERSION/opam-$OPAMBSVERSION-$(uname -m)-$(uname -s)"
        chmod a+x ~/local/bin/opam
        exit 0
        ;;
    install)
        # Note: this part is cached, and must be idempotent
        # Re-init opam from scratch if the install fails
        if [ -d $OPAMBSROOT ]
        then install-bootstrap || { rm -rf $OPAMBSROOT; install-bootstrap; }
        else install-bootstrap
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
    eval $(opam config env --root=$OPAMBSROOT)

    [ "$(ocaml -vnum)" = "$OCAML_VERSION" ] || exit 12

    ./configure --prefix ~/local

    if [ "$OPAM_TEST" != "1" ]; then make lib-ext; fi
    make all opam-check

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
        OPAMEXTERNALSOLVER="$EXTERNAL_SOLVER" make -C tests || (tail -2000 tests/fulltest-*.log; exit 1)
    fi
)

( # Finally run the tests, in a clean environment
    export OPAMKEEPLOGS=1

    if [ "$OPAM_TEST" = "1" ]; then
        cd ~/build/opam-rt-*
        OPAMEXTERNALSOLVER="$EXTERNAL_SOLVER" make KINDS="local git" run
    else
        # Test basic actions
        opam init
        eval $(opam config env)
        opam install lwt
        opam list
        opam config report
    fi
)
