#!/bin/bash -xue

. .github/scripts/preamble.sh

unset-dev-version () {
  # disable git versioning to allow OPAMYES use for upgrade
  touch src/client/no-git-version
}

export OPAMYES=1
export OCAMLRUNPARAM=b

( # Run subshell in bootstrap root env to build
  (set +x ; echo -en "::group::build opam\r") 2>/dev/null
  if [[ $OPAM_TEST -eq 1 ]] ; then
    export OPAMROOT=$OPAMBSROOT
    # If the cached root is newer, regenerate a binary compatible root
    opam env || { rm -rf $OPAMBSROOT; init-bootstrap; }
    eval $(opam env)
  fi

  ./configure --prefix ~/local --with-mccs
  if [ "$OPAM_TEST" != "1" ]; then
    echo 'DUNE_PROFILE=dev' >> Makefile.config
  fi

  if [[ $OPAM_TEST$OPAM_COLD -eq 0 ]] ; then
    make lib-ext
  fi
  if [ $OPAM_UPGRADE -eq 1 ]; then
    unset-dev-version
  fi
  make all admin

  rm -f ~/local/bin/opam
  make install
  (set +x ; echo -en "::endgroup::build opam\r") 2>/dev/null

  export PATH=~/local/bin:$PATH
  opam config report

  if [ "$OPAM_TEST" = "1" ]; then
    # test if an upgrade is needed
    set +e
    opam list 2> /dev/null
    rcode=$?
    if [ $rcode -eq 10 ]; then
      echo "Recompiling for an opam root upgrade"
      (set +x ; echo -en "::group::rebuild opam\r") 2>/dev/null
      unset-dev-version
      make all admin
      rm -f ~/local/bin/opam
      make install
      opam list 2> /dev/null
      rcode=$?
      set -e
      if [ $rcode -ne 10 ]; then
        echo -e "\e[31mBad return code $rcode, should be 10\e[0m";
        exit $rcode
      fi
      (set +x ; echo -en "::endgroup::rebuild opam\r") 2>/dev/null
    fi
    set -e

    # Note: these tests require a "system" compiler and will use the one in $OPAMBSROOT
    make tests

    make distclean

    # Compile and run opam-rt
    (set +x ; echo -en "::group::opam-rt\r") 2>/dev/null
    opamrt_url="https://github.com/ocaml-opam/opam-rt"
    if [ ! -d $CACHE/opam-rt ]; then
      git clone $opamrt_url  $CACHE/opam-rt
    fi
    cd $CACHE/opam-rt
    git fetch origin
    if git ls-remote --exit-code origin $BRANCH ; then
      if git branch | grep -q $BRANCH; then
        git checkout $BRANCH
        git reset --hard origin/$BRANCH
      else
        git checkout -b $BRANCH origin/$BRANCH
      fi
    else
      git checkout master
      git reset --hard origin/master
    fi

    test -d _opam || opam switch create . --no-install --formula '"ocaml-system"'
    eval $(opam env)
    opam pin $GITHUB_WORKSPACE -yn --with-version to-test
    # opam lib pins defined in opam-rt are ignored as there is a local pin
    opam pin . -yn --ignore-pin-depends
    opam install opam-rt --deps-only opam-devel.to-test
    make || { opam reinstall opam-client -y; make; }
    (set +x ; echo -en "::endgroup::opam-rt\r") 2>/dev/null
  fi
)

export PATH=~/local/bin:$PATH

if [ $OPAM_UPGRADE -eq 1 ]; then
  OPAM12=$OPAM12CACHE/bin/opam
  if [[ ! -f $OPAM12 ]]; then
    mkdir -p $OPAM12CACHE/bin

    os="Linux"
    if [ "$RUNNER_OS" = "macOS" ]; then
      os="Darwin"
    fi
    wget "https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-$os" -O $OPAM12
    chmod +x $OPAM12
  fi
  export OPAMROOT=/tmp/opamroot
  rm -rf $OPAMROOT
  if [[ ! -d $OPAM12CACHE/root ]]; then
    $OPAM12 init
    cp -r /tmp/opamroot/ $OPAM12CACHE/root
  else
    cp -r $OPAM12CACHE/root /tmp/opamroot
  fi
  set +e
  $OPAM12 --version
  opam --version
  opam update
  rcode=$?
  if [ $rcode -ne 10 ]; then
    echo "[31mBad return code $rcode, should be 10[0m";
    exit $rcode
  fi
  opam_version=$(sed -ne 's/opam-version: *//p' $OPAMROOT/config)
  if [ "$opam_version" = '"1.2"' ]; then
    echo -e "\e[31mUpgrade failed, opam-root is still 1.2\e[0m";
    cat $OPAMROOT/config
    exit 2
  fi
  exit 0
fi

( # Finally run the tests, in a clean environment
  export OPAMKEEPLOGS=1

  if [[ $OPAM_TEST -eq 1 ]] ; then
    cd $CACHE/opam-rt
    make KINDS="local git" run
  else
    if [[ $OPAM_COLD -eq 1 ]] ; then
      export PATH=$PWD/bootstrap/ocaml/bin:$PATH
    fi

    # Test basic actions
    # The SHA is fixed so that upstream changes shouldn't affect CI. The SHA needs
    # to be moved forwards when a new version of OCaml is added to ensure that the
    # ocaml-system package is available at the correct version.
    opam init --bare default git+https://github.com/ocaml/opam-repository#$OPAM_TEST_REPO_SHA
    cat >> $(opam var root --global 2>/dev/null)/config <<EOF
archive-mirrors: "https://opam.ocaml.org/cache"
EOF
    opam switch create default ocaml-system
    eval $(opam env)
    opam install lwt
    opam list
    opam config report
  fi
)
