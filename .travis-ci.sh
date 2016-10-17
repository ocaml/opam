# Git should be configured properely to run the tests
git config --global user.email "travis@example.com"
git config --global user.name "Travis CI"

install_on_linux () {
  # Install OCaml PPAs
  case "$OCAML_VERSION" in
  3.12.1) ppa=avsm/ocaml312+opam12 ;;
  4.00.1) ppa=avsm/ocaml40+opam12 ;;
  4.01.0) ppa=avsm/ocaml41+opam12 ;;
  4.02.3) ppa=avsm/ocaml42+opam12 ;;
  4.03.0) ppa=avsm/ocaml43+opam12 ;;
  *) echo Unknown $OCAML_VERSION; exit 1 ;;
  esac

  echo "yes" | sudo add-apt-repository ppa:$ppa
  sudo apt-get update -qq
  sudo apt-get install -qq ocaml ocaml-native-compilers time $EXTERNAL_SOLVER ${OPAM_TEST:+opam}
}

install_on_osx () {
  curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
  sudo hdiutil attach XQuartz-2.7.6.dmg
  sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
  case "$OCAML_VERSION" in
  4.03.0) brew update; brew install ocaml;;
#  4.03.0) brew update; brew install ocaml --HEAD ;;
  *) echo Skipping $OCAML_VERSION on OSX; exit 0 ;;
  esac
  if [ -n "$EXTERNAL_SOLVER$OPAM_TEST" ]; then
      brew install $EXTERNAL_SOLVER ${OPAM_TEST:+opam}
  fi
}

source_branch_name () {
    if [ "$TRAVIS_PULL_REQUEST" = "false" ]; then
        echo $TRAVIS_BRANCH
    else
        curl -s https://api.github.com/repos/$TRAVIS_REPO_SLUG/pulls/$TRAVIS_PULL_REQUEST | sed -n '/^ *"head":/ { :loop; s/^ *"ref": "\(.*\)",/\1/p; t quit; n; b loop; :quit }'
    fi
}

case $TRAVIS_OS_NAME in
osx) install_on_osx ;;
linux) install_on_linux ;;
esac

OCAMLV=$(ocaml -vnum)
echo === OCaml version $OCAMLV ===
if [ "$OCAMLV" != "$OCAML_VERSION" ]; then
    echo "OCaml version doesn't match: travis script needs fixing"
    exit 12
fi

export OPAMYES=1
export OCAMLRUNPARAM=b

if [ "$OPAM_TEST" = "1" ]; then
    # Compile OPAM using the system libraries (install them using OPAM)
    # ignore the warnings

    echo "Bootstrapping for opam with:"
    opam config report

    opam init

    eval `opam config env`
    opam install ocamlfind lwt.2.5.2 cohttp.0.20.2 ssl cmdliner dose.3.3 jsonm
    ./configure
    make
    # overwrite the previous install of OPAM with the new binary
    # and libraries
    sudo make install
    make libinstall prefix=$(opam config var prefix)
    # Compile and run opam-rt
    wget https://github.com/ocaml/opam-rt/archive/$(source_branch_name).tar.gz -O opam-rt.tar.gz || \
    wget https://github.com/ocaml/opam-rt/archive/master.tar.gz -O opam-rt.tar.gz
    tar xvfz opam-rt.tar.gz
    cd opam-rt-*
    make
    OPAMEXTERNALSOLVER=$EXTERNAL_SOLVER make KINDS="local git" run
else
    # Compile OPAM from sources and run the basic tests
    ./configure
    make lib-ext
    make
    make opam-check
    make tests > tests.log 2>&1 || (tail -1000 tests.log && exit 1)
    # Let's see basic tasks works
    sudo make install
    opam init
    opam install lwt
    opam list
fi
