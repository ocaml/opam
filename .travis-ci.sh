# Git should be configured properely to run the tests
git config --global user.email "travis@example.com"
git config --global user.name "Travis CI"

install_on_linux () {
  # Install OCaml PPAs
  case "$OCAML_VERSION" in
  3.12.1) ppa=avsm/ocaml312+opam11 ;;
  4.00.1) ppa=avsm/ocaml40+opam11 ;;
  4.01.0) ppa=avsm/ocaml41+opam11 ;;
  4.02.0) ppa=avsm/ocaml42+opam11 ;;
  *) echo Unknown $OCAML_VERSION; exit 1 ;;
  esac

  echo "yes" | sudo add-apt-repository ppa:$ppa
  sudo apt-get update -qq
  sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra time $EXTERNAL_SOLVER
}

install_on_osx () {
  curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
  sudo hdiutil attach XQuartz-2.7.6.dmg
  sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
  case "$OCAML_VERSION" in
  4.01.0) brew install ocaml;;
  4.02.0) brew install ocaml --HEAD ;;
  *) echo Skipping $OCAML_VERSION on OSX; exit 0 ;;
  esac
  if [ "$EXTERNAL_SOLVER" != "" ]; then
    brew install $EXTERNAL_SOLVER
  fi
}

case $TRAVIS_OS_NAME in
osx) install_on_osx ;;
linux) install_on_linux ;;
esac

echo OCaml version
ocaml -version

export OPAMYES=1
export OCAMLRUNPARAM=b

if [ "$OPAM_TEST" = "1" ]; then
    # Compile OPAM using the system libraries (install them using OPAM)
    opam init
    eval `opam config env`
    opam install ocamlfind lwt cohttp ssl cmdliner ocamlgraph dose cudf re
    ./configure
    make
    # overwrite the previous install of OPAM with the new binary
    # and libraries
    sudo make install
    make libinstall
    # Compile and run opam-rt
    wget https://github.com/ocaml/opam-rt/archive/master.tar.gz
    tar xvfz master.tar.gz
    cd opam-rt-master
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
