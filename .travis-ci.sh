# Git should be configured properely to run the tests
git config --global user.email "travis@example.com"
git config --global user.name "Travis CI"

# Install OCaml and OPAM PPAs
case "$OCAML_VERSION" in
3.12.1) ppa=avsm/ocaml312+opam11 ;;
4.00.1) ppa=avsm/ocaml40+opam11 ;;
4.01.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION; exit 1 ;;
esac

# Install OCaml
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra time

echo OCaml version
ocaml -version

export OPAMYES=1

if [ "$OPAM_TEST" = "1" ]; then
    # Compile OPAM using the system libraries (install them using OPAM)
    sudo apt-get install opam aspcud
    opam init
    eval `opam config env`
    opam install lwt cohttp ssl cmdliner ocamlgraph dose cudf re
    ./configure
    make prepare
    make compile
    # overwrite the previous install of OPAM with the new binary
    # and libraries
    sudo make install
    make libinstall
    # Compile and run opam-rt
    wget https://github.com/ocaml/opam-rt/archive/master.tar.gz
    tar xvfz master.tar.gz
    cd opam-rt-master
    make
    make KINDS="local git" run
else
    # Compile OPAM from sources and run the basic tests
    ./configure
    make
    make tests > tests.log 2>&1 || (tail -1000 tests.log && exit 1)
    # Let's see basic tasks works
    sudo make install
    opam init
    opam install lwt
    opam list
fi
