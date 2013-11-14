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
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time

echo OCaml version
ocaml -version

# Compile OPAM and run the basic tests
export PATH=$HOME/bin:$PATH
mkdir -p $HOME/bin
./configure --prefix=$HOME
make
make tests > tests.log 2>&1 || (tail -1000 tests.log && exit 1)
make install

export OPAMYES=1
opam init
opam install lwt

# Run opam-rt
if [ "$OPAM_TEST" = "1" ]; then
    eval `opam config env` # put ocamlfind in the PATH
    opam install lwt cohttp ssl cmdliner ocamlgraph dose cudf re
    make libinstall
    wget https://github.com/ocaml/opam-rt/archive/master.tar.gz
    tar xvfz master.tar.gz
    cd opam-rt-master
    make
    make run > opam-rt.log 2>&1 || (tail -1000 opam-rt.log && exit 1)
fi
