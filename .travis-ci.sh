sudo apt-get install -qq ocaml
./configure
make
make tests > tests-3121.log 2>&1 || (tail -1000 tests-3121.log && exit 1)
make distclean

sudo apt-get install -qq ocaml-native-compilers
./configure
make
make tests > tests-3121native.log 2>&1
make distclean

echo "yes" | sudo add-apt-repository ppa:avsm/ppa-testing
sudo apt-get update -qq
sudo apt-get -y upgrade
./configure
make
sudo make install

export OPAMYES=1
opam init
opam install lwt
make tests > tests.log 2>&1
