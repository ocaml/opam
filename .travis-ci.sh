sudo apt-get install -qq ocaml
./configure
make
make distclean

sudo apt-get install -qq ocaml-native-compilers
./configure
make
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
make tests
