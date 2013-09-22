sudo apt-get install -qq ocaml
./configure
make
make tests
make distclean

sudo apt-get install -qq ocaml-native-compilers
./configure
make
make tests
make distclean

echo "yes" | sudo add-apt-repository ppa:avsm/ppa-testing
sudo apt-get update -qq
sudo apt-get -y upgrade
./configure
make
make tests
