sudo apt-get update
sudo apt-get install -y -qq ocaml
git config --global user.email "travis@example.com"
git config --global user.name "Travis CI"
./configure
make
make tests > tests-3121.log 2>&1 || (tail -1000 tests-3121.log && exit 1)
make distclean

sudo apt-get install -y -qq ocaml-native-compilers
./configure
make
make tests > tests-3121native.log 2>&1
make distclean

sudo add-apt-repository -y ppa:avsm/ppa-testing
sudo apt-get update -qq
sudo apt-get -y upgrade
./configure
make
sudo make install

export OPAMYES=1
opam init
opam install lwt
make tests > tests.log 2>&1
