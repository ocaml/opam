# OPAM packages needed to build tests.
OPAM_PACKAGES="mirage-unix mirage cow mirage-fs mirari cohttp mirage-net"

sudo apt-get install -qq ocaml
make
make tests
make distclean

sudo apt-get install -qq ocaml-native-compilers
make
make tests
make distclean

echo "yes" | sudo add-apt-repository ppa:avsm/ppa-testing
sudo apt-get update -qq
sudo apt-get -y upgrade
make
make tests
