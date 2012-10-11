#!/bin/sh
BASE="ocaml ocaml-compiler-libs"

# alt-ergo
BASE="${BASE} autoconf"
# cairo
BASE="${BASE} libcairo2-dev"
# dbm
BASE="${BASE} libgdbm-dev"
# lablgtk2
BASE="${BASE} libgtk2.0-dev"
# lwt-zmq
BASE="${BASE} libzmq-dev"
# postgresql-ocaml
BASE="${BASE} libpq-dev"
# camlbz2
BASE="${BASE} libbz2-dev"
# imagemagick
BASE="${BASE} libgraphicsmagick1-dev libmagickcore-dev"
# sqlite3
BASE="${BASE} libsqlite3-dev"
# ocaml-glpk
BASE="${BASE} libglpk-dev"
# lablgl
BASE="${BASE} mesa-common-dev"
# ocamlgsl
BASE="${BASE} gawk"
# ocaml-lua
BASE="${BASE} liblua5.1-0-dev
# ocurl
BASE="${BASE} libcurl4-gnutls-dev"
# planets
BASE="${BASE} tcl8.5-dev tk8.5-dev"
# ocaml-mysql
BASE="${BASE} libmysqlclient-dev"
sudo apt-get -y install ${BASE}
