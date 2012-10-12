#!/bin/sh
BASE="ocaml ocaml-compiler-libs"

add () { BASE="${BASE} $1"; }

# alt-ergo
add autoconf
# cairo
add libcairo2-dev
# ocaml-sdl
add libsdl-dev
# dbm
add libgdbm-dev
# lablgtk2
add libgtk2.0-dev
# lwt-zmq
add libzmq-dev
# postgresql-ocaml
add libpq-dev
# camlbz2
add libbz2-dev
# imagemagick
add "libgraphicsmagick1-dev libmagickcore-dev"
# sqlite3
add libsqlite3-dev
# ocaml-glpk
add libglpk-dev
# lablgl
add "mesa-common-dev libglu1-mesa-dev freeglut3-dev"
# ocamlgsl
add gawk
# mlgmp
add libmpfr-dev
# ocaml-lua
add liblua5.1-0-dev
# ocurl
add libcurl4-gnutls-dev
# gpr
add libgsl0-dev
# planets
add tcl8.5-dev tk8.5-dev
# ocaml-mysql
add libmysqlclient-dev
# odepack
add gfortran-4.7
# fftw-ocaml
add libfftw3-dev
# ocaml-taglib
add libtag1-dev
sudo apt-get -y install ${BASE}

