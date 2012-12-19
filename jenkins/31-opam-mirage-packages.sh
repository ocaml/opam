#!/bin/sh -ex
PREFIX=$1
OPAM=$HOME/opam-bin/$PREFIX/bin/opam
ROOT=`echo /x/${JOB_NAME} | sed -e "s,=,_,g" -e "s/,/-/g"`
rm -rf ${ROOT}
export OPAMROOT=$ROOT
export OPAMYES=1
export OPAMVERBOSE=1
$OPAM init .
$OPAM remote add dev git://github.com/mirage/opam-repo-dev
if [ "${compiler}" != "system" ]; then
  $OPAM switch ${compiler}
fi
if [ "${packages}" = "all" ]; then
  packages=`$OPAM list -s`
fi
$OPAM install ${packages}
