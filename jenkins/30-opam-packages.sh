#!/bin/sh -ex
PREFIX=$1
OPAM=$HOME/opam-bin/$PREFIX/bin/opam
ROOT=`echo /b/${JOB_NAME} | sed -e s,=,_,g`
rm -rf ${ROOT}
$OPAM --yes --root $ROOT init $2
if [ "${compiler}" != "system" ]; then
  $OPAM --yes --root $ROOT switch ${compiler}
fi
if [ "${packages}" = "all" ]; then
  packages=`$OPAM --root $ROOT list -short`
fi
$OPAM --yes --root $ROOT install ${packages}
