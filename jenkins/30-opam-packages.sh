#!/bin/sh -ex
OPAM=$HOME/opam-bin/bin/opam
ROOT="`pwd`/.opam"
rm -rf .opam
$OPAM --yes --root $ROOT init .
$OPAM --yes --root $ROOT remote -add dev git://github.com/mirage/opam-repo-dev
if [ "${compiler}" != "system" ]; then
  $OPAM --yes --root $ROOT switch ${compiler}
fi
if [ "${packages}" = "all" ]; then
  packages=`$OPAM --root $ROOT list -short`
fi
$OPAM --yes --root $ROOT install ${packages}
