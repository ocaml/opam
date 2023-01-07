#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

export OCAMLRUNPARAM=b
# XXX This should be matching up with $PREFIX in main
export PATH=~/local/bin:$PATH

OPAM12=$OPAM12CACHE/bin/opam
if [[ ! -f $OPAM12 ]]; then
  mkdir -p $OPAM12CACHE/bin

  os="Linux"
  if [ "$RUNNER_OS" = "macOS" ]; then
    os="Darwin"
  fi
  curl -sL "https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-$os" -o $OPAM12
  chmod +x $OPAM12
fi
export OPAMROOT=/tmp/opamroot
rm -rf $OPAMROOT
if [[ ! -d $OPAM12CACHE/root ]]; then
  $OPAM12 init
  cp -r /tmp/opamroot/ $OPAM12CACHE/root
else
  cp -r $OPAM12CACHE/root /tmp/opamroot
fi
set +e
$OPAM12 --version
opam --version
opam update
rcode=$?
if [ $rcode -ne 10 ]; then
  echo "[31mBad return code $rcode, should be 10[0m";
  exit $rcode
fi
opam_version=$(sed -ne 's/opam-version: *//p' $OPAMROOT/config)
if [ "$opam_version" = '"1.2"' ]; then
  echo -e "\e[31mUpgrade failed, opam-root is still 1.2\e[0m";
  cat $OPAMROOT/config
  exit 2
fi
