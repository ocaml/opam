#!/bin/bash

set -xue

. .github/scripts/main/preamble.sh

rm -f $OPAM_LOCAL/bin/opam-bootstrap
mkdir -p $OPAM_LOCAL/bin/

os=$( (uname -s || echo unknown) | awk '{print tolower($0)}')
if [ "$os" = "darwin" ] ; then
  os=macos
fi

curl -sL -o $OPAM_LOCAL/bin/opam-bootstrap \
  "https://github.com/ocaml/opam/releases/download/$OPAMBSVERSION/opam-$OPAMBSVERSION-$(uname -m)-$os"
cp -f $OPAM_LOCAL/bin/opam-bootstrap $OPAM_LOCAL/bin/opam
chmod a+x $OPAM_LOCAL/bin/opam

opam --version

if [[ -d $OPAMBSROOT ]] ; then
  init-bootstrap || { rm -rf $OPAMBSROOT; init-bootstrap; }
else
  init-bootstrap
fi
