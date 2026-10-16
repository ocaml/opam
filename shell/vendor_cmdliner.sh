#!/bin/bash

set -euo pipefail

cd src/core/cmdliner
rm -rf *.ml *.mli dune tool

trap "rm -rf tmp-vendor" EXIT
git clone https://github.com/dbuenzli/cmdliner tmp-vendor
git -C tmp-vendor switch --detach v2.1.1

mv tmp-vendor/src/*.{ml,mli} .
mkdir -p tool
mv tmp-vendor/src/tool/*.ml tool/

for f in patches/*.patch; do
  git apply "$f"
done
