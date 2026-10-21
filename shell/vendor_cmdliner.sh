#!/bin/sh

set -euo pipefail

cd src/core/cmdliner
rm -rf *.ml *.mli dune tool

echo "State of src/core/cmdliner before applying the new release:"
ls -alR

echo ----

trap "rm -rf tmp-vendor" EXIT
git clone https://github.com/dbuenzli/cmdliner tmp-vendor
git -C tmp-vendor switch --detach v2.1.1

mv tmp-vendor/src/*.ml tmp-vendor/src/*.mli .
mkdir -p tool
mv tmp-vendor/src/tool/*.ml tool/
rm -rf tmp-vendor

# order glob result (important when applying patches)
LC_COLLATE=C

for f in patches/*.patch; do
  git apply "$f"
done
