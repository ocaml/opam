#!/bin/sh

set -euo pipefail

cd src/core/cmdliner
rm -rf *.ml *.mli dune

git clone https://github.com/dbuenzli/cmdliner tmp-vendor
git -C tmp-vendor switch --detach v1.3.0

mv tmp-vendor/src/*.{ml,mli} .
rm -rf tmp-vendor

mv cmdliner.ml opamCmdliner.ml
mv cmdliner.mli opamCmdliner.mli
rm cmdliner_exit.ml{,i}

cat > dune << EOF
(library
 (name opamCmdliner)
 (public_name opam-core.cmdliner)
 (flags :standard -w -27-32-35-50))
EOF
