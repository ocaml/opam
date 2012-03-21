#/bin/bash

echo "Building P3"
ocamlbuild p3.cma p3.cmxa p3_bar.cma p3_bar.cmxa
