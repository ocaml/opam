#!/bin/bash -ue

if ! OCAMLC=$(which ocamlc); then
    echo "No OCaml compiler was found on the system" >&2
    exit 2
fi

LIBDIR=$("$OCAMLC" -where)
STUBLIBS=$(cat "$LIBDIR/ld.conf" | tr '\n' ':')

echo "Using ocaml compiler found at $OCAMLC with base lib at $LIBDIR"

bool() {
    if "$@"; then echo "true"; else echo "false"; fi
}

cat >ocaml.config <<EOF
opam-version: "1.3.0~dev4"
file-depends: ["$OCAMLC" "$(md5sum "$OCAMLC" | cut -d' ' -f1)"]
variables {
    ocaml-version: "$("$OCAMLC" -version)"
    compiler: "system"
    preinstalled: true
    ocaml-native: $(bool [ -x "$(dirname "$OCAMLC")"/ocamlopt ])
    ocaml-native-tools: $(bool [ -x "$OCAMLC".opt ])
    ocaml-native-dynlink: $(bool [ -e "$LIBDIR"/dynlink.cmxa ])
    ocaml-stublibs: "$STUBLIBS"
}
EOF
