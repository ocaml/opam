#!/bin/sh -ue

if ! OCAMLC=$(command -v ocamlc); then
    echo "No OCaml compiler was found on the system" >&2
    exit 2
fi

if [ $($OCAMLC -config | sed -ne "s/os_type: //p" | tr -d '\r') = Win32 ] ; then
  OCAMLC_FILE=$(echo $OCAMLC| cygpath -w -f - | sed -e 's/\\/\\\\/g')
  LIBDIR=$("$OCAMLC" -where | tr -d '\r' | cygpath -f -)
else
  OCAMLC_FILE=$OCAMLC
  LIBDIR=$("$OCAMLC" -where)
fi

STUBLIBS=$(cat "$LIBDIR/ld.conf" | tr -d '\r' | tr '\n' ':' | sed -e 's/\\/\\\\/g')

echo "Using ocaml compiler found at $OCAMLC with base lib at $LIBDIR"

bool() {
    if "$@"; then echo "true"; else echo "false"; fi
}

cat >ocaml.config <<EOF
opam-version: "1.3.0~dev4"
file-depends: ["$OCAMLC_FILE" "$(md5sum "$OCAMLC" | cut -d' ' -f1)"]
variables {
    compiler: "system"
    native: $(bool [ -x "$(dirname "$OCAMLC")"/ocamlopt ])
    native-tools: $(bool [ -x "$OCAMLC".opt ])
    native-dynlink: $(bool [ -e "$LIBDIR"/dynlink.cmxa ])
    stublibs: "$STUBLIBS"
}
EOF
