#!/bin/sh -e
exec unshare -Umnr /bin/sh -se "$@" <<EOF
mount --bind ~ ~ # ro
mount --bind "$OPAM_SWITCH_PREFIX" "$OPAM_SWITCH_PREFIX" # rw
mount --bind "$OPAM_SWITCH_PREFIX/.opam-switch" "$OPAM_SWITCH_PREFIX/.opam-switch" # ro
mount -o remount,ro,bind "$OPAM_SWITCH_PREFIX/.opam-switch"
mount -o remount,ro,bind ~
cd .
exec "\$@"
EOF
