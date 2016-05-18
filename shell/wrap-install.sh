#!/bin/sh -e
exec unshare -Umnr /bin/sh -se "$@" <<EOF
mount --bind ~ ~ # ro
mount --bind "$OPAM_SWITCH_PREFIX" "$OPAM_SWITCH_PREFIX" # rw
mount --bind "$OPAM_SWITCH_PREFIX/.opam-switch" "$OPAM_SWITCH_PREFIX/.opam-switch" # ro
mount --bind . . # rw: many packages need to write to their build dir on install, if just to update some logs
mount -o remount,ro,bind "$OPAM_SWITCH_PREFIX/.opam-switch"
mount -o remount,ro,bind ~
cd . # required
exec "\$@"
EOF
