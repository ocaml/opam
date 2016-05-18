#!/bin/sh -e
exec unshare -Umnr /bin/sh -se "$@" <<EOF
mount --bind ~ ~ # ro
mount --bind . . # rw
mount -o remount,ro,bind ~
cd . # Sans ça $PWD ne pointe pas sur le nouveau mount!
exec "\$@"
EOF
