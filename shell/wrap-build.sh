#!/bin/sh -e
exec unshare -Umnr /bin/sh -se "$@" <<EOF
mount --bind ~ ~ # ro
mount --bind . . # rw
mount -o remount,ro,bind ~
cd . # Required to update the mount pointed to by PWD
exec "\$@"
EOF
