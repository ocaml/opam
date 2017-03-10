#!/bin/sh -e

USERID=$(id -u)
GROUPID=$(id -g)

# Run in separate user, network and mount namespace
# -r maps USERID to root, which is needed for doing the mounts, then we map it
# back to USERID in a child namespace for running the child process
exec unshare -Umnr /bin/sh -es "$@" <<EOF

# Remount ~ read-only, OPAM_SWITCH_PREFIX read-write,
# OPAM_SWITCH_PREFIX/.opam-switch read-only, and . read-write when needed
mount --bind ~ ~ # ro
mount --bind "$OPAM_SWITCH_PREFIX" "$OPAM_SWITCH_PREFIX" # rw
mount --bind "$OPAM_SWITCH_PREFIX/.opam-switch" "$OPAM_SWITCH_PREFIX/.opam-switch" # ro
if [ "X${PWD#$OPAM_SWITCH_PREFIX}" != "X$PWD" ]; then
  mount --bind . . # rw: packages may run e.g. ./configure
fi
mount -o remount,ro,bind "$OPAM_SWITCH_PREFIX/.opam-switch"
mount -o remount,ro,bind ~
cd . # Required to update the mount pointed to by PWD

# Now restore UID and GID in a child namespace

# Setup synchronisation channels
tmpdown=\$(mktemp -u)
tmpup=\$(mktemp -u)
mkfifo -m 600 "\$tmpdown"
mkfifo -m 600 "\$tmpup"
trap "rm -f \$tmpdown \$tmpup" EXIT

# Run a shell in a child user namespace
unshare -U /bin/sh -es "\$@" <\$tmpdown 4>\$tmpup &
pid=\$!

exec 3>\$tmpdown 4<\$tmpup

# Synchronise to make sure the child ns is ready
echo 'echo >&4' >&3; read sync <&4

# Setup the uid and gid map (reverse of what 'unshare -r' did) to get back to
# the original uid
echo "$USERID 0 1" >/proc/\$pid/uid_map
echo "$GROUPID 0 1" >/proc/\$pid/gid_map

# Exec the original command that was passed through argv to the child shell
echo 'exec "\$@"' >&3

# Cleanup
exec 3>&- 4<&-
wait \$pid

EOF
