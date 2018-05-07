#!/bin/sh -ue

POL='(version 1)(allow default)(deny network*)(deny file-write*)'
POL="$POL"'(allow file-write* (literal "/dev/null"))'

add_mounts() {
    local DIR="$(cd "$2" && pwd -P)"
    case "$1" in
        ro) POL="$POL"'(deny file-write* (subpath "'"$DIR"'"))';;
        rw) POL="$POL"'(allow file-write* (subpath "'"$DIR"'"))';;
    esac
}

add_mounts rw "${TMPDIR:-/tmp}"

# This case-switch should remain identical between the different sandbox implems
COMMAND="$1"; shift
case "$COMMAND" in
    build)
        add_mounts ro "$OPAM_SWITCH_PREFIX"
        add_mounts rw "$PWD"
        ;;
    install)
        add_mounts rw "$OPAM_SWITCH_PREFIX"
        add_mounts ro "$OPAM_SWITCH_PREFIX/.opam-switch"
        add_mounts rw "$PWD"
        ;;
    remove)
        add_mounts rw "$OPAM_SWITCH_PREFIX"
        add_mounts ro "$OPAM_SWITCH_PREFIX/.opam-switch"
        [ "X${PWD#$OPAM_SWITCH_PREFIX/.opam-switch}" != "X${PWD}" ] && add_mounts rw "$PWD"
        ;;
    *)
        echo "$0: unknown command $COMMAND, must be one of 'build', 'install' or 'remove'" >&2
        exit 2
esac

exec sandbox-exec -p "$POL" "$@"
