#!/bin/bash -ue

# Configure with the following lines in ~/.opam/config:
#
#   wrap-build-commands:   ["%{root}%/opam-init/hooks/bwrap.sh" "build"]
#   wrap-install-commands: ["%{root}%/opam-init/hooks/bwrap.sh" "install"]
#   wrap-remove-commands:  ["%{root}%/opam-init/hooks/bwrap.sh" "remove"]

if ! command -v bwrap >/dev/null; then
    echo "The 'bwrap' command was not found. Install 'bubblewrap' on your system, or" >&2
    echo "disable sandboxing in $OPAMROOT/config at your own risk." >&2
    echo "See https://github.com/projectatomic/bubblewrap for details." >&2
    exit 10
fi

ARGS=(--unshare-net --new-session)
ARGS=("${ARGS[@]}" --proc /proc --dev /dev)
ARGS=("${ARGS[@]}" --tmpfs /tmp --tmpfs /run --tmpfs /var)

add_mounts() {
    case "$1" in
        ro) B="--ro-bind";;
        rw) B="--bind";;
    esac
    for dir in "$@"; do
        [ -d "$dir" ] && ARGS=("${ARGS[@]}" "$B" "$dir" "$dir")
    done
}

add_mounts ro /usr /bin /lib /lib32 /lib64 /etc /opt /nix/store /home

COMMAND="$1"; shift
case "$COMMAND" in
    build)
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
        [ "X${PWD#$OPAM_SWITCH_PREFIX}" != "X${PWD}" ] && add_mounts rw "$PWD"
        ;;
    *)
        echo "bwrap.sh: unknown command $COMMAND, must be one of 'build', 'install' or 'remove'" >&2
        exit 2
esac

# Note: we assume $1 can be trusted, see https://github.com/projectatomic/bubblewrap/issues/259
exec bwrap "${ARGS[@]}" "$@"
