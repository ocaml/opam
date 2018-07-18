#!/bin/bash -ue

if ! command -v bwrap >/dev/null; then
    echo "The 'bwrap' command was not found. Install 'bubblewrap' on your system, or" >&2
    echo "disable sandboxing in ${OPAMROOT:-~/.opam}/config at your own risk." >&2
    echo "See https://github.com/projectatomic/bubblewrap for bwrap details." >&2
    echo "For 'bwrap' use in opam, see the FAQ:" >&2
    echo "  https://opam.ocaml.org/doc/2.0/FAQ.html#Why-does-opam-require-bwrap" >&2
    exit 10
fi

ARGS=(--unshare-net --new-session)
ARGS=("${ARGS[@]}" --proc /proc --dev /dev)
ARGS=("${ARGS[@]}" --bind /tmp /tmp --tmpfs /run --tmpfs /var)

add_mounts() {
    case "$1" in
        ro) B="--ro-bind";;
        rw) B="--bind";;
    esac
    for dir in "$@"; do
        [ -d "$dir" ] && ARGS=("${ARGS[@]}" "$B" "$dir" "$dir")
    done
}

add_mounts ro /usr /bin /lib /lib32 /lib64 /etc /opt /nix/store /rw/usrlocal /home

# C compilers using `ccache` will write to a shared cache directory
# that remain writeable. ccache seems widespread in some Fedora systems.
add_ccache_mount() {
  if command -v ccache > /dev/null; then
      CCACHE_DIR=$HOME/.ccache
      ccache_dir_regex='cache_dir = (.*)$'
      local IFS=$'\n'
      for f in $(ccache --print-config 2>/dev/null); do
        if [[ $f =~ $ccache_dir_regex ]]; then
          CCACHE_DIR=${BASH_REMATCH[1]}
        fi
      done
      add_mounts rw $CCACHE_DIR
  fi
}

# This case-switch should remain identical between the different sandbox implems
COMMAND="$1"; shift
case "$COMMAND" in
    build)
        add_mounts ro "$OPAM_SWITCH_PREFIX"
        add_mounts rw "$PWD"
        add_ccache_mount
        ;;
    install)
        add_mounts rw "$OPAM_SWITCH_PREFIX"
        add_mounts ro "$OPAM_SWITCH_PREFIX/.opam-switch"
        add_mounts rw "$PWD"
        ;;
    remove)
        add_mounts rw "$OPAM_SWITCH_PREFIX"
        add_mounts ro "$OPAM_SWITCH_PREFIX/.opam-switch"
        [ "X${PWD#$OPAM_SWITCH_PREFIX}/.opam-switch/" != "X${PWD}" ] && add_mounts rw "$PWD"
        ;;
    *)
        echo "$0: unknown command $COMMAND, must be one of 'build', 'install' or 'remove'" >&2
        exit 2
esac

# Note: we assume $1 can be trusted, see https://github.com/projectatomic/bubblewrap/issues/259
exec bwrap "${ARGS[@]}" "$@"
