#!/usr/bin/env bash
set -ue

POL='(version 1)(allow default)(deny network*)(deny file-write*)'
POL="$POL"'(allow network* (remote unix))'
POL="$POL"'(allow file-write* (literal "/dev/null") (literal "/dev/dtracehelper"))'

add_mounts() {
    local DIR="$(cd "$2" && pwd -P)"
    case "$1" in
        ro) POL="$POL"'(deny file-write* (subpath "'"$DIR"'"))';;
        rw) POL="$POL"'(allow file-write* (subpath "'"$DIR"'"))';;
    esac
}

add_mounts rw "${TMPDIR:-/tmp}"

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
        if [ "X${PWD#$OPAM_SWITCH_PREFIX/.opam-switch}" != "X${PWD}" ]; then
          add_mounts rw "$PWD"
        fi
        ;;
    *)
        echo "$0: unknown command $COMMAND, must be one of 'build', 'install' or 'remove'" >&2
        exit 2
esac

exec sandbox-exec -p "$POL" "$@"
