#!/bin/bash
# This script is only used on macOS, where /bin/bash is both guaranteed to exist and
# and to be for the native architecture, which is why /usr/bin/env bash is not used.
# See https://github.com/ocaml/opam/issues/5450
set -ue

POL='(version 1)(allow default)(deny network*)(deny file-write*)'
POL="$POL"'(allow network* (remote unix))'
POL="$POL"'(allow file-write* (literal "/dev/null") (literal "/dev/dtracehelper"))'

add_mounts() {
    if [ -d "$2" ]; then
      local DIR="$(cd "$2" && pwd -P)"
      case "$1" in
          ro) POL="$POL"'(deny file-write* (subpath "'"$DIR"'"))';;
          rw) POL="$POL"'(allow file-write* (subpath "'"$DIR"'"))';;
      esac
    fi
}

if [ -z ${TMPDIR+x} ]; then
  # Others applications obtain the per-user temporary
  # directory differently; the latter should be made readable/writable
  # too and getconf seems to be a robust way to get it
  if [ -z /usr/bin/getconf ]; then
    TMPDIR=$(getconf DARWIN_USER_TEMP_DIR)
    add_mounts rw "$TMPDIR"
    export TMPDIR
  fi
else
  add_mounts rw "$TMPDIR"
fi

# C compilers using `ccache` will write to a shared cache directory
# that remain writeable. ccache seems widespread in some Fedora systems.
add_ccache_mount() {
  if command -v ccache > /dev/null; then
      ccache_dir_regex='cache_dir = (.*)$'
      local IFS=$'\n'
      for f in $(ccache -p 2>/dev/null); do
        if [[ $f =~ $ccache_dir_regex ]]; then
          ccache_dir=${BASH_REMATCH[1]}
          break
        fi
      done
      CCACHE_DIR=${CCACHE_DIR-$HOME/.ccache}
      ccache_dir=${ccache_dir-$CCACHE_DIR}
      add_mounts rw "$ccache_dir"
  fi
}

add_dune_cache_mount() {
  local dune_cache=${XDG_CACHE_HOME:-$HOME/.cache}/dune
  mkdir -p "${dune_cache}"
  add_mounts rw "$dune_cache"
}

# When using opam variable that must be defined at action time, add them also
# at init check in OpamAuxCommands.check_and_revert_sandboxing (like
# OPAM_SWITCH_PREFIX).
# This case-switch should remain identical between the different sandbox implems
COMMAND="$1"; shift
case "$COMMAND" in
    build)
        add_mounts ro "$OPAM_SWITCH_PREFIX"
        add_mounts rw "$PWD"
        add_ccache_mount
        add_dune_cache_mount
        ;;
    install)
        add_mounts rw "$OPAM_SWITCH_PREFIX"
        add_mounts ro "$OPAM_SWITCH_PREFIX/.opam-switch"
        add_mounts rw "$PWD"
        ;;
    remove)
        add_mounts rw "$OPAM_SWITCH_PREFIX"
        add_mounts ro "$OPAM_SWITCH_PREFIX/.opam-switch"
        if [ "X${PWD#$OPAM_SWITCH_PREFIX/.opam-switch/}" != "X${PWD}" ]; then
          add_mounts rw "$PWD"
        fi
        ;;
    *)
        echo "$0: unknown command $COMMAND, must be one of 'build', 'install' or 'remove'" >&2
        exit 2
esac

if ! command -v "$1" >/dev/null; then
    echo "[ERROR] Command not found: $1" >&2
    exit 10
fi

exec sandbox-exec -p "$POL" "$@"
