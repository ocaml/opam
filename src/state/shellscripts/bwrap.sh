#!/usr/bin/env bash

set -ue

# Used to make /* match all files (even those beginning with a '.')
shopt -s dotglob

if ! command -v bwrap >/dev/null; then
    echo "The 'bwrap' command was not found. Install 'bubblewrap' on your system, or" >&2
    echo "disable sandboxing in ${OPAMROOT:-~/.opam}/config at your own risk." >&2
    echo "See https://github.com/projectatomic/bubblewrap for bwrap details." >&2
    echo "For 'bwrap' use in opam, see the FAQ:" >&2
    echo "  https://opam.ocaml.org/doc/FAQ.html#Why-does-opam-require-bwrap" >&2
    exit 10
fi

# --new-session requires bubblewrap 0.1.7
# --die-with-parent requires bubblewrap 0.1.8
ARGS=(--unshare-net --new-session --die-with-parent)
ARGS=("${ARGS[@]}" --proc /proc --dev /dev)
ARGS=("${ARGS[@]}" --setenv TMPDIR /opam-tmp --setenv TMP /opam-tmp --setenv TEMPDIR /opam-tmp --setenv TEMP /opam-tmp)
ARGS=("${ARGS[@]}" --tmpfs /opam-tmp)
ARGS=("${ARGS[@]}" --tmpfs /run)
# NOTE: When adding a new mount-point please sync with the loop below to avoid overriding the mount point

add_mount() {
    if [ -d "$dir" ]; then
      case "$1" in
          ro) B="--ro-bind";;
          rw) B="--bind";;
          sym) B="--symlink";;
      esac
      ARGS=("${ARGS[@]}" "$B" "$2" "$3")
    fi
}

add_mounts() {
    local flag="$1"; shift
    for dir in "$@"; do
      add_mount "$flag" "$dir" "$dir"
    done
}

# Mounts the standard system paths. Maintains symlinks, to handle cases
# like `/bin` -> `usr/bin`, where `/bin/../foo` resolves to `/usr/foo`,
# not `/foo`. We handle symlinks here but not in `add_mounts` because
# system paths are pretty much guaranteed not to accidentally escape into
# off-limits directories.
add_sys_mounts() {
    for dir in "$@"; do
        if [ -L "$dir" ]; then
            local src=$(readlink -f "$dir")
            add_mount sym "$src" "$dir"
        else
            add_mounts ro "$dir"
        fi
    done
}

# NOTE: The list of moint-points to avoid is always to be sync'd with the list at the top of the file
# It is due to a limitation of bubblewrap that we have to mount the directories one by one
# See https://github.com/containers/bubblewrap/issues/413
for dir in /*; do
    case "$dir" in
    "/proc" | "/dev" | "/run" | "/opam-tmp") ;;
    "/sys") ;; # Disabled without a corresponding bind, due to security concerns
    *) add_sys_mounts "$dir";;
    esac
done

mount_linked_cache() {
  local l_cache=$1
  local cache=$(readlink -m "$l_cache")
  mkdir -p "$cache"
  add_mount rw "$l_cache" "$cache"
}

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
      mount_linked_cache "$ccache_dir"
  fi
}

add_dune_cache_mount() {
  local dune_cache=${XDG_CACHE_HOME:-$HOME/.cache}/dune
  mount_linked_cache "$dune_cache"
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

# Note: we assume $1 can be trusted, see https://github.com/projectatomic/bubblewrap/issues/259
# As of now we are compatible up to 0.1.8, '--' can be added here when we require >= 0.3.0
exec bwrap "${ARGS[@]}" "$@"
