#!/bin/bash -uex

COMMAND=$1; shift
ID=$1; shift

if [ -z "$ID" ]; then
    if [ $COMMAND = wrap ]; then exec "$@"
    else exit 0
    fi
fi

CACHE_DIR=~/.cache/opam-bin-cache/$ID

case $COMMAND in
    restore)
        NAME=$1; shift
        if [ -d "$CACHE_DIR" ]; then
            rm -f "$NAME.install"
            cp -aT "$CACHE_DIR" "$OPAM_SWITCH_PREFIX"
        else exit 0
        fi;;
    wrap)
        if [ -d "$CACHE_DIR" ]; then exit 0
        else exec "$@"
        fi;;
    store)
        if [ -d "$CACHE_DIR" ]; then exit 0
        else
            for fl in "$@"; do
                echo "ARR=$fl"
                while IFS= read -d ' ' f; do
                    echo "STORING FILE: $f"
                    if [ -d "$OPAM_SWITCH_PREFIX/$f" ]; then mkdir -p "$CACHE_DIR/$f"
                    else
                        mkdir -p "$(dirname "$CACHE_DIR/$f")"
                        cp -aT "$OPAM_SWITCH_PREFIX/$f" "$CACHE_DIR/$f"
                    fi
                done <<<"$fl "
            done
        fi;;
    *)
        echo "Invalid command '$COMMAND'. Valid commands:" >&2
        echo "    restore ID NAME" >&2
        echo "    wrap ID COMMAND [ARGS]..." >&2
        echo "    store ID [FILES]..." >&2
        exit 2
esac

# Use as:
# pre-install-commands: ["opam-bin-cache.sh" "restore" build-id name] {?build-id}
# wrap-build-commands: ["opam-bin-cache.sh" "wrap" build-id] {?build-id}
# wrap-install-commands: ["opam-bin-cache.sh" "wrap" build-id] {?build-id}
# post-install-commands: ["opam-bin-cache.sh" "store" build-id installed-files] {?build-id & error-code = 0}
