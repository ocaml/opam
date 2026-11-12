# Sandboxing
The purpose of this page is to document the rules enforced by opam
sandboxes. The target audience of this documentation is both opam and
package maintainers.

## Supported platforms
Sandboxing is not supported on all platforms. Currently, it is
supported and the default on Linux and macOS.
- On Linux, sandboxing is based on the
[`bubblewrap`](https://github.com/containers/bubblewrap) tool,
- On macOS, sandboxing is based on the `sandbox-exec` command.

## General rules
The following general rules are enforced on all supported platforms:
- No network access,
- Limited access to the file system. See below for more details,

On Linux, each command runs with a new session identifier. In particular,
commands cannot take control of the user's terminal.

## File system permissions
The sandbox can be granted read-only access (`ro`) or read-write access
(`rw`). Note that permissions in the sandbox cannot override
permissions on the host. For the sake of clarity, the following directory names
are used throughout the rest of the documentation:
- `PWD` is the current directory of the command,
- `switch` is the current switch,
- `.opam-switch` refers to the subfolder of the current switch with the
same name,
- `opam root` refers to the opam installation directory,
- `dune cache` refers to a directory used by the `dune` build system. The
location of this cache is determined by the first defined expression in the
following list: `$DUNE_CACHE_ROOT`, `$XDG_CACHE_HOME/dune`, `$HOME/.cache/dune`,
- `ccache` is a directory used by some C compilers and must remain
writable for build commands.

The sandbox is granted the following access:
| cmd \ dir | PWD   | switch | .opam-switch | dune cache | ccache | opam root |
|-----------|-------|--------|--------------|------------|--------|-----------|
| build     | rw    | ro     | ro           | rw         | rw     | ro        |
| install   | rw    | rw     | ro           | ro         | ro     | ro        |
| remove    | ro/rw | rw     | ro           | ro         | ro     | ro        |

- For remove commands, the `PWD` directory is read-write if and only if it is
a strict subdirectory of `.opam-switch`,
- The `$OPAMROOT` directory is read-only if the variable is defined.

### Specific directories for Linux
The sandbox is granted read-write access to the following directories:
- A limited view of `/dev`.
- The host directory `/proc`.
- The host directory `/tmp`.
- A fresh directory for `/opam-tmp` and `/run`. Note that fresh
directories are generated for each command.

The sandbox is granted read-only access to all top-level directories
of the host except `/dev` and `/sys`.

### Specific directories for macOS
The sandbox is granted read-write access to the following directories:
- Several temporary directories on the host: `/tmp`, `/var/tmp`, ...
- The `/dev/null` and `/dev/dtracehelper` devices.
- The host directory `/var/db/mds/`.
- The `DARWIN_USER_TEMP_DIR` directory if the variable is defined.

The sandbox is granted read-only access to all the other directories.
