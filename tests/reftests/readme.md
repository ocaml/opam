# Simple CRAM-like test framework for opam tests.

## Features and format

### General view

A reftest looks like
```
REPO_HASH
### opam command
output line 1
output...
### <filename>
contents...
### opam command
output...
### ENV_VAR=x opam command
output...
```

- its first line is
  * the git hash of the opam repository to use, an opamroot is already
    initialised with that repo as "default"
  * or N0REP0 if there are no dependencies on opam repository. In this case, an opamroot is
    already initialised with an empty `default` repository in `./REPO`
    directory, that you need to populate
- 'opam' is automatically redirected to the correct binary
- the command prefix is `### `
- comments have the following syntax: `### : comment`

### File creation

- use `### <FILENAME>` followed by the content of the file, to create a file verbatim
- use `### <pkg:NAME.VERSION>` followed by the content of an opam file, to
  add this package to `default` repository in `./REPO`. This will also
  implicitly run `opam update default`. Look for files in `files/` directory
  and add 'extra-files:' field automatically.

- use `### <pkg:NAME.VERSION:FILENAME>` followed by the content of the file, to add this
  file as a extra-file of the given package in the `default` repository, and
  implicitly run `opam update default`. Associated opam file updated
  automatically with that file as an
  'extra-files:'

- use `### <pin:path>` followed by the content of an opam file, to have some 
  fields automatically filled to be able to pin it without lint errors

### Commands to run

- `### FOO=x BAR=y` to export variables for subsequent commands
- shell-like command handling:
  * **NO pattern expansion, shell pipes, sequences or redirections**
  * **Each `$VARIABLE` expansion is one argument unlike POSIX shell. So `rm $VARIABLE` will always remove one file, not several**
  * `FOO=x BAR=y command`
  * Arguments can be quoted: eg `"foo\"bar"`, `'foo\bar'`, but not combined
    (`foo'bar'` is not translated to `foobar`)
  * Variable expansion in arguments (`$FOO` or `${FOO}`). Undefined variables
    are left as-is
  * There is some rewrite functions available:
    * `| 'REGEXP' -> 'STR'` (can be repeated; set `STR` to `\c` to
      clear the line)
    * `| grep REGEXP`
    * `| grep -v REGEXP`
    * `| unordered` compares lines without considering their ordering
    * `| sort` sorts output
    * `| sed-cmd command` replaces full path resolved command by `command`
  * variables from command outputs: `cmd args >$ VAR`
- additional commands
  * `opam-cat file`: prints a normalised opam file
  * `json-cat file`: print a human readable opam output json file, with
    replacement of some duration and temporary files names. meant to be used
    on opam generated json files.
  * `opam-cache installed <switch> [nvs]`: print the content of installed
    packages cache for switch `<switch>`. If `[nvs]` is specified, filter over
    these package names or packages.
  * `opam-cache repo [nvs]`: print the content of repository cache. If
    `[nvs]` is specified, filter over these package names or packages.

- if you need more shell power, create a script using <FILENAME> then run it.
  Or just use `sh -c`... but beware for compatibility.

The opam roots are generated using dynamically generated dune rules (see
gen.ml and dune.inc), then the tests are run using this script.
