# Quick upgrade guide from opam 1.2 to opam 2.0

This guide is not a complete list of changes, but it highlights changes that
users of opam 1.2 should know about, as well as some important new features.

## Command-line

### What you need to be aware of

Some commands have changed syntax:

- [`opam switch`](man/opam-switch.html): `create`
  must be specified to create a new switch. You should then specify either
  `--empty` or a base compiler package, _e.g._ use `opam switch create 4.06
  ocaml-base-compiler.4.06.0` to create a new switch named `4.06`. Just `opam
  switch create 4.06.0` also still works in most cases.

- [`opam repository`](man/opam-repository.html)
  (or `opam remote`): repositories are still configured globally, but are now
  selected for each switch. So by default `opam repository add` will only affect
  the current switch. You can change the defaults with `--set-defaults`, and
  choose the repositories at switch creation time with `opam switch create
  --repositories REPOS`

- [`opam list`](man/opam-list.html) and [`opam show`](man/opam-show.html) have
  been largely reworked to offer more options

- options to build tests and documentation are now respectively `--with-test`
  and `--with-doc`. They only apply to packages listed on the command-line


The `opam-admin` tool, for repository operations, is now built into opam, use
the [`opam admin`](man/opam-admin.html) command instead.

Opam now comes with [a solver built-in](https://github.com/AltGr/ocaml-mccs),
which means that installing `aspcud` alongside opam is no longer required.

Pinning to a version-controlled directory will now only use what is committed by
default (opam 1.2 had a "mixed mode" where it would use the current versions of
files, but only the version-controlled ones. This was good most of the time, but
could become puzzling _e.g._ when opam didn't see an added source file). The
option
[`--working-dir`](man/opam-install.html#lbAH) can
be used to temporarily make opam fetch uncommitted changes (see also
[`--inplace-build`](man/opam-install.html#lbAG)).
Upon pinning, opam 2.0 will also select the current branch by default if
unspecified, so that later running `git checkout BRANCH` in the source tree
doesn't affect the pinned package.

### New features

- __new command aliases__: [`opam var`](man/opam-var.html),
  [`opam exec`](man/opam-exec.html), [`opam env`](man/opam-env.html) can be used
  in place of the corresponding [`opam config`](man/opam-config.html)
  subcommands

- __new command__: [`opam clean`](man/opam-clean.html) to clear caches, logs, etc.

- __local switches__: use _e.g._
  [`opam switch create .`](man/opam-switch.html#lbAE) to create a switch in the
  current directory. The switch contents will be in a `_opam` directory, which
  can be safely removed once done. The switch path can then be used as a handle
  to refer to the switch. Additionally, the above command will install any
  packages which definitions are found in the selected directory into the local
  switch.

- __automatic pinning__: use _e.g._ [`opam install .`](man/opam-install.html#lbAD)
  to pin and install any packages found in the current directory. `opam install
  . --deps-only` can also be used to prepare a switch for working with a source
  tree. This extension also concerns the `remove`, `upgrade`, `reinstall` and
  `show` commands, and specifying the path to a specific `opam` file is also
  supported.

- __archive caching__: opam now uses a much better caching mechanism, both locally
  and on the opam-repository. In particular, upstream repositories being down
  should no longer prevent package installation (even for older or removed
  packages). Git repositories are also cached.

- __better error mitigation__, messages and recovery.
  [`opam install --restore`](man/opam-install.html#lbAF) can be used to retry
  after a failed operation.

- a plugin, _e.g._
  [`opam-depext`](https://github.com/ocaml/opam-depext/tree/2.0), will now be
  available from all switches once installed in one.

- [`opam install --destdir`](man/opam-install.html#lbAF) can be used to copy
  build artefacts of given packages to an external prefix


## File formats

### What you need to be aware of

> #### Repositories and migration
>
> Repositories for 1.2 and 2.0 have a different format.
>
> - the `master` branch of the
>   [main repository](https://github.com/ocaml/opam-repository/tree/master)
>   remains in 1.2 format for now
>
> - opam can do the conversion (1.2 → 2.0) on the fly
>
> - the repository has a
>   [`2.0.0` branch](https://github.com/ocaml/opam-repository/tree/2.0.0) that has
>   all patches to the `master` branch ported automatically
>
> - opam 2.0 will be redirected to the repository at https://opam.ocaml.org/2.0,
>   which is based on the `2.0.0` branch, automatically
>
> - you can use
>   [`opam admin upgrade [--mirror=BASEURL]`](man/opam-admin-upgrade.html)
>   to manually upgrade a repository to the 2.0 format

- compiler definition files: these no longer exist, as compilers have been
  replaced by normal package definitions (that should have
  [`flags: compiler`](Manual.html#opamflag-compiler))

- the base syntax of `opam` files didn't change, but:
  - compilers now being packages, _e.g._ the field `available:
    [ ocaml-version >= "4.00.1" ]` is now encoded as a dependency towards the
    `ocaml` package with `depends: [ "ocaml" {>= "4.00.1"} ]`. The
    `ocaml-version` variable is no longer defined.
  - extra dependencies needed for test and documentation must now be flagged
    with resp.
    [`with-test`](Manual.html#opamfield-depends)
    and
    [`with-doc`](Manual.html#opamfield-depends).
    Fields `build-test:` and `build-doc:` are deprecated in favour of filters on
    [`build:`](Manual.html#opamfield-build)
    instructions, and there is a new
    [`run-test:`](Manual.html#opamfield-run-test)
    field
  - the format of the
    [`depexts:`](Manual.html#opamfield-depexts)
    field is changed
  - the host system is now polled through the `arch`, `os`, `os-distribution`
    and `os-version` variables. `os` may now take the value `macos` instead of
    `darwin`.
  - [`depopts: [ "foo" >= "v0" ]`](Manual.html#opamfield-depopts) now means that
    the optional dependency is only active for the corresponding versions, there
    is no implicit conflict with `"foo" < "v0"`

- URLs must now be non-ambiguous in files, _e.g._ you must use
  `git+https://github.com/owner/pkg.git` rather than
  `https://github.com/owner/pkg.git`. The latter will still be understood as
  `git` and rewritten to the former if used from the command-line.

- Any specified
  [`patches:`](Manual.html#opamfield-patches)
  must now apply with `patch -p1`

- `opam switch export/import` format has been changed (but files in the 1.2
  format can still be read)

- __the conversion from the 1.2 format is done internally and automatic, both
  for repositories and when pinning.__ Be careful, however, not to submit 2.0
  format files if they are to be used by opam 1.2, or published to the main
  repository before it makes the transition.

### New features

`opam` files have been extended in a lot of ways:

- more expressive [dependencies](/blog/opam-extended-dependencies/)

- new fields:
  - [`pin-depends:`](Manual.html#opamfield-depexts)
  - [`run-test:`](Manual.html#opamfield-run-test)
  - [`setenv:`](Manual.html#opamfield-setenv)
  - [`conflict-class`](Manual.html#opamfield-conflict-class)
  - [`extra-source:`](Manual.html#opamsection-extra-sources)
  - [`extra-files:`](Manual.html#opamfield-extra-files)

- opam now tracks installed files, so the `remove:` field can now in general be
  omitted. Specify
  [`flags: light-uninstall`](Manual.html#opamflag-light-uninstall) if you do
  need `remove:` instructions, but these are not required to be run from the
  source tree of the package.

- the `descr` file is now preferably replaced by
  [`synopsis:`](Manual.html#opamfield-synopsis) and
  [`description:`](Manual.html#opamfield-description) fields in the `opam` file
  (and strings can be put between `"""` to avoid escaping issues)

- the `url` file can be replaced by a section of the form
  [`url { src: "URL" checksum: "CKSUM" }`](Manual.html#opamsection-url). With
  the above, this allows single-file package definitions

- [checksums](Manual.html#Checksums) now accept SHA256 and SHA512 besides MD5.
  Use the strings `"md5=HEX"`, `"sha256=HEX"`, `"sha512=HEX"`.



For more details on the new opam, see:
- [the manual](Manual.html)
- `opam COMMAND --help`
- [the changelog](https://github.com/ocaml/opam/blob/master/CHANGES)
- [the blog](/blog/opam-2-0-0-rc/) for announcements of some of the new features
- [the tracker](https://github.com/ocaml/opam/issues)
