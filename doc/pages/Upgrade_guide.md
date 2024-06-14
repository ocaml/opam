# Quick Upgrade Guide from opam 1.2 to opam 2.0

This guide is not a complete list of changes, but it highlights changes that
users of opam 1.2 should know about, as well as some important new features.

## Command-line

### What You Need to Know

Some commands have changed syntax:

- [`opam switch`](man/opam-switch.html): `create`
  must be specified to create a new switch. You should then specify either
  `--empty` or a base compiler package, _e.g.,_ use `opam switch create 4.06
  ocaml-base-compiler.4.06.0` to create a new switch named `4.06`. Just `opam
  switch create 4.06.0` also still works in most cases.

- [`opam repository`](man/opam-repository.html)
  (or `opam remote`): repositories are still configured globally but are now
  selected for each switch. So by default `opam repository add` will only affect
  the current switch. You can change the defaults with `--set-defaults` and
  choose the repositories at switch creation time with `opam switch create
  --repositories REPOS`

- [`opam list`](man/opam-list.html) and [`opam show`](man/opam-show.html) have
  been largely reworked to offer more options

- options to build tests and documentation are now respectively `--with-test`
  and `--with-doc`. They only apply to packages listed on the command line


The `opam-admin` tool, for repository operations, is now built into opam, so use
the [`opam admin`](man/opam-admin.html) command instead.

Opam now comes with [a solver built-in](https://github.com/AltGr/ocaml-mccs),
which means that installing `aspcud` alongside opam is no longer required.

Pinning to a version-controlled directory will now only use what is committed by
default (opam 1.2 had a "mixed mode" where it would use the current versions of
files, but only the version-controlled ones. This was good most of the time, but
could become puzzling _e.g.,_ when opam didn't see an added source file). The
option
[`--working-dir`](man/opam-install.html#lbAH) can
be used to temporarily make opam fetch uncommitted changes (see also
[`--inplace-build`](man/opam-install.html#lbAG)),  and [`--assume-built`](man/opam-install.html#lbAG)
to run only installation instructions, assuming that build has been done locally.
Upon pinning, opam 2.0 will also select the current branch by default if
unspecified, so that later running `git checkout BRANCH` in the source tree
doesn't affect the pinned package.

### New Features

- __new command aliases__: [`opam var`](man/opam-var.html),
  [`opam exec`](man/opam-exec.html), [`opam env`](man/opam-env.html) can be used
  in place of the corresponding [`opam config`](man/opam-config.html)
  subcommands

- __new command__: [`opam clean`](man/opam-clean.html) to clear caches, logs, etc.

- __local switches__: use _e.g.,_
  [`opam switch create .`](man/opam-switch.html#lbAE) to create a switch in the
  current directory. The switch contents will be in a `_opam` directory, which
  can be safely removed once done. The switch path can then be used as a handle
  to refer to the switch. Additionally, the above command will install any
  packages which definitions are found in the selected directory into the local
  switch.

- __automatic pinning__: use _e.g.,_ [`opam install .`](man/opam-install.html#lbAD)
  to pin and install any packages found in the current directory. `opam install
  . --deps-only` can also be used to prepare a switch for working with a source
  tree. This extension also concerns the `remove`, `upgrade`, `reinstall`, and
  `show` commands, and specifying the path to a specific `opam` file is also
  supported.

- __archive caching__: opam now uses a much better caching mechanism, both locally
  and on the opam-repository. In particular, upstream repositories being down
  should no longer prevent package installation (even for older or removed
  packages). Git repositories are also cached.

- __better error mitigation__, messages, and recovery.
  [`opam install --restore`](man/opam-install.html#lbAF) can be used to retry
  after a failed operation.

- a plugin, _e.g.,_
  [`opam-depext`](https://github.com/ocaml/opam-depext/tree/2.0), will now be
  available from all switches once installed in one.

- [`opam install --destdir`](man/opam-install.html#lbAF) can be used to copy
  build artefacts of given packages to an external prefix

- __sandboxing__: on Linux and MacOS, all package commands will now be sandboxed by
  default. The [`bubblewrap`](https://github.com/projectatomic/bubblewrap) tool
  is now required to this end on Linux, and the `sandbox_exec` command is used
  on MacOS. See [`faq entry`](FAQ.html#Why-does-opam-require-bwrap) for more details.

## File Formats

### What You Need to Know

> #### Repositories and Migration
>
> **Repositories for 1.2 and 2.0 have a different format**, but everything
> should be transparent unless you publish packages:
>
> - [**The main repository**](https://github.com/ocaml/opam-repository/tree/master)
>   is now in format **2.0**. This means the `master` branch, and the
>   contents of `https://opam.ocaml.org/packages`.
> - [**There is a 1.2 branch**](https://github.com/ocaml/opam-repository/tree/1.2)
>   that is served at `opam.ocaml.org/1.2.2`.
>
> When publishing packages, remember that:
>
> - Packages in **2.0 format** must be published to the `2.0.0` branch — e.g.,
>   using the new
>   [opam-publish.2.0](https://github.com/ocaml/opam-publish/tree/2.0). They
>   will **only** be available to opam 2.0 users.
> - Packages in **1.2 format** are no longer accepted, expect for relevant fixes.
>   In that case they must be published to `1.2` branch.
>
> - [`opam-publish.2.0.0`](https://github.com/ocaml/opam-publish/tree/2.0) has
>   a fully revamped interface and many new features, like filing a single PR
>   for multiple packages. It files pull requests in 2.0 format only to master
>   branch of the repository. The new version of
>   [`dune-release.1.0.1`](https://github.com/samoht/dune-release/tree/master)
>   handles the new format.
>
> - It is also advised to keep in-source `opam` files in 1.2 format until that
> date, so as not to break uses of `opam pin add --dev-repo` by opam 1.2 users.
>
> - The small `opam-package-upgrade` plugin can be used to upgrade single 1.2
> opam files to 2.0 format. You can also use API to upgrade you `opam` files,
> using
> [`OpamFormatUpgrade.opam_file`](https://opam.ocaml.org/doc/api/opam-state/OpamFormatUpgrade/#val-opam_file),
> available in package `opam-state`.
>
> - More advice for package maintainers and custom repositories in this [blog
> post](https://opam.ocaml.org/blog/opam-2-0-0-repo-upgrade-roadmap/#Advice-for-package-maintainers).

- Compiler definition files: these no longer exist, as compilers have been
  replaced by normal package definitions (that should have
  [`flags: compiler`](Manual.html#opamflag-compiler))

- The base syntax of `opam` files didn't change, but:
  - Compilers now being packages, _e.g.,_ the field `available:
    [ ocaml-version >= "4.00.1" ]` is now encoded as a dependency towards the
    `ocaml` package with `depends: [ "ocaml" {>= "4.00.1"} ]`. The
    `ocaml-version` variable is no longer defined.
  - Extra dependencies needed for test and documentation must now be flagged
    with resp.
    [`with-test`](Manual.html#opamfield-depends)
    and
    [`with-doc`](Manual.html#opamfield-depends).
    Fields `build-test:` and `build-doc:` are deprecated in favour of filters on
    [`build:`](Manual.html#opamfield-build)
    instructions, and there is a new
    [`run-test:`](Manual.html#opamfield-run-test)
    field
  - The format of the
    [`depexts:`](Manual.html#opamfield-depexts)
    field is changed
  - the host system is now polled through the `arch`, `os`, `os-distribution`,
    and `os-version` variables. `os` may now take the value `macos` instead of
    `darwin`.
  - [`depopts: [ "foo" >= "v0" ]`](Manual.html#opamfield-depopts) now means that
    the optional dependency is only active for the corresponding versions. There
    is no implicit conflict with `"foo" < "v0"`

- URLs must now be unambiguous in files, _e.g.,_ you must use
  `git+https://github.com/owner/pkg.git` rather than
  `https://github.com/owner/pkg.git`. The latter will still be understood as
  `git` and rewritten to the former if used from the command line.

- Any specified
  [`patches:`](Manual.html#opamfield-patches)
  must now apply with `patch -p1` and use unified, rather than context, diffs.

- `opam switch export/import` format has been changed (but files in the 1.2
  format can still be read).

- __the conversion from the 1.2 format is done internally and automatic, both
  for repositories and when pinning.__ Be careful, however, not to submit 2.0
  format files if they are to be used by opam 1.2.

### New Features

`opam` files have been extended in a lot of ways:

- More expressive [dependencies](/blog/opam-extended-dependencies/)

- New fields:
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

- The `descr` file is now preferably replaced by
  [`synopsis:`](Manual.html#opamfield-synopsis) and
  [`description:`](Manual.html#opamfield-description) fields in the `opam` file
  (and strings can be put between `"""` to avoid escaping issues).

- The `url` file can be replaced by a section of the form
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
