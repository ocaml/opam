# The OPAM manual

This manual gathers reference information on OPAM and its file formats. It is
primarily of use for packagers, package maintainers and repository maintainers.

* For simple usage of OPAM, see the [Usage](Usage.html) page, and the
  comprehensive built-in documentation `opam [command] --help`.
* For a gentler introduction to packaging, see the
  [Packaging guide](Packaging.html)
* For a more complete specification, and if you are interested in more details
  on the layout of `~/.opam`, you may want to check the
  [OPAM Developer's Manual](http://opam.ocaml.org/doc/manual/dev-manual.html)

This version of the manual documents the version `1.2` of the file formats, with
some `1.2.1` experimental extensions mentionned.


## General file format

### General syntax

The OPAM file formats share a common base syntax. The files are utf8 encoded
and made of a list of fields in the form `<field-name>: <value>`.

Available field names are specific to each specific file format, and each of
them only allows values of a certain format.

Base values can be literal booleans, integers or strings, identifiers, and
operators. Strings allow the escapes `\\`, `\n`, `\r`, `\b`, `\t`, as well as
three-digit decimal and two-digit hexadecimal character codes (`\NNN` and
`\xNN`), and escaped newlines. Lists must be enclosed in square brackets unless
they contain a single element. Values can be followed by an argument in braces.
Parentheses may be used to group sub-expressions.

<table class="table">
<thead><tr> <th>type</th> <th>format</th> <th>example</th> </tr></thead>
<tbody>
  <tr> <td>bool</td> <td><code>true|false</code></td> <td><code>true</code></td> </tr>
  <tr> <td>int</td> <td><code>-?[0-9]+</code></td> <td><code>42</code></td> </tr>
  <tr> <td>string</td> <td><code>"..."</code></td> <td><code>"a string"</code></td> </tr>
  <tr> <td>ident</td> <td><code>[a-zA-Z_][a-zA-Z0-9:_+-]*</code></td> <td><code>foo</code></td> </tr>
  <tr> <td>operator</td> <td><code>[!=&lt;&gt;|&amp;+:]+</code></td> <td><code>&gt;=</code></td> </tr>
  <tr> <td>list</td> <td> <code>&lt;value&gt;</code> or <code>[ &lt;value&gt; &lt;value&gt; ... ]</code> </td> <td><code>[ foo "bar" ]</code></td> </tr>
  <tr> <td>option</td> <td> <code>&lt;value&gt;</code> or <code>&lt;value&gt; { &lt;value&gt; &lt;value&gt; ... }</code> </td> <td><code>foo { &gt; "0" }</code></td> </tr>
  <tr> <td>parens</td> <td><code>(&lt;value&gt;)</code></td> <td><code>(foo &amp; bar)</code></td> </tr>
</tbody>
</table>

Comments may be either enclosed in `(*` and `*)`, or `#` and newline. They are
ignored by OPAM.


### Package Formulas

Both package names and versions are encoded as strings. Formulas allow to
specify conditions on installed packages. They are logic formulas using the
operators `&` and `|` for AND and OR, over package names:

```
("neat_package" | "also_neat_package") & "required_package"
```

Package names can be suffixed with _version constraints_, which restricts the
requirement further. Version constraints have the form `<relop> <version>` and
can be combined with binary AND `&` and OR `|`, and prefix NOT `!`. The allowed
relational operators are `=`, `!=`, `<`, `<=`, `>` and `>=`, and their meaning
is defined by Version Ordering.

> <a id="version-ordering">**Version Ordering**</a> follows the basics of the
> [Debian definition](https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version).
>
> It is basically "lexicographical order, with numbers handled properly". In
> more detail:
>
> - version strings are sliced into alternate, possibly empty non-digit / digit
>   sequences, always starting with a non-digit sequence. `1.0~beta2` is thus
>   `["";1;".";0;"~beta";2]`.
> - those are sorted lexicographically, using resp. ASCII (with symbol > letter)
>   and number order. For example `a` gives `["a"]`, and `1` gives `["";1]`, so
>  `a` is latest (`"" < "a"`).
> - the `~` character is special as it sorts even before the end of sequence
>   (_i.e._ before anything shorter: "~" sorts before "", "a~b" before "a").
>   It's most convenient for pre-releases, allowing `1.0~beta` to be before
>   `1.0`.
>
> Here is an example of an ordered sequence: `~~`, `~`, `~beta2`, `~beta10`,
> `0.1`, `1.0~beta`, `1.0`, `1.0-test`, `1.0.1`, `1.0.10`, `dev`, `trunk`.

Here is a full example:

```
"foo" { >= "3.12" } & ("bar" | "baz" { !(> "2" & < "3.5") & != "5.1" })
```

### Variables

#### Usage

Variables may appear at a few different places in OPAM files and configuration.
They can be used in two forms:

- raw idents: `foo < bar`
- within strings, using _interpolation_, when enclosed in `%{` and `}%`:
  `"%{foo}%/bar"`

For both forms, and within values that allow them, the variables are replaced by
their contents, if any, just before the value is used. Variable contents can be
either _strings_, _booleans_ or _undefined_, and automatic conversion may take
place using the strings `"true"` and `"false"` (leading to an _undefined_ bool
when converting from any other string). Undefined values are propagated through
boolean expressions, and lead otherwise to context-dependent default values (the
empty string and `false`, unless specified otherwise).

> **1.2.1 experimental feature**: boolean-to-string conversion specifiers
>
> In 1.2.1, the additional syntax
> `"%{var?string-if-true:string-if-false-or-undefined}%"` can be used to insert
> different strings depending on the boolean value of a variable. This is not
> allowed yet in the main repository.

Additionally, boolean package variables may be combined using the following
form: `name1+name2+name3:var` is the conjunction of `var` for each of `name1`,
`name2` and `name3`, _i.e_ it is equivalent to `name1:var & name2:var &
name3:var`

#### Scopes

The defined variables depend on the specific fields being defined. There are
three scopes:

1. Global variables correspond to the general current configuration (system
   setup, OPAM configuration, current switch, etc.). For example `opam-version`,
   `arch`, or `make`.
2. Package variables have the form `package-name:var-name` and contain values
   specific to a given package, e.g. `foo:installed` is a boolean variable that
   can be used to check if package `foo` is installed, `foo:lib` is its library
   installation directory. The prefix `package-name:` can be omitted within the
   package's own OPAM file (when not shadowed by another variable), in which
   case the variables will relate to the version of the package being defined.
   Otherwise, the installed version is always used, and most variables will be
   undefined when the package isn't installed.
3. Some fields define their own local variables, like `success` in the field
   `post-messages`

For a comprehensive list of predefined variables, and their current local
values, run:

```
opam config list
```

### Filters

"Filters" are formulas based on variables. Their main use is as optional
conditions to commands or command arguments, using the postfix-braces syntax:

```
[ "./configure" "--use-foo" {foo:installed} ]
```

In build instructions, this adds the `"--use-foo"` argument only when variable
`foo:installed` is `true`. This is evaluated just when the command is going to
be run.

Filters and package formulas have a different scope and should not be mistaken:
- Package formulas are over constant _packages_ and _versions_ and can not refer
  to variables
- Filters are over _variables_, and can't refer to packages directly
- Package formulas denote abstract requirements, they're independent from any
  given setup. They are solved, not evaluated
- Filters evaluate at a precise moment in time, in a given setup, to return a
  value.

The following are allowed in filters:
- String and boolean literals
- Idents
- Parens
- Logical operators (binary AND `&`, binary OR `|`, prefix, unary NOT `!`)
- Binary relational operators (`=`, `!=`, `<`, `<=`, `>`, `>=`)

The comparisons are done using Version Order. Relational operators have a higher
precedence than logical operators.

Undefined values are propagated through relational operators, and logical
operators unless absorbed (`undef & false` is `false`, `undef | true` is
`true`).

### Interpolation

Some files can be rewritten using variable interpolation expansion: when looking
for `file` when this is available, if `file.in` is found, any `%{var}%`
interpolations found in it are replaced by the contents of `var` and the results
are written back to `file`.

This can also be done explicitly using the command `opam config subst "file"`.

### Environment updates

Some fields define updates to environment variables in the form:

```
<ident> <update-op> <string>
```

The allowed update operators `update-op` denote how the string is applied to the
environment variable:
- `=` override (or set if undefined)
- `+=` prepend (or set if undefined)
- `=+` append (or set if undefined)
- `:=` prepend (or set to if undefined) `<string>:`
- `=:` append (or set to if undefined) `:<string>`

### URLs

URLs are provided as strings. When pointing to origins for package
sources, they may be:
- raw local filesystem paths
- ssh addresses `user@host:path`
- URLs of the form `http://`, `https://`, `ftp://`, `ssh://`, `file://`, `rsync://`
- Version control URLs for git, mercurial and darcs: `git://`, `hg://`,
  `darcs://`. This assumes http transport for `hg` and `darcs`, _i.e._
  `hg://` is short for `hg+http://`
- Version control bound to a specific URL: `<vc>+<scheme>://`, e.g. `git://`,
  `hg+https://`, `git+file://`, etc. (**NOTE:** this has been added in OPAM 1.2.1)

> Backwards-compatibility note: to allow unambiguous urls, e.g. for the
> `dev-repo` field, without triggering a failure on OPAM 1.2.0, you should use
> the compatible syntax `<vc>://<scheme>://`, e.g. `hg://https://` at the
> moment. This will get rewritten to the nicer `<vc>+<scheme>://` in the next
> repository format upgrade.

In addition, version control URLs may be suffixed with the `#` character and a
reference name (branch, commit, HEAD...): `git://foo.com/git/bar#master`,
`hg+file://foo.com/hg/bar#dev1`

The URLs given for user information (e.g. package homepages and bugtrackers) are
not concerned and should just load nice in common browsers.

## Repository format

A repository is a target that can be pointed to by the `opam repository add`
command and provides metadata (and possibly archives) to OPAM.

### Layout

An OPAM repository is a directory structure containing the following elements:

- a `packages/` sub-directory, holding all package definitions
- a `compilers/` sub-directory, holding the compilers definitions (used by `opam
  init` and `opam switch`)
- a `repo` file with some metadata on the repository itself
- an `archives/` subdirectory that may hold source archives for the packages
- possibly `index.tar.gz` and `urls.txt` files, when serving over HTTP.

All of them are optional, although the `repo` file is recommended if the repo
isn't for purely local use.

The packages definitions are held in `package-name.package-version` directories
below `packages/`, at any depth. The convention on the official repository is to
have a directory per package name, gathering all versions, as such:
`packages/package-name/package-name.package-version/`. If the directory listing
gets too large in the future, it may be split into sub-directories by prefix
(e.g. `packages/p/pa/package-name/package-name.version`).

The compiler definitions are held in directories below `compilers/`, at any
depth. The convention is to place them in separate sub-directories depending on
the OCaml version they are based on: They should have a name of the form
`base-ocaml-version+patch` (e.g. `4.02.1+BER` for the `BER` compiler), and be
placed in a directory `compilers/base-ocaml-version/base-ocaml-version+patch/`.
For the official OCaml releases, the `+patch` suffix is omitted.

### Repo specification

The `repo` file, placed at the root of a repository, has the following optional
fields:

* <a id="repofield-opam-version">`opam-version: <string>`</a>:
  File format and repository format version, should be `1.2` as of writing.
* <a id="repofield-browse">`browse: <string>`</a>:
  An URL where the users may browse available packages online
* <a id="repofield-upstream">`upstream: <string>`</a>:
  The source that this repo is generated and maintained from. Typically, a
  version-control system address.
* <a id="repofield-redirect">`redirect: [ <string> { <filter> } ... ]`</a>:
  List of URLs to (permanently) redirect to if their filters evaluate to `true`.
  Can be used to serve different repositories for different OSes or different
  versions of OPAM.

### Indexes and serving over HTTP(S)

Browsing a large tree hierarchy over HTTP is unpractical. If the repository is
to be served over HTTP, the `opam-admin` tool bundled with OPAM needs to be used
to generate digests of all files in the repository, as well as a compressed
archive:

```
opam-admin make --index
```

should be run at each repository update, and will generate the `urls.txt` and
`index.tar.gz` files at the root of the repository.

Additionally, if the `--index` option is omitted, **all** package archives
listed in **all** packages that aren't present already will be fetched and
repackaged inside the `archives/` directory, allowing direct download from the
server. These are used by `opam update` even when fetching from non HTTP
repositories.

### Metadata precedence

When several repositories are configured in a given OPAM installation, all
versions of all packages present in them are available. In case of conflict
(same version of same package available in two of them), metadata from the
repository configured with the highest priority is used.

When pinning a package, metadata is diverted from its normal, repository source.
Metadata is searched:

- in a `package-name.opam/` subdirectory of the package source
- in an `opam/` subdirectory of the package source
- in a `package-name.opam` file at the root of the package source
- in an `opam` file at the root of the package source
- from the repository if no metadata subdirectory was found above, excluding the
  opam file is one was found above
- from a template, opening an editor, if nothing was found in either the package
  source or the repositories.

Furthermore, the metadata of pinned package is then stored within the switch (at
`$(opam show --where <package>)`), and the `opam` file can be edited through
`opam pin edit`

> NOTE: handling directory or files of the form `package-name.opam` is a new
> feature in 1.2.1. It should only be used for source repositories that may hold
> more than one OPAM package.


## Compiler specification

Compilers are specified by

- a file `compiler-name.descr`, similar to the package `descr` files: raw utf8
  file starting with a one-line description.

- a file `compiler-name.comp` specifying the source and details of the compiler.

The `compiler-name.comp` file has the following fields:

- <a id="compfield-opam-version">`opam-version: <string>`</a>:
  File format version, should be `1.2` as of writing.

- <a id="compfield-name">`name: <string>`</a>:
  the compiler name, should be of the form `base-version+patch`, and the same
  used in the filename.

- <a id="compfield-version">`version: <string>`</a>:
  the base OCaml compiler version this is based on, e.g. `4.02.1`

- <a id="compfield-src">`src: <string>`</a> or
  <a id="compfield-archive">`archive: <string>`</a>:
  as for package `url` files, the URL where the compiler source can be
  retrieved. Older, more specific URL fields are deprecated.

- <a id="compfield-patches">`patches: [ <string> ... ]`</a>:
  URLs of patches that will be retrieved and applied to the source from `src:`
  or `archive:`.

- <a id="compfield-build">
  `build: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  commands that will be run from the compiler source root to build and install
  the compiler.

- <a id="compfield-configure">`configure: [ <string> ... ]`</a> and
  <a id="compfield-make">`make: [ <string> ... ]`</a>:
  alternatively, this will build using `./configure` and `make`, with the given
  flags (`--prefix` is automatically appended to the configure command)

- <a id="compfield-packages">`packages: [ <package-formula> ... ]`</a>:
  these packages will be installed right after the `build:` or `make:` steps
  have been run. They must be self-contained (no external dependencies), and the
  user won't be able to change or remove them in this switch (except via
  explicit pinning).

- <a id="compfield-env">`env: [ <ident> <update-op> <string> ]`</a>:
  specifies environment variables updates that will be performed whenever in
  this switch.

- <a id="compfield-preinstalled">`preinstalled: <bool>`</a>:
  should not be set by hand, specifies that the `.comp` file refers to an
  OCaml installation that lies outside of OPAM (files using this are normally
  auto-generated).

Note that, since OPAM `1.2.1` all build instructions can be omitted, and that it
is therefore possible to delegate all the build process to the packages in
`packages:`.


## Package specification

Metadata for a single package is made of all the following files, and should be
gathered in a single directory (named `package-name.package-version` when in a
repository).

### descr

Descr is a simple utf8 text file -- it doesn't follow the OPAM syntactic
conventions. The first line of the file is a short description of the package,
suitable for displaying in package listings, and a longer description may follow
on next lines.

### url

The `url` file describes the source of the package and how it may be obtained.
It has the following fields:

- One of <a id="urlfield-src">`src: <string>`</a> or
  <a id="urlfield-archive">`archive: <string>`</a>,
  specifying the URL where the package can be downloaded from. When using HTTP
  or FTP, this should be an archive. The older alternative field names
  <a id="urlfield-http">`http:`</a>,
  <a id="urlfield-local">`local:`</a>,
  <a id="urlfield-git">`git:`</a>,
  <a id="urlfield-hg">`hg:`</a> and
  <a id="urlfield-darcs">`darcs:`</a>
  are deprecated, prefer explicit URLs.

    On the official repository, this should always point to a stable archive
    over HTTP or FTP.
- <a id="urlfield-checksum">`checksum: <string>`</a>:
  the MD5 of the referred-to archive, to warrant integrity. Mandatory on the
  official repository.
- <a id="urlfield-mirrors">`mirrors: [ <string> ... ]`</a>:
  an optional list of mirrors. They must use the same protocol as the main URL.

### files/

This subdirectory may contain any files or directories (of reasonable size) that
will be copied over the root of the package source. `opam` file fields like
`patches:` refer to files at that same root, so patches are typically included
here.

Note that repository archives generated by `opam-admin make` in `archives/`
_already_ include these overlay files.

#### package-name.install

This file is convenient to automate package installation without resorting to
external commands like `make install`. It can be included in `files/` but also
at the root of the package source, or generated by the build-system.

To avoid duplicating efforts for managing installations, a stand-alone
`opam-installer` tool is provided that can perform installations and
uninstallations from these files, without requiring OPAM.

It specifies what files and compilation artefacts must be installed where. All
the fields have the form

```
field: [ <string> { <string> } ]
```

The following take a list of filenames (relative to the root of the package
source) to be installed to the field's respective directory. An optional
relative path and destination filename can be given using the postfix braces
syntax. A leading `?` in the origin filename is stripped and informs OPAM to
continue silently when the file is not found.

- <a id="installfield-lib">`lib:`</a>
  installs to `<prefix>/lib/package-name`
- <a id="installfield-libexec">`libexec:`</a>
  installs to `<prefix>/lib/package-name`, but the `exec` bit is set (since
  OPAM 1.2.1)
- <a id="installfield-bin">`bin:`</a>
  installs to `<prefix>/bin`, with the `exec` bit set
- <a id="installfield-sbin">`sbin:`</a>
  installs to `<prefix>/sbin`, with the `exec` bit set
- <a id="installfield-toplevel">`toplevel:`</a>
  installs to `<prefix>/lib/toplevel`
- <a id="installfield-share">`share:`</a>
  installs to `<prefix>/share/package-name`
- <a id="installfield-share_root">`share_root:`</a>
  installs relative to `<prefix>/share` (since OPAM 1.2.0)
- <a id="installfield-etc">`etc:`</a>
  installs to `<prefix>/etc/package-name`
- <a id="installfield-doc">`doc:`</a>
  installs to `<prefix>/doc/package-name`
- <a id="installfield-stublibs">`stublibs:`</a>
  installs to `<prefix>/lib/stublibs`, with the `exec` bit set

The following are treated slightly differently:

- <a id="installfield-man">`man:`</a>
  installs relative to `<prefix>/man`, with the exception that when the
  destination is unspecified, the proper destination directory is extracted from
  the extension of the source file (so that `man: [ "foo.1" ]` is equivalent to
  `man: [ "foo.1" {"man1/foo.1"} ]`
- <a id="installfield-misc">`misc:`</a>
  requires files to specify an absolute destination, and the user will be
  prompted before the installation is done.

### opam

The main file specifying a package's metadata. The `opam lint` command is
recommended to check the validity and quality of your `opam` files.

`opam` files allow the following fields:

- <a id="opamfield-opam-version">`opam-version: <string>`</a> (mandatory):
  the file format version, should be `1.2` as of writing.

- <a id="opamfield-name">`name: <string>`</a>,
  <a id="opamfield-version">`version: <string>`</a>:
  the name and version of the package. Both fields are optional when they can be
  inferred from the directory name (e.g. when the file sits in the repository).

- <a id="opamfield-maintainer">`maintainer: <string>`</a> (mandatory):
  A contact address for the package maintainer (the format `"name <email>"` is
  allowed).

- <a id="opamfield-authors">`authors: [ <string> ... ]`</a>:
  a list of strings listing the original authors of the software.

- <a id="opamfield-license">`license: [ <string> ... ]`</a>:
  the abbreviated name(s) of the license(s) under which the source software is
  available.

- <a id="opamfield-homepage">`homepage: <string>`</a>,
  <a id="opamfield-doc">`doc: <string>`</a>,
  <a id="opamfield-bug-reports">`bug-reports: <string>`</a>:
  URLs pointing to the related pages for the package, for user information

- <a id="opamfield-dev-repo">`dev-repo: <string>`</a>:
  the URL of the package's source repository, which may be useful for
  developpers: not to be mistaken with the URL file, which points to the
  specific packaged version.

- <a id="opamfield-tags">`tags: [ <string> ... ]`</a>:
  an optional list of semantic tags used to classify the packages. The
  `"org:foo"` tag is reserved for packages officially distributed by
  organization ``foo''.

    Tags in the form `"flags:<ident>"` can be used to pass meaningful `flags` to
    OPAM: see the `flags:` field below. This is mostly intended for
    compatibility with earlier releases in the 1.2 branch, which will complain
    about unknown values in the `flags:` field.

- <a id="opamfield-patches">`patches: [ <string> { <filter> } ... ]`</a>:
  a list of files relative to the project source root (often added through the
  `files/` metadata subdirectory). The listed patch files will be applied
  sequentially to the source as with the `patch` command. Variable interpolation
  is available, so you can specify `patches: [ "file" ]` to have the patch
  processed from `file.in`.

    Patches may be applied conditionally by adding _filters_.

- <a id="opamfield-substs">`substs: [ <string> ... ]`</a>:
  a list of files relative to the project source root. These files will be
  generated from their `.in` counterparts, with variable interpolations
  expanded.

- <a id="opamfield-build">
  `build: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  the list of commands that will be run in order to compile the package.

    Each command is provided as a list of terms (a command and zero or more
    arguments) ; individual terms as well as full commands can be made
    conditional by adding filters: they will be ignored if the filter evaluates
    to `false` or is undefined. Variable interpolations are also evaluated.
    These commands will be executed in sequence, from the root of the package
    source.

    Any command is allowed, but these should write exclusively to the package's
    source directory, be non-interactive and perform no network i/o. All
    libraries, syntax extensions, binaries, platform-specific configuration and
    `package-name.install` files should be produced within the source directory
    subtree during this step. (**NOTE**: installation instructions used to be
    included in this step, so you may find examples of older packages that do
    not respect the above. This behaviour is deprecated)

- <a id="opamfield-install">
  `install: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  the list of commands that will be run in order to install the package.

    This field follows the exact same format as `build:`, but should only be used
    to move products of `build:` from the build directory to their final
    destination under the current `prefix`, and adjust some configuration files
    there when needed. Commands in `install:` are executed sequentially after the
    build is finished. These commands should only write to subdirectories of
    `prefix`, without altering the source directory itself.

    This field contains typically just `[make "install"]`. It is recommended,
    though, to prefer the usage of a static or generated `package-name.install`
    file and omit the `install:` field.

- <a id="opamfield-build-doc">
  `build-doc: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>,
  <a id="opamfield-build-test">
  `build-test: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  the list of commands to build documentation and tests. They are processed
  after the build phase when documentation or tests have been requested. These
  follow the same specification as the `build:` field.

- <a id="opamfield-remove">
  `remove: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  the commands used to uninstall the package. It should be the reverse operation
  of `install:`, and absent when `install:` is. It follows the same format as
  `build:`.

- <a id="opamfield-depends">`depends: [ <package-formula> ... ]`</a>:
  the package dependencies. This describes the requirements on other packages
  for this package to be built and installed. It contains a list of package
  formulas, understood as a conjunction.

    As an addition to the package formula format, the version constraints may be
    prefixed by _dependency flags_. These are one of `build`, `test` and `doc`
    and limit the meaning of the dependency:

    * `build` dependencies are no longer needed at run-time: they won't trigger
      recompilations of your package.
    * `test` dependencies are only needed when building tests (_i.e._ by
      instructions in the `build-test` field)
    * likewise, `doc` dependecies are only required when building the package
      documentation

    Dependency flags must be first, and linked by `&`:

        depends: [
          "foo" {build}
          "bar" {build & doc}
          "baz" {build & >= "3.14"}
        ]

- <a id="opamfield-depopts">
  `depopts: [ <string> { <dependency-flags> } ... ]`</a>:
  the package optional dependencies. This flag is similar to `depends:` in
  format, but with some restrictions. It contains packages that will be _used_,
  if present, by the package being defined, either during build or runtime, but
  that are not _required_ for its installation. The implementation uses this
  information to define build order and trigger recompilations, but won't
  automatically install _depopts_ when installing the package.

    The optional dependencies may have _dependency flags_, but they may not
    specify version constraints nor formulas. `depopts:` can be combined with
    `conflicts:` to add version constraints on the optional dependencies.

- <a id="opamfield-conflicts">
  `conflicts: [ <string> { <version-constraint> } ... ]`</a>:
  a list of package names with optional version constraints indicating that the
  current package can't coexist with those.

- <a id="opamfield-depexts">
  `depexts: [ [ [ <string> ... ] [ <string> ... ] ] ... ]`</a>:
  the package external dependencies. This field is a list that can be used for
  describing the dependencies of the package toward software or packages
  external to the OPAM ecosystem, for various systems. It contains pairs of
  lists of the form `[ predicates ext-packages ]`. `predicates` is used to
  select the element of the list based on the current system: it is a list of
  tags (strings) that can correspond to the OS, architecture or distribution.
  The `predicates` is used as a conjunction: the pair will only be selected when
  _all_ tags are active. The resulting `ext-packages` should be identifiers of
  packages recognised by the system's package manager.

    There is currently no definite specification for the precise tags you should
    use, but the closest thing is the
    [opam-depext project](https://github.com/OCamlPro/opam-depext). The
    `depexts` information can be retrieved through the `opam list --external`
    command.

- <a id="opamfield-messages">`messages: [ <string> { <filter> } ... ]`</a>:
  used to display an additional (one-line) message when prompting a solution
  implying the given package. The typical use-case is to tell the user that some
  functionality will not be available as some optional dependencies are not
  installed.

- <a id="opamfield-post-messages">
  `post-messages: [ <string> { <filter> } ... ]`</a>:
  allows to print specific messages to the user after the end of installation.
  The special boolean variable `failure` is defined in the scope of the filter,
  and can be used to print messages in case there was an error (typically, a
  hint on how it can be resolved, or a link to an open issue). `success` is also
  defined as syntactic sugar for `!failure`.

- <a id="opamfield-available">`available: [ <filter> ]`</a>:
  can be used to add constraints on the OS and OCaml versions currently in use,
  using the built-in `os` and `ocaml-version` variables. In case the filter is
  not valid, the package is disabled. The `os` and `ocaml-version` fields are
  deprecated, please use `available` instead in newly created packages.

    This field is evaluated before request solving or any actions take place ;
    it can only refer to global variables, since it shouldn't depend on the
    current switch state. An unavailable package won't generally be seen on the
    system, except with `opam list -A`.

- <a id="opamfield-flags">`flags: [ <ident> ... ]`</a>:
  specify package flags that may alter package behaviour. Currently available
  flags are:

    - <a id="opamflag-light-uninstall">`light-uninstall`</a>:
      the package's uninstall instructions don't require
      the package source. This is currently inferred when the only uninstall
      instructions have the form `ocamlfind remove...`, but making it explicit
      is preferred (since OPAM 1.2.0).
    - <a id="opamflag-verbose">`verbose`</a>:
      when this is present, the stdout of the package's build and
      install instructions will be printed to the user (since OPAM 1.2.1).
    - <a id="opamflag-plugin">`plugin`</a>:
      the package installs a program named `opam-<name>` and may be
      auto-installed and run with `opam <name>` (since OPAM 1.2.2 ; the use is
      discouraged in the 1.2 branch for compatibility, use `tag: "flags:plugin"`
      instead)
    - <a id="opamflag-compiler">`compiler`</a>:
      the package is to be treated as a compiler, and available through the
      `opam switch` command. This as several consequences:
      - when installed this way, the package and all its dependencies will be
        immutable and excluded from `opam switch rebuild`
      - the package _must_ include or generate a `global-config.config` file at its root
      - 

- <a id="opamfield-features">
  `features: [ <ident> <string> { <filter> } ... ]`</a> (EXPERIMENTAL):
  this field is currently experimental and shouldn't be used on the main package
  repository. It allows to define custom variables that better document what
  _features_ are available in a given package build. Each feature is defined as
  an identifier, a documentation string, and a filter expression. The filter
  expression can evaluate to either a boolean or a string, and the defined
  identifier can be used as a variable in any filter (but recursive features are
  not allowed and will be _undefined_).

    This is typically useful to pass appropriate flags to `./configure` scripts,
    depending on what is installed.
