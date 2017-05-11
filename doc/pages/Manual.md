<style type="text/css"><!--
  .opam {font-family: Tahoma,Verdana,sans-serif; font-size: 110%; font-weight: lighter; line-height: 90.9%}
--></style>

# The <span class="opam">opam</span> manual

This manual gathers reference information on <span class="opam">opam</span> and its file formats. It is
primarily of use for packagers, package maintainers and repository maintainers.

* For simple usage of <span class="opam">opam</span>, see the [Usage](Usage.html) page, and the
  comprehensive built-in documentation `opam [command] --help`.
* For a gentler introduction to packaging, see the
  [Packaging guide](Packaging.html)
* If you want to hack on <span class="opam">opam</span> or build related tools, the API documentation can
  be browsed [here](api/index.html)

## File hierarchies

### opam root

<span class="opam">opam</span> holds its configuration, metadata, logs, temporary directories and caches
within a directory that we will call _opam root_. By default, this is `~/.opam`,
and we may refer to it by this name in this manual for the sake of simplicity,
but this can be changed using the `OPAMROOT` environment variable or the
`--root` command-line argument.

An existing <span class="opam">opam</span> root is required for <span class="opam">opam</span> to operate normally, and one is
created upon running `opam init`. The initial configuration can be defined
through a configuration file at `~/.opamrc`, `/etc/opamrc` or at a location
specified through the `--config` command-line option. If none is present, <span class="opam">opam</span>
defaults to its built-in configuration that binds to the OCaml repository at
`https://opam.ocaml.org`.

Except in explicit cases, <span class="opam">opam</span> only alters files within its <span class="opam">opam</span> root. It is
organised as follows:
- [`~/.opam/config`](#config): the global <span class="opam">opam</span> configuration file
- `~/.opam/repo/`: contains the mirrors of the configured package repositories
- [`~/.opam/repo/repos-config`](#reposconfig): lists the configured package repositories and their URLs
- `~/.opam/repo/<repo>`: mirror of the given repository
- `~/.opam/opam-init/`: contains <span class="opam">opam</span> configuration scripts for the outside world, e.g. shell environment initialisation
- `~/.opam/download-cache/`: caches of downloaded files
- `~/.opam/plugins/<plugin>`: reserved for plugins
- `~/.opam/<switch>`: prefixes of named [switches](#Switches)

### Repositories

Repositories are collection of <span class="opam">opam</span> package definitions. They respect the
following hierarchy:
- [`/repo`](#repo): repository configuration file
- [`/packages/<pkgname>/<pkgname>.<version>/opam`](#opam): holds the metadata
  for the given package. `url` and `descr` may also be present, in which case
  they override what may already be present in the `opam` file
- [`/packages/<pkgname>/<pkgname>.<version>/files/`](#files): contains files
  that are copied over the root of the source tree of the given package before
  it gets used.
- `/cache/`: cached package files, by checksum. Note that the cache location is
  configured in the [repo](#repofield-archive-mirrors) file, this name is only
  where `opam admin cache` puts it by default.
- `/archives/`: this is no longer used by opam automatically, but is the
  canonical place where you should place your package archives if you want to
  serve them from the repository server directly. The URLs of the packages will
  have to be set accordingly.
- `/index.tar.gz`: archive containing the whole repository contents (except the
  cache), needed when serving over HTTP. It can be generated using `opam admin
  index`.

<span class="opam">opam</span> repositories can be accessed using local or
remote (ssh) paths, HTTP URLs, or one of the supported version control systems
(git, Mercurial, Darcs). A repository is set up using

```
opam repository add <name> <URL>
```

and can subsequently be then selected for use in specific switches using `opam
repository select <name>`. Use `opam repository list --all` for an overview of
configured repositories. Repository selection is always ordered, with the
definition of a given version of a package being taken from the repository with
the lowest index where it is found.

Data from the configured repositories is updated from the upstreams manually
using the `opam update` command. This only updates repositories in use by the
currently selected switches, unless `--all` is specified.

### Switches

<span class="opam">opam</span> is designed to hold any number of concurrent installation prefixes, called
_switches_. Switches are isolated from each other and have their own set of
installed packages, selection of repositories, and configuration options. All
package-related commands operate on a single switch, and require one to be
selected.

The current switch can be selected in the following ways:
- globally, using `opam switch <switch>`. <span class="opam">opam</span> will use that switch for all
  further commands, except when overriden in one of the following ways.
- for local switches, which are external to the opam root, when in the directory
  where the switch resides or a descendant.
- by setting the `OPAMSWITCH=<switch>` environment variable, to set it within a
  single shell session. This can be done by running `eval $(opam env --switch
  <switch>)` to set the shell environment at once, see below.
- through the `--switch <switch>` command-line flag, for a single command.

Switches have their own prefix, normally `~/.opam/<switch>`, where packages get
intalled ; to use what is installed in a switch, some environment variables need
to be set, _e.g._ to make executables installed into `~/.opam/<switch>/bin`
visible, that directory needs to be added to `PATH`, but individual packages can
define their own settings as well.

Command `opam config env` returns the environment updates corresponding to the
current switch, in a format readable by your shell, and when needed <span class="opam">opam</span> will
prompt you to run:

```
eval $(opam config env)
```

A switch is created using `opam switch create <switch> (<compiler>|--empty)`.
- `<switch>` can be either a plain name, or a directory name (if containing `/`
  or starting with `.`). In the latter case the switch is _local_ and instead of
  being held at `~/.opam/<switch>`, it will be created in the given directory,
  as a `_opam` subdirectory. Local switches are automatically selected depending
  on the current directory, see above.
- If a `<compiler>` is selected, <span class="opam">opam</span> will install the corresponding packages
  and their dependencies in the new switch. These packages will be marked as
  _base_, protected against removal and unaffected by upgrade commands.
  `<compiler>` can be selected among packages which have the `compiler` flag
  set, or their versions. Use `opam switch list-available` to list them.

#### Structure

If we define `<switch-prefix>` as:
- `~/.opam/<switch>` for plain switches
- `<switch>/_opam` for local switches, when `<switch>` is a path

Switches are laid out thusly:
- `<switch-prefix>/`: prefix of the switch, holding the installation hierarchy
  in the UNIX `/usr` standard (with subdirectories `bin`, `lib`, `share`, `man`,
  `doc`, `etc`...)
- `<switch-prefix>/.opam-switch/`: holds all <span class="opam">opam</span> data regarding this switch
- [`<switch-prefix>/.opam-switch/switch-config`: switch-specific
  configuration](#switchconfig)
- [`<switch-prefix>/.opam-switch/switch-state`: stores the sets
  of installed, base, pinned packages](#switchstate)
- `<switch-prefix>/.opam-switch/environment`: contains the environment variable
  settings for this switch
- `<switch-prefix>/.opam-switch/reinstall`: list of packages marked for
  reinstallation (development packages where changes have been detected)
- [`<switch-prefix>/.opam-switch/config/<pkgname>.config`](#ltpkgnamegtconfig):
  installed package's, opam specific configuration
- [`<switch-prefix>/.opam-switch/install/<pkgname>.install`](#ltpkgnamegtinstall):
  `.install` files used to install the given package
- `<switch-prefix>/.opam-switch/install/<pkgname>.changes`: file system changes
  done by the installation of the given package, as tracked by <span
  class="opam">opam</span>
- `<switch-prefix>/.opam-switch/packages/<pkgname>.<version>/`: metadata of the
  given package as it has been used for its installation
- `<switch-prefix>/.opam-switch/sources/<pkgname>.<version>/` or `<pkgname>/`:
  unpacked sources of packages. The version is omitted from the directory name
  for pinned packages, which are typically synchronised to a version-control
  system rather than unpacked from an archive.
- `<switch-prefix>/.opam-switch/overlay/<pkgname>/`: custom definition for the
  given pinned packages
- `<switch-prefix>/.opam-switch/build/<pkgname>.<version>/`: temporary
  directories where the packages are compiled
- `<switch-prefix>/.opam-switch/remove/<pkgname>.<version>/`: temporary
  directories used for calling the packages' `remove` commands, when those need
  the source.
- `<switch-prefix>/.opam-switch/backup`: snapshots of previous states of the
  switch, and other backup files.

#### Pinning

Pinning is an operation by which a package definition can be created or altered
locally in a switch.

In its most simple form, `opam pin <package> <version>`, `<package>` is bound to
the specified version and won't be changed on `opam upgrade` (assuming it is an
existing package). `opam pin edit <package>` provides a way to directly pin and
edit the metadata of the given package, locally to the current switch, for
example to tweak a dependency.

`opam pin` can further be used to divert the source of a package, or even create
a new package, using `opam pin <package> <URL>` ; this is also very useful to
point to a local path containing a development or patched version of the package
source. If the source you are pointing to contains a matching `<pkgname>.opam/`
or `opam/` directory, a matching `<pkgname>.opam` or `opam` file, that will be
used as the initial package metadata.

Whenever an install, reinstall or upgrade command-line refers to a pinned
package, <span class="opam">opam</span> first fetches its latest source. `opam
update [--development]` is otherwise the standard way to update the sources of
all the packages pinned in the current switch.

## Common file format

### Conventions

Syntax is given in a BNF-like notation. Non-terminals are written `<like this>`,
terminals are either plain text or written in double-quotes (`"terminal"`),
curly brackets denote zero or more repetitions when suffixed with `*`, or one or
more when suffixed with `+`, and square brackets denote zero or one occurence.
Parentheses are for grouping. `(")` and `(""")` respectively mean one and three
quotation mark characters.

As a special case, and for readability, we add simplified notations for _lists_
and _options_:
- `[ <list-contents> ... ]` means
  `"[" { <list-contents> }* "]" | <list-contents>`.
  It corresponds to a case of the `<list>` non-terminal and is a list of
  `<list-contents>` repeated any number of times. The square brackets can be
  omitted when `<list-contents>` occurs just once.
- `<element> { <opt> }` means
  `<element> [ "{" <opt> "}" ]`.
  It corresponds to a specific case of the `<option>` non-terminal.

### General syntax

```BNF
<file-contents> ::= { <file-item> }*
<file-item>     ::= <field-binding> | <section>
<field-binding> ::= <ident> : <value>
<section>       ::= <ident> [ <string> ] "{" <file-contents> "}"
<ident>         ::= { <identchar> }* <letter> { <identchar> }*
<varident>      ::= [ ( <ident> | "_" ) { "+" ( <ident> | "_" ) }* : ] <ident>
<identchar>     ::= <letter> | <digit>  | "_" | "-"
<letter>        ::= "a".."z" | "A".."Z"
<digit>         ::= "0".."9"
<value>         ::= <bool> | <int> | <string> | <ident> | <varident> | <operator> | <list> | <option> | "(" <value> ")"
<bool>          ::= true | false
<int>           ::= [ "-" ] { <digit> }+
<string>        ::= ( (") { <char> }* (") ) | ( (""") { <char> }* (""") )
<operator>      ::= { "!" | "=" | "<" | ">" | "|" | "&" }+ | [ ":" ] <operator> [ ":" ]
<list>          ::= "[" { <value> }* "]"
<option>        ::= <value> "{" { <value> }* "}"
<comment>       ::= ( "(*" { <char> }* "*)" ) | ( "#" { <char\newline> }* <newline> )
```

The <span class="opam">opam</span> file formats share a common base syntax. The
files are UTF-8 encoded and define a list of _fields_ and _sections_.

<span class="opam">opam</span> uses a range of different files, each allowing their own set of fields and
sections, in a specific format.

Base values can be literal booleans, integers or strings, identifiers, and
operators. Strings allow the escapes `\"`, `\\`, `\n`, `\r`, `\b`, `\t`, as well
as three-digit decimal and two-digit hexadecimal character codes (`\NNN` and
`\xNN`), and escaped newlines. As a special case, they can be enclosed in triple
double-quotes (`"""`), so that single quotes don't need to be escaped. Lists
must be enclosed in square brackets unless they contain a single element. Values
can be followed by an argument in braces. Parentheses may be used to group
sub-expressions.

Comments may be either enclosed in `(*` and `*)`, or `#` and newline. They are
ignored by <span class="opam">opam</span>.


### Package Formulas

Package formulas are used to express requirements on the set of installed
packages.

```BNF
<package-formula> ::= <package-formula> <logop> <package-formula>
                    | ( <package-formula> )
                    | <pkgname> { { <version-formula> }* }
<logop>           ::= "&" | "|"
<pkgname>         ::= (") <ident> (")
<version-formula> ::= <version-formula> <logop> <version-formula>
                    | "!" <version-formula>
                    | "(" <version-formula> ")"
                    | <relop> <version>
<relop>           ::= "=" | "!=" | "<" | "<=" | ">" | ">="
<version>         ::= (") { <identchar> | "+" | "." | "~" }+ (")
```

Package names have the same restrictions as idents — only letter, digits, dash
and underscore, and contain at least one letter — but must be enclosed in
quotes. Versions are non-empty strings, with some restrictions on the characters
allowed.

We are using logic formulas with the operators `&` and `|` for AND and OR, over
package names. `&` is higher priority than `|`, so the parentheses in this
example are required:

```
("neat_package" | "also_neat_package") & "required_package"
```

Package names can be suffixed with _version constraints_, which restricts the
requirement further. Version constraints have the form `<relop> <version>` and
can be combined with binary AND `&` and OR `|`, and prefix NOT `!`. The allowed
relational operators are `=`, `!=`, `<`, `<=`, `>` and `>=`, and their meaning
is defined by Version Ordering. They always have higher priority than logical
operators.

> <a id="version-ordering">**Version Ordering**</a> follows the basics of the
> [Debian definition](https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version).
>
> It is basically "lexicographical order, with numbers handled
> properly". In more detail (see the [Debian
> page](https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version)
> for full details):
>
> - Version strings are sliced into alternate, possibly empty non-digit / digit
>   sequences, always starting with a non-digit sequence. `1.0~beta2` is thus
>   `["";1;".";0;"~beta";2]`.
>
> - Those sequences are sorted lexicographically. Because of the split
>   as `non-digit; digit; non-digit; ...`, non-digit sequences are
>   always compared to non-digit sequences (and conversely). For
>   example, the versions `a` and `1` are sliced into `["a"]` and
>   `[""; 1]`, so we have `1 < a` because the non-digit component of
>   1, which is the empty string `""`, is smaller than `"a"`.
>
> - For non-digit components, the ordering used is that letters are
>   always smaller than non-letters (for example `z` < `"#"`), while
>   non-letters are compared by ASCII order.
>
> - The `~` character is special as it sorts even before the end of sequence
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

Variables may appear at a few different places in <span class="opam">opam</span> files and configuration.
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
empty string or `false`, depending on the expected type, unless specified
otherwise).

The syntax`"%{var?string-if-true:string-if-false-or-undefined}%"` can be used to
insert different strings depending on the boolean value of a variable.

Additionally, boolean package variables may be combined using the following
form: `name1+name2+name3:var` is the conjunction of `var` for each of `name1`,
`name2` and `name3`, _i.e_ it is equivalent to `name1:var & name2:var &
name3:var`

#### Scopes

The defined variables depend on the specific fields being defined. There are
three scopes:

1. Global variables correspond to the general current configuration, or to the
   current switch settings (system setup, <span class="opam">opam</span> configuration, current switch
   name, etc.). For example `opam-version`, `arch`, or `make`.
2. Package variables have the form `package-name:var-name` and contain values
   specific to a given package, for example `foo:installed` is a boolean
   variable that can be used to check if package `foo` is installed, and
   `foo:lib` is its library installation directory.

     `foo:bar` refers to `bar` in the installed version of `foo`, and may be
     undefined if `foo` is not installed: for example, a package depending on
     `foo` can use `foo:version` to toggle different compatibility modes
     depending on the installed version of `foo`.

     One exception is references emanating from the package being defined itself:
     within `foo`'s definition, `foo:bar`, which can in this case be abridged in
     `_:bar`, always refers to the version being defined.

3. Some fields define their own local variables, like `success` in the field
   [`post-messages`](#opamfield-post-messages). Common examples of this include
   the following variables, available in the `depends:`, `depopts:`, `build:` and
   `install:` fields:

    * `with-test`: `true` when testing has been turned on for the package
      (through the `--with-test` flag)
    * `with-doc`: similarly for documentation.

    Within package definition files, the variables `name` and `version`, as
    shortcuts to `_:name` and `_:version`, corresponding to the package being
    defined, are always available.

For a comprehensive list of predefined variables, and their current local
values, run:

```
opam config list
```

### Filters

_Filters_ are formulas based on variables. Their main use is as optional
conditions to commands or command arguments, using the postfix-braces syntax:

```
[ "./configure" "--use-foo" {foo:installed} ]
```

In build instructions, this adds condition `foo:installed` to the `"--use-foo"`
argument, which will cause it to be omitted unless the variable `foo:installed`
is `true`. This is evaluated just when the command is going to be run.

```BNF
<filter> ::= <filter> <logop> <filter>
           | "!" <filter>
           | "?" <filter>
           | ( <filter> )
           | <filter> <relop> <filter>
           | <varident>
           | <string>
           | <bool>
```

Filters are evaluated at a certain point in time, and should not be mistaken
with package formulas, which express requirements.

The following are allowed in filters:
- String and boolean literals
- Idents
- Parentheses
- Logical operators (binary AND `&`, binary OR `|`, prefix, unary NOT `!`)
- The unary operator `?` for detecting whether an expression contains undefined
  variables
- Binary relational operators (`=`, `!=`, `<`, `<=`, `>`, `>=`)

The comparisons are done using [Version Order](#version-ordering). Relational
operators have a higher precedence than logical operators.

Undefined values are propagated through relational operators, and logical
operators unless absorbed (`undef & false` is `false`, `undef | true` is
`true`). Undefined values may be detected using the `?` operator, for example,
`!(?foo & foo != bar)` requires either `foo` to be undefined or equal in value
to `bar`.

### Filtered package formulas

This extension to package formulas allows variables to be referenced within
version constraints, and parts of the formula to be made optional. This is
evaluated as a first pass, before any action is taken, to deduce a concrete
package formula.

The definition is similar to that of `<package-formula>`, except that two cases
`<filter>` and `<relop> <filter>` ar added to `<version-formula>`

```BNF
<filtered-package-formula> ::= <filtered-package-formula> <logop> <filtered-package-formula>
                             | ( <filtered-package-formula> )
                             | <pkgname> { { <filtered-version-formula> }* }

<filtered-version-formula> ::= <filtered-version-formula> <logop> <filtered-version-formula>
                             | "!" <filtered-version-formula>
                             | "?" <filtered-version-formula>
                             | "(" <filtered-version-formula> ")"
                             | <relop> <version>
                             | <filter>
                             | <relop> <filter>
```

For example, a dependency on `"foo" { = version }` will require package `foo` at
the version defined by variable `version` (which is the version of the package
being defined). Conditions can be added using the same logical operators present
in pure package formulas, so one can also write `"foo" { >= "3.12" & build }`,
which makes the dependency dependent on the boolean value of the variable
`build`.

When the version formula reduces to `false`, as would be the case here when
`build=false`, the dependency is removed from the formula.

### Interpolation

Some files can be rewritten using variable interpolation expansion: in cases
where this is available, when looking for `file` and `file.in` is found, any
`%{var}%` interpolations found in it are replaced by the contents of `var` and
the results are written back to `file`.

This can also be done explicitly using the command `opam config subst "file"`.

### Environment updates

Some fields define updates to environment variables in the form:

```
<ident> <update-op> <string>
```

The allowed update operators `update-op` denote how the string is applied to the
environment variable. Prepend and append operators assume list of elements
separated by an OS-specific character (`;` on Windows, `:` on Cygwin or any
other system).
- `=` override (or set if undefined)
- `+=` or `:=` prepend. They differ when the variable is unset of empty, where `:=` adds a trailing separator.
- `=+` or `=:` append. They differ when the variable is unset of empty, where `=:` adds a leading separator.
- `=+=` is similar to `+=`, except that when the variable was previously altered
  by opam, the new value will replace the old one at the same position instead
  of being put in front.

### URLs

URLs are provided as strings. They can refer to:
- raw local filesystem paths
- ssh addresses `user@host:path`
- URLs of the form `http://`, `https://`, `ftp://`, `ssh://`, `file://`, `rsync://`
- Version control URLs for git, mercurial and darcs: `git://`, `hg://`,
  `darcs://`. This assumes http transport for `hg` and `darcs`, _i.e._
  `hg://` is short for `hg+http://`
- Version control bound to a specific URL: `<vc>+<scheme>://`, e.g. `git://`,
  `hg+https://`, `git+file://`, etc. (**NOTE:** this has been added in <span class="opam">opam</span> 1.2.1)

In addition, version control URLs may be suffixed with the `#` character and a
reference name (branch, commit, HEAD...): `git://foo.com/git/bar#master`,
`hg+file://foo.com/hg/bar#dev1`

The URLs given for user information (e.g. package homepages and bugtrackers) are
not concerned and should just load nice in common browsers.

### Checksums

```
<checksum> ::= (") [ "md5=" | "sha256=" | "sha512=" ] { <hexchar> }+ (")
<hexchar>  ::= "a".."f" | "A".."F" | "0".."9"
```

Checksums are specified as strings, in hexadecimal form, and should be prefixed
by the name of the hashing algorithm (when unspecified, MD5 is assumed, for
backwards compatibility only).

Additionally, the number of hexadecimal chars must match exactly what is
expected by the corresponding algorithm (resp. 32, 64 and 128 for MD5, SHA256
and SHA512).

If `openssl` is installed on the system, it will be used for faster computation
of SHA hashes.

## Specific file formats

This section describes the precise file formats of the different kinds of files
used by <span class="opam">opam</span>.

### Public configuration files

These files are intended to be publicly distributed as part of public
repositories or initial distributions or packages.

#### repo
<a id="Repospecification"></a>

The `repo` file is placed at the root of a repository, and allows to specify
some specifics of the repository. It has the following optional fields:

* <a id="repofield-opam-version">`opam-version: <string>`</a>:
  File format and repository format version, should be `2.0` as of writing.
* <a id="repofield-browse">`browse: <string>`</a>:
  An URL where the users may browse available packages online
* <a id="repofield-upstream">`upstream: <string>`</a>:
  The source that this repo is generated and maintained from. Typically, a
  version-control system address.
* <a id="repofield-redirect">`redirect: [ <string> { <filter> } ... ]`</a>:
  List of URLs to (permanently) redirect to if their filters evaluate to `true`.
  Can be used to serve different repositories for different OSes or different
  versions of <span class="opam">opam</span>. Relative URLs are supported from
  opam 2.0, but discouraged for compatibility reasons.
* <a id="repofield-archive-mirrors">`archive-mirrors: [ <string> ... ]`</a>:
  Archive proxy URLs specific to this repository, with the same semantics as the
  similar [config field](#configfield-archive-mirrors), except a path without a
  `protocol://` prefix is accepted and will be considered relative to the
  repository root (e.g. `cache/`).

#### opamrc

This file has a format close to that of [config](#config), and can be used to
define an initial setup for <span class="opam">opam</span>. When running `opam init`, if `~/.opamrc` or
`/etc/opamrc` is present, or if `--config` was specified, the configuration
options from that file will be used, overriding the defaults.

The default, built-in initial config of <span class="opam">opam</span> can be seen with `opam init
--help`.

- <a id="opamrcfield-opam-version">`opam-version: <string>`</a>:
  the file format version.
- <a id="opamrcfield-repositories">`repositories: [ <string> { <URL> } { {<string>}+ } { <int> } ... ]`</a>:
  preconfigured repository names and the corresponding URLs. The last two
  optional arguments for each repository are the trust anchors (fingerprints of
  the trusted signing identities, see
  [repository-validation-command](#configfield-repository-validation-command)) and the
  quorum, _i.e._ how many of them are required for a signature to be accepted.
- [`jobs:`](#configfield-jobs),
  [`download-command:`](#configfield-download-command),
  [`download-jobs:`](#configfield-download-jobs),
  [`archive-mirrors:`](#configfield-archive-mirrors).
  [`solver-criteria:`](#configfield-solver-criteria),
  [`solver-upgrade-criteria:`](#configfield-solver-upgrade-criteria),
  [`solver-fixup-criteria:`](#configfield-solver-fixup-criteria),
  [`solver:`](#configfield-solver),
  [`global-variables:`](#configfield-global-variables),
  [`default-compiler:`](#configfield-default-compiler): these have the same
  format as the same-named fields in the [config](#config) file, and will be
  imported to that file on `opam init`.
  [`default-compiler:`](#configfield-default-compiler) is additionally used to
  select the switch that will be created by `opam init` without `--bare`.

#### switch-config

This file is located in `<switch-prefix>/.opam-switch/switch-config` and
contains configuration options specific to that switch:

- <a id="switchconfigfield-opam-version">`opam-version: <string>`</a>:
  the file format version.
- <a id="switchconfigfield-synopsis">`synopsis: <string>`</a>:
  a short description for the switch, shown when listing. By default, this is
  initialised to the synopsis of the chosen compiler package.
- <a id="switchconfigfield-repos">`repositories: [ <string> ... ]`</a>:
  lists the repositories in use in this switch, higher priority first. The
  repository names should correspond to configured repositories in
  `~/.opam/repo` (they are otherwise ignored). If unset, the
  [set of repositories](#configfield-repositories) from the global configuration
  is used.
- <a id="switchconfigfield-opam-root">`opam-root: <string>`</a>:
  the opam root the switch belongs to. Used for local switches, to avoid
  automatically selecting a switch belonging to a different opam root.
- [`pre-build-commands:`](#configfield-pre-build-commands),
  [`pre-install-commands:`](#configfield-pre-install-commands),
  [`pre-remove-commands:`](#configfield-pre-remove-commands):
  as the corresponding [global config](#config) fields.
- [`wrap-build-commands:`](#configfield-pre-build-commands),
  [`wrap-install-commands:`](#configfield-pre-install-commands),
  [`wrap-remove-commands:`](#configfield-pre-remove-commands):
  as the corresponding [global config](#config) fields.
- [`post-build-commands:`](#configfield-pre-build-commands),
  [`post-install-commands:`](#configfield-pre-install-commands),
  [`post-remove-commands:`](#configfield-pre-remove-commands):
  as the corresponding [global config](#config) fields.
- <a id="switchconfigsection-paths">`paths: "{" { <ident>: <string> ... } "}"`</a>:
  defines the standard paths within the switch: recognised fields include
  `prefix:`, `bin:`, `sbin:`, `lib:`, `share:`, `etc:`, `doc:`, `man:`,
  `stublibs:`, `toplevel:`.
- <a id="switchconfigsection-variables">`variables: "{" { <ident>: <string> ... } "}"`</a>:
  allows the definition of variables local to the switch.

### Package definitions

Package definitions can be a single [`opam`](#opam) file. A [`files/`](#files)
subdirectory can also be used to add files over the package source. Older
versions of <span class="opam">opam</span> used [`descr`](#descr) and
[`url`](#url) files besides the `opam` file, and this is still supported, but
the preferred way is now to include their information into the `opam` file
instead.

[`<pkgname>.install`](#ltpkgnamegtinstall) and
[`<pkgname>.config`](#ltpkgnamegtconfig), on the other hand, are metadata files
used by opam but that are found in the package source directory, after it has
been built.

#### opam

Package definition files, specifying a package's metadata. Usage of the `opam
lint` command is recommended to check the validity and quality of your `opam`
files.

`opam` files allow the following fields and sections:

- <a id="opamfield-opam-version">`opam-version: <string>`</a> (mandatory):
  the file format version, should be `2.0` as of writing.

- <a id="opamfield-name">`name: <pkgname>`</a>,
  <a id="opamfield-version">`version: <version>`</a>:
  the name and version of the package. Both fields are optional when they can be
  inferred from the directory name (e.g. when the file sits in the repository),
  but should always be set for package definitions within source trees.

- <a id="opamfield-maintainer">`maintainer: [ <string> ... ]`</a> (mandatory):
  A contact address for the package maintainer (the format `"name <email>"` is
  allowed).

- <a id="opamfield-authors">`authors: [ <string> ... ]`</a>:
  a list of strings listing the original authors of the software.

- <a id="opamfield-license">`license: [ <string> ... ]`</a>:
  the abbreviated name(s) of the license(s) under which the source software is
  available.

- <a id="opamfield-homepage">`homepage: [ <string> ... ]`</a>,
  <a id="opamfield-doc">`doc: [ <string> ... ]`</a>,
  <a id="opamfield-bug-reports">`bug-reports: [ <string> ... ]`</a>:
  URLs pointing to the related pages for the package, for user information

- <a id="opamfield-dev-repo">`dev-repo: <string>`</a>:
  the URL of the package's source repository, which may be useful for
  developers: not to be mistaken with the URL file, which points to the
  specific packaged version.

- <a id="opamfield-tags">`tags: [ <string> ... ]`</a>:
  an optional list of semantic tags used to classify the packages. The
  `"org:foo"` tag is reserved for packages officially distributed by
  organization ``foo''.

- <a id="opamfield-patches">`patches: [ <string> { <filter> } ... ]`</a>: a list
  of files relative to the project source root (often added through the `files/`
  metadata subdirectory). The listed patch files will be applied sequentially to
  the source as with the `patch` command, stripping one level of leading
  directories (`-p1`) -- which is what version control systems generally use .
  Variable interpolation is available, so you can specify `patches: [ "file" ]`
  to have the patch processed from `file.in`.

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
    These commands will be executed in sequence, from the root of a fresh
    package source tree.

    The commands executed during the `build:` stage may write exclusively to
    this source tree, should be non-interactive and should perform no network
    i/o. All libraries, syntax extensions, binaries, platform-specific
    configuration and `package-name.config` or `package-name.install` files are
    expected to be produced within the source directory subtree, _i.e._ below
    the command's `$PWD`, during this step.

    The following filter variables are available in the scope of this field:

    * `with-test`: this is set to true when the user asked for the tests of
      this package to be built. Use it to filter testing commands (_e.g._ `[make
      "test"] {with-test}`).
    * `with-doc`: likewise for documentation.
    * not limited to this scope, but the `dev` variable can be useful here to
      detect that the package is not installed from a release tarball, and may
      need additional preprocessing (_e.g._ `automake`).

- <a id="opamfield-install">
  `install: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  the list of commands that will be run in order to install the package.

    This field follows the exact same format as `build:`. It is used to move
    products of `build:` from the build directory to their final destination
    under the current `prefix`, and do any required additional setup. Commands
    in `install:` are executed sequentially, from the root of the source tree
    from where the `build:` commands have been run. These commands should only
    write to subdirectories of `prefix`, without altering the source directory
    itself.

    This field contains typically just `[make "install"]`. If a
    `package-name.install` is found at the source of the build directory, <span
    class="opam">opam</span> will install files from there to the prefix
    according to its instructions after calling the commands specified in the
    `install:` field have been run, if any.

    Variables `with-test` and `with-doc` are also available to the filters used
    in this field, to run specific installation commands when tests or
    documentation have been requested.

- <a id="opamfield-build-doc">
  `build-doc: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>,
  <a id="opamfield-build-test">
  `build-test: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a> (__deprecated__):
  you should use the [`build:`](#opamfield-build) and
  [`install:`](#opamfield-install) fields with filters based on the `with-test`
  and `with-doc` variables, to specify test and documentation specific
  instructions. The instructions in the deprecated `build-test:` are currently
  understood as part of the [`run-test:`](#opamfield-run-test) field.

- <a id="opamfield-run-test">
  `run-test: [ [ <string> { <filter> } ... ] { <filter> } ... ]`</a>:
  specific instructions for running the package tests, in a format similar to
  the [`build:`](#opamfield-build) field.

- <a id="opamfield-remove"> `remove: [ [ <string> { <filter> } ... ] { <filter>
  } ... ]`</a>: commands to run before removing the package, in the same format
  as `build:` and `install:`.
  As of `2.0`, <span class="opam">opam</span> tracks the files added to the prefix during package
  installation, and automatically removes them on package removal, so this
  should not be needed anymore in most cases (and may even be harmful if files
  from different packages overlap, which remove scripts generally don't handle).
  Use it for special actions, like reverting updates to files, or stopping
  daemons: removing what was just added is alredy taken care of.

    The commands are run from the root of a fresh copy of the package source,
    unless the [`light-uninstall`](#opamflag-light-uninstall) package flag is
    present, in which case they are run from the prefix.

- <a id="opamfield-depends">`depends: [ <filtered-package-formula> ... ]`</a>:
  the package dependencies. This describes the requirements on other packages
  for this package to be built and installed. It contains a list of filtered
  package formulas, understood as a conjunction.

    The filtered package formula can access the global and switch variables, but
    not variables from other packages. Additionally, special boolean variables
    `build`, `with-test` and `with-doc` are defined to allow limiting the scope
    of the dependency.

    * `build` dependencies are no longer needed at run-time: they won't trigger
      recompilations of your package.
    * `with-test` dependencies are only needed when building tests (when the
      package is explicitely installed with `--with-test`)
    * likewise, `with-doc` dependecies are only required when building the
      package documentation

- <a id="opamfield-depopts">
  `depopts: [ <pkgname> { <filtered-package-formula> } ... ]`</a>:
  the package optional dependencies. This flag is similar to
  [`depends:`](#opamfield-depends) in format. It contains packages that will be
  _used_, if present, by the package being defined, either during build or
  runtime, but that are not _required_ for its installation. The implementation
  uses this information to define build order and trigger recompilations, but
  won't automatically install _depopts_ when installing the package.

    Variables in the filtered package formula are evaluated as for
    [`depends:`](#opamfield-depends), with the same specific variables
    available.

    Note that `depopts: [ "foo" { = "3" } ]` means that the optional dependency
    only applies for `foo` version `3`, not that your package can't be installed
    with other versions of `foo`: for that, use the
    [`conflicts:`](#opamfield-conflicts) field.

- <a id="opamfield-conflicts">
  `conflicts: [ <pkgname> { <version-constraint> } ... ]`</a>:
  a list of package names with optional version constraints indicating that the
  current package can't coexist with those.

- <a id="opamfield-conflict-class">`conflict-class: [ <pkgname> ... ]`</a>:
  an alternate, symmetric way of defining package conflicts. Conflict classes
  defined by this flag have the same constraints as package names, but occupy a
  different namespace. Any two packages having a common conflict class will be
  considered incompatible. This is useful to define sets of mutually conflicting
  packages.

- <a id="opamfield-depexts">
  `depexts: [ [ [ <string> ... ] [ <string> ... ] ] ... ]`</a>:
  the package external dependencies. This field is a list that can be used for
  describing the dependencies of the package toward software or packages
  external to the <span class="opam">opam</span> ecosystem, for various systems. It contains pairs of
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

    The `depexts:` field should preferably be used on [`conf`](#opamflag-conf)
    packages, which make the dependencies clearer and avoids duplicating the
    efforts of documenting the appropriate system packages on the various
    OSes available.

- <a id="opamfield-messages">`messages: [ <string> { <filter> } ... ]`</a>:
  used to display an additional (one-line) message when prompting a solution
  implying the given package. A typical use-case is to tell the user that some
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
  can be used to add constraints on the OS and other global variables.
  In case the filter doesn't evaluete to `true`, the package is disabled.

    This field is evaluated before request solving or any actions take place ;
    it can only refer to global variables, since it shouldn't depend on the
    current switch state. An unavailable package won't generally be seen on the
    system, except with `opam list -A`.

- <a id="opamfield-flags">`flags: [ <ident> ... ]`</a>:
  specify package flags that may alter package behaviour. Currently available
  flags are:

    - <a id="opamflag-light-uninstall">`light-uninstall`</a>:
      the package's uninstall instructions don't require
      the package source.
    - <a id="opamflag-verbose">`verbose`</a>:
      when this is present, the stdout of the package's build and
      install instructions will be printed to the user.
    - <a id="opamflag-plugin">`plugin`</a>:
      the package installs a program named `opam-<name>` and may be
      auto-installed and run with `opam <name>`.
      The convention is to name the plugin package `opam-<name>`.
    - <a id="opamflag-compiler">`compiler`</a>: the package is to be treated as
      a compiler, and will be advertised for installing as a `compiler` package
      when creating a fresh prefix through the `opam switch` command.
    - <a id="opamflag-conf">`conf`</a>: this is a "`conf`" package, that is
      intended to document capabilities of the system, or the presence of
      software installed outside of <span class="opam">opam</span>. As such, the package may not
      include a source URL or install anything, but just do some checks, and
      fail to install if they don't pass. `conf` packages should have a name
      starting with `conf-`, and include the appropriate
      [`depexts:`](#opamfield-depexts) field.

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

- <a id="opamfield-synopsis">`synopsis: <string>`</a>:
  defines a short description (one line) for the package. This can also be
  defined as the first line of an external [`descr`](#descr) file.

- <a id="opamfield-description">`description: <string>`</a>:
  defines a long description (one or more paragraphs) for the package. This can
  also be defined as the body of an external [`descr`](#descr) file.

- <a id="opamsection-url">`url "{" <url-file> "}"`</a>:
  defines the URL where the package source can be obtained. This section has
  contents in the same format as the [`url`](#url) file, and has the same effect
  as using a separate `url` file.

- <a id="opamfield-setenv">`setenv: [ <environment-update> ... ]`</a>:
  defines environment variables updates that will be applied upon installing the
  package. The updates will be visible to any dependent package, as well as
  exported to the shell environment through `opam config env` and
  `~/.opam/opam-init/` scripts.

- <a id="opamfield-build-env">`build-env: [ <environment-update> ... ]`</a>:
  defines environment updates that will be applied when running the package's
  build, install and remove scripts.

- <a id="opamsection-extra-sources">`extra-source <string> "{" <url-file> "}"`</a>:
  allows the definition of extra files that need downloading into the source
  tree before the package can be patched (if necessary) and built. The format is
  similar to that of the `url` section, but here it is expected to point to a
  single file (version-controlled remotes are not allowed). The leading
  `<string>` indicates the name the file should be saved to, and is relative to
  the root of the source tree.

- <a id="opamfield-extra-files">`extra-files: [ [ <string> <checksum> ] ... ]`</a>:
  optionally lists the files below `files/` with their checksums. Used
  internally for integrity verifications.

- <a id="opamfield-extra-fields">`x-*:`</a>:
  extra fields prefixed with `x-` can be defined for use by external tools. <span class="opam">opam</span>
  will ignore them except for some search operations.

#### descr

Descr is a plain UTF-8 text file without specific syntactic constraints. The
first line of the file defines the package's synopsis, while the rest defines
its long description.

This information can be embedded in `opam` package definition files using the
[`synopsis:`](#opamfield-synopsis) and [`description:`](#opamfield-description)
fields since <span class="opam">opam</span> version 2.0. However, if a `descr` file is present alongside
the `opam` file, it takes precedence.

#### url

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
- <a id="urlfield-checksum">`checksum: [ <checksum> ... ]`</a>:
  the checksums of the referred-to archive, to warrant integrity. At least one
  is mandatory on the official repository.
- <a id="urlfield-mirrors">`mirrors: [ <string> ... ]`</a>:
  an optional list of mirrors. They must use the same protocol as the main URL.

These contents can be embedded within the [`url {}`](#opamsection-url) section
of an `opam` file, which is the preferred way. You shouldn't have both an `url`
file and an `opam` file with an `url {}` section: in that case, the `url` file
will be ignored with a warning.

#### files/

A special subdirectory that can appear in package definition directories,
alongside the `opam` file.

This subdirectory may contain any files or directories (of reasonable size) that
will be copied over the root of the package source. If already present, files
are overwritten, and directories are recursively merged. [`opam`](#opam) file
fields like [`patches:`](#opamfield-patches) refer to files at that same root,
so patches specific to <span class="opam">opam</span> are typically included in
this subdirectory.

Also see the [`extra-sources:`](#opamsection-extra-sources) opam section, which has
a similar behaviour and is processed before the `files/` are copied.

#### <pkgname>.install
<a id="packagenameinstall"></a>

This file format describes the installation from a source directory to an
installation prefix. It will be used by <span class="opam">opam</span> if
present at the root of the package's source directory after the `build:`
instructions have been run: it can thus be generated by the build system, be
static in the package source, or be added by <span class="opam">opam</span>
through the [`files/`](#files) mechanism.

To avoid duplicating efforts for managing installations, a stand-alone
`opam-installer` tool is provided with <span class="opam">opam</span> that can perform installations and
uninstallations from these files, or even generate corresponding shell scripts,
without requiring <span class="opam">opam</span>.

All the fields have the form

```
field: [ <string> { <string> } ]
```

The following take a list of filenames (relative to the root of the package
source) to be installed to the field's respective directory. An optional
relative path and destination filename can be given using the postfix braces
syntax. A leading `?` in the origin filename is stripped and informs <span class="opam">opam</span> to
continue silently when the file is not found.

Absolute paths, or paths referencing the parent directory (`..`), are not
allowed.

- <a id="installfield-lib">`lib:`</a>
  installs to `<prefix>/lib/<pkgname>/`
- <a id="installfield-lib_root">`lib_root:`</a>
  installs to `<prefix>/lib/` (since <span class="opam">opam</span> 2.0.0)
- <a id="installfield-libexec">`libexec:`</a>
  installs to `<prefix>/lib/<pkgname>/`, but the `exec` bit is set (since
  <span class="opam">opam</span> 1.2.1)
- <a id="installfield-libexec_root">`libexec_root:`</a>
  installs to `<prefix>/lib/`, with the `exec` bit set (since <span
  class="opam">opam</span> 2.0.0)
- <a id="installfield-bin">`bin:`</a>
  installs to `<prefix>/bin/`, with the `exec` bit set
- <a id="installfield-sbin">`sbin:`</a>
  installs to `<prefix>/sbin/`, with the `exec` bit set
- <a id="installfield-toplevel">`toplevel:`</a>
  installs to `<prefix>/lib/toplevel/`
- <a id="installfield-share">`share:`</a>
  installs to `<prefix>/share/<pkgname>/`
- <a id="installfield-share_root">`share_root:`</a>
  installs relative to `<prefix>/share/` (since <span class="opam">opam</span> 1.2.0)
- <a id="installfield-etc">`etc:`</a>
  installs to `<prefix>/etc/<pkgname>/`
- <a id="installfield-doc">`doc:`</a>
  installs to `<prefix>/doc/<pkgname>/`
- <a id="installfield-stublibs">`stublibs:`</a>
  installs to `<prefix>/lib/stublibs/`, with the `exec` bit set

The following are treated slightly differently:

- <a id="installfield-man">`man:`</a>
  installs relative to `<prefix>/man`, with the exception that when the
  destination is unspecified, the proper destination directory is extracted from
  the extension of the source file (so that `man: [ "foo.1" ]` is equivalent to
  `man: [ "foo.1" {"man1/foo.1"} ]`
- <a id="installfield-misc">`misc:`</a>
  requires files to specify an absolute destination, and the user will be
  prompted before the installation is done.

#### <pkgname>.config

This file is used by packages to give <span class="opam">opam</span> specific options upon
installation. A file with this name will be installed by <span class="opam">opam</span> into
`<switch-prefix>/.opam-switch/config/` if found at the root of the package
source tree after its installation instructions have been run.

- <a id="dotconfigfield-opam-version">`opam-version: <string>`</a>:
  the file format version.
- <a id="dotconfigfield-file-depends">`file-depends: [ "[" <string> <checksum> "]" ... ]`</a>:
  when a package defines `absolute-filename` - `hash` bindings using this field,
  on state-changing operations, <span class="opam">opam</span> will check that the file at the given path
  still exists and has the given hash. This can be used to guarantee the
  consistency of packages that rely on system-wide files or system packages when
  those are changed, _e.g._ by `apt-get upgrade`. The user will be warned if the
  file was removed, and the package marked for reinstallation if it was changed.
- <a id="dotconfigsection-variables">`variables: "{" { <ident>: <string> ... }
  "}"`</a>: allows the definition of package variables, that will be available
  as `<pkgname>:<varname>` to dependent packages.

### Local configuration files

These files are local to the opam root, and managed by <span class="opam">opam</span>. [`config`](#config)
and [`switch-config`](#switchconfig) can be manually edited to set configuration
options when <span class="opam">opam</span> isn't running. [`switch-state`](#switchstate) and
[`repos-config`](#reposconfig) store internal state and are documented here, but
shouldn't be edited except by <span class="opam">opam</span>.

#### config

This file is stored as `~/.opam/config` and defines global configuration options
for <span class="opam">opam</span>.

- <a id="configfield-opam-version">`opam-version: <string>`</a>:
  the version of the format of this opam root, used in particular to trigger
  format migrations.
- <a id="configfield-repositories">`repositories: [ <string> ... ]`</a>:
  the set of configured repositories to use for newly created switches. The
  repositories themselves are set up using the
  [repos-config](#reposconfigfield-repositories) file.
- <a id="configfield-installed-switches">`installed-switches: [ <string> ... ]`</a>:
  lists the switches configured in this opam root, either internal or local.
  Deleted local switches are collected by <span class="opam">opam</span> automatically, and it is possible
  to use local switches that are not recorded in this field. It remains useful
  for cross-switch listings, repository configuration updates, or <span class="opam">opam</span> format
  migrations.
- <a id="configfield-switch">`switch: <string>`</a>:
  the currently globally selected switch.
- <a id="configfield-jobs">`jobs: <int>`</a>:
  the number of concurrent jobs to run for build processes.
- <a id="configfield-download-jobs">`download-jobs: <int>`</a>:
  the maximum number of concurrent downloads.
- <a id="configfield-download-command">`download-command: [ ( <string> | <ident> ) { <filter> } ... ]`</a>:
  the command to use for downloading packages. If set to a single element, will
  use the `curl` and `wget` presets if set to those idents, and use the named
  executable with a curl-compatible command-line otherwise. Should otherwise be
  a full command, and in this scope, the following variables (only) are defined:
    - `opam-version` is the current opam version
    - `url` is the remote URL to fetch from
    - `out` is the expected output file
    - `retry` is the number of retries allowed
    - `compress` is whether HTTP compression should be enabled
    - `checksum` is the expected checksum of the file, including the
      `md5=`/`sha256=`/`sha512=` prefix
    - `hashalgo` is the hashing algorithm used (`md5`, `sha256` or `sha512`)
    - `hashvalue` is the raw value of the hash, in hexa, without prefix
    - `hashpath` is a relative path that can be used for per-hash caching,
      containing hashing algorithm, two-digit prefix and full hash value, _e.g._
      `md5/d4/d41d8cd98f00b204e9800998ecf8427e`.
- <a id="configfield-archive-mirrors">`archive-mirrors: [ "<URL>" ... ]`</a>:
  defines proxy URLs that package archives can be retrieved from. The URL may
  use any supported non-version control protocol, and the files will be looked
  up by (their first) hash with the relative path
  `<hash-algo>/<first-2-hash-characters>/<hash>`, where the hash is in
  hexadecimal. Proxies are tried in order, and the file is looked up from
  upstream if not found. They can also be configured
  [per-repository](#repofield-archive-mirrors). The command `opam admin cache`
  generates a suitable cache in `./cache`, see the [Repositories](#repositories)
  section.
- <a id="configfield-solver-criteria">`solver-criteria: <string>`</a>: can be
  used to tweak the solver criteria used for the resolution of operations. These
  depend on the external solver used, see the
  [Solver Criteria](Specifying_Solver_Preferences.html) page for details.
- <a id="configfield-solver-upgrade-criteria">`solver-upgrade-criteria: `</a>,
  <a id="configfield-solver-fixup-criteria">`solver-fixup-criteria: `</a>:
  similar to [`solver-criteria`](#configfield-solver-criteria), but specific to
  some actions.
- <a id="configfield-solver">`solver: [ ( <string> | <ident> ) { <filter> } ... ]`</a>:
  the external solver command to use. See the
  [External Solvers](Install.html#ExternalSolvers) section of the install guide
  for context. If set to a single `<ident>` element, that may point to one of
  the `aspcud`, `packup` or `mccs` predefined command-lines. Otherwise, the
  following variables are defined in the command scope:
    - `input` is the name of the input file, in [Cudf](http://mancoosi.org/cudf/) format
    - `output` is the expected name of the output file, containing the solution
    - `criteria` is the defined solver criteria.
- <a id="configfield-global-variables">`global-variables: [ "[" <ident> ( <string> | <bool> ) <string> "]" ... ]`</a>:
  allows the definition of global variables. The last `<string>` is for
  documentation and is shown in the output of `opam config list`.
- <a id="configfield-eval-variables">`eval-variables: [ "[" <ident> [ <string> ... ] <string> "]" ... ]`</a>:
  allows the definition of global variables that will be lazily initialised to
  the output of the given command. The last `<string>` documents the variable.
- <a id="configfield-pre-build-commands">`pre-build-commands: [ [ <string> { <filter> } ... ] ... ]`</a>,
  <a id="configfield-pre-install-commands">`pre-install-commands: [ [ <string> { <filter> } ... ] ... ]`</a>,
  <a id="configfield-pre-remove-commands">`pre-remove-commands: [ [ <string> { <filter> } ... ] ... ]`</a>:
  specify commands that will be run just before processing the package's commands
  for the corresponding action, on any package. The filters are evaluated in the
  same scope as the package commands.
- <a id="configfield-wrap-build-commands">`wrap-build-commands: [ <string> { <filter> } ... ]`</a>,
  <a id="configfield-wrap-install-commands">`wrap-install-commands: [ <string> { <filter> } ... ]`</a>,
  <a id="configfield-wrap-remove-commands">`wrap-remove-commands: [ <string> { <filter> } ... ]`</a>:
  specify wrappers around every command executed during the corresponding action
  of any package. The command-line elements will be prefixed to the package
  command, so for example command `[ make "install" ]` with wrapper
  `[ "time" "-o" "/tmp/timings" "-a" ]` will result in the command
  `[ "time" "-o" "/tmp/timings" "-a" make "install" ]`.
  The filters are evaluated in the same scope as the package commands.
- <a id="configfield-post-build-commands">`post-build-commands: [ [ <string> { <filter> } ... ] ... ]`</a>,
  <a id="configfield-post-install-commands">`post-install-commands: [ [ <string> { <filter> } ... ] ... ]`</a>,
  <a id="configfield-post-remove-commands">`post-remove-commands: [ [ <string> { <filter> } ... ] ... ]`</a>:
  specify commands that will be run just after processing the package's commands
  for the corresponding action, and the `<pkgname>.install` file in the case of
  install and remove, on any package. The post commands are run wether or not
  the package script succeeded. The filters are evaluated in the same scope as
  the package commands, with the addition of the variable `error-code`, which is
  the return value of the package script.
- <a id="configfield-repository-validation-command">`repository-validation-command: [ <string> { <filter> } ... ]`</a>:
  defines a command to run on the upstream repositories to validate their
  authenticity. When this is specified, and for repositories that define
  [trust anchors](#opamrcfield-repositories), opam will refuse any update that
  doesn't pass this validation. The command should return 0 on successful
  validation, and the following opam variables are made available:
    - `anchors`: comma-separated list of fingerprints
    - `quorum`: integer, the currently defined quorum
    - `repo`: directory containing the already-validated state of the repository
      (empty for an initial validation)
    - `patch`: for incremental validation, filename of a patch applying to
      `repo` (with `patch -p1`) and that needs verification
    - `dir`: for initial validation, the directory to verify
    - `incremental`: `false` if doing an initial validation based on `dir`,
      `true` for an incremental validation based on `repo` and `patch`.

    Since there are two modes (initial and incremental), all variables may not be
    defined in all cases.
- <a id="configfield-default-compiler">`default-compiler: [ <package-formula> ... ]`</a>:
  a list of compiler package choices. On `opam init`, the first available
  compiler in the list will be chosen for creating the initial switch if
  `--bare` wasn't specified.

#### switch-state

This file, located at `<switch-prefix>/.opam-switch/switch-state`, is used by
<span class="opam">opam</span> to store the current state of a switch. All fields are lists of packages
specified as strings in the format `<package>.<version>`.

- <a id="switchstatefield-opam-version">`opam-version: <string>`</a>:
  the file format version.
- <a id="switchstatefield-compiler">`compiler: [ <string> ... ]`</a>:
  the package that form the base of the current switch. They won't be affected
  by `opam upgrade`, and are protected against deletion.
- <a id="switchstatefield-roots">`roots: [ <string> ... ]`</a>:
  lists the package "roots", _i.e._ packages that have been installed manually
  by the user (as opposed to the ones which got installed to fulfil
  dependencies). Note that packages in this list may not be installed, if they
  were removed due to build failures.
- <a id="switchstatefield-installed">`installed: [ <string> ... ]`</a>:
  lists all currently installed packages.
- <a id="switchstatefield-pinned">`pinned: [ <string> ... ]`</a>:
  lists all packages which have been pinned, and to what version. If an overlay
  package definition is present in
  `<switch-prefix>/.opam-switch/overlay/<pkgname>/`, that can be used to alter
  the package definition and modify its source URL. Otherwise, the package is
  simply pinned to the given version.

#### repos-config

This file is at `~/.opam/repo/repos-config` and lists all package repositories
configured on the system, and their source URLs.

- <a id="reposconfigfield-repositories">`repositories: [ <string> { <URL> } { {<string>}+ } { <int> } ... ]`</a>:
  lists the configured repository idents, their URLs and trust anchors. The
  format is similar to [repositories:](#opamrcfield-repositories) from `opamrc`,
  except that the `<URL>` itself is optional.

Note that repositories without URLs can be used. In this case, the corresponding
subdirectory in `~/.opam/repo/` is supposed to be put in place and updated by
the user, so opam will never write to it and won't cache its contents.
