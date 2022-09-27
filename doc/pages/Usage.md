# Using opam

This document starts with a quick introduction, then covers most commonly-used
opam features.

If you are a developer and want to get a project packaged or change an existing
package, see the step-by-step [packaging guide](Packaging.html).

The full documentation is available inline, using

```
opam --help
opam <command> --help
```

This document is intended as a quicker overview, use the above to dig into the
details.

## Basics

```
# ** Get started **
opam init            # Initialize ~/.opam

# ** Lookup **
opam list -a         # List all available packages
opam search QUERY    # List packages with QUERY in their name or description
opam show PACKAGE    # Display information about PACKAGE

# ** Install **
opam install PACKAGE # Download, build and install the latest version of PACKAGE
                     # and all its dependencies
opam remove PACKAGE  # Uninstall the given package

# ** Upgrade **
opam update          # Update the packages database
opam upgrade         # Bring everything to the latest version possible

# ** More **
opam CMD --help      # Command-specific manpage
```

You may prefer to [browse packages online](https://opam.ocaml.org/packages). If
you find a package there but not on your computer, either it has been recently
added and you should simply run `opam update`, or it's not available on your
system or OCaml version — `opam install PACKAGE` will give you the reason.

## `--cli` option

Since opam 2.1, opam is able to be invoked using a previous version of its
command line. It's recommended that all opam commands in scripts use it, and
also blog posts and other sources of information, so you may come across
examples which include it. We don't recommend using it in the shell because it's
more to type!

## Details on commands

### opam init

opam needs to initialize its internal state in a `~/.opam` directory to work.
This command will also automatically pick a compiler to install, unless `--bare`
has been specified.

To make your shell aware of what has been installed in opam, some variables need
to be set in your environment. You will be prompted to update your
configuration, and given instructions on how to proceed manually if you decline.

### opam update

This command synchronizes opam's database with the package repositories. The
lists of available packages and their details are stored into
`~/.opam/repo/<name>`. Remember to run this regularly if you want to keep
up-to-date, or if you are having trouble with a package.

It will also update any packages that are bound to version-controlled sources.

### Looking up packages

There are three commands for that:
* `opam list` List installed packages, or packages matching various selection
  criteria.
* `opam search` Search in package descriptions.
* `opam show` Print details on a given package.

### opam install

This command downloads, builds and installs packages along with all their
dependencies. You can specify one or several packages, along with version
constraints. E.g:

```
opam install lwt
opam install ocp-indent ocp-index.1.0.2
opam install "ocamlfind>=1.4.0"
```

If package source is not found, opam tries to retrieve it from [Software
Heritage](www.softwareheritage.org) archives (see related [opam
option](Manual.html#configfield-swh-fallback)).

### opam upgrade

Will attempt to upgrade the installed packages to their newest versions. You
should run it after `opam update`, and may use `opam pin` to prevent specific
packages from being upgraded.

### opam switch

This command enables the user to have several installations on disk, each with
their own prefix, set of installed packages, compiler version, etc. Use cases
include having to work or test with different OCaml versions, keeping separate
development environments for specific projects, etc.

Use `opam switch create [name] <package-or-version>` to _switch_ to a different
compiler. Don't forget to run the advertised `eval $(opam env)` to update your
PATH accordingly. Replace `[name]` with a directory name to have the switch
created in that directory, and automatically selected when opam is run from
there: this is typically done within projects that require a specific compiler
or set of opam packages.

Creating a new switch requires re-compiling OCaml, unless you use the
`ocaml-system` package, that relies on the global OCaml installation.

### opam pin

This command allows one to pin a package to a specific version, but has been
extended to allow much more than that.

The syntax is

```
opam pin add <package name> <target>
```

Where `<target>` may be a version, but also an [URL](Manual.html#URLs) of a
local path, an archive, or even a git, mercurial or darcs repository. The
package will be kept up-to-date with its origin on `opam update` and when
explicitly mentioned in a command, so that you can simply run `opam upgrade
<package name>` to re-compile it from its upstream. If the upstream includes
opam metadata, that will be used as well.

```
opam pin add camlpdf 1.7                                      # version pin
opam pin add camlpdf ~/src/camlpdf                            # path
opam pin add opam-lib https://github.com/ocaml/opam.git#1.2   # specific branch or commit
opam pin add opam-lib --dev-repo                              # upstream repository
```

This is actually a powerful mechanism to divert any package definition, and can
even be used to locally create packages that don't have entries in the
repositories.

This can be used in conjunction with `opam source` to patch an existing package
in a breeze:

```
opam source <package> --dev-repo --pin
cd <package>; hack hack hack;
opam upgrade .
```

### opam repo

opam is configured by default to use the community's software repository at
[opam.ocaml.org](https://opam.ocaml.org), but third-party repositories can
easily be used in addition, or in replacement.

```
opam repo add <name> <address>
```

defines the alias `<name>` to refer to the package repository found at
`<address>`. Without further options, that repository will be set to lookup for
package definitions over what was already defined **in the current switch
only**. See options `--all` and `--set-default` to affect other and newly
created switches, respectively.

The `<address>` may point to an HTTP, local or version-controlled repository. To
create a new switch bound to specific repositories, it's easier to use instead:

```
opam switch create --repos <name>=<address>,default
```


Defining your own repository, either locally or online, is quite easy: you can
start off by cloning
[the official repository](https://github.com/ocaml/opam-repository) if you
intend it as a replacement, or just create a new directory with a `packages`
sub-directory, and a [`repo` file](Manual.html#repo) containing at least an
`opam-version` field. See the [packaging guide](Packaging.html) if you need help
on the package format.

If your repository is going to be served over HTTP, you should generate an index
using the `opam admin index` command.
