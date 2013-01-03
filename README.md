# OPAM - A package manager for OCaml

OPAM is a source-based package manager for OCaml. It supports multiple simultaneous
compiler installations, flexible package constraints, and a Git-friendly development
workflow.

### Prerequisites:

* ocaml (>= 3.12.1)
* curl or wget
* git   (optional)
* rsync (optional)

### Quick install

To download, compile and install OPAM, you can simply run:

```
$ wget https://raw.github.com/OCamlPro/opam/master/shell/install.sh && sh ./install.sh
```

### Compiling OPAM

To compile `opam`, simply run:

```
$ ./configure
# (or ./configure --prefix=$HOME if you want to install under your $HOME)
$ make
```

This will fetch the archives if they are not already downloaded and then build
OPAM. If you just want to get the necessary dependencies without compiling the
project, run `make clone`.

### Installing OPAM

To install opam simply run:

```
$ make install
```

or

```
$ sudo make install
```

if you need root privileges.

This way, OPAM binaries will be installed in `$prefix/bin`, where
`prefix` can been specified during the `./configure` phase.

### Using opam

All of the OPAM state is held in the `~\.opam` directory in your home
directory, including compiler installations. You should never need to switch to
a root user to install packages. Package listings are obtained through remote
sources.

```
$ opam init
$ eval `opam config env`
```

The first command creates `~\.opam` and sets up `opam.ocamlpro.com` as the
default repository.  The second command updates your local environment to use
the packages installed by OPAM.  You should add the `eval` line to your
`~/.profile` so you don't forget to run it before using OCaml.

```
$ opam list
```

As expected, this command lists all the available packages. We can now install
some packages (for instance `lwt`):

```
$ opam install lwt
```

OPAM is able to track optional dependencies. This mean that installing an
optional dependency of a package will recompile the package and all its
forward dependencies. For instance:

```
$ opam install react
```

will install `react` and also recompile `lwt` with the right `./configure`
options to support `react`, and also all the packages which further depend on
`lwt`.


### Installing on other OCaml compiler

OPAM also manages meta-data about OCaml compilers. So in order to install a new
version of the compiler, you can run:

```
$ opam switch 4.00.1
$ eval `opam config env`
```

The first command will download and install ocaml-4.00.1, and the second
command will update the environment variables. You can then install packages --
they will be installed on a different under `~/.opam/4.00.1`.

In order to return to the system-wide OCaml installation, simply run:

```
$ opam switch system
$ eval `opam config env`
```

You can use `opam switch list` to display the list of available compilers.

### Version pinning

```
$ opam pin <package> </local/path>
```

This command will use the content of `</local/path>` to compile `<package>`.
This means that the next time you will do `opam install <package>`, the
compilation process will be using a mirror of `</local/path>` instead of
downloading the archive. This also means that any modification to
`</local/path>` will be picked up by `opam update`, and thus `opam upgrade`
will recompile `<package>` (and its forward dependencies) if needed.

To unpin a package, simply run:

```
$ opam pin <package> none
```

You can also pin a package to a specific version: `opam pin <package> <version>`
or to a git repository: `opam pin <package> </remote/path/to/git>` or 
`opam pin <package> </local/path/to/git> -k git`.

### Repositories

OPAM supports multiple package repositories, for example for development
packages.

```
$ opam remote list
```

### Uninstall

To uninstall OPAM, use:

```
$ make uninstall
```

### Auto-completion

`shell/` contains shell-scripts to add auto-completion to OPAM. The script is not
installed by default, so to activate the mode you can either:

* move it at the right location (depending on your OS, for instance
  `/etc/auto-complete.d/opam` on Debian)

* copy it somewhere in you path and source it in your `.profile`.

## Documentation

* The main documentation entry point to OPAM is the user manual, available using `opam --help`. To see the
  help for a specific command, use `opam <command> --help`.
* The API documentation is available in `doc/html/`.
* A collection of tutorials are available on the [OPAM wiki](https://github.com/OCamlPro/opam/wiki/_pages).
  They are also available on http://opam.ocamlpro.com/ (in the "Documentation" menu), or in PDF
  in `doc/tutorials`.
* The developer manual is available in `doc/dev-manual/`.
