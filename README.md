# OPAM - A package manager for OCaml

OPAM is a package manager for OCaml, based on the
[CUDF](http://mancoosi.org/cudf/) library developped by the
[Mancoosi](http://www.mancoosi.org/) project, which is, among other
things, used by Debian to manage their packages.
 
### Prerequisites:

* ocaml
* curl or wget
* git
* rsync

### Compiling OPAM

To compile `opam`, simply run:

```
$ make
```

This will fetch the necessary archives if they are not already
downloaded and then build OPAM. If you just want to get the
necessary dependencies without compiling the project, run
`make clone`.

If you don't have `curl` installed on your system, you can run
`make FETCH=wget clone` before `make`.

### Installing OPAM

To install opam in `/usr/local/bin` simply run:

```
$ sudo make install
```

If you want to install OPAM in a different path:

```
$ make BIN=$HOME/bin install
```

### Using opam

All of the OPAM state is held in the `~\.opam` directory in your home
directory, including compiler installations. You should never need to
switch to a root user to install packages. Package listings are
obtained through remote sources.

```
$ opam init
$ eval `opam config -env`
```

The first command creates `~\.opam` and set-up `opam.ocamlpro.com` as
default repository. 
The second command updates your local environment
to use the packages installed by OPAM. For convenience, you can add
this line in your `~/.profile`.

```
$ opam list
```

As expected, this command lists all the available packages. We can now
install some packages (for instance `lwt`):

```
$ opam install lwt
```

OPAM is able to track optional dependencies. This mean that installing an
optional dependency of a package will recompile the package and all its
forward dependencies. For instance:

```
$ opam install react`
```

will install `react`, will recompile `lwt` (with the right `./configure` options)
and all the packages which depend on `lwt`.


### Installing on other OCaml compiler

OPAM also manages meta-data about OCaml compilers. So in order to install a new version
of the compiler, you can run:

```
$ opam switch 4.00.0
$ eval `opam config -env`
```

The first command will download and install ocaml-4.00.0, and the second command will
update the environment variables. You can then install packages -- they will be installed
on a different under `~/.opam/4.00.0`.

In order to come-back to the system-wide OCaml installation, simply run:

```
$ opam switch system
$ eval `opam config -env`
```

You can use `opam switch -list` to display the list of available compilers.

### Version pinning

```
$ opam pin <package> </local/path>
```

This command will use the content of `</local/path>` to compile `<package>`. This means
that the next time you will do `opam install <package>`, the compilation process will be
using a mirror of `</local/path>` instead of downloading the archive. This also means that
any modification to `</local/path>` will be picked up by `opam update`, and thus `opam upgrade`
will recompile `<package>` (and its forward dependencies) if needed.

To unpin a package, simply run:

```
$ opam pin <package> none
```

You can also pin a package to a specific version: `opam pin <package> <version>`

### Repositories

OPAM supports multiple repositories.

```
$ opam remote -list
```


## Documentation

Some technical documentations are available in `doc`:

* the API is available in `doc/html/`
* The design documents are available in `doc/specs/`
