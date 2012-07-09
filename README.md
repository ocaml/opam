# OPAM - A package manager for OCaml

OPAM is a package manager for OCaml, based on the
[CUDF](http://mancoosi.org/cudf/) library developped by the
[Mancoosi](http://www.mancoosi.org/) project, which is, among other
things, used by Debian to manage their packages.
 
### Prerequisites:

* ocaml

### Compiling OPAM

To compile `opam`, simply run:

```
make
```

This will fetch the necessary archives if they are not already
downloaded and then build OPAM. If you just want to get the
necessary dependencies without compiling the project, run
`make clone`.

### Installing OPAM

To install opam in `/usr/local/bin` simply run:

```
sudo make install
```

If you want to install OPAM in a different path:

```
make BIN=$HOME/bin install
```

### Using opam

All of the OPAM state is held in the `~\.opam` directory in your home
directory, including compiler installations. You should never need to
switch to a root user to install packages. Package listings are
obtained through remote sources.

```
opam init
eval `opam config -env`
```

The first command creates `~\.opam` and set-up `opam.ocamlpro.com` as
default repository. The second command update your local environment
to use the packages installed by OPAM. For convenience, you can add
this line in your `~/.profile`.

```
opam list
```

As expected, this command lists all the available packages. We can now
install some packages (for instance `lwt`):

```
opam install lwt
```

## Documentation

Some technical documentations are available in `doc`:

* the API is available in `doc/html/`
* The design documents are available in `doc/specs/`
