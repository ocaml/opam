# OPAM - A package manager for OCaml

OPAM is a source-based package manager for OCaml. It supports multiple simultaneous
compiler installations, flexible package constraints, and a Git-friendly development
workflow.

## Installation

The recommanded way of installing OPAM is via your OS package manager.
See the [Quick Install](http://opam.ocamlpro.com/doc/Quick_Install.html) webpage.

## Upgrade from an older version of OPAM

OPAM stores its state under `~/.opam` whose format is (relatively)
stable between OPAM version.  If upgrade the version of OPAM your are
using, you only need to upgrade the OPAM binary and it will be able to
pick-up any pre-existing package and compiler installation.

## Documentation

### User Manual

The main documentation entry point to OPAM is the user manual,
available using `opam --help`. To get help for a specific command, use
`opam <command> --help`.

### Tutorials

A collection of tutorials are available online at http://opam.ocamlpro.com.
These tutorials are automatically generated from the [OPAM wiki](https://github.com/OCamlPro/opam/wiki/_pages) and
are also available in PDF format in the `doc/tutorials` directory.

### API, Code Documentation and Developpper Manual

The API documentation is available under the `doc/html/`
directory: all the modules have an associated (and properly
documentated) `.mli`.

The developer manual is available in the `doc/dev-manual/` directory.

