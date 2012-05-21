# OPAM - A package manager for OCaml

*Warning* do not use this software in production, it is not yet stable

## Prerequisites:

* ocaml

## (optional) Preparing the build

    make clone

This command will download and extract the following archives:

* http://www.ocamlpro.com/pub/cudf.tar.gz
* http://www.ocamlpro.com/pub/dose.tar.gz
* http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz
* http://www.ocamlpro.com/pub/ocaml-arg.tar.bz2
* http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz
* http://www.ocamlpro.com/pub/ocaml-re.tar.bz2

## Building OPAM

To compile `opam`, simply run:

    make

This will download the necessary archives if they are not already downloaded and then build OPAM.

## Tests

In order to run the test you should run:

```
make tests
```

## Documentation

* the API is available in html/

* The design documents are available in specs/