# ocp-get - A package manager for OCaml

*Warning* do not use this software in production, it is not yet stable

## Prerequisites:

* ocaml

## (optional) Preparing the build

    make clone

This command will download and extract the following archives:

* http://www.ocamlpro.com/pub/cudf.tar.bz2
* http://www.ocamlpro.com/pub/dose.tar.bz2
* http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz
* http://www.ocamlpro.com/pub/ocaml-arg.tar.bz2
* http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz
* http://www.ocamlpro.com/pub/ocaml-re.tar.bz2

## Building ocp-get

To compile `ocp-get`, simply run:

    make

## Tests

In order to run the test you should run:

```
make tests-runserver
```

This will run the server in debug mode. It will show the IP address it
is listening to. If this address is different of `127.0.0.1` you
should set the environment variable `LOCALHOST` accordingly.

You can then open a new terminal window and either run:

```
make tests
```

## Bootstrapping 

Running:

```
make -C tests ocpget
```

will compile `ocp-get` once again.But this time, the complete installation will be orchestrated
by the existing `ocp-get` itself (ie. this will bypass the `clone` step).
