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

You can then open a new terminal window and run:

```
make tests
```

## Bootstrapping 

`ocp-get` is able to bootstrap itself, using an external index repository.

You can test the bootrap process by:

* compiling `ocp-get` following the steps described previously
* run `./ocp-get init https://github.com/samoht/opam-test.git` at the root path
  of your cloned repository
* run `./ocp-get install ocp-get`: this will download, compile and install all the
  needed dependencies. The new binary will be in `~/.opam/<ocaml-version/bin/ocp-get`.
  If you want to use, you may want to use update your `$PATH` variable.
