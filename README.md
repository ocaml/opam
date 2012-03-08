# ocp-get - A package manager for OCaml

*Warning* do not use this software in production, it is not yet stable

# Prerequisites:

* ocaml
* wget

To compile `ocp-get`, simply run:

```
make
```

## Tests

In order to run the test you should run:

```
ocp-get-server --debug
```

This will run the server in debug mode. It will show the IP address it
is listening to. You can open an other terminal and run:

```
make tests
```

