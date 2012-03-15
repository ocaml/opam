# ocp-get - A package manager for OCaml

*Warning* do not use this software in production, it is not yet stable

# Prerequisites:

* ocaml
* wget
* tar

To compile `ocp-get`, simply run:

```
make
```

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

