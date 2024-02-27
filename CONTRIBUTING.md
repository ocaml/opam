Contributions are most welcome in any form.

If you have bug reports and feature requests for the opam tool, please report them at:
* http://github.com/ocaml/opam/issues
  (please include the output of `opam config report` whenever possible)

For packaging issues (e.g. packages failed to build) please report them at
* http://github.com/ocaml/opam-repository/issues

## Working on the opam codebase

### Dependencies

To get the dependencies required to build and test opam there are several ways depending on what you have currently.

If you do not have OCaml already installed, you can simply call:
```
make cold
```

If you have OCaml but do not have opam already installed, you can simply call:
```
./configure --with-vendored-deps
```

If you have opam installed already, you can call:
```
opam install --deps-only -t .
```

### Building

As long as you have the dependencies, compiling opam is as simple as:
```
./configure
make
```

### Running the tests

Once built, you can run the testsuite using:
```
make test
```

If you want to run one test in particular:
```
make reftest-<name of the test>
```
or
```
make reftest-<name of the test> DUNE_ARGS="--auto-promote"
```
You can also run the test with `make reftest-<name of the test>, see the diff,
and call afterwards `dune promote` to promote the new output.

If you just want to run the quickest tests, you can run:
```
make quick-reftests
```

### Adding a new test

If you want to add a new test, create a new `.test` file in `tests/reftests` and run:
```
make reftest-gen
```

### Layout

The source code of opam is located in `src`. In this directory you will find subdirectories.
Each subdirectories have different puposes:
* `core`: is where all the lowest level common code used everywhere else is (opam stdlib, IOs, retrocompatibility with older versions of OCaml, code for version handling, …)
* `format`: depends on `opam-core` and is entirely dedicated to parsing opam files (higher level, but still using `opam-file-format`) and other internal config files
* `repository`: depends on `opam-format` and gathers code handling everything related to how to download and store repositories. The same code path is also reused when downloading a package for example.
* `solver`: depends on `opam-format` and gathers everything related to the various options for constraint solving that opam can use (custom search, dose, mccs, z3, 0install, …)
* `state`: depends on `opam-format` and gathers code dealing with the diverse states of opam (environments, depexts, internal state files handling, pinning, …)
* `client`: depends on all the above and is where the entry point for the opam binary and all the code handling all the opam subcommands is.

See https://opam.ocaml.org/doc/api/ for more detail.

The tests are all located in the `tests` directory.
From there:
* `bench` deals with benchmarks (used by ocurrent-bench)
* `reftests` is where the majority of the tests are. It consists of a series of cram-like tests (system testing) using a custom syntax parsed and driven by [`tests/reftests/run.ml`](https://github.com/ocaml/opam/blob/master/tests/reftests/run.ml#L12). See below for more details.

### Reftests inner-workings

Each `*.test` file in `tests/reftests` starts with either `N0REP0` (empty repository, note the zeros) or a valid hash coming from opam-repository that the test starts with.

After that, each line starting with `### ` will be parsed as either a command or a custom action as defined by `tests/reftests/run.ml`. Everything else is the output of the command or the input of the custom commands, with the exception of `# Return code <n> #` when the command returns a non-zero exit code.

### Releasing

See release/readme.md
