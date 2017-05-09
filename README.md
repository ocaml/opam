# opam - A package manager for OCaml

[![TravisCI Build Status](https://travis-ci.org/ocaml/opam.svg?branch=master)](https://travis-ci.org/ocaml/opam)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ocaml/opam?branch=master&svg=true)](https://ci.appveyor.com/project/AltGr/opam)

Opam is a source-based package manager for OCaml. It supports multiple simultaneous
compiler installations, flexible package constraints, and a Git-friendly development
workflow.

Opam was created and is maintained by [OCamlPro](http://www.ocamlpro.com).

To get started, checkout the [Install](http://opam.ocaml.org/doc/Install.html)
and [Usage](http://opam.ocaml.org/doc/Usage.html) guides.

## Compiling this repo

* Make sure you have OCaml and GNU make installed. If you don't have a recent
  enough version of OCaml (>= 4.01.0) at hand, see the next section.
* Run `./configure`
* Run `make lib-ext` as advertised by `./configure` if you don't have the
  dependencies installed. This will locally take care of all OCaml dependencies
  for you (downloading them, unless you used the inclusive archive we provide
  for each release).
* Run `make`
* Run `make install`

This is all you need for installing and using opam, but if you want to use the
`opam-lib` (to work on opam-related tools), you need to link it to installed
libraries, rather than use `make lib-ext` which would cause conflicts. It's
easier to already have a working opam installation in this case, so you can do
it as a second step.

* Make sure to have ocamlfind, ocamlgraph, cmdliner >= 0.9.8, jsonm, cudf,
  dose3, re >= 1.2.0, opam-file-format installed. Or run `opam install
  opam-lib --deps-only` if you already have a working instance. Re-run
  `./configure` once done
* Run `make libinstall` at the end

## Compiling without OCaml

`make cold` is provided as a facility to compile OCaml, then bootstrap opam.
You don't need need to run `./configure` in that case, but
you may specify `CONFIGURE_ARGS` if needed, e.g.:

```
make cold CONFIGURE_ARGS="--prefix ~/local"
```

NOTE: You'll still need GNU make.

## Bug tracker

Have a bug or a feature request ? Please open an issue on [our
bug-tracker](https://github.com/ocaml/opam/issues). Please search for existing
issues before posting, and include the output of `opam config report` and any
details that may help track down the issue.

## Documentation

#### User Manual

The main documentation entry point to opam is the user manual,
available using `opam --help`. To get help for a specific command, use
`opam <command> --help`.

#### Guides and Tutorials

A collection of guides and tutorials is available
[online](http://opam.ocaml.org/doc/Usage.html). They are generated from the
files in [doc/pages](https://github.com/ocaml/opam/tree/master/doc/pages).

#### API, Code Documentation and Developer Manual

A more thorough technical document describing opam and specifying the package
description format is available in the
[developer manual](http://opam.ocaml.org/doc/manual/dev-manual.html). `make
doc` will otherwise make the API documentation available under `doc/`.

## Community

Keep track of development and community news.

* Have a question that's not a feature request or bug report?
  [Ask on the mailing list](http://lists.ocaml.org/listinfo/infrastructure).

* Chat with fellow opamers on IRC. On the `irc.freenode.net` server,
  in the `#ocaml` or the `#opam` channel.

## Contributing

We welcome contributions ! Please use Github's pull-request mechanism against
the master branch of the [opam repository](https://github.com/ocaml/opam). If
that's not an option for you, you can use `git format-patch` and email us.

## Versioning

The release cycle respects [Semantic Versioning](http://semver.org/).

## Related repositories

- [ocaml/opam-repository](https://github.com/ocaml/opam-repository) is the
  official repository for opam packages and compilers. A number of non-official
  repositories are also available on the interwebs, for instance on
  [Github](https://github.com/search?q=opam-repo&type=Repositories).
- [opam2web](https://github.com/ocaml/opam2web) generates a collection of
  browsable HTML files for a given repository. It is used to generate
  http://opam.ocaml.org.
- [opam-rt](https://github.com/ocaml/opam-rt) is the regression framework for opam.
- [opam-publish](https://github.com/AltGr/opam-publish) is a tool to facilitate
  the creation, update and publication of opam packages.

## Copyright and license

The version comparison function in `src/core/opamVersionCompare.ml` is part of
the Dose library and Copyright 2011 Ralf Treinen. Some code in
`src/core/opamJson.ml` is taken from the documentation of `Jsonm` and is
Copyright 2012 Daniel C. Bünzli. See the specific file for details.

All other code is:

Copyright 2012-2016 OCamlPro
Copyright 2012 INRIA

All rights reserved. Opam is distributed under the terms of the GNU Lesser
General Public License version 2.1, with the special exception on linking
described in the file LICENSE.

Opam is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

