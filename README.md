# opam - A Package Manager for OCaml

|master|2.0|2.1|
|--|--|--|
|[![GH actions](https://github.com/ocaml/opam/workflows/Builds,%20tests%20&%20co/badge.svg)](https://github.com/ocaml/opam/actions?query=workflow%3A%22Builds%2C+tests+%26+co%22+branch%3Amaster) | [![2.0 GH actions](https://github.com/ocaml/opam/workflows/Builds,%20tests%20&%20co/badge.svg?branch=2.0)](https://github.com/ocaml/opam/actions?query=workflow%3A%22Builds%2C+tests+%26+co%22+branch%3A2.0) | [![2.1 GH actions](https://github.com/ocaml/opam/workflows/Builds,%20tests%20&%20co/badge.svg?branch=2.1)](https://github.com/ocaml/opam/actions?query=workflow%3A%22Builds%2C+tests+%26+co%22+branch%3A2.1) |

Opam is a source-based package manager for OCaml. It supports multiple simultaneous
compiler installations, flexible package constraints, and a Git-friendly development
workflow.

Opam was created and is maintained by [OCamlPro](http://www.ocamlpro.com).

To get started, checkout the [Install](http://opam.ocaml.org/doc/Install.html)
and [Usage](http://opam.ocaml.org/doc/Usage.html) guides.

## Compiling This Repo

Either from an existing opam installation, use
`opam pin add .` if you cloned locally the repository,
`opam pin add git+https://github.com/ocaml/opam` otherwise, or:

* Make sure you have the required dependencies installed:
  - GNU make
  - OCaml >= 4.08 (or see [below](#compiling-without-ocaml))
  - A C++ compiler (unless building without a solver, see `./configure --without-mccs`)
* Run `./configure`. If you don't have the dependencies installed, this will
  locally take care of all OCaml dependencies for you (downloading them, unless
  you used the inclusive archive we provide for each release).
* Run `make`
* Run `make install`

This is all you need for installing and using opam, but if you want to use the
`opam-lib` (to work on opam-related tools), you need to link it to installed
libraries. It's easier to already have a working opam installation in this case,
so you can do it as a second step.

* Make sure to have `ocamlfind`, `ocamlgraph`, `cmdliner` >= 1.0.0, `cudf` >= 0.7,
  `dose3` >= 6.1, `re` >= 1.9.0, `opam-file-format` installed. Or run `opam install
  . --deps-only` if you already have a working instance. Re-run
  `./configure` once done
* Run `make libinstall` at the end

_Note_: If you install on your system (without changing the prefix), you will
need to install as root (`sudo`). As `sudo` does not propagate environment
variables, there will be some errors. You can use `sudo -E "PATH=$PATH"` in order
to ensure you have a good environment for install.

## Developer Mode

If you are developing opam, you may enable developer features by including the
`--enable-developer-mode` parameter with `./configure`.

## Compiling on Native Windows

Cygwin (https://www.cygwin.com/setup-x86_64.exe) is always required to build opam on
Windows.

The following Cygwin packages are required:
* From Devel - `make`
* From Devel - `patch` (not required if OCaml and all required packages are
                        pre-installed)
* From Devel - `autoconf`
* From Devel - `curl`
* From Devel - `mingw64-i686-gcc-g++` & `mingw64-x86_64-gcc-g++` (not required if
                                                                 building with MSVC)

Alternatively, having downloaded Cygwin's setup program, Cygwin can be installed
using the following command line:

```
setup-x86_64 --root=C:\cygwin64 --quiet-mode --no-desktop --no-startmenu --packages=make,mingw64-i686-gcc-g++,mingw64-x86_64-gcc-g++,patch,autoconf,curl
```

The `--no-desktop` and `--no-startmenu` switches may be omitted in order to create
shortcuts on the Desktop and Start Menu, respectively. Executed this way, setup will
still be interactive, but the packages will have been preselected. To make setup
fully unattended, choose a mirror URL from https://cygwin.com/mirrors.lst and add
the `--site` switch to the command line
(e.g., `--site=https://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/`).

It is recommended that you set the `CYGWIN` environment variable to
`nodosfilewarning winsymlinks:native`.

Cygwin is started either from a shortcut or by running:

```
C:\cygwin64\bin\mintty -
```

We recommended you build opam outside Cygwin's root
(so in `/cygdrive/c/...`). From an elevated Cygwin shell, edit `/etc/fstab` and
ensure that the file's content is exactly:

```
none /cygdrive cygdrive noacl,binary,posix=0,user 0 0
```

The change is the addition of the `noacl` option to the mount instructions for
`/cygdrive` and this stops from Cygwin from attempting to emulate POSIX permissions
over NTFS (which can result in strange and unnecessary permissions showing up in
Windows Explorer). It is necessary to close and restart all Cygwin terminal windows
after changing `/etc/fstab`.

Opam is able to be built **without** a preinstalled OCaml compiler. For the MSVC
ports of OCaml, the Microsoft Windows SDK 7 or later or Microsoft Visual Studio is
required (https://www.microsoft.com/en-gb/download/details.aspx?id=8442 - either x86
or x64 may be installed, as appropriate to your system). It is not necessary to
modify PATH, INCLUDE, or LIB. Opam's build system will automatically detect the
required changes.

If OCaml is not preinstalled, run:
```
make compiler [OCAML_PORT=mingw64|mingw|msvc64|msvc|auto]
```
The `OCAML_PORT` variable determines which flavour of Windows OCaml is compiled;
`auto` will attempt to guess. As long as GCC is **not** installed in Cygwin
(i.e., the native C compiler *for Cygwin*), `OCAML_PORT` does not need to be
specified and `auto` will be assumed. Once the compiler is built, you may run:
```
./configure --with-vendored-deps
```
and build opam as above.

## Compiling Without OCaml

`make cold` is provided as a facility to compile OCaml, then bootstrap opam.
You don't need to run `./configure` in that case, but
you may specify `CONFIGURE_ARGS` if needed. E.g.,:

```
make cold CONFIGURE_ARGS="--prefix ~/local"
make cold-install
```

NOTE: You'll still need GNU make.

## Bug Tracker

Have a bug or a feature request? Please open an issue on [our
bug-tracker](https://github.com/ocaml/opam/issues). Please search for existing
issues before posting and include the output of `opam config report` and any
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

#### API, Code Documentation, and Developer Manual

A more thorough technical document describing opam and specifying the package
description format is available in the
[developer manual](https://opam.ocaml.org/doc/Manual.html). `make
doc` will otherwise make the API documentation available under `doc/`.

## Community

Keep track of development and community news.

* Have a question that's not a feature request or bug report?
  [Ask on the mailing list](http://lists.ocaml.org/listinfo/infrastructure).

* Chat with fellow opamers on IRC. On the `irc.libera.chat` server,
  in the `#ocaml` or the `#opam` channel.

## Contributing

We welcome contributions! Please use GitHub's pull-request mechanism against
the master branch of the [opam repository](https://github.com/ocaml/opam). If
that's not an option for you, you can use `git format-patch` and email us.

## Versioning

The release cycle of the opam binary respects [Semantic Versioning](http://semver.org/).
Note however that the version ordering used for user packages managed by opam
follows the Debian definition (more details in [this
section](https://opam.ocaml.org/doc/Manual.html#version-ordering) of the user manual).

## Related Repositories

- [ocaml/opam-repository](https://github.com/ocaml/opam-repository) is the
  official repository for opam packages and compilers. A number of unofficial
  repositories are also available on the interwebs, for instance on
  [Github](https://github.com/search?q=opam-repo&type=Repositories).
- [opam2web](https://github.com/ocaml/opam2web) generates a collection of
  browsable HTML files for a given repository. It is used to generate
  http://opam.ocaml.org.
- [opam-rt](https://github.com/ocaml/opam-rt) is the regression framework for opam.
- [opam-publish](https://github.com/ocaml-opam/opam-publish) is a tool to facilitate
  the creation, update, and publication of opam packages.

## Copyright and license

The version comparison function in `src/core/opamVersionCompare.ml` is part of
the Dose library and Copyright 2011 Ralf Treinen.

All other code is:

Copyright 2012-2020 OCamlPro
Copyright 2012 INRIA

All rights reserved. Opam is distributed under the terms of the GNU Lesser
General Public License version 2.1, with the special exception on linking
described in the file LICENSE.

Opam is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
