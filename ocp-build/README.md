# ocp-build

ocp-build is a simple tool to compile a project containing OCaml
sources. ocp-build uses a simple declarative language to describe the
project. A project is composed of packages (libraries and
executables), and packages are composed of source files and depend on
other packages.

ocp-build is able to do incremental and parallel compilation. The
descriptions of the packages can be stored in different files, so that
it is easy to move directories around without changing the description
of the project.

## Build and Installation

In this directory, just do "make". This will create an "ocp-build"
binary for your platform. Then, copy this file in your favorite places
for development programs (/usr/local/bin/, for example).

## Sources

The sources of ocp-build can be found in https://github.com/OCamlPro/typerex