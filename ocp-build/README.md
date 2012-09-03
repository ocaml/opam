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

We have not yet released the sources of this version (although the
sources of an old version are available in TypeRex version 1), because
it depends on many of our internal libraries, and we need to clean the
code of these libraries before releasing them.

## The Project Description Language

### A simple example of library

### A simple example of program

## ocp-build options
