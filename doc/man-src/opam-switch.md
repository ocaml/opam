% OPAM-SWITCH(1) Opam Manual | Version 0.4
% OCamlPro
% September 03, 2012

# NAME

opam-switch - Switch compiler version

# SYNOPSIS

*opam switch* -list

*opam switch* [-no-base-packages] [-clone] [-alias \<alias\>] \<version\>

# DESCRIPTION

This command allows to switch between different compiler versions,
installing the compiler if *opam switch* is used to switch to that
compiler for the first time. The different compiler versions are
totally independant from each other, meaning that OPAM maintains a
separate state (e.g. list of installed packages...) for each.

# PARAMETERS

\<version\>
:   Switch to the compiler version \<version\>. The list of available
    versions is obtained by doing *opam switch -list*

# OPTIONS

-list
:   Displays the list of available compilers.

-no-base-packages
:   Will prevent *opam switch* from installing the base packages.

-clone
:   Will try to install the packages that were installed before
    switching (that are not currently installed). If not set, *opam
    switch* will not install any package, thus creating a brand new
    state for the newly selected compiler.

-alias \<alias\>
:   Will switch to requested compiler version, but use the alternative
    name \<alias\> instead of the the default compiler version’s
    name. This is useful to create multiple instances of the same compiler
    version.

# OPAM

Part of the opam(1) suite
