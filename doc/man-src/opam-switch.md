% OPAM-SWITCH(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-switch - Switch compiler version

# SYNOPSIS

*opam switch* -list

*opam switch* -current

*opam switch* \<alias\>

*opam switch* [-no-base-packages] -install \<alias\> [-alias-of \<alias\>]

*opam switch* -remove \<alias\>

*opam switch* [-no-base-packages] -clone \<alias\>

# DESCRIPTION

This command allows to switch between different compiler versions,
installing the compiler if *opam switch* is used to switch to that
compiler for the first time. The different compiler versions are
totally independant from each other, meaning that OPAM maintains a
separate state (e.g. list of installed packages...) for each. See
the EXAMPLES section to learn how to use this command.

# PARAMETERS

-list
:   Displays the list of available compilers.

-current
:   Displays the current compiler in use.

\<alias\>
:   Switch to the compiler version \<alias\>, installing it if it is not
    already installed. This is a shortcut to *opam switch -install
    \<alias\>*. The list of available versions is obtained by doing
    *opam switch -list*.

-install \<alias\>
:   Install a compiler version \<alias\>. See EXAMPLES.

-remove \<alias\>
:   Remove the compiler \<alias\>.

-clone \<alias\>
:   Install all packages installed in compiler \<alias\> into the
    currently selected compiler.

# OPTIONS

-no-base-packages
:   Will prevent *opam switch* from installing the base packages.


-alias-of \<alias\>
:   Will switch to compiler version \<alias\>, but use the name
    specified as an argument to -install instead of \<alias\>. This is
    useful to create multiple instances of the same compiler
    version. See EXAMPLES.

# EXAMPLES

* To install a brand new OCaml 4.00.0 and switch into it as the current
compiler, do:

`$ opam switch -install 4.00.0`

* To remove OCaml 3.12.1, do:

`$ opam switch -remove 3.12.1`

* To install OCaml 4.00.0 under alias "foo" such that you will have
  two instances of OCaml 4.00.0, each with its own environment
  (i.e. list of installed packages, and so on):

`$ opam switch -install foo -alias-of 4.00.0`

* To install in "foo" all packages that are installed in "4.00.0":

`$ opam switch foo
 $ opam switch -clone 4.00.0
`
# OPAM

Part of the opam(1) suite
