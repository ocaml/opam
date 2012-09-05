% OPAM-INSTALL(1) Opam Manual | Version 0.4
% OCamlPro
% September 03, 2012

# NAME

opam-install - Install packages

# SYNOPSIS

*opam install* \<package\>...

# DESCRIPTION

This command installs one or more packages to the currently selected
compiler. To install packages for another compiler, you need to switch
compilers using *opam switch*. You can remove installed packages with
*opam remove*, and list installed packages with *opam list
-installed*.

This command will make opam use the dependency solver to compute the
transitive closure of dependencies to be installed, and will handle
conflicts as well. If the dependency solver returns more than one
solution, opam will ask which one should be selected. If dependencies
are to be installed, opam will ask if the installation should really
be performed.

# OPTIONS

\<package\>...
:   Package(s) to be installed.

# SEE ALSO

**opam-remove**(1) **opam-switch**(1) **opam-list**(1)

# OPAM

Part of the opam(1) suite
