% OPAM-INSTALL(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-install - Install packages

# SYNOPSIS

*opam install* \<package\>[.\<version\>]...

# DESCRIPTION

This command installs one or more packages to the currently selected
compiler. To install packages for another compiler, you need to switch
compilers using *opam switch*. You can remove installed packages with
*opam remove*, and list installed packages with *opam list
-installed*. See *opam pin* as well to understand how to manage
package versions.

This command will make opam use the dependency solver to compute the
transitive closure of dependencies to be installed, and will handle
conflicts as well. If the dependency solver returns more than one
solution, opam will ask which one should be selected. If dependencies
are to be installed, opam will ask if the installation should really
be performed.

# PARAMETERS

\<package\>[.\<version\>]...
:   Package(s) to be installed. An optional version can be specified,
    instructing opam to install a particular version of the specified
    package.

# EXAMPLES

* To install the last version of package "lwt", do:

`$ opam install lwt`

* To install "lwt" version 2.4.0, do:

`$ opam install lwt.2.4.0`

# SEE ALSO

**opam-remove**(1) **opam-switch**(1) **opam-list**(1) **opam-pin**(1)

# OPAM

Part of the opam(1) suite
