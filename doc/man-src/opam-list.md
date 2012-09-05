% OPAM-LIST(1) Opam Manual | Version 0.4
% OCamlPro
% September 03, 2012

# NAME

opam-list - List packages

# SYNOPSIS

*opam list* [-short] [-installed] [\<packagepattern\>...]

# DESCRIPTION

This command displays the list of available packages, or the list of
installed packages if the -installed switch is used.

Unless the -short switch is used, the output format displays one
package per line, and each line contains the name of the package, the
installed version or "--" if the package is not installed, and a short
desctription. The full description can be obtained by doing *opam info
<package>*. You can search into the package list with the *opam
search* command.

# OPTIONS

\<packagepattern\>...
:   List only packages whose name matches at least one
    \<packagepattern\>. 

-short
:   Output the names of packages separated by one whitespace instead of
    using the usual formatting.

-installed
:   List installed packages only.

# SEE ALSO

**opam-search**(1) **opam-info**(1)

# OPAM

Part of the opam(1) suite
