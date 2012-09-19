% OPAM-SEARCH(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-search - Search into the package database

# SYNOPSIS

*opam search* [-short] [-installed] \<packagepattern\>...

# DESCRIPTION

This command displays the list of available packages that match one of
the \<packagepattern\>s specified as arguments. 

Unless the -short switch is used, the output format is the same as the
*opam list* command. It displays one
package per line, and each line contains the name of the package, the
installed version or "--" if the package is not installed, and a short
description. The full description can be obtained by doing *opam info
<package>*.

# OPTIONS

\<packagepattern\>...
:   Search for packages whose name or description matches (in the "glob"
    sense) at least one \<packagepattern\>. The case is not taken into
    account by default.

-case-sensitive
:   Force the search in case sensitive mode

-short
:   Output the names of packages separated by one whitespace instead of
    using the usual formatting.

-installed
:   List installed packages only.

# SEE ALSO

**opam-list**(1), **opam-info**(1)

# OPAM

Part of the opam(1) suite
