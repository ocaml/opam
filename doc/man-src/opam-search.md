% OPAM-SEARCH(1) Opam Manual | Version 0.4
% OCamlPro
% September 03, 2012

# NAME

opam-search - Search into the package database

# SYNOPSIS

*opam search* \<packagepattern\>...

# DESCRIPTION

This command displays the list of available packages that match one of
the \<packagepattern\>s specified as arguments. This command is almost
identical to the *opam list* command, except that packages get
selected if their descriptions match the pattern, in addition of their
names.

# OPTIONS

\<packagepattern\>...
:   Search for packages whose name or description matches at least one
    \<packagepattern\>. The case is not taken into account.

# SEE ALSO

**opam-list**(1) **opam-info**(1)

# OPAM

Part of the opam(1) suite
