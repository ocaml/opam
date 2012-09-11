% OPAM-PIN(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-pin - Pin a package to a specific version

# SYNOPSIS

*opam pin* \<package\> \<version\>

*opam pin* \<package\> \<url\>

*opam pin* \<package\> none

# DESCRIPTION

This command will "pin" a package to a specific version, or use a
specific source path for installing and upgrading the package. Using
*opam pin \<package\> none* will undo the "pinned" status of
\<package\>.

# PARAMETERS

\<package\>
:   Specify package to pin or unpin.

\<version\>
:   Pin the selected package to a specific version.

\<url\>
:   Use the specified url for installing or upgrading the package. This
    means that from now on, *opam install* and *opam upgrade* will use the
    specified url to install (respectively upgrade) the package.

none
:   Unpin the package, i.e. use the default sources and version for the
    package.

# OPAM

Part of the opam(1) suite
