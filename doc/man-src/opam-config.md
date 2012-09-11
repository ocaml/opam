% OPAM-CONFIG(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-config - Getting package configuration

# SYNOPSIS

*opam config* -env

*opam config* -list-vars

*opam config* -var \<var\>

*opam config* -subst \<filename\>...

*opam config* [-r] -I \<name\>...

*opam config* [-r] -bytecomp \<name\>[.\<lib\>]...

*opam config* [-r] -asmcomp \<name\>[.\<lib\>]...

*opam config* [-r] -bytelink \<name\>[.\<lib\>]...

*opam config* [-r] -asmlink \<name\>[.\<lib\>]...

# DESCRIPTION

This command uses opam state to output information on how to use
installed libraries, updating the user’s $PATH, and substitute
variables used in opam packages. Apart from *opam config -env*, most
of these commands are used by opam internally, and thus are of limited
interest for the user.

# OPTIONS

-env
:   Set the environment variables PATH, MANPATH, OCAML_TOPLEVEL_PATH
    and CAML_LD_LIBRARY_PATH according to the current selected
    compiler. The output of this command is meant to be evaluated by a
    shell, for example by doing *eval `opam config -env`*.

-list-vars
:   Return the list of all variables defined in installed packages.

-var \<var\>
:   Return the value associated with variable \<var\>

-subst \<filename\>...
:   Substitute variables in file \<filename\>.in to create \<filename\>

-I \<name\>...

:   Return the list of paths to include when compiling a project using
    the package \<name\>. Using the -r option will take into account the
    transitive closure of dependencies.

-bytecomp|-asmcomp|-bytelink|-asmlink \<name\>[.\<lib\>]...
:   Return the associated value for section \<lib\> in the config file
    for package \<name\>.

# OPAM

Part of the opam(1) suite
