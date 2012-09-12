% OPAM-INIT(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-init - Creating a fresh client state

# SYNOPSIS

*opam init* [-kind \<kind\>] [\<repo-name\> \<repo-address\>]

# DESCRIPTION

This command creates a fresh client state, that is initialize opam
configuration in $HOME/.opam and setup a default repository by calling
*opam-\<kind\>-init*. Additional repositories can later be added by
using the *opam remote* command. The local cache of a repository state
can be updated by using *opam update*.

# PARAMETERS

\<repo-name\>
:    Name of the repository. Default value "default".

\<repo-address\>
:    Address of the repository. Default value "http://opam.ocamlpro.com"

# OPTIONS

-kind \<kind\>
:    Specify the kind of the repository to be set. Possible
     values: "http", "rsync", "git". Default value "http".


# SEE ALSO

**opam-remote**(1) **opam-update**(1) **opam-http-init**(1)
  **opam-rsync-init**(1) **opam-git-init**(1)

# OPAM

Part of the opam(1) suite
