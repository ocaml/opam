% OPAM-REMOTE(1) Opam Manual | Version 0.4
% OCamlPro
% September 03, 2012

# NAME

opam-remote - Manage OPAM repositories

# SYNOPSIS

*opam remote* -list

*opam remote* -rm \<name\>

*opam remote* -add [-kind \<kind\>] \<name\> \<address\>

# DESCRIPTION

This command is used to manage OPAM repositories. To synchronize OPAM
with the last versions of the packages available in remote
repositories, *opam update* should be used.

# PARAMETERS

-list
:   Lists all repositories used by OPAM.

-rm \<name\>
:   Removes the repository named \<name\> from the list of repositories used by OPAM.

-add \<name\> \<address\>
:   Add the repository \<name\> available at address \<address\> to
    the list of repositories used by OPAM. The kind of the repository
    can be specified with the *-kind* option, otherwise it will be
    determined automatically.

# OPTIONS

-kind \<kind\>
:   Specify the kind of the repository to be added. If not used, *opam
    remote add* will try to figure out automatically what kind of
    repository to use.


# SEE ALSO

**opam-update**(1)

# OPAM

Part of the opam(1) suite
