% OPAM-UPLOAD(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam-upload - Upload a new package to a remote repository

# SYNOPSIS

*opam upload* -opam \<opam-file\> -descr \<descr-file\> -archive
 \<name.version.tar.gz\> [-repo \<repository\>]

# DESCRIPTION

This command uploads an already built package to a remote repository,
if the remote repository is not read-only.

# PARAMETERS

-opam \<opam-file\>
:   Specify the .opam file that will be uploaded to repo://packages/name.version/opam

-descr \<descr-file\>
:   Specify the .descr file that will be uploaded to repo://packages/name.version/descr

-archive \<name.version.tar.gz\>
:   Specify the archive that will be uploaded to repo://archives/name.version+opam.tar.gz

# OPTIONS

-repo \<repository\>
:   Specify the repository to upload to. Defaults to the default repository.

# SEE ALSO

**opam-remote**(1) **opam-upgrade**(1)

# OPAM

Part of the opam(1) suite
