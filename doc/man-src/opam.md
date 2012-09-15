% OPAM(1) opam 0.6.0 | OPAM Manual
% OCamlPro
% 10/09/2012

# NAME

opam - The OCaml Package Manager

# SYNOPSIS

*opam* [--version] [--help] [--debug] [--verbose] [--quiet] [--yes] [--makecmd] [--root] [--no-checksums] \<command\>

# DESCRIPTION

OPAM is a package manager for OCaml. It uses the powerful mancoosi
tools to handle dependencies, including support for version
constraints, optional dependencies, and conflicts management. It has
support for different repository backends such as HTTP, rsync and
git. It handles multiple OCaml versions concurrently, and is flexible
enough to allow you to use your own repositories and packages in
addition of the ones it provides.

# OPTIONS

--version
:    Print version information and exit.

--help
:    Display help and exit.

--debug
:    Print debug messages on stdout.

--verbose
:    Be more verbose.

--quiet
:    Be less verbose.

--yes
:    Disable interactive mode and answer yes to all questions that would
     otherwise be asked to the user.

--makecmd \<command\>
:    Use \<command\> instead of the standard *make* (*make* on Linux,
     *gmake* on BSDs) to build packages in opam.

--root \<root-path\>
:    Change root path (default is ~/.opam).

--no-checksum
:    Do not verify packages checksum on download.

# COMMANDS

To obtain help about any of these commands, use *opam* \<command\>
--help

**init**
:    Initialize OPAM state in ~/.opam, or in the directory specified as
     argument of the --root option.

## Query packages

**list**
:    Display a list of packages.

**search**
:    Search repositories for packages.

**info**
:    Display information about packages.

## Manage packages

**install**
:    Install packages.

**reinstall**
:    Reinstall packages.

**upgrade**
:    Upgrade your packages.

**remove**
:    Remove packages.

## Manage repositories

**remote**
:    Manage remote repositories.

**update**
:    Fetch latest packages from remote repositories.

**upload**
:    Upload packages to repositories.

## Advanced

**pin**
:    Pin a package to a specific version.

**switch**
:    Manage multiple compiler installations.

**config**
:    Manage various configuration options about opam and packages.

# SEE ALSO

**curl**(1), **wget**(1), **git**(1), **rsync(1)**

# LINKS

* **http://opam.ocamlpro.com**
* **http://www.mancoosi.org**

# OPAM

Part of the opam(1) suite
