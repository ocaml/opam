# opam and other package managers: distributions list

This page tracks the state of binary packaging of OPAM on upstream
distributions. If you do package up OPAM for your various OS, please feel free
to add it below, update [this file](https://github.com/ocaml/opam/tree/master/doc/pages/Distribution.md)
and open a [pull request](https://github.com/ocaml/opam/compare).

The pages/files linked are the ones that give the best overview of the available
versions.

Those [_pkgs_](http://pkgs.org/search/opam) and
[_repology_](https://repology.org/project/opam/versions) pages may be used to
get an up-to-date overview of official packages on most Linux distributions.

### Official distribution packages

| Distribution                  | Latest OPAM    | Maintainer                                                                                                                    | Notes |
|-------------------------------|-------------   |-------------------------------------------------------------------------------------------------------------------------------|-------|
| Debian Linux (8,jessie)       | 1.2.0          | Mehdi Dogguy, Nicolas Braud-Santoni [@nbraud](https://www.github.com/nbraud)                                                  | [Package search](https://packages.debian.org/search?keywords=opam&searchon=names&suite=all&section=all)
| Debian Linux (9,stretch)      | 1.2.2          | Mehdi Dogguy, Nicolas Braud-Santoni [@nbraud](https://www.github.com/nbraud)                                                  | [Package search](https://packages.debian.org/search?keywords=opam&searchon=names&suite=all&section=all)
| Debian Linux (10,buster)      | 1.2.2          | Mehdi Dogguy, Nicolas Braud-Santoni [@nbraud](https://www.github.com/nbraud)                                                  | [Package search](https://packages.debian.org/search?keywords=opam&searchon=names&suite=all&section=all)
| Debian Linux (unstable, sid)  | 2.0.0          | Mehdi Dogguy, Nicolas Braud-Santoni [@nbraud](https://www.github.com/nbraud)                                                  | [Package search](https://packages.debian.org/search?keywords=opam&searchon=names&suite=all&section=all)
| Ubuntu Linux (14.04,trusty)   | 1.1.1          | -                                                                                                                             | [Package search](http://packages.ubuntu.com/search?keywords=opam&searchon=names&suite=all&section=all) - bwrap unavailable
| Ubuntu Linux (16.04,xenial)   | 1.2.2          | -                                                                                                                             | [Package search](http://packages.ubuntu.com/search?keywords=opam&searchon=names&suite=all&section=all) - bwrap unavailable
| Ubuntu Linux (17.04,artful)   | 1.2.2          | -                                                                                                                             | [Package search](http://packages.ubuntu.com/search?keywords=opam&searchon=names&suite=all&section=all)
| Ubuntu Linux (18.04,bionic)   | 1.2.2          | Anil Madhavapeddy [@avsm](https://www.github.com/avsm)                                                                        | [ppa post](https://discuss.ocaml.org/t/opam-2-0-experimental-ppas/2446)
| Homebrew (MacOS X)            | 2.0.0          | Anil Madhavapeddy [@avsm](https://www.github.com/avsm)                                                                        |
| Macports (MacOS X)            | 2.0.0          | Perry E. Metzger [@pmetzger](https://www.github.com/pmetzger)                                                                 | [Package definition](https://github.com/macports/macports-ports/blob/master/sysutils/opam/Portfile)
| Arch Linux                    | 1.1.1/2.0.0    | Vincent Bernardoff [@vbmithr](https://www.github.com/vbmithr), Pieter Goetschalckx [@314eter](https://www.github.com/314eter) | [Package search](https://aur.archlinux.org/packages/?O=0&C=0&SeB=nd&K=opam&outdated=&SB=n&SO=a&PP=50&do_Search=Go)
| Mageia Linux (cauldron)       | 2.0.0          | David Geiger [@david_david](https://www.github.com/david_david)                                                               | [Package definition](http://svnweb.mageia.org/packages/cauldron/opam/current/SPECS/opam.spec?view=markup)
| NixOS                         | 1.2.2/2.0.0    | Henry Till                                                                                                                    | [Package definitions](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/tools/ocaml/opam)
| Gnu Guix                      | 2.0.4          | -                                                                                                                             | [Package definition](https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/ocaml.scm#n42)
| FreeBSD                       | 2.0.3          | Hannes Mehnert [@hannesm](https://www.github.com/hannesm)                                                                     | [Package search](http://www.freebsd.org/cgi/ports.cgi?query=opam&stype=all)
| OpenBSD                       | 1.2.2          | Anil Madhavapeddy [@avsm](https://www.github.com/avsm)                                                                        | [Package page](http://ports.su/sysutils/opam,-main)
| Fedora 27 +                   | 2.0.0          | Ben Rosser [@TC01](https://www.github.com/TC01)                                                                               | [Package page](https://apps.fedoraproject.org/packages/opam)

### Other

| Distribution                | Latest OPAM | Maintainer                                                         | Notes |
|-----------------------------|-------------|------------------------------------                                |-------|
| Ubuntu Linux (PPA)          | 2.0.0       | Anil Madhavapeddy [@avsm](https://www.github.com/avsm)             | [Anil's official OPAM PPA](https://launchpad.net/~avsm) (http://anil.recoil.org/2013/09/30/travis-and-ocaml.html) and [discuss post](https://discuss.ocaml.org/t/opam-2-0-experimental-ppas/2446) 
| Exherbo Linux               | 1.1.1       | Nicolas Braud-Santoni [@nbraud](https://www.github.com/nbraud) (?) | [Package page](http://git.exherbo.org/summer/packages/dev-ocaml/opam/index.html) (_ocaml-unofficial_)
| CentOS (6,7)                | -           | Anil Madhavapeddy [@avsm](https://www.github.com/avsm)             |
| OpenSuse                    | -           | Anil Madhavapeddy [@avsm](https://www.github.com/avsm)             |
| Windows                     | -           | David Allsopp [@dra27](https://www.github.com/dra27)               | [MinGW repo](https://github.com/fdopen/opam-repository-mingw) - Andreas Hauptmann [@fdopen](https://www.github.com/fdopen)


If you can't find latest version packages for your distribution here, see [the
OPAM installation page](Install.html) for binaries or building from source.

[Docker containers](http://hub.docker.com/r/ocaml/opam) for severals
distributions and OCaml compiler versions are also available.
