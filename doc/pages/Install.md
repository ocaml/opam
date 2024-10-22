# How to install opam

This page describes how to install and configure opam. For further help on how
to use opam, either read [`opam --help`](man/opam.html) or move on to the
[Usage](Usage.html) guide.

## Upgrading from a previous version

Generally, you should just reproduce the same installation steps as for the
original installation: upgrade from your system's package manager, or re-run the
binary installer. Opam will automatically update its internal repository at
`~/.opam` on first run if needed (if using our installer script, a backup can be
made automatically).

To upgrade shell scripts, and enable sandboxing, don't forget to run `opam init
--reinit -ni`.

Then see the [Upgrade guide](Upgrade_guide.html) to check the changes.


## Binary distribution

The quickest way to get the latest opam up and working is to run
[this script](https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh):
```
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```
or [this script](https://raw.githubusercontent.com/ocaml/opam/master/shell/install.ps1) on Windows using PowerShell:
```
Invoke-Expression "& { $(Invoke-RestMethod https://raw.githubusercontent.com/ocaml/opam/master/shell/install.ps1) }"
```

This will simply check your architecture, download and install the proper
pre-compiled binary, backup your opam data if from an older version, and run
`opam init`.

(If you have trouble with `curl`, just
[download the script](https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
and run `sh install.sh`)

We provide pre-compiled binaries for:
- Linux (amd64, arm64, ppc64le, s390x, riscv64, armhf, i686)
- macOS (amd64, arm64)
- FreeBSD (amd64)
- OpenBSD (amd64)
- Windows (amd64)
(other platforms are available using the other methods below)

If you don't like scripts, you can just pick your download
[here](https://github.com/ocaml/opam/releases), put it in your PATH as
`opam`, and set it as executable, e.g.

```
sudo install <downloaded file> /usr/local/bin/opam
```

> Note that this script is intended for end-users, not CI. For that purpose,
> you can use pre-built [Docker images for various
> configurations](https://hub.docker.com/r/ocaml/opam).

## Using your distribution's package system

This is generally the recommended way, **when available and up-to-date** (you
can check [here](Distribution.html) the latest
available release per distribution). Here is a list of supported distributions:

#### Arch Linux

[![badge](https://repology.org/badge/version-for-repo/arch/opam.svg)](https://repology.org/project/opam/versions)

The [opam](https://www.archlinux.org/packages/community/x86_64/opam/)
package is available in the official distribution. To install it simply run:

```
pacman -S opam
```

If you'd like to use the development version there is an [opam-git](https://aur.archlinux.org/packages/opam-git/)
package available in the [AUR](https://wiki.archlinux.org/index.php/Arch_User_Repository).
Assuming you have [yay](https://github.com/Jguer/yay) installed just run the following command:

```
yay -S opam-git
```

#### Debian

[![badge](https://repology.org/badge/version-for-repo/debian_stable/opam.svg)](https://repology.org/project/opam/versions) [![badge](https://repology.org/badge/version-for-repo/debian_testing/opam.svg)](https://repology.org/project/opam/versions) [![badge](https://repology.org/badge/version-for-repo/debian_unstable/opam.svg)](https://repology.org/project/opam/versions)

Binary packages of opam are available for the
[stable](http://packages.debian.org/jessie/opam),
[testing](http://packages.debian.org/stretch/opam) and
[unstable](http://packages.debian.org/sid/opam) distributions, from the official
repositories. You should be set with:

```
apt-get install opam
```

#### [Exherbo](http://exherbo.org)

The
[`dev-ocaml/opam`](http://git.exherbo.org/summer/packages/dev-ocaml/opam/index.html)
package can be installed with the command:

```
cave resolve -x dev-ocaml/opam
```

You might need to add the `::ocaml-unofficial` repository first:

```
cave resolve -x repository/ocaml-unofficial
```

#### [Fedora](http://fedoraproject.org), [CentOS](http://centos.org) and RHEL

[![Fedora 39](https://repology.org/badge/version-for-repo/fedora_39/opam.svg)](https://repology.org/project/opam/versions)

The opam package for Fedora can be installed with the command:

```
dnf install opam
```

There is not currently a package for CentOS/RHEL. You will need to use our
pre-built binaries, or build from sources.

#### Mageia

[![badge](https://repology.org/badge/version-for-repo/mageia_cauldron/opam.svg)](https://repology.org/project/opam/versions)

The opam package for Mageia can be installed with the command:

```
urpmi opam
```

#### OpenBSD

[![badge](https://repology.org/badge/version-for-repo/openbsd/opam.svg)](https://repology.org/project/opam/versions)

The opam package for OpenBSD can be installed with the command (since OpenBSD 5.7):

```
pkg_add opam
```

#### FreeBSD

[![badge](https://repology.org/badge/version-for-repo/freebsd/opam.svg)](https://repology.org/project/opam/versions)

Opam is available in the ports and packages tree on FreeBSD 11 or higher.

```
pkg install ocaml-opam
```

or to install from source:

```
cd /usr/ports/devel/ocaml-opam
make install
```

#### macOS

[![badge](https://repology.org/badge/version-for-repo/homebrew/opam.svg)](https://repology.org/project/opam/versions) [![badge](https://repology.org/badge/version-for-repo/macports/opam.svg)](https://repology.org/project/opam/versions)

Opam packages for [homebrew](http://mxcl.github.com/homebrew/) and [MacPorts](http://www.macports.org/) are available.

```
# Homebrew
brew install opam

# MacPort
port install opam
```

See also
[howto setup Emacs.app](https://github.com/ocaml/opam/wiki/Setup-Emacs.app-on-macosx-for-opam-usage)
for Opam usage.

#### Ubuntu

[![badge](https://repology.org/badge/version-for-repo/ubuntu_24_04/opam.svg)](https://repology.org/project/opam/versions)

```
apt install opam
```

#### Guix & Guix System

[![badge](https://repology.org/badge/version-for-repo/gnuguix/opam.svg)](https://repology.org/project/opam/versions)

The opam package for [guix](https://www.gnu.org/software/guix/) can be installed with the command:

```
# Guix
guix install opam
```

## From Sources

#### Getting the Sources

Sources of the latest stable version of opam are available on Github:

* [Opam releases on Github](https://github.com/ocaml/opam/releases)

You can also download the full archives, including opam dependencies (these
don't require any extra downloads, just the OCaml compiler -- 4.02.3 or later
for the latest version):

* [2.2.0](https://github.com/ocaml/opam/releases/download/2.2.0/opam-full-2.2.0-2.tar.gz)
 - MD5: ba94fd83c0e023b0d3c91857f28b8755
 - SHA384: 365eb949bfe18d0f189b35e620fa854628ab3e962721ee43488865456bf80da0dbba037b8fdb3830abb83f64b7c79106

Follow the instructions in the included
[`README.md`](https://github.com/ocaml/opam#readme) to get opam built and
installed from there.
