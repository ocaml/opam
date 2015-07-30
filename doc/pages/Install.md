> NOTE: 1.2.2 is just being released. It may take a few days before binary
> distributions catch up

# How to install OPAM

This page describes how to install and configure OPAM and [external
solvers](#Externalsolvers). For further help on how to use OPAM,
either read `opam --help` or move on to the [Usage](Usage.html) guide.

## Upgrading from a previous version

Generally, you should just reproduce the same installation steps as for the
original installation: upgrade from your system's package manager, or re-run the
binary installer. OPAM will automatically update its internal repository at
`~/.opam` on first run if needed (backup that if you may want to rollback the
upgrade without starting over).

## Using your distribution's package system

This is the recommended way, when available. Here is a list of supported
distributions:

#### Archlinux

The [opam](http://aur.archlinux.org/packages.php?ID=62127) and
[opam-git](http://aur.archlinux.org/packages.php?ID=62387) packages are
available in the [AUR](https://wiki.archlinux.org/index.php/AUR). Replace `opam`
with `opam-git` in the following instruction to get the development version:

```
yaourt -S opam
```

#### Debian

Binary packages of OPAM are available for the
[testing](http://packages.debian.org/jessie/opam) and
[unstable](http://packages.debian.org/sid/opam) distributions, from the official
repositories. You should be set with:

```
apt-get install opam
```

There are also unofficial packages from the
[OpenSUSE Build Service](http://software.opensuse.org/download.html?project=home%3Aocaml&package=opam),
for wheezy users or if the latest version isn't yet in the official repository:

```
wget http://download.opensuse.org/repositories/home:ocaml/Debian_7.0/Release.key
apt-key add - < Release.key
echo 'deb http://download.opensuse.org/repositories/home:/ocaml/Debian_7.0/ /' >> /etc/apt/sources.list.d/opam.list
apt-get update
```

#### [Exherbo](http://exherbo.org)

The
[`dev-ocaml/opam`](http://git.exherbo.org/summer/packages/dev-ocaml/opam/index.html) package can be installed with the command:

```
cave resolve -x dev-ocaml/opam
```

You might need to add the `::ocaml-unofficial` repository first:

```
cave resolve -x repository/ocaml-unofficial
```

#### [Fedora](http://fedoraproject.org), [CentOS](http://centos.org) and RHEL

RPMs for Fedora, CentOS and Red Hat Enterprise Linux are available with
instructions on the
[OpenSUSE Build Server](http://software.opensuse.org/download.html?project=home%3Aocaml&package=opam).

#### Mageia

The opam package for Mageia can be installed with the command:

```
urpmi opam
```

#### OpenBSD

OPAM builds via sources fine on OpenBSD 5.6 or earlier, and is available in the
ports and packages tree on OpenBSD 5.7 or higher.

```
cd /usr/ports/sysutils/opam
make install
```

Note that the `aspcud` external solver is not yet available on OpenBSD, so you
may see some odd upgrade attempts due to the use of the internal solver.

#### OSX

OPAM packages for [homebrew](http://mxcl.github.com/homebrew/) and
[MacPorts](http://www.macports.org/) are available:

```
brew install opam                   # Homebrew, OSX Mavericks or later
brew install opam --without-aspcud  # Homebrew, OSX Mountain Lion or lower
port install opam                   # MacPort
```

See also
[howto setup Emacs.app](https://github.com/ocaml/opam/wiki/Setup-Emacs.app-on-macosx-for-opam-usage)
for OPAM usage.

#### Ubuntu

> **Warning**: although there is an OPAM package available officially in
> "**Utopic**" (14.10), it's currently broken. Don't use it, see the
> [bug report](https://bugs.launchpad.net/ubuntu/+source/opam/+bug/1401346).
> The Ubuntu "Vivid" (15.04) package is fine.

We provide binary package for "Precise" and "Trusty"

```
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install ocaml ocaml-native-compilers camlp4-extra opam
```

There are also PPAs available that are
[pinned to specific revisions](http://launchpad.net/~avsm) of OCaml and OPAM --
we use them for our
[automated testing](http://anil.recoil.org/2013/09/30/travis-and-ocaml.html).

If the command `add-apt-repository` is not available, you can install the
package `software-properties-common` with `apt-get install
software-properties-common`. Alternatively, you may manually edit the file
`/etc/apt/sources.list` to add the PPA for your Ubuntu release.


## Binary installer

Pre-compiled versions for most common architectures and OSes are available on
[the Github "releases" page](https://github.com/ocaml/opam/releases/latest). We
also provide a very simple installer script that will automatically download the
right version for you, put it in your binary directory and initialize it.

Download
[opam_installer.sh](https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh)
and run it as follows:

```
sh <path to>/opam_installer.sh /usr/local/bin
```

You can also specify which version of OCaml you want to install:

```
sh ./opam_installer.sh /usr/local/bin 3.12.1 # Install the latest OPAM and OCaml 3.12.1
sh ./opam_installer.sh /usr/local/bin system # Install the latest OPAM using the system compiler (if any)
```

Note that this only gives you the basic `opam` command, not other binaries like
`opam-admin`. You can install those through OPAM afterwards.

## From Sources

#### Getting the Sources

Sources of the latest stable version of OPAM are available on Github:

* [OPAM releases on Github](https://github.com/ocaml/opam/releases)

You can also download the full archives, including OPAM dependencies (these
don't require any extra downloads, just the OCaml compiler -- 3.12.1 or later):

* [1.2.2](https://github.com/ocaml/opam/releases/download/1.2.2/opam-full-1.2.2.tar.gz)
  MD5: 7d348c2898795e9f325fb80eaaf5eae8
  SHA1: 415ff0506378ab8dfa428fcd0aff3aa28337d93b
* [1.2.1](https://github.com/ocaml/opam/releases/download/1.2.1/opam-full-1.2.1.tar.gz)
  MD5: 04e8823a099ab631943952e4c2ab18fc
  SHA1: 189e309ee0659723abaaad5f887f5caf89b34422
* [1.2.0](https://github.com/ocaml/opam/releases/download/1.2.0/opam-full-1.2.0.tar.gz)
  MD5 (opam-full-1.2.0.tar.gz) = 17cc252c6c80fc503c4878eac8d123e7
* [1.1.2](https://github.com/ocaml/opam/releases/download/1.1.2/opam-full-1.1.2.tar.gz)
  MD5 (opam-full-1.1.2.tar.gz) = ba2a4136b65003c04d905de786f3c3ab
* [1.1.1](https://github.com/ocaml/opam/releases/download/1.1.1/opam-full-1.1.1.tar.gz)
  MD5 (opam-full-1.1.1.tar.gz) = a7bebe947b3e6c1c10ccafabb839d374
* [1.1.0](http://www.ocamlpro.com/pub/opam-full-1.1.0.tar.gz)
  MD5 (opam-full-1.1.0.tar.gz) = d6e2f56b10c0be73b5677963e6659d24

Follow the instructions in the included
[`README.md`](https://github.com/ocaml/opam#readme) to get OPAM built and
installed from there.


#### Using ocamlbrew

[ocamlbrew](https://github.com/hcarty/ocamlbrew) is a script that can bootstrap
an OCaml environment including OPAM, from source. This option does not require
an existing OCaml installation, or a pre-compiled OPAM binary for your platform.
To bootstrap a new OCaml environment including OPAM, make sure that you have the
necessary pre-requisites installed to run ocamlbrew, and then run:

```
curl -kL https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install | env OCAMLBREW_FLAGS="-r" bash
```

# External Solvers

Resolving package installations in the presence of dependencies and
conflicts is known to be an [NP-complete
problem](https://hal.archives-ouvertes.fr/file/index/docid/149566/filename/ase.pdf).
Thankfully, a big effort has already been put into solving it very
efficiently: OPAM relies on this effort and delegates the solving
process to _external solvers_. OPAM integrates a simple solver, so it
can used without any extra dependencies, but for best results you
should have one of those solvers on your system:

- aspcud (recommended)
- packup
- mccs (no built-in support at the moment, but may be used with the following
  solver configuration string: `mccs -i %{input}% -o %{output}%
  -lexagregate[%{criteria}%]`.)

We recommend installing one through your packaging system whenever
possible: this should already have been taken care of if you installed
OPAM through your packaging system. If you have trouble installing an
external solver and have reliable network connectivity,
[Irill](http://www.irill.org/) kindly provides a ["Solver
farm"](http://cudf-solvers.irill.org/) which can be used as a remote
solver by OPAM.

If you use the internal solver only, the following symptoms may be
sign that you need an external solver: very bad upgrade proposals, or
dependency solving becoming very slow.

OPAM will detect the availability of `aspcud` or `packup` commands on
your system and should switch to using them directly. You can
explicitly specify which external solver to use by using the `--solver
<foo>` command-line argument, or
`$OPAMEXTERNALSOLVER` environment variable.

External solvers also allow to specify [fine-grained
preferences](Specifying_Solver_Preferences.html).
