# How to install opam

This page describes how to install and configure opam and [external
solvers](#ExternalSolvers). For further help on how to use opam,
either read `opam --help` or move on to the [Usage](Usage.html) guide.

## Upgrading from a previous version

Generally, you should just reproduce the same installation steps as for the
original installation: upgrade from your system's package manager, or re-run the
binary installer. Opam will automatically update its internal repository at
`~/.opam` on first run if needed (backup that if you may want to rollback the
upgrade without starting over).


## Binary distribution

The quickest way to get the latest opam up and working is to run:
```
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
```

This will simply check your architecture, download and install the proper
pre-compiled binary and run `opam init`.

We provide pre-compiled binaries for:
- Linux i686, amd64 and arm7
- OSX (intel 64 bits)
(other platforms are available using the other methods below)

You can pick your download
[here](https://github.com/ocaml/opam/releases/latest), and simply put it in your
PATH as `opam`, e.g.

```
sudo cp <downloaded file> /usr/local/bin
```


## Using your distribution's package system

This is generally the recommended way, when available and up-to-date. Here is a
list of supported distributions:

#### Archlinux

The [opam](https://www.archlinux.org/packages/community/x86_64/opam/)
package is available in the official distribution. To install it simply run:

```
pacman -S opam
```

If you'd like to use the development version there is an [opam-git](https://aur.archlinux.org/packages/opam-git/)
package available in the [AUR](https://wiki.archlinux.org/index.php/AUR).
Assuming you have [yaourt](https://aur.archlinux.org/packages/yaourt) installed just run the following command:

```
yaourt -S opam-git
```

#### Debian

Binary packages of opam are available for the
[stable](http://packages.debian.org/jessie/opam) (a bit outdated),
[testing](http://packages.debian.org/stretch/opam) and
[unstable](http://packages.debian.org/sid/opam) distributions, from the official
repositories. You should be set with:

```
apt-get install opam
```

There are also unofficial packages from the
[OpenSUSE Build Service](http://software.opensuse.org/download.html?project=home%3Aocaml&package=opam),
for the latest version, when not yet in the Debian repositories:

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

Opam builds via sources fine on OpenBSD 5.6 or earlier, and is available in the
ports and packages tree on OpenBSD 5.7 or higher.

```
cd /usr/ports/sysutils/opam
make install
```

Note that the `aspcud` external solver is not yet available on OpenBSD, so you
may see some odd upgrade attempts due to the use of the internal solver.

#### OSX

Opam packages for [homebrew](http://mxcl.github.com/homebrew/) and
[MacPorts](http://www.macports.org/) are available:

```
brew install opam                   # Homebrew, OSX Mavericks or later
brew install opam --without-aspcud  # Homebrew, OSX Mountain Lion or lower
port install opam                   # MacPort
```

See also
[howto setup Emacs.app](https://github.com/ocaml/opam/wiki/Setup-Emacs.app-on-macosx-for-opam-usage)
for Opam usage.

#### Ubuntu

> **Warning**: although there is an opam package available officially in
> "**Utopic**" (14.10), it's currently broken. Don't use it, see the
> [bug report](https://bugs.launchpad.net/ubuntu/+source/opam/+bug/1401346).
> The Ubuntu "Vivid" (15.04) package is fine.

We provide binary packages for "Precise" and "Trusty"

```
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install ocaml ocaml-native-compilers camlp4-extra opam
```

There are also PPAs available that are
[pinned to specific revisions](http://launchpad.net/~avsm) of OCaml and opam --
we use them for our
[automated testing](http://anil.recoil.org/2013/09/30/travis-and-ocaml.html).

If the command `add-apt-repository` is not available, you can install the
package `software-properties-common` with `apt-get install
software-properties-common`. Alternatively, you may manually edit the file
`/etc/apt/sources.list` to add the PPA for your Ubuntu release.


## From Sources

#### Getting the Sources

Sources of the latest stable version of opam are available on Github:

* [Opam releases on Github](https://github.com/ocaml/opam/releases)

You can also download the full archives, including opam dependencies (these
don't require any extra downloads, just the OCaml compiler -- 4.01.0 or later):

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
[`README.md`](https://github.com/ocaml/opam#readme) to get opam built and
installed from there.


#### Using ocamlbrew

[ocamlbrew](https://github.com/hcarty/ocamlbrew) is a script that can bootstrap
an OCaml environment including opam, from source. This option does not require
an existing OCaml installation, or a pre-compiled opam binary for your platform.
To bootstrap a new OCaml environment including opam, make sure that you have the
necessary pre-requisites installed to run ocamlbrew, and then run:

```
curl -kL https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install | env OCAMLBREW_FLAGS="-r" bash
```

# External Solvers

Resolving package installations in the presence of dependencies and
conflicts is known to be an [NP-complete
problem](https://hal.archives-ouvertes.fr/file/index/docid/149566/filename/ase.pdf).
Thankfully, a big effort has already been put into solving it very
efficiently: opam relies on this effort and delegates the solving
process to _external solvers_. opam integrates a simple solver, so it
can used without any extra dependencies, but for best results you
should have one of those solvers on your system:

- [aspcud](http://www.cs.uni-potsdam.de/wv/aspcud/) (recommended)
- [packup](http://sat.inesc-id.pt/~mikolas/sw/packup/)
- [mccs](http://www.i3s.unice.fr/~cpjm/misc/mccs.html)
- [p2Cudf](https://wiki.eclipse.org/Equinox/p2/CUDFResolver), which may be the
  easiest if dependencies are a problem, as it comes as a single jar file.
  [Dowload it](http://eclipse.org/equinox/p2/p2CUDF/org.eclipse.equinox.p2.cudf-1.14.jar)
  and set the solver configuration string to
  `java -jar <jarfile-location> -obj %{criteria}% %{input}% %{output}%`.

We recommend installing one through your packaging system whenever
possible: this should already have been taken care of if you installed
opam through your packaging system (see https://github.com/ocaml/opam/wiki/Distributions
for details for each distribution). If you have trouble installing an
external solver and have reliable network connectivity,
[Irill](http://www.irill.org/) kindly provides a ["Solver
farm"](http://cudf-solvers.irill.org/) which can be used as a remote
solver by opam.

Opam will detect the availability of `aspcud`, `packup` or `mccs` commands on
your system and should switch to using them directly. You can explicitly specify
which external solver to use by using the `--solver` command-line argument, the
`$OPAMEXTERNALSOLVER` environment variable, or the `solver:` field in the
`~/.opam/config` file.

External solvers also allow to specify [fine-grained
preferences](Specifying_Solver_Preferences.html). `aspcud`
is currently recommended because it supports a richer language of
[solver preferences](Specifying_Solver_Preferences.html#Yestherearedifferentversionsoftheuserpreferencelanguage)
giving opam more control over the requested solution.
