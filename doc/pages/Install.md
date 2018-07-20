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

Then see the [Upgrade guide](Upgrade_guide.html) to check the changes.


## Binary distribution

The quickest way to get the latest opam up and working is to run
[this script](https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh):
```
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```

This will simply check your architecture, download and install the proper
pre-compiled binary, backup your opam data if from an older version, and run
`opam init`.

(If you have troule with `curl`, just
[download the script](https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
and run `sh install.sh`)

We provide pre-compiled binaries for:
- Linux i686, amd64, arm7, arm64
- OSX (intel 64 bits)
(other platforms are available using the other methods below)

If you don't like scripts, you can just pick your download
[here](https://github.com/ocaml/opam/releases), put it in your PATH as
`opam`, and set it as executable, e.g.

```
sudo cp <downloaded file> /usr/local/bin/opam
sudo chmod a+x /usr/local/bin/opam
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

No native packages at the moment, you will need to use our pre-built binaries,
or build from sources.

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

#### FreeBSD

Opam is available in the ports and packages tree on FreeBSD 11 or higher.

```
cd /usr/ports/devel/ocaml-opam
make install
```

#### OSX

Opam packages for [homebrew](http://mxcl.github.com/homebrew/) and
[MacPorts](http://www.macports.org/) are available:

```
brew install opam                   # Homebrew
port install opam                   # MacPort
```

See also
[howto setup Emacs.app](https://github.com/ocaml/opam/wiki/Setup-Emacs.app-on-macosx-for-opam-usage)
for Opam usage.

#### Ubuntu

Ubuntu has native packages for opam:

```
apt install opam
```

## From Sources

#### Getting the Sources

Sources of the latest stable version of opam are available on Github:

* [Opam releases on Github](https://github.com/ocaml/opam/releases)

You can also download the full archives, including opam dependencies (these
don't require any extra downloads, just the OCaml compiler -- 4.02.3 or later
for the latest version):

* [2.0.0~rc](https://github.com/ocaml/opam/releases/download/2.0.0-rc/opam-full-2.0.0-rc.tar.gz)
  MD5: 6e89905dbe9203dee3e883b70e210285
  SHA384: 8a9ee03cdcd78a7d44e92c9b1c6e841605a49ecff4ebd977a632708ef6250f9f3ec488ecd1852f76d1b6cfc2d8ad9117
* [1.2.2](https://github.com/ocaml/opam/releases/download/1.2.2/opam-full-1.2.2.tar.gz)
  MD5: 7d348c2898795e9f325fb80eaaf5eae8
  SHA384: 3a0a7868b5f510c1248959ed350eecacfe1abd886e373fd31066ce10871354010ef057934df026e5fad389ead6c2857d

Follow the instructions in the included
[`README.md`](https://github.com/ocaml/opam#readme) to get opam built and
installed from there.

> Note that opam1.2.2 doesn't build from source with OCaml 4.06.0. Use this command to compile `lib_ext`
> ```
> OCAMLPARAM="safe-string=0,_" make lib-ext
> ```


#### Using ocamlbrew

[ocamlbrew](https://github.com/hcarty/ocamlbrew) is a script that can bootstrap
an OCaml environment including opam, from source. This option does not require
an existing OCaml installation, or a pre-compiled opam binary for your platform.
To bootstrap a new OCaml environment including opam, make sure that you have the
necessary pre-requisites installed to run ocamlbrew, and then run:

```
curl -kL https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install | env OCAMLBREW_FLAGS="-r" bash
```
