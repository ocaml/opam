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
bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
```
or [this script](https://raw.githubusercontent.com/ocaml/opam/master/shell/install.ps1) on Windows using PowerShell:
```
Invoke-Expression "& { $(Invoke-RestMethod https://opam.ocaml.org/install.ps1) }"
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
- NetBSD (amd64)
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

## Using your system's package manager

This is generally the recommended way, **when available and up-to-date**. You
can check the following table, and the way to install opam thereafter:

[![repology package status](https://repology.org/badge/vertical-allrepos/opam.svg?exclude_unsupported=1)](https://repology.org/project/opam/versions)

#### Arch Linux

```
pacman -S opam
```

#### Debian and Ubuntu

```
apt install opam
```

#### Fedora

```
dnf install opam
```

#### Mageia

```
urpmi opam
```

#### Alpine Linux

```
apk add opam
```

#### OpenBSD

```
pkg_add opam
```

#### FreeBSD

```
pkg install ocaml-opam
```

#### macOS

Opam packages for [Homebrew](https://brew.sh/) and [MacPorts](http://www.macports.org/) are available.

```
# Homebrew
brew install opam

# MacPort
port install opam
```

#### Guix & Guix System

```
guix install opam
```

#### Windows

```
winget install Git.Git OCaml.opam
```

WinGet the Windows Package Manager is available on Windows 11, modern versions of Windows 10, and Windows Server 2025.
See the [official documentation](https://learn.microsoft.com/en-us/windows/package-manager/winget/) for additional info.

## From Sources

#### Getting the Sources

Sources of the latest stable version of opam are available on Github:

* [Opam releases on Github](https://github.com/ocaml/opam/releases)

You can also download the full archives, including opam dependencies (these
don't require any extra downloads):

* [2.5.0](https://github.com/ocaml/opam/releases/download/2.5.0/opam-full-2.5.0.tar.gz)
 - MD5: 300a43aca6a20c984100ec6855c2f979
 - SHA512: 4e47b03e22de3ab975f1e14b4a6b8e98f159a065be8f9d56f110e6a2a5275b42d4646350b230f912b057b768f182225db24d5343da41a716d0e9cdc1cb435c54

Follow the instructions in the included
[`README.md`](https://github.com/ocaml/opam#readme) to get opam built and
installed from there.
