# OPAM FAQ

> This FAQ is for general questions about OPAM and its usage. You may also be
> interested in the more advanced [Tricks](Tricks.html) for specific use-cases
> or advanced users.

#### 🐫  OPAM fails, trying to reinstall already installed packages at first upgrade ?

**Ubuntu "utopic" currently ships with a broken OPAM package**, that shouldn't
happen and didn't in any stable OPAM release. See
[the bug-report on Ubuntu's launchpad](https://bugs.launchpad.net/ubuntu/+source/opam/+bug/1401346)
for the details.

The best fix is to install [OPAM 1.2](Install.html) using the community
packages.

---

#### 🐫  What is OPAM for ?

Easily installing, upgrading and managing your OCaml compiler(s), tools and
libraries. It's made of a tool, the
[OPAM package manager](https://github.com/ocaml/opam), and a
community-maintained
[package repository](https://github.com/ocaml/opam-repository).

---

#### 🐫  How to get, install and upgrade OPAM ?

See the [install guide](Install.html).

---

#### 🐫  Where is the manual ?

OPAM has git-like, hierarchical manpages. Try `opam --help` for a starting point.

Or get started from the [Usage](Usage.html) guide.

If you want to know more about OPAM packages, see the [Packaging Howto](Packaging.html).

Last but not least, the reference on the file formats and more is in the
[Developper Guide](http://opam.ocaml.org/doc/manual/dev-manual.html).

---

#### 🐫  Gasp! `opam init` gives me screens fulls of errors about upgrading !

This is a glitch, at init only, on the older OPAM 1.1. We recommend upgrading
to 1.2 -- but 1.1 is still supported through a compatibility layer on the
repository, just use the following initialisation command to workaround the
errors:

```
opam init https://opam.ocaml.org/1.1
```

If your repository is already initialised, `opam update` should automatically
redirect to 1.1 and you should be fine.

---

#### 🐫  What changes does OPAM do to my filesystem ?

OPAM is designed to be run strictly as user (non-root), and apart for the
explicit options provided during `opam init`, only writes within `~/.opam` (and
`/tmp`). This directory -- the default "OPAM root" -- contains configuration,
various internal data, a cache of downloaded archives, and your OCaml
installations.

---

#### 🐫  Why does ``opam init`` need to add stuff to my init scripts / why is ``eval $(opam config env)`` needed ?

You need two things when you install OPAM packages: to have their libraries
accessible, and to access binary programs. To provide those, OPAM needs to setup
a few ocaml-related environment variables, and to prepend to your PATH variable.

Of course, you may choose not to let OPAM change anything at `opam init`, and
run `eval $(opam config env)` yourself whenever you will be needing it.

---

#### 🐫  What is a "switch" ?

An OCaml installation and a set of installed packages within an OPAM
installation. This can be used to keep different OCaml versions side-by-side, or
different sets of packages. See the [related
section](Usage.html#opamswitch) in the usage manual and
`opam switch --help`. The "prefix" for a given installation is simply
`~/.opam/<switch-name>`.

A switch is either based on a system-wide OCaml installation, or on a local
installation. In the former case, OPAM will need to recompile all packages when
your system compiler changes. In the latter case, OCaml will be downloaded and
compiled on creation of the switch.

---

#### 🐫  Can I work on different switches at the same time in different shells ?

Yes. Use one of:

```
eval $(opam config env --switch <switch>)        # for the current shell
opam config exec --switch <switch> -- <command>  # for one command
```

This only affects the environment.

---

#### 🐫  Can I get a new switch with the same packages installed ?

Yes. Use:

```
opam switch export file.export  # from the previous switch
opam switch <new switch>
opam switch import file.export
```

OPAM might fail if you had packages installed that are not compatible with the
OCaml version in your new switch. In that case, you'll need to remove them from
the `file.export` file by hand (the format is straight-forward, one line per
package).

---

#### 🐫  I installed a package by hand / used ``ocamlfind remove`` / fiddled with the installed packages and OPAM is out of sync. Help !

Don't panic. OPAM assumes it's controlling what's below `~/.opam/<switch>`, but
there are several ways you can recover:
* `opam remove --force` will run the uninstall instructions even if the package
  is not registered as installed. Then retry `opam install`.
* `opam reinstall` will try to remove an installed package, but go on to
  re-installing even if that fails.
* If all else fails, you can re-install your current set of packages from
  scratch using `opam switch reinstall`.
* You can force OPAM to register an installation or removal _without actually
  performing anything_ using `opam install|remove --fake`. This is not
  recommended though, as your manual install may not be exactly equivalent to
  the one expected by other OPAM packages, and OPAM may later on trigger
  reinstallations or upgrades of the package. Don't complain if you mess up your
  installation using this! If you want to control how a package is installed or
  modify it, the right way is `opam pin`.
* You shouldn't have to, but if you want to restart from scratch, just delete
  `~/.opam` and get back to `opam init`

---

#### 🐫  What are the minimum requirements ?

1GB of memory should be all you need. It was reported that you may run into
problems with 512MB of RAM and no swap. Of course, software packages themselves
may be more greedy.

---

#### 🐫  Some package fail during compilation, complaining about missing dependencies ("m4", "libgtk", etc.)

They probably depend on system, non-OCaml libraries: they need to be installed
using your system package manager (apt-get, yum, pacman, homebrew, etc.) since
they are outside the scope of OPAM. These external dependencies ("depexts") are
in the process of being documented in the package repository, and the
`opam-depext` tool should take care of that for you:

```
opam install depext
opam depext <packages>
```

If that doesn't work...
* Check for hints printed by the failing package
* Dependencies for your specific system may not be known, but check the output
  of `opam list --rec --required-by <package>,<package>... --external`: it will
  list dependencies on all known systems and may get you in the right direction.
* Lookup the development packages corresponding to the error in your system's
  package repositories.

In any of those case, that's useful information that was missing from the OPAM
repository: we would really appreciate that you take a minute to save others the
trouble of looking by filling an issue in
[the opam-repository tracker](https://github.com/ocaml/opam-repository/issues),
with your system details, the output of `opam depext --flags`, and the solution,
if you found it. Thanks!

---

#### 🐫  I have weird checksum errors: where do they come from ?

First of all, you should make sure your repositories are up-to-date:

```
opam update
```

If this isn't enough, or if you get the checksum errors while running `opam
init`, this could be caused by a badly configured proxy cache that is serving
stale files. To clear your proxy cache, you can use `wget --no-cache
<remote-file>` and retry.

As a last resort, you can bypass the checksum checks using `--no-checksums`.

---

#### 🐫  OPAM is prompting me to install or upgrade packages that I am not interested in, or doesn't install the latest version by default. Why ? What can I do ?

* You can be more explicit in your request (`opam upgrade PACKAGES`, `opam
  install 'PACKAGE>=VERSION' PACKAGE...`, etc.)
* Action resolution in a package set is known to be a NP-complete problem; OPAM
  uses state-of-the-art algorithms through an external, dedicated solver: make
  sure you have [an external solver installed](Install.html#ExternalSolvers)
* Another benefit of the external solvers is that they allow to be [quite
  expressive](Specifying_Solver_Preferences.html) on your expectations.

---

#### 🐫  When trying to install a new package, OPAM wants to remove or downgrade packages that I have installed. How to know why ?

There is likely a conflict between them or their dependencies and what you are
requesting, here is how to find out. We'll suppose you were trying to install
`foo` and `bar` got removed:

* `opam install foo bar`, if not possible, will tell you why.
* The above may find a solution by using older version of the packages, in that
  case try and force the latest versions thusly: `opam install foo.2.0 bar.1.1`
  (you can also use constraints like `'foo>=2.0'`).
* Like in previous question, make sure you have
  [aspcud](http://potassco.sourceforge.net/) installed, the proposed solutions
  may not be as accurate without it.

---

#### 🐫  Where do I report Bugs, Issues and Feature Requests?

- Bug reports and feature requests for the OPAM tool should be reported on
[OPAM's issue-tracker](https://github.com/ocaml/opam/issues).

- Packaging issues or requests for a new package can be reported on the
[official repository's
issue-tracker](https://github.com/ocaml/opam-repository/issues).

- General queries for both the tool and the packages can be addressed on the
[OCaml-platform mailing-list](http://lists.ocaml.org/listinfo/platform) and
insights and evolution of OPAM internals can discussed on the [OPAM-devel
mailing-list](http://lists.ocaml.org/listinfo/opam-devel).

- You may also try IRC channel `#opam` on Freenode.

---

#### 🐫  How to link to libraries installed with OPAM ?

The standard way of doing this is to use
[ocamlfind](http://opam.ocaml.org/packages/ocamlfind/ocamlfind.1.5.1/), which is
orthogonal to OPAM: `ocamlfind query <lib>`.

Your libraries are installed to the directory returned by ``opam config var
lib``, which is by default `~/.opam/<switch>/lib`. Note that using `ocamlc`'s
option `-I +dir` will make `dir` relative to `lib/ocaml`, and will only work for
the libraries that ship with the compiler. Also, remember to add the dependency when
you package your project !

---

#### 🐫  How does OPAM tell which version of a package is newer ?

We use the basics of the [version
ordering](https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version)
from Debian:

- version strings are sliced into alternate, possibly empty non-digit / digit
  sequences, always starting with a non-digit sequence.
- those are sorted lexicographically, using resp. ASCII (with symbol > letter)
  and number order. For example `a` gives `["a"]`, and `1` gives `["";1]`, so
  `a` is latest (`"" < "a"`).
- the `~` character is special as it sorts even before the end of sequence. It's
  most convenient for pre-releases, allowing `1.0~beta` to be before `1.0`.

Here is an example of an ordered sequence: `~~`, `~`, `~beta2`, `~beta10`, `0.1`,
`1.0~beta`, `1.0`, `1.0-test`, `1.0.1`, `1.0.10`, `dev`, `trunk`

---

#### 🐫  What does the `--jobs` option do ? It doesn't seem to enable parallel builds.

It does, but at the _package_ granularity: it will only be noticeable if you
build independent packages in the same command. Each package has its own build
commands, and it's up to them to enable parallelism.

If you are a packager, you may use the `jobs` opam variable, e.g. `make
"-j%{jobs}%"`.

---

#### 🐫  What is "mixed mode VC pin" ?

When you pin a package to a local directory that is under version control, we
used to have OPAM use the version control mechanism to synchronise the package.
This turned to be often confusing, because it wouldn't "see" your latest changes
until you committed them. Pinning the directory as a raw path isn't perfect
either, because it makes OPAM register all files, including temporary files or
build artifacts.

The idea of "mixed mode", which is the default for version-control pins in
OPAM 1.2.1, is to take the best of both worlds: OPAM will synchronise only files
under version control, but at their current state on the filesystem. You may
just need to remember to register them if you added new files (e.g. `git add`).

If for some reason you want the old behaviour, use one of:
* `--kind path` for raw filesystem pinning
* an address of the form `vc-controlled-dir#branch`, typically `#master`
* on Git you can use `#HEAD` to always get to the currently checked out branch
  (which used to be the default, and still is for remote gits)

---

#### 🐫  OPAM wants to do reinstallations after update. Can I skip them ?

OPAM registers the need to recompile packages when they had meaningful changes
in the repository, to guarantee consistency and allow to propagate fixes to
already installed packages ; the official repository maintainers generally don't
abuse this. There is no built-in command to reset them, in purpose, but removing
the file `~/.opam/<switch>/reinstall` is enough to make OPAM forget about them.
It's quite safe, but don't report issues if you did and your system is not up to
date, obviously.

---

#### 🐫  Some package installation fails with "ocamlfind: already installed"

Probably the package was either installed from outside of OPAM, or uncleanly
removed. You should try:

```
opam remove <package> --force
opam install <package>
```

This will process the uninstall instructions, even if OPAM has no knowledge of
the package being installed. You may also try to uninstall directly with
ocamlfind, or just remove the problematic files.
