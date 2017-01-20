# opam FAQ

> This FAQ is for general questions about opam and its usage. You may also be
> interested in the more advanced [Tricks](Tricks.html) for specific use-cases
> or advanced users.

#### 🐫  opam fails, trying to reinstall already installed packages at first upgrade ?

It might be that you are still using Ubuntu "utopic", that shipped with a broken
opam package. That shouldn't happen and didn't in any stable opam release. See
[the bug-report on Ubuntu's launchpad](https://bugs.launchpad.net/ubuntu/+source/opam/+bug/1401346)
for the details.

The best fix is to upgrade your [opam](Install.html) using the community
packages.

---

#### 🐫  What is opam for ?

Easily installing, upgrading and managing your OCaml compiler(s), tools and
libraries. It's made of a tool, the
[opam package manager](https://github.com/ocaml/opam), and a
community-maintained
[package repository](https://github.com/ocaml/opam-repository).

Note that the tool itself is not specific to OCaml at its core, and could be
used for different systems using specific repositories (e.g. for the
[Coq theorem prover](http://coq.io/opam/))

---

#### 🐫  How to get, install and upgrade opam ?

See the [install guide](Install.html).
If upgrading, you can bootstrap using `opam install opam-devel`.

---

#### 🐫  Where is the manual ?

opam has git-like, hierarchical manpages. Try `opam --help` for a starting point.

Or get started from the [Usage](Usage.html) guide.

If you want to know more about opam packages, see the [Packaging Howto](Packaging.html).

The reference on the internals and file formats is in the [Manual](Manual.html).

You may also want to browse the [library APIs](api/).

---

#### 🐫  What changes does opam do to my filesystem ?

opam is designed to be run strictly as user (non-root), and apart for the
explicit options provided during `opam init`, only writes within `~/.opam` (and
`/tmp`). This directory -- the default "opam root" -- contains configuration,
various internal data, a cache of downloaded archives, and your OCaml
installations.

If you choose to create "local switches", the installation prefix will be put in
the specified directory with `/_opam/` appended. Nothing else will be changed.
The `opam build` command creates a local switch in the current directory, in the
form of a `_opam/` subdirectory.

---

#### 🐫  Why does ``opam init`` need to add stuff to my init scripts / why is ``eval $(opam env)`` needed ?

This is not strictly needed, but by updating your `PATH` and a few specific
environment variables, allows to:

1. Automatically find executables installed in opam (current switch)
2. Ensure the OCaml tools are going to look at the right places for installed
   libraries (e.g. when running `make`)

If you don't want to update your startup scripts or change your environment, you
can also:
- Remember to use `opam exec -- COMMAND` whenever you want to run `COMMAND`
  while being aware of the opam installation, or
- Run `eval $(opam env)` in the shell specifically when you are going to work on
  a project that uses your opam installation.

Just be careful, in this case, that running e.g. `make` while forgetting to do
this may lead to use a system-wide installation of OCaml, that may be in
conflict (and typically trigger "Inconsistent assumptions" errors in OCaml).

---

#### 🐫  What is a "switch" ?

An OCaml installation and a set of installed packages within an opam
installation. This can be used to keep different OCaml versions side-by-side, or
different sets of packages. See the [related
section](Usage.html#opamswitch) in the usage manual and
`opam switch --help`. The "prefix" for a given installation is simply
`~/.opam/<switch-name>`.

A switch is either based on a system-wide OCaml installation, or on a local
installation. In the former case, opam will need to recompile all packages when
your system compiler changes. In the latter case, OCaml will be downloaded and
compiled on creation of the switch.

Standard switches are held within `~/.opam`, but by using a directory as switch
handle (instead of a plain name), you may create switches that have their
contents in any local repository. They are put in a `_opam/` subdirectory in
this case, and it is safe to just remove that subdirectory to clear the switch.

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
opam switch export <file.export> --switch <old switch>
opam switch import <file.export> --switch <new switch>
```

The file format is human-readable, so you are free to edit the file before doing
the `import` if you need to customise the installation.

---

#### 🐫  I installed a package by hand / used ``ocamlfind remove`` / fiddled with the installed packages and opam is out of sync. Help !

Don't panic. opam assumes it's controlling what's below `~/.opam/<switch>`, but
there are several ways you can recover:
* `opam remove --force` will run the uninstall instructions even if the package
  is not registered as installed. Then retry `opam install`.
* `opam reinstall` will try to remove an installed package, but go on to
  re-installing even if that fails.
* If all else fails, you can re-install your current set of packages from
  scratch using `opam switch reinstall`.
* You can force opam to register an installation or removal _without actually
  performing anything_ using `opam install|remove --fake`. This is not
  recommended though, as your manual install may not be exactly equivalent to
  the one expected by other opam packages, and opam may later on trigger
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
they are outside the scope of opam.

opam metadata includes documentation about these external dependencies, on a
variety of systems/distributions, in the form of a `depext:` field. The `depext`
opam plugin can take care of them for you:

```
opam depext <opam-packages>
```

This should install `opam-depext` if needed, check your OS, and prompt to
install the system packages required by your packages or their dependencies,
through your OS's packaging system.

If that doesn't work...
* Check for hints printed by the failing package
* Dependencies for your specific system may not be known, but check the output
  of `opam list --rec --required-by <package>,<package>... --external`: it will
  list dependencies on all known systems and may get you in the right direction.
* Lookup the development packages corresponding to the error in your system's
  package repositories.

In any of these cases, that's useful information that was missing from the opam
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

#### 🐫  opam is prompting me to install or upgrade packages that I am not interested in, or doesn't install the latest version by default. Why ? What can I do ?

* You can be more explicit in your request (`opam upgrade PACKAGES`, `opam
  install 'PACKAGE>=VERSION' PACKAGE...`, etc.)
* Action resolution in a package set is known to be a NP-complete problem; opam
  uses state-of-the-art algorithms through an external, dedicated solver: make
  sure you have a recent
  [external solver installed](Install.html#ExternalSolvers)
* Another benefit of the external solvers is that they allow to be [quite
  expressive](Specifying_Solver_Preferences.html) on your expectations.

---

#### 🐫  When trying to install a new package, opam wants to remove or downgrade packages that I have installed. How to know why ?

There is likely a conflict between them or their dependencies and what you are
requesting, here is how to find out. We'll suppose you were trying to install
`foo` and `bar` got removed:

* `opam install foo bar`, if not possible, will tell you why.
* The above may find a solution by using older version of the packages, in that
  case try and force the latest versions thusly: `opam install foo.2.0 bar.1.1`
  (you can also use constraints like `'foo>=2.0'`).
* Like in the previous question, make sure you have
  [aspcud](http://potassco.sourceforge.net/) or another supported solver
  installed, the proposed solutions may not be as accurate without it.

---

#### 🐫  Where do I report Bugs, Issues and Feature Requests?

- Bug reports and feature requests for the opam tool should be reported on
[opam's issue-tracker](https://github.com/ocaml/opam/issues). Please include the
output of `opam config report` whenever applicable.

- Packaging issues or requests for a new package can be reported on the
[official repository's
issue-tracker](https://github.com/ocaml/opam-repository/issues).

- General queries for both the tool and the packages can be addressed on the
[OCaml-platform mailing-list](http://lists.ocaml.org/listinfo/platform) and
insights and evolution of opam internals can discussed on the [opam-devel
mailing-list](http://lists.ocaml.org/listinfo/opam-devel).

- You may also try IRC channel `#opam` on Freenode.

---

#### 🐫  How to link to libraries installed with opam ?

The standard way of doing this is to use
[ocamlfind](https://opam.ocaml.org/packages/ocamlfind), which is
orthogonal to opam: `ocamlfind query <lib>`.

Your libraries are installed to the directory returned by ``opam config var
lib``, which is by default `~/.opam/<switch>/lib`. Note that using `ocamlc`'s
option `-I +dir` will make `dir` relative to `lib/ocaml`, and will only work for
the libraries that ship with the compiler. Also, remember to add the dependency when
you package your project !

---

#### 🐫  How does opam tell which version of a package is newer ?

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

#### 🐫  opam wants to do reinstallations after update. Can I skip them ?

When opam detects meaningful changes in upstream packages, it marks them for
reinstallation, to be sure you get the latest fixes — repository managers
generally don't abuse modifying existing packages. You can check this with:

```
opam reinstall --list-pending
```

And, in case you want to skip them:
```
opam reinstall --forget-pending
```

You should only do this if you know the changes you are skipping, though.

---

#### 🐫  Some package installation fails with "ocamlfind: already installed"

Probably the package was either installed from outside of opam, or uncleanly
removed. You should try:

```
opam remove <package> --force
opam install <package>
```

This will process the uninstall instructions, even if opam has no knowledge of
the package being installed. You may also try to uninstall directly with
ocamlfind, or just remove the problematic files.
