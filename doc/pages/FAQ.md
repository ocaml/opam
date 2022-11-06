# opam FAQ

> This FAQ is for general questions about opam and its usage. You may also be
> interested in the more advanced [Tricks](Tricks.html) for specific use-cases
> or advanced users.

#### 🐫  What is opam for?

Easily installing, upgrading and managing your OCaml compiler(s), tools and
libraries. It's made of a tool, the
[opam package manager](https://github.com/ocaml/opam), and a
community-maintained
[package repository](https://github.com/ocaml/opam-repository).

Note that the tool itself is not specific to OCaml at its core, and could be
used for different systems using specific repositories (e.g. for the
[Coq theorem prover](http://coq.io/opam/))

---

#### 🐫  How to get, install and upgrade opam?

See the [install guide](Install.html).

If upgrading, you can bootstrap using `opam install opam-devel`, and follow the
instructions.

---

#### 🐫  Where is the manual?

opam has git-like, hierarchical
[manpages](https://opam.ocaml.org/doc/2.0/man/opam.html). Try `opam --help` for
a starting point.

Or get started from the [Usage](Usage.html) guide.

If you want to know more about opam packages, see the [Packaging Howto](Packaging.html).

The full reference on the internals and file formats is in the [Manual](Manual.html).

You may also want to browse the [library APIs](api/).

---

#### 🐫  What changes does opam do to my filesystem?

opam is designed to be run strictly as user (non-root), and except for the
explicit options provided during `opam init`, opam only writes within `~/.opam`
(and `/tmp`). This directory — the default "opam root" — contains configuration,
various internal data, a cache of downloaded archives, and your OCaml
installations.

If you choose to create "local switches", the installation prefix will be put in
the specified directory with `/_opam/` appended. Nothing else will be changed.

Please note, however, that programs you install using opam won't themselves be
bound by any restrictions.

On Linux, and since opam 2.0.0~rc2, package instructions (build, install,
remove) are also run in a sandbox and guaranteed not to affect your system.

---

#### 🐫  Why does opam require ``bwrap``?

Since opam 2.0.0~rc2, opam uses `bwrap` on Linux to run package instructions in
a sandbox. This restricts their access to parts of the system (e.g., forbid
access to operating system, user data, or network). See the
[bubblewrap page](https://github.com/projectatomic/bubblewrap) for details. A
similar mechanism is used on macOS, using the `sandbox-exec` command.

We use `bwrap` to prevent packages from writing outside their allotted
filesystem space or use the network. For example, build commands have restricted
write access, restrained to their dedicated build directory and `/tmp`. These
sandboxing instructions are specified in the built-in configuration, that you
can display with `opam init --show-default-opamrc`:

```
init-scripts: ["sandbox.sh" """ [...] """] {os = "linux"}

wrap-build-commands: ["%{hooks}%/sandbox.sh" "build"] {os = "linux"}
wrap-install-commands: ["%{hooks}%/sandbox.sh" "install"] {os = "linux"}
wrap-remove-commands: ["%{hooks}%/sandbox.sh" "remove"] {os = "linux"}
```

Sandboxing provides an important level of security, and should always be kept
enabled. Note, however, that:
- Only the _package_ build/install/remove commands are protected: if you install
  a program using opam and execute it, it will run with your standard user
  rights.
- If your installation uses unusual paths (opam root outside `HOME`, system
  folder, etc.), since `2.0.1` you can use the environment variable
  `OPAM_USER_PATH_RO` to have them handled by then sandbox script, e.g. This
  variable format is the same as `PATH`, you can add it in your shell
  configuration file, e.g `export OPAM_USER_PATH_RO=/rw/usrlocal:/media`.
  Contained paths are added as read-only.
  Note: As of opam 2.2.0, the sandbox will always mount every directories readonly
  and OPAM_USER_PATH_RO thus does nothing.
- If needed, for special cases like unprivileged containers, sandboxing can be
  disabled on `opam init` with the `--disable-sandboxing` flag (only for
  non-initialised opam). Or by using a [custom
  `opamrc`](Manual.html#configfield-wrap-build-commands). Use wisely, broken
  Makefiles that run `rm -rf /` [__do__
  happen](https://github.com/ocaml/opam/issues/3231).

---

#### 🐫  Why does ``opam init`` need to add stuff to my init scripts / why is ``eval $(opam env)`` needed?

This is not strictly needed, but by updating your `PATH` and a few specific
environment variables, one can:

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

#### 🐫  What is a "switch"?

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

#### 🐫  Can I work on different switches at the same time in different shells?

Yes. Use one of:

```
eval $(opam env --switch <switch> --set-switch)  # for the current shell
opam exec --switch <switch> -- <command>         # for one command
```

This only affects the environment.

---

#### 🐫  Can I get a new switch with the same packages installed?

Yes. Use:

```
opam switch export <file.export> --switch <old switch>
opam switch import <file.export> --switch <new switch>
```

The file format is human-readable, so you are free to edit the file before doing
the `import` if you need to customise the installation.

### 🐫 How to share my working switch setup for a specific package ?

When working on a project, it is sometimes needed to share a set of
dependencies that you know (locally) the project is working with. You can share
this set by generating a _locked_ opam file. Ths is easily done using the [`lock`
command](man/opam-lock.html): it creates an opam file with a `depends:` field
populated with all dependencies, at their exact version in the current
(working) switch. You can then share this `opam.locked` file, or check it
in your version-control system.

```shell
$ opam lock <pkg> # generate a <pkg>.opam.lock file
$ opam install --locked <pkg> # use <pkg> locked file, if present
```

---

#### 🐫  I installed a package by hand / used ``ocamlfind remove`` / fiddled with the installed packages and opam is out of sync. Help!

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

#### 🐫  What are the minimum requirements?

1GB of memory should be all you need. It was reported that you may run into
problems with 512MB of RAM and no swap. Of course, compiling the packages may
need more.

---

#### 🐫  Some packages fail during compilation, complaining about missing dependencies ("m4", "libgtk", etc.)

> NOTE: since opam 2.1.0, the following is directly handled by opam, without
> relying on a plugin.

They probably depend on system, non-OCaml libraries: they need to be installed
using your system package manager (apt-get, yum, pacman, homebrew, etc.) since
they are outside the scope of opam.

opam metadata includes documentation about these external dependencies, on a
variety of systems/distributions, in the form of a
[`depexts:`](https://opam.ocaml.org/doc/2.0/Manual.html#opamfield-depexts)
field. Opam should print the required system dependencies, as documented for
your OS, upon failure, and the `depext` opam plugin can take care of installing
them for you:

```
opam depext <opam-packages>
```

This should install `opam-depext` if needed and prompt to install the system
packages required by your opam packages or their dependencies, through your OS's
packaging system.

If that doesn't work...
* Check for hints printed by the failing package
* Lookup the development packages corresponding to the error in your system's
  package repositories.
* Dependencies for your specific system may not be known, but check the output
  of `opam list --rec --resolve <package>,<package>... --columns name,depexts:`:
  it will list dependencies on all known systems and may get you in the right
  direction.

In any of these cases, that's useful information that was missing from the opam
repository: we would really appreciate that you take a minute to save others the
trouble of looking by filling an issue in
[the opam-repository tracker](https://github.com/ocaml/opam-repository/issues),
with your system details, the output of `opam config report`, and the solution,
if you found it. Thanks!

---

#### 🐫  I have weird checksum errors: where do they come from?

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

#### 🐫  opam is prompting me to install or upgrade packages that I am not interested in, or doesn't install the latest version by default. Why? What can I do?

* You can be more explicit in your request (`opam upgrade PACKAGES`, `opam
  install 'PACKAGE>=VERSION' PACKAGE...`, etc.). The latest version may not be
  available on your system, in this case this will tell you why.
* See how to set [solver preferences](External_solvers.html) that could match your intentions better than the defaults
* Check for pending reinstallations `opam reinstall --list-pending`

---

#### 🐫  When trying to install a new package, opam wants to remove or downgrade packages that I have installed. How to know why?

There is likely a conflict between them or their dependencies and what you are
requesting, here is how to find out. We'll suppose you were trying to install
`foo` and `bar` got removed:

* `opam install foo bar`, if not possible, will tell you why.
* The above may find a solution by using older version of the packages, in that
  case try and force the latest versions thusly: `opam install foo.2.0 bar.1.1`
  (you can also use constraints like `'foo>=2.0'`).

---

#### 🐫  Where do I report Bugs, Issues and Feature Requests?

- Bug reports and feature requests for the opam **tool** should be reported on
[opam's issue-tracker](https://github.com/ocaml/opam/issues). Please include the
output of `opam config report` whenever applicable.

- Packaging issues or requests for a new package can be reported on the
[official repository's
issue-tracker](https://github.com/ocaml/opam-repository/issues).

- General queries for both the tool and the packages can be addressed on the
[OCaml-platform mailing-list](http://lists.ocaml.org/listinfo/platform) and
insights and evolution of opam internals can discussed on the [opam-devel
mailing-list](http://lists.ocaml.org/listinfo/opam-devel).

- https://discuss.ocaml.org is a good place for community assistance.

- You may also try IRC channel `#ocaml` on Libera.Chat.

---

#### 🐫  How to link to libraries installed with opam?

The standard way of doing this is to use
[ocamlfind](https://opam.ocaml.org/packages/ocamlfind), which is orthogonal to
opam: `ocamlfind query <lib>`. If you use [dune](https://github.com/ocaml/dune),
this should be completely transparent.

Your libraries are installed to the directory returned by ``opam var lib``,
which is by default `~/.opam/<switch>/lib`. Note that using `ocamlc`'s option
`-I +dir` will make `dir` relative to `lib/ocaml`, and will only work for the
libraries that ship with the compiler. Also, remember to add the dependency when
you package your project!

---

#### 🐫  How does opam tell which version of a package is newer?

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

#### 🐫  What does the `--jobs` option do? It doesn't seem to enable parallel builds.

It does, but at the _package_ granularity: it will only be noticeable if you
build independent packages in the same command. Each package has its own build
commands, and it's up to them to enable parallelism.

If you are a packager, you may use the `jobs` opam variable, e.g. `make
"-j%{jobs}%"`.

---

#### 🐫  opam wants to do reinstallations after update. Can I skip them?

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

---

#### 🐫  opam is slow on top of NFS. How can I make it faster?

opam root is usually located in the `home` directory, which, on top of NFS,
slows down opam operations. Locating opam root in `/tmp` is not a solution:
you could lose your opam configuration at each reboot.

You can use the [`nfsopam`](https://github.com/UnixJunkie/nfsopam) script to
have the best of both worlds: persistence of NFS directories and fast operations
of local directories.

---

#### 🐫  What does the `--cli` option do? Should I be using it everywhere?

`--cli` was introduced in opam 2.1 to deal with changes in the command line
interface between releases. It tells opam to interpret the command line as a
specific version, in particular it means that new options or options which
have had their meaning altered will not be available, or will be behave as they
did in that version. It only affects the command-line - it does not, for
example, stop a root from being upgraded from an older version to the current
version.

We recommend using it in scripts (and programs which call opam) since they can
then expect to work seamlessly with future versions of the opam client. It's
also a good idea to use it in blog posts, or other documentation you may share,
since it allows copy-and-paste to work reliably (a user with a newer version of
opam should have no issues and a user with an older opam gets a clearer error
message).

We don't recommend using it in day-to-day use of opam in the shell, because
you'll be typing more and you won't get to notice exciting new features! If the
behaviour of a command or option is altered, and you write something which in no
longer valid, opam will try to tell you what the new command should look like.
