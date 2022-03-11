# Vendored Dependencies Maintenance

opam's build system supports bootstrapping on systems which have only a C compiler and GNU make. For this to work, the configuration and build system has to be able to build OCaml and also build opam's dependencies.

Files related to the dependency vendoring are kept in the `src_ext` directory. There are two related scripts which live elsewhere:
- `shell/bootstrap-ocaml.sh` - assembles an OCaml compiler in `bootstrap/`
- `shell/re-patch.sh` - when run from the `src_ext/` directory, normalises all the patch files (eliminating git-specific features, etc.)

## lib-ext and lib-pkg

There are two modes of operation. `lib-ext` uses Dune vendoring and assembles the dependencies of opam in subdirectories of `src_ext/`, allowing Dune to build them with opam. In this mode, only the `opam` binary can then be installed: the libraries are not installable (as with all Dune vendoring with libraries). `lib-pkg` installs the libraries to a given compiler using either `dune install` (for dependencies which use Dune) or `ocamlfind install` for any others. This mode is typically used to install dependencies to the compiler built in `bootstrap/` by `shell/bootstrap-ocaml.sh`. In `lib-pkg`, the dependency's preferred build system is used, in `lib-ext`, the package must use Dune.

## Patching

Package sources have patches applied from two places. For package `foo`, the patches in `src_ext/foo.common` and then patches in `src_ext/foo` (for `lib-ext` mode) or `src_ext/foo.pkg` (for `lib-pkg` mode) are applied (within those directories, patches apply in the order returned by shell globbing). Patches placed here are either to back-port the package to an older version of OCaml than upstream supports, or to back-port a fix from an unreleased version of package.

In `lib-ext` mode, `dune` files can be dropped into the extracted package sources, after patching, by creating a file named `src_ext/dune-foo`. If the package needs dropping in a subdirectory, then these can be appended to the filename, using `-` instead of `/`. For example, `src_ext/dune-foo-src-lib` will be copied to `src/lib/dune` when package `foo` is extracted.

Finally, `src_ext/Makefile` provides an opportunity for package-specific commands to be run by adding specific commands to the `.stamp` targets. See, for example, the horrors in [f721385](https://github.com/ocaml/opam/commit/f721385).

## Updating the dependencies

`src_ext/update-sources.sh` will attempt to use repository data from your current opam switch to automatically update `src_ext/Makefile.sources`. For some packages, the script may make the wrong determination (e.g. cudf 0.9, where opam's sources stick with the original gforge source archive), so the changes suggested should be reviewed carefully! After updating the dependencies, it's important to check that the patches still work. These can be triggered manually with `make clone` (for `lib-ext`) or `make clone-pkg` (for `lib-pkg`). If a patch no longer applies, it needs to be updated or replaced (e.g. by rebasing a git commit and using `git format-patch` or by manually applying the changes to a clean worktree and using `diff -Naur`, etc.). Once the patches are correct and `make clone` is working, run `../shell/re-patch.sh` to normalise the patches. This script will apply the patches as given and then regenerate them using `diff`.

## Dune and mccs

The Dune vendored dependency is a special case: firstly, in lib-ext mode it's optional and secondly the package is called `dune-local`. The name avoids conflict with a directory called `dune` (which Dune itself can't cope with). mccs is also a special case, because it's optional in lib-ext mode.

## Adding a new dependency

1. Add the tarball information to `src_ext/Makefile.sources`. If the package is used in `lib-ext` mode, define `URL_package` and `MD5_package`. If the same package is used in `lib-pkg` mode, add `$(call PKG_SAME,package)`, otherwise define `URL_PKG_package` and `MD5_PKG_package`. If a package is only used in `lib-pkg` mode, don't define `URL_package` and `MD5_package`.
2. Add `package` to `SRC_EXTS` or `PKG_EXTS in `src_ext/Makefile`.
3. Put any requires patches in `src_ext/patches/` (in `package.common/` for both modes, `package/` for `lib-ext` mode and `package.pkg/` for `lib-pkg` mode).
4. If the package's build system is not Dune, create `src_ext/dune-package`. The build system `touch`es `package.opam` when assembling the sources.
5. Add a `package.pkgbuild:` entry to `src_ext/Makefile.packages` containing the dependencies of `package`.
6. Add a `package-pkg-build:` target with the build steps for building and installing `package`. Various utility variables are available in `src_ext/Makefile.packages`, in particular `$(OCAMLBIN)` is the compiler's `bin` directory and `$(SITELIB)` is the ocamlfind library root (effectively `%{lib}%` in opam) and `$(OCAMLROOT)` is the actual prefix of the compiler's installation.
