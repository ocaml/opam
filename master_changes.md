Working version changelog, used as a base for the changelog and the release
note.
Prefixes used to help generate release notes, changes, and blog posts:
* ✘ Possibly scripts breaking changes
* ◈ New option/command/subcommand
* [BUG] for bug fixes
* [NEW] for new features (not a command itself)
* [API] api updates 🕮
If there is changes in the API (new non optional argument, function renamed or
moved, etc.), please update the _API updates_ part (it helps opam library
users)

## Version
  * Bump version to 2.3.0~alpha~dev [#6045 @rjbou]
  * Bump opam-root-version to 2.2 [#5980 @kit-ty-kate]

## Global CLI
  * Add cli version 2.3 [#6045 @rjbou]

## Plugins

## Init
  * Suppress all the Windows menus when running with `opam init -ya` [#6034 @dra27]

## Config report

## Actions

## Install
  * Fix package name display for no agreement conflicts [#6055 @rjbou - fix #6030]

## Remove

## Switch

## Config

## Pin

## List

## Show

## Var/Option

## Update / Upgrade

## Tree

## Exec

## Source

## Lint

## Repository

## Lock

## Clean

## Env

## Opamfile

## External dependencies
 * Always pass --no-version-check and --no-write-registry to Cygwin setup [#6046 @dra27]
 * Use --quiet-mode noinput for the internal Cygwin installation (which is definitely a fully-specified command line) and --quiet-mode unattended for external Cygwin installations (in case the user does need to select something, e.g. a mirror) [#6046 @dra27]
  * [BUG] Fix apt/debian lookup for installed packages [#6054 @rjbou]

## Format upgrade

## Sandbox

## VCS

## Build
 * Synchronise opam-core.opam with opam-repository changes [#6043 @dra27]
  * Bump src_exts and fix build compat with Dune 2.9.0 [#4752 @dra27]
  * Upgrade to dose3 >= 6.1 and vendor dose3 7.0.0 [#4760 @kit-ty-kate]
  * Change minimum required OCaml to 4.03.0 [#4770 @dra27]
  * Change minimum required Dune to 2.0 [#4770 @dra27]
  * Change minimum required OCaml to 4.08.0 for everything except opam-core, opam-format and opam-installer [#4775 @dra27]
  * Fix the cold target in presence of an older OCaml compiler version on macOS [#4802 @kit-ty-kate - fix #4801]
  * Harden the check for a C++ compiler [#4776 @dra27 - fix #3843]
  * Add `--without-dune` to configure to force compiling vendored Dune [#4776 @dra27]
  * Use `--without-dune` in `make cold` to avoid picking up external Dune [#4776 @dra27 - fix #3987]
  * Add `--with-vendored-deps` to replace `make lib-ext` instruction [#4776 @dra27 - fix #4772]
  * Fix vendored build on mingw-w64 with g++ 11.2 [#4835 @dra27]
  * Switch to vendored build if spdx_licenses is missing [#4842 @dra27]
  * Check versions of findlib packages in configure [#4842 @dra27]
  * Fix dose3 download url since gforge is gone [#4870 @avsm]
  * Update bootstrap ocaml to 4.12.1 to integrate mingw fix [#4927 @rjbou]
  * Update bootstrap to use `-j` for Unix (Windows already does) [#4988 @dra27]
  * Update cold compiler to 4.13 [#5017 @dra27]
  * Bring the autogen script from ocaml/ocaml to be compatible with non-ubuntu-patched autoconf [#5090 @kit-ty-kate #5093 @dra27]
  * configure: Use gmake instead of make on Unix systems (fixes BSDs) [#5090 @kit-ty-kate]
  * Patch AltGr/ocaml-mccs#36 in the src_ext build to fix Cygwin32 [#5094 @dra27]
  * Silence warning 70 [#5104 @dra27]
  * Add `jsonm` (and `uutf`) dependency [#5098 @rjbou - fix #5085]
  * Bump opam-file-format to 2.1.4 [#5117 @kit-ty-kate - fix #5116]
  * Add `sha` dependency [#5042 @kit-ty-kate]
  * Add a 'test' target [#5129 @kit-ty-kate @mehdid - partially fixes #5058]
  * Add `with-tools` handling in selection process [#5016 @rjbou]
  * Bump cudf to 0.10 [#5195 @kit-ty-kate]
  * shell/bootstrap-ocaml.sh: do not fail if curl/wget is missing [#5223 #5233 @kit-ty-kate]
  * Upgrade to cmdliner >= 1.1 [#5269 @kit-ty-kate]
  * Cleared explanation of dependency vendoring in configure [#5277 @dra27 - fix #5271]
  * Switch autoconf required version to 2.71 [#5161 @dra27]
  * Remove src/client/no-git-version when calling make clean [#5290 @kit-ty-kate]
  * Remove src/client/no-git-version and use the OPAM_BUILD_NO_GIT_VERSION environment variable as compile-time instead [#5291 @kit-ty-kate]

## Infrastructure

 * Bump opam version used in the depext CI from 2.1.0 to 2.1.6 [#6074 @RyanGibb]

## Release scripts
  * Add the missing mccs and dune archives to the opam-full-<version>.tar.gz archive [#6066 @kit-ty-kate]
  * Ensure the configure file stays as it is in the tag, in the opam-full-<version>.tar.gz archive [#6066 @kit-ty-kate]
  * Exclude the .git directory from the release archive when using GNU tar [#6066 @kit-ty-kate]
  * Ensure non-existing %.cache target fail with a fatal error [#6066 @kit-ty-kate]
  * Remove opam 2.1 support from the release script [#6084 @kit-ty-kate]

## Install script
  * Provide a shell/install.ps1 PowerShell script to install opam on Windows [#5906 @kit-ty-kate @dra27]
  * Add opam 2.2.0 to the install scripts [#6062 @kit-ty-kate]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests
  * cli versioning: untie output from current major version [#6045 @rjbou]
  * Set `opam-version` to 2.2 for some conflict message tests based on opam repository to stabilise their output [#6045 @rjbou]
  * [BUG]: head -c is not posix compliant. Use cut -b instead. [#5989 @madroach]
  * Add bad cudf package name encoding (dose3 lib) [#6055 @rjbou]
  * Add test for filter operators in opam file [#5642 @rjbou]
  * Update init test to make it no repo [#5327 @rjbou]

### Engine

## Github Actions
  * Depexts: replace centos docker with almalinux to fake a centos [#6079 @rjbou]
  * Depexts: fix conf package install check [#6079 @rjbou]
  * Depexts: specify packages to test per distribution [#6079 @rjbou]
  * Depexts: add update depexts check [#6079 @rjbou]
  * Depexts: move parts to docker build image, for caching [#6079 @rjbou]
  * Depexts: set version for conf packages to check [#6079 @rjbou]
  * Depexts: add package to test containing `os-version` in filter [#6079 @rjbou]
  * Depexts: fix opensuse job [#6079 @rjbou]

## Doc
  * Remove the ppa from the installation instructions on Ubuntu [#5988 @kit-ty-kate - fix #5987]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state
 * `OpamStateConfig.opamroot_with_provenance`: restore previous behaviour to `OpamStateConfig.opamroot` for compatibility with third party code [#6047 @dra27]

## opam-solver

## opam-format

## opam-core
