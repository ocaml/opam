Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  *

## Plugins
  *

## Init
  * Add rsync package to internal Cygwin packages list (enables local pinning and is used by the VCS backends [#5808 @dra27]
  * Recommend enabling Developer Mode on Windows [#5831 @dra27]
  * Disable ACL in Cygwin internal install to avoid permission mismatch errors [#5796 @kit-ty-kate - fix #5781]
  * Add `sys-pkg-manager-cmd` as an accepted field in opamrc files [#5847 @rjbou - fix #5844]
  * Fix `git-location` handling in init config file [#5848 @rjbou - fix #5845]
  * Fix MSYS2 support [#5843 @rjbou - fix #5683]
  * Test if file exists before sourcing in fish + powershell [#5864 @ElectreAAS]
  * Replace the dependency on GNU patch by a strict dependency on git [#5400 @kit-ty-kate - fix #3433 #3782 #3639]

## Config report
  *

## Install
  *

## Remove
  *

## Switch

## Pin
  *

## List

## Show

## Var/Option

## Lint
  *

## Lock
  *

## Opamfile
  *

## External dependencies

## Sandbox

## Repository management
  *

## VCS
  *

## Build
  * Bump src_exts and fix build compat with Dune 2.9.0 [#4752 @dra27]
  * Upgrade to dose3 >= 6.1 and vendor dose3 7.0.0 [#4760 @kit-ty-kate]
  * Change minimum required OCaml to 4.03.0 [#4770 @dra27]
  * Change minimum required Dune to 2.0 [#4770 @dra27]
  * Do not check for cppo in the configure script (not used directly anymore since #5498) [#5794 @kit-ty-kate]
  * Upgrade vendored cmdliner to 1.2.0 [#5797 @kit-ty-kate]
  * Add winsymlinks:native to the CYGWIN environment variable when installing a package on Windows [#5793 @kit-ty-kate - fix #5782]
  * Upgrade the vendored dune to 3.14.0 [#5869 @kit-ty-kate]

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State

# Opam file format
  *

## Solver
  *

## Client

## Internal
  *

## Test
  *

## Reftests
### Tests
### Engine

## Github Actions

## Shell

## Doc

## Security fixes
  *

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
