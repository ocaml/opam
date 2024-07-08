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

## Format upgrade

## Sandbox

## VCS

## Build
 * Synchronise opam-core.opam with opam-repository changes [#6043 @dra27]

## Infrastructure

 * Bump opam version used in the depext CI from 2.1.0 to 2.1.6 [#6074 @RyanGibb]

## Release scripts
  * Add the missing mccs and dune archives to the opam-full-<version>.tar.gz archive [#6066 @kit-ty-kate]
  * Ensure the configure file stays as it is in the tag, in the opam-full-<version>.tar.gz archive [#6066 @kit-ty-kate]
  * Exclude the .git directory from the release archive when using GNU tar [#6066 @kit-ty-kate]
  * Ensure non-existing %.cache target fail with a fatal error [#6066 @kit-ty-kate]

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

### Engine

## Github Actions

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
