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
  * Bump version to 2.2.0~alpha4~dev [#5732 @kit-ty-kate]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Remove

## Switch
  * Precise message for Ctrl-c-ed opam switch creation "Switch left partially installed" [#5713 @rjbou - fix #5710]

## Config

## Pin

## List

## Show

## Var/Option

## Update / Upgrade

## Tree
  * Fix `--dev` option, force dev dependencies when option is given [#5687 @rjbou - fix #5675]
  * Fix `--no-switch` option, instead of emptying switch from it installed packages, load a virtual switch at the beginning when `--no-switch` is given [#5687 @rjbou - fix #5675]

## Exec

## Source

## Lint

## Repository

## Lock

## Clean

## Env

## Opamfile

## External dependencies

## Format upgrade

## Sandbox
  * Mark the user temporary directory (as returned by `getconf DARWIN_USER_TEMP_DIR`) as writable when TMPDIR is not defined on macOS [#5780 @ElectreAAS]

## VCS

## Build
  * Vendor mccs.1.1+17 [#5769 @kit-ty-kate]
  * Require mccs >= 1.1+17 [#5769 @kit-ty-kate]
  * Add ./configure --enable-static to compile the opam binary statically [#5680 @kit-ty-kate - fixes #5647]

## Infrastructure

## Release scripts
  * Workaround incorrect `NGROUPS_MAX` in `<limits.h>` in musl for release builds [#5383 @dra27]
  * Fix check for adding `-lsha_stubs` only on `master` on OpenBSD [#5733 @punchagan]

## Admin

## Opam installer

## State

## Opam file format

## Solver
  * Fix debug logs showing up regardless of verbosity on macOS 12.7.1 / 13.6.3 / 14.2 and FreeBSD [#5769 @kit-ty-kate]

## Client

## Shell

## Internal

## Internal: Windows

## Test

## Reftests
### Tests
  * Add some additional test to tree, for `--dev` && `--no-switch` [#5687 @rjbou]

### Engine
  * Set `SHELL` to `/bin/sh` in Windows to ensure `opam env` commands are consistent [#5723 @dra27]
  * Substitution for `BASEDIR` and `OPAMTMP` now recognise the directory with either forward-slashes, back-slashes, or converted to Cygwin notation (i.e. C:\cygwin64\tmp\..., C:/cygwin64/tmp/..., or /tmp/...) [#5723 @dra27]

## Github Actions

## Doc
  * Improve the installation instructions on FreeBSD [#5775 lukstafi]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamSystem.apply_cygpath`: runs `cygpath` over the argument [#5723 @dra27 - function itself added in #3348]
