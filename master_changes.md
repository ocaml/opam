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
  * Bump version to 2.2.0~beta3~dev [#5917 @kit-ty-kate]
  * Bump the version number after the release of 2.2.0~beta1 [#5785 @kit-ty-kate]
  * Upgrade the opam-root-version to 2.2~beta [#5904 @kit-ty-kate]

## Global CLI

## Plugins

## Init
  * ◈ New option `opam init --cygwin-extra-packages=CYGWIN_PKGS --cygwin-internal-install`, to specify additional packages for internal Cygwin [#5930 @moyodiallo - fix #5834]

## Config report

## Actions

## Install

## Remove

## Switch
  * Allow to parse opam 2.1 switch import files containing extra-files [#5943 @kit-ty-kate - fix #5941]

## Config

## Pin

## List

## Show

## Var/Option

## Update / Upgrade

## Tree
  * [BUG] Fix `opam tree --with-*` assigning the `with-*` variables to unrequested packages [#5919 @kit-ty-kate @rjbou - fix #5755]
  * [BUG] Fix combinations of `opam tree --with-*` and `--no-switch` [#5919 @kit-ty-kate @rjbou - fix #5920]

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
  * Handle init OCaml `sys-ocaml-*` eval variables during format upgrade from 2.0 -> 2.1 -> 2.2 [#5829 @dra27]
  * Reset the "jobs" config variable when upgrading from opam 2.1 to 2.2, instead of 2.0 to 2.1 [#5904 @kit-ty-kate - fix #5816]

## Sandbox

## VCS

## Build

## Infrastructure
  * Ensure GNU coreutils available on the macOS 14 CI runners [#5938 @dra27]

## Release scripts

## Install script

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client
  * Fix rounding error when displaying the timestamp in debug mode [#5912 @kit-ty-kate - fix #5910]

## Shell

## Internal

## Internal: Windows

## Test

## Benchmarks
  * Benchmark opam install --deps-only of an already installed package [#5909 @kit-ty-kate]

## Reftests
### Tests
  * tree: add a test for packages that have variables in their transitive dependencies [#5919 @rjbou]
  * tree: add test for `opam tree pkg --with-test --no-switch` [#5919 @rjbou]
  * Update opam root version test with root version bump [#5904 @rjbou]

### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client
  * `OpamClient.init` and `OpamClient.reinit`: now can have additional cygwin packages to install [#5930 @moyodiallo]

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
