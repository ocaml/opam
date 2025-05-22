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

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)
  * ◈ Add `--verbose-on` option to enable verbose mode on specified package names [#5682 @desumn @rjbou]
  * Remove unnecessary copies/move when fetching archives [#5018 @kit-ty-kate @rjbou]
  * [BUG] On install driven by `.install` file, track intermediate directories too, in order to have them suppressed at package removal [#5691 @rjbou - fix #5688]
  * [BUG] With `--assume-built`, resolve variables in depends filter according switch & global environment, not only depends predefined variables [#570 @rjbou - fix #5698]
  * Check if with-test & with-doc depends are installed to launch a reinstall [#4513 @rjbou - fix #4507]

## Remove

## UI

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

## Format upgrade

## Sandbox

## VCS

## Build

## Infrastructure

## Release scripts
  * Add opam `2.4.0~alpha2` to the install script [#6511 @kit-ty-kate]

## Install script

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Unix

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests

### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
