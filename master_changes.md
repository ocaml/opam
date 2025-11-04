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
  * Bump version to 2.6.0~alpha1~dev [#6749 @rjbou]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

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
  * Allow the macOS sandbox to write in the `/var/folders/` and `/var/db/mds/` directories as it is required by some of macOS core tools [#4797 @kit-ty-kate - fix #4389 #6460]

## VCS

## Build

## Infrastructure

## Release scripts
  * Fix the placement of the vendored archives in the release tarball [#6765 @kit-ty-kate - fix #6762]

## Install script
  * Add `2.5.0~alpha1` to the installers [#6748 @kit-ty-kate]

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
  *  Add test cases to `update.test` for version-equivalent renames [#6774 @arozovyk fix #6754]

### Engine

## Github Actions
  * Add OCaml 5.4 to the test matrix [#6732 @kit-ty-kate]

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
