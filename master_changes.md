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
  * [BUG] Fix `opam install --deps-only` set direct dependencies as root packages [#6125 @rjbou]

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

## Format upgrade

## Sandbox

## VCS

## Build
 * Synchronise opam-core.opam with opam-repository changes [#6043 @dra27]

## Infrastructure

## Release scripts
  * Add the missing mccs and dune archives to the opam-full-<version>.tar.gz archive [#6067 @kit-ty-kate]
  * Ensure the configure file stays as it is in the tag, in the opam-full-<version>.tar.gz archive [#6067 @kit-ty-kate]
  * Exclude the .git directory from the release archive when using GNU tar [#6067 @kit-ty-kate]
  * Ensure non-existing %.cache target fail with a fatal error [#6067 @kit-ty-kate]
  * Remove opam 2.1 support from the release script [#6084 @kit-ty-kate]

## Install script

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
  * Add a test for --deps-only setting direct dependencies as root packages [#6125 @rjbou]

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
  * Use actions/cache instead of our own fork ocaml-opam/cache [#6081 @rjbou]
  * Update action cache to v4 [#6081 @rjbou]

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
