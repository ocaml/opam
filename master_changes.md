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
  * Bump version to `2.7.0~alpha1~dev` [#7064 @kit-ty-kate]

## Global CLI
  * Make error message for opam repo add more explicit [#7014 @yannl35133 - fix #6943]

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

## VCS

## Build
  * Upgrade the autoconf generated files (`configure`) to autoconf 2.72 [#7052 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Make x86\_32 binaries take full advantage of i686 [#7120 @kit-ty-kate]
  * Ensure arm32 binaries are really armhf as advertised instead of armv7 [#7120 @kit-ty-kate]

## Install script
  * Add opam 2.6.0\~rc1 to the install scripts [#7135 @kit-ty-kate]

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
  * Add an exhaustive test showing the behaviour of the `conflicts` field [#7127 @kit-ty-kate]

### Engine

## Github Actions
  * The Hygiene workflow has been upgraded to Ubuntu 26.04 [#7052 @kit-ty-kate]

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamStd.Map.update`: was changed to the stdlib version which has a more flexible API and has better performances [#7130 @NathanReb - fix #4915]
