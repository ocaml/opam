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
  * Fix the depexts installation on `opam install --deps` when the system packages already exist in a repository [#7158 @kit-ty-kate - fix #7153]

## Format upgrade

## Sandbox

## VCS

## Build
  * Upgrade to checkseum 0.5.4 [#7172 @kit-ty-kate]
  * Upgrade to decompress 1.6.1 [#7172 @kit-ty-kate]

## Infrastructure

## Release scripts

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
  * Add more depexts related tests to the testsuite (pins, autopins, deps-only, …) [#7158 @kit-ty-kate @rjbou]

### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state
  * `OpamSwitchState.update_sys_packages` (new in 2.6.0) was removed in favour of the already existing `update_package_metadata` or `update_pin` functions [#7158 @kit-ty-kate]

## opam-solver

## opam-format

## opam-core
