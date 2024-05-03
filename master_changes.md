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

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

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
  * Add test for `opam tree pkg --with-test --no-switch` [#XXX @rjbou]

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
