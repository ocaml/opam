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

## Plugins

## Init

## Config report

## Actions

## Install

  * Fix a bug triggering unnecessary reinstallation of packages not directly involved in the solution [#7154 @NathanReb]

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
  * Upgrade to cudf 0.11\~rc1 [#7145 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Make x86\_32 binaries take full advantage of i686 [#7120 @kit-ty-kate]
  * Ensure arm32 binaries are really armhf as advertised instead of armv7 [#7120 @kit-ty-kate]

## Install script
  * Add opam 2.6.0\~rc1 to the install scripts [#7135 @kit-ty-kate]
  * Add opam 2.6.0 to the install scripts [#7150 @kit-ty-kate]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal
  * Remove unecessary set union operations `packages ++ installed` since `installed` is included in `packages` [#7148 @NathanReb]
  * Remove unnecessary set operations in `OpamSwitchSate.universe` [#7154 @NathanReb]

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
  * auto-cancel unreachable jobs in PRs [#7147 @kit-ty-kate]
  * Always start CI runs by an `apt update` [#7156 @kit-ty-kate]
  * Update the opam-repository SHA to the latest commit [#7145 @kit-ty-kate]

## Doc
  * Update the documentation about the latest opam release [#7150 @kit-ty-kate]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamStd.Map.update`: was changed to the stdlib version which has a more flexible API and has better performances [#7130 @NathanReb - fix #4915]
