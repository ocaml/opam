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
  * Bump the version number to `2.4.0~alpha2~dev` [#6476 @kit-ty-kate]

## Global CLI
  * Remove handling of the `OPAMSTATS` environment variable [#6485 @hannesm]

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI

## Switch
  * Do not include compiler packages flagged with `avoid-version`/`deprecated` in the invariant when calling `opam switch create [name] <version>` [#6494 @kit-ty-kate]

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
  * Cygwin: Fallback to the existing `setup-x86_64.exe` if its upgrade failed to be fetched [#6482 @kit-ty-kate - fix #6495, partial fix #6474]

## Format upgrade

## Sandbox

## VCS

## Build

## Infrastructure

## Release scripts
  * Add `2.4.0~alpha1` to the install scripts [#6475 @kit-ty-kate]

## Install script

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal
  * Fix a memory leak happening when running large numbers of commands or opening large number of opam files [#6485 @hannesm - fix #6484]

## Internal: Unix

## Internal: Windows

## Test
  * patchDiff: add some tests showing the handling of empty files and new directories [#6490 @rjbou]

## Benchmarks

## Reftests
### Tests
  * Show the behaviour of `opam switch create` in presence of `avoid-version`/`deprecated` packages [#6494 @kit-ty-kate]
  * Add some tests showing the behaviour of the internal patch implementation [#6490 @rjbou]

### Engine

## Github Actions
  * Add depext job for depext test on Altlinux [#6277 @rjbou]
  * Upgrade the opam binary used for the depexts run to `2.4.0~alpha1` [#6277 @kit-ty-kate]

## Doc

## Security fixes

# API updates
## opam-client
  * `OpamClientConfig`: remove `STATS` variant and related `print_stats` field in config record [#6485 @hannesm]
  * `OpamArg.environment_variable`: make `STATS` as removed from cli 2.3 [#6485 @rjbou]

## opam-repository

## opam-state

## opam-solver

## opam-format
  * `OpamFile`: remove `Stats` module [#6485 @hannesm]

## opam-core
  * `OpamSystem`: remove `print_stats` function [#6485 @hannesm]
