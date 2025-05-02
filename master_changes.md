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
  * Add a note about to enforce no more upgrading last hard upgrade version (2.0.0~beta5), as far as possible. [#6416 @rjbou]
  * Complete upgrade mechanism to permit on the fly upgrade and write upgrade from repo and switch level [#6416 @rjbou]

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

## Benchmarks

## Reftests
### Tests

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
