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
  * Bump the version number after the release of 2.2.0~beta3 [#6009 @kit-ty-kate]

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
  * Fix `opam upgrade` wanting to recompile opam files containing the `x-env-path-rewrite` field [#6029 @kit-ty-kate - fix #6028]

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
  * add a complete test to make sure effectively_equal does not take the location of the fields into account [#6029 @kit-ty-kate]

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
  * `OpamTypesBase`: Add `nullify_pos_map` and `nullify_pos_value` [#6029 @kit-ty-kate]

## opam-core
