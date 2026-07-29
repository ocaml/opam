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
  * [BUG] Fix package name inference from a local project directory name: the name is everything up to the first dot, and directory names starting with a dot no longer infer a mangled name (`opam install ./.foo` used to infer, pin and install a package named `oo`) [#XXXX @MavenRain - fix #7043]

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

## Infrastructure

## Release scripts

## Install script
  * Add `2.6.0~alpha1` to the install scripts [#7047 @kit-ty-kate]

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
  * Add a test covering package name inference from local project directory names [#XXXX @MavenRain]

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
