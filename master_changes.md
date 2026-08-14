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
  * Do not make `opam init --reinit` ask to retry the command when upgrading from a 2.1 root [#7058 @kit-ty-kate - fix #7057]

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
  * Add level 4 debug output of the patch operations during a load from diff [#7072 @kit-ty-kate]

## Internal: Unix

## Internal: Windows

## Test
  * patchDiff: fix random git related failures [#7069 @kit-ty-kate - fix #7037]

## Benchmarks

## Reftests
### Tests
  * Add a test showing `opam init --reinit` upgrading from pre opam 2.6 `OPAMREPOTARRING=1` (aka. opam 2.1's default) [#7058 @kit-ty-kate]

### Engine
  * Stop the testsuite from generating files containing CRLF [#7071 @kit-ty-kate]

## Github Actions

## Doc

  * Add Makefile command to convert HTML man pages to markdown with YAML header using `pandoc`.

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
