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

## Internal: Unix

## Internal: Windows

## Test
  * Add `opamUnit` as a basic unit test framework [#6953 @NathanReb]
  * Add unit tests for `OpamFilename.starts_with` and `dir_starts_with` in `tests/lib/core` [#6953 @NathanReb]

## Benchmarks

## Reftests
### Tests
  * Add a test ensuring installing files through a .install file can't escape the opam switch (CVE-2026-57825) [#7005 @NathanReb]

### Engine

## Github Actions

## Doc

## Security fixes
  * Fix a bug that allowed a package to install files anywhere on the system using a symlink to an external directory without warning the user and asking for their permission: CVE-2026-57825. [#7005 @NathanReb]

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamFilename.{,dir_}starts_with`: Fix a bug where `foo/bar` would be considered a prefix of `foo/bar-baz` [#6953 @NathanReb - fix #6948]
  * `OpamFilename.{,dir_}starts_with`: `/` and `\` are now equivalent on Windows [#6953 @NathanReb]
  * `OpamFilename.starts_with`: `starts_with "a/b" "a/b"` no longer returns `true` [#6953 @NathanReb]
  * `OpamSystem.real_path`: fix a bug where paths after a non existent directory where not resolve [#7011 @kit-ty-kate - fix #7010]
