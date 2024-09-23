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
  * Bump the version of opam to 2.4.0~alpha1~dev [#6204 @kit-ty-kate]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

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
  * [BUG] Fix SWH archive cooking request for wget [#6036 @rjbou - fix #5721]
  * [BUG] Fix SWH liveness check [#6036 @rjbou]
  * Update SWH API request [#6036 @rjbou]

## Lock

## Clean

## Env

## Opamfile

## External dependencies

## Format upgrade

## Sandbox

## VCS

## Build
  * Bump the requirement for dune to 2.8 [#6204 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Fix the release script after the bump of dune lang to 2.6 [#6204 @kit-ty-kate]
  * Fix the release script after the introduction of opam\_core\_stubs [#6204 @kit-ty-kate]
  * Improve the release script by ignoring interactive questions asked by the FreeBSD package manager [#6204 @kit-ty-kate]

## Install script
  * Add 2.3.0\~alpha1 to the install scripts [#6203 @kit-ty-kate]

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

### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository
  * `OpamDownload.get_output`: fix `wget` option for `POST` requests [#6036 @rjbou]
  * `OpamDownload.get_output`: use long form for `curl` `POST` request option [#6036 @rjbou]

## opam-state

## opam-solver

## opam-format

## opam-core
