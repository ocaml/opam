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
  * Add opam 2.4.0~beta1 [#6559 @kit-ty-kate]

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

### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client
  * Remove the heuristic of recomputing depexts of additional (pinned) packages in `OpamSolution` and move the logic to `OpamClient.install_t` and `OpamAuxCommand.autopin` [#6461 @arozovyk]

## opam-repository

## opam-state
  * OpamSwitchState: add `update_sys_packages` function to update depexts status of a set of packages. [#6461 @arozovyk]
  * Add a function to update system dependencies status of additional packages in `OpamSwitchState` [#6461 @arozovyk]
  * `OpamSysInteract`: add `available_status` and `installed_status` for available and installed to be computed separately [#6489 @arozovyk]
  * Split depexts status function in `OpamSysInteract` for available and installed to be computed separately [#6461 @arozovyk]
  * Add available system package status field in `repos_state` for all the depexts declared in repo's packages. The new field is also added to the cache [#6461 @arozovyk]
  * Compute repo's available packages on opam update [#6461 @arozovyk]

## opam-solver

## opam-format

## opam-core
