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
  * Add opam `2.4.0~alpha2` to the install script [#6511 @kit-ty-kate]

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

## Benchmarks

## Reftests
### Tests
  * Use `opam-set-os` in reftests following the depexts update.

### Engine
  * Add `opam-set-os` command that combines setting global `os-family` variable followed by a (silent) `opam update` [#6461 @arozovyk]

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client
  * Remove the heuristic of recomputing depexts of additional (pinned) packages in `OpamSolution` and move the logic to `OpamClient.install_t` and `OpamAuxCommand.autopin` [#6461 @arozovyk]
  * Update the system package status check in `OpamClient` for dependencies during `opam install --deps-only`, including support for pinned packages; also update this in `OpamAuxCommands.autopin` [#6461 @arozovyk]
  * During `OpamSolution.install_sys_packages_t`, check for availability of system packages in `repo_state` before installing depexts [#6461 @arozovyk]

## opam-repository

## opam-state
  * OpamSwitchState: add `update_sys_packages` function to update depexts status of a set of packages. [#6461 @arozovyk]
  * Add a function to update system dependencies status of additional packages in `OpamSwitchState` [#6461 @arozovyk]
  * Split depexts status function in `OpamSysInteract` for available and installed to be computed separately [#6461 @arozovyk]
  * Add available system package status field in `repos_state` for all the depexts declared in repo's packages. The new field is also added to the cache [#6461 @arozovyk]
  * Compute repo's available packages on opam update [#6461 @arozovyk]
  * During `OpamSwitchState.update_sys_packages`, check for availability of packages in `repo_state` when updating the depexts status of additional packages [#6461 @arozovyk]

## opam-solver

## opam-format

## opam-core
