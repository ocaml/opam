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
  * `OpamSolution` Remove the heuristic of recomputing depexts of additional (pinned) packages. [#6489 @arozovyk]
  * `OpamClient.install_t` and `OpamAuxCommand.autopin` update depexts status earlier using `OpamSwitchState.update_sys_packages` [#6489 @arozovyk]
  * `OpamClient` update the system package status check for dependencies during `opam install --deps-only`, including support for pinned packages; also update this in `OpamAuxCommands.autopin` [#6489 @arozovyk fix #6461]
  * `OpamSolution.install_sys_packages_t` check for availability of system packages in `repo_state` before installing depexts [#6489 @arozovyk fix #6489]
  * `OpamSolution.get_depexts` remove no longer needed `recover` option that was used with `--depext-only` option  [#6489 @arozovyk fix #6489]

## opam-repository

## opam-state
  * `OpamSwitchState`: add `update_sys_packages` to update depexts status of a set of packages. [#6489 @arozovyk]
  * `OpamSysInteract`: add `available_status` and `installed_status` to be computed separately, redefine `packages_status` accordingly [#6489 @arozovyk]
  * `OpamStateTypes`: add available system package status field in `repos_state` for all the depexts declared in repo's packages. The new field is also added to the cache [#6489 @arozovyk fix #6461]
  * `OpamUpdate.repositories`: Compute repo's available system packages on opam update [#6489 @arozovyk fix #6461]
  * `OpamRepositoryState.load`: load repo's available system packages [#6489 @arozovyk fix #6461]
  * `OpamRepositoryState`: add `get_declared_depexts` that returns all the declared depexts in a set of opam files. [#6489 @arozovyk]
  * `OpamSwitchState.update_sys_packages` check for availability of packages in `repo_state` when updating the depexts status of additional packages [#6489 @arozovyk fix #6461]

## opam-solver

## opam-format
  * `OpamSysPkg`: add `available` type to indicate the availability of a set of system packages. [#6489 @arozovyk]
  * `OpamSysPkg`: add `available_equal`. [#6489 @arozovyk]

## opam-core
