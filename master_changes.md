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
  * Add `opam 2.4.0~rc1` to the install scripts [#6583 @kit-ty-kate]
  * Bump the version number to `2.5.0~alpha1~dev` [#6584 @kit-ty-kate]

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
  * Use `OpamRepositoryState.load_opams_incremental` to efficiently update the `repo_opams`, adding/updating/removing only changed packages. [#6614 @arozovyk - fix #5824]

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

## Benchmarks

## Reftests
### Tests
  * Add tests for repository update using `OpamRepositoryState.load_opams_incremental` [#6614 @arozovyk - fix #5824]
### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository
  * `OpamRepository.update` include the list of file-level changes in the return type `['Changes of Patch.operation list]` [#6614 @arozovyk - fix #5824]
  * `OpamRepositoryBackend.update` type: Add new `Update_diffs of (filename * Patch.t list)` variant for incremental updates with file-level patch operations
  * `OpamRepositoryBackend.get_diff`: Change return type from `filename option` to `(filename * Patch.t list) option` to include both patch file and list of operations
  * Add `strip_repo_suffix` internal function to process patch operations and remove repository path prefixes

## opam-state
  * Add `OpamRepositoryState.load_opams_incremental`: that only updates opam files that actually changed, using a list of `Patch.operation`; use it in `OpamUpdate.repository` [#6614 @arozovyk - fix #5824]

## opam-solver

## opam-format

## opam-core
  * `OpamSystem.patch`, `OpamFilename.patch`: change return type from `exn option` to `['Patched of Patch.operation list | 'Exception of exn]` to include the list of applied patch operations [#6614 @arozovyk - fix #5824]
  * `OpamSystem.patch`: Remove optional `preprocess` parameter and change signature to accept `['Patch_file of string | 'Patch_diffs of Patch.t list]` instead of just file path
  * `OpamFilename.patch`: Remove optional `preprocess` parameter and change signature to accept patch source variants instead of just file path