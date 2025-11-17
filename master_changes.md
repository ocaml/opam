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
  * Bump version to 2.6.0~alpha1~dev [#6749 @rjbou]

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
  * Fixed the bug occuring on version-equivalent package rename (i.e `pkg.00 -> pkg.0`) leading to the package being completely removed. [#6774 @arozovyk fix #6754]
  * Compute the list of available depexts on `opam update` [#6489 @arozovyk - fix #6461]
  * Update depexts availability repository state cache when running `opam update --depexts` [#6489 @arozovyk - fix #6461]
  * Display status message while loading system package availability during `opam update` [#6489 @arozovyk - fix #6461]

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
  * Allow the macOS sandbox to write in the `/var/folders/` and `/var/db/mds/` directories as it is required by some of macOS core tools [#4797 @kit-ty-kate - fix #4389 #6460]

## VCS

## Build
  * opam no longer depends on `cmdliner` [#6755 @kit-ty-kate - fix #6425]
  * Clean variables before calling make on different projects to avoid clashes [#6769 @kit-ty-kate]
  * Add the upcoming OCaml 5.5 (trunk) support when using dune's dev profile [#6670 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Fix the placement of the vendored archives in the release tarball [#6765 @kit-ty-kate - fix #6762]
  * Fix the Windows build [#6769 @kit-ty-kate]

## Install script
  * Add `2.5.0~alpha1` to the installers [#6748 @kit-ty-kate]
  * Add `2.5.0~beta1` to the installers [#6795 @kit-ty-kate]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client
  * Improved depexts handling by caching system package availability during update, avoiding redundant system checks at install time. [#6489 @arozovyk fix #6461]

## Shell

## Internal

## Internal: Unix

## Internal: Windows

## Test

## Benchmarks
  * Add an even larger real-world diff to benchmark `opam update` [#6567 @kit-ty-kate]

## Reftests
### Tests
  *  Add test cases to `update.test` for version-equivalent renames [#6774 @arozovyk fix #6754]
  * Add more tests for depexts behaviour with unknown family types [#6489 @arozovyk]
  * Add depexts tests with debug section that demostrate system availability polling [#6489 @arozovyk]

### Engine
  * Add a silent `opam update default` to `opam-set-os` command [#6489 @arozovyk]

## Github Actions
  * Add OCaml 5.4 to the test matrix [#6732 @kit-ty-kate]
  * Ensure `curl`'s exit status to be non-zero on failure [#6684 @kit-ty-kate]
  * Add OCaml trunk to the test matrix [#6684 #6670 @kit-ty-kate]
  * Rename the hygiene script for `shell/install.sh` to install-check [#6768 @kit-ty-kate]
  * Remove the unconditional Windows binary uploads on PRs [#6771 @kit-ty-kate]

## Doc

## Security fixes

# API updates
## opam-client
  * `OpamSolution` remove the heuristic of recomputing depexts of additional (pinned) packages. [#6489 @arozovyk fix #6461]
  * `OpamClient` update the system package status check for dependencies during `opam install --deps-only`, including support for pinned packages; also update this in `OpamAuxCommands.autopin` [#6489 @arozovyk fix #6461]
  * `OpamSolution.get_depexts` remove no longer needed `recover` option that was used with `--depext-only` option  [#6489 @arozovyk fix #6461]

## opam-repository

## opam-state
  * `OpamRepositoryState.load_opams_from_diff` track added packages to avoid removing version-equivalent packages [#6774 @arozovyk fix #6754]
  * `OpamSwitchState`: add `update_sys_packages` to update depexts status of a set of packages. [#6489 @arozovyk]
  * `OpamSysInteract`: add `available_packages` and `installed_packages` to be computed separately, redefine `packages_status` accordingly [#6489 @arozovyk]
  * `OpamStateTypes`: add available system package status field in `repos_state` for all the depexts declared in repo's packages. The new field is also added to the cache [#6489 @arozovyk fix #6461]
  * `OpamRepositoryState.load`: load repo's available system packages [#6489 @arozovyk fix #6461]
  * `OpamFileTools`: add `get_depexts` to consolidate depexts extraction logic from individual opam files and package maps [#6489 @arozovyk fix #6461]
  * `OpamUpdate`: add `update_sys_available_cache` to update the system package availability cache in repository state [#6489 @arozovyk fix #6461]
  * `OpamUpdate.get_sys_available`: factorize depexts availability computation logic from `OpamUpdate.repositories` [#6489 @arozovyk fix #6461]

## opam-solver

## opam-format
  * `OpamSysPkg`: add `availability_mode` type to indicate the availability of system packages on a given system. [#6489 @arozovyk]
  * `OpamSysPkg`: add `combine_status` and `equal_availability_mode` functions. [#6489 @arozovyk]

## opam-core
  * `OpamCmdliner` was added. It is accessible through a new `opam-core.cmdliner` sub-library [#6755 @kit-ty-kate]
