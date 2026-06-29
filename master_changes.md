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
  * Bump version to `2.7.0~alpha1~dev` [#7064 @kit-ty-kate]

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
  * Fix the depexts installation on `opam install --deps` when the system packages already exist in a repository [#7158 @kit-ty-kate - fix #7153]

## Format upgrade

## Sandbox

## VCS

## Build
  * Upgrade the autoconf generated files (`configure`) to autoconf 2.72 [#7052 @kit-ty-kate]
  * Upgrade to cudf 0.11\~rc1 [#7145 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Make x86\_32 binaries take full advantage of i686 [#7120 @kit-ty-kate]
  * Ensure arm32 binaries are really armhf as advertised instead of armv7 [#7120 @kit-ty-kate]

## Install script
  * Add opam 2.6.0\~rc1 to the install scripts [#7135 @kit-ty-kate]
  * Add opam 2.6.0 to the install scripts [#7150 @kit-ty-kate]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal
  * Remove unecessary set union operations `packages ++ installed` since `installed` is included in `packages` [#7148 @NathanReb]
  * Rewrite inefficient package set <-> map operations [#7159 @NathanReb]

## Internal: Unix

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests
  * Add an exhaustive test showing the behaviour of the `conflicts` field [#7127 @kit-ty-kate]
  * Add more depexts related tests to the testsuite (pins, autopins, deps-only, …) [#7158 @kit-ty-kate @rjbou]
  *  Add test cases to `update.test` for version-equivalent renames [#6774 @arozovyk fix #6754]
  * Fix a failure when two hashes start with the same two characters [#6793 @kit-ty-kate]
  * Add a test showing the behaviour of `opam init --config` when the file given does not exist [#5979 @kit-ty-kate @rjbou]
  * Add a test for switch link when a local switch is already present [#6860 @rjbou]
  * Add more tests for depexts behaviour with unknown family types [#6489 @arozovyk]
  * Add disabled depexts tests [#6489 @rjbou]
  * Add depexts tests with debug section that demostrate system availability polling [#6489 @arozovyk]
  * Add a test showing the behaviour of .install files containing destination filepath trying to escape their scope [#6897 @rjbou @kit-ty-kate]
  * Add a test showing that `opam install ./` will leave packages pinned if
    aborted or failed [#6922 @NathanReb]
  * Add test for update in repository that changes directories to files and vice versa [#6915 @rjbou]
  * Add an http repository test [#6939 #6961 @rjbou]
  * Fix `extrafile` test : remove trailing mkdir, the error was fixed in #6679 [#6970 rjbou]
  * Fix trailing full path for `tar` call in `no-depexts-sandboxed.unix.test` [#6970 @rjbou]
  * Fix some forgotten sed in `extrasource` and `update` tests in #6734 [#6970 @rjbou]
  * Add a test for `opam config subst` [#6936 @NathanReb]
  * Add a lock test for undefined variables in a lock file [#6947 @rjbou - fix #6946]
  * Add a test showing the behaviour of `opam repo add` and `opam update` when faced with a repository containing an `opam` directory [#6995 @kit-ty-kate]
  * Add a tests for the several layouts of packages in a repository [#6941 @rjbou]
  * Add a test ensuring installing files through a .install file can't escape the opam switch (CVE-2026-57825) [#7005 @NathanReb]
  * Add a `opam repo set-url` case in repository-http [#6625 @rjbou]
  * Add in `repository-http` a test case for switching from directory to archive format, automatically [#6625 @rjbou]
  * Add in `repository` test cases for switching automatically from directory to archive format & vice versa [#6625 @rjbou]
  * Add in `repository` test cases for upgrade opam root from 2.5 with repo tarring or 2.1 to 2.6, with `OPAMREPOSITORYTARRING` enabled (trigger upgrade) [#6625 @rjbou]
  * Add 2.6 root test cases in opamroot-versions [#6625 @rjbou]
  * Add tests for `.install` fields handling [#6956 #67026 @rjbou]
  * Add a test showing opam pin list not working when the source git directory is missing [#6597 @kit-ty-kate]
  * Add a test making sure the global or system `git` config doesn't change the behaviour of opam [#6992 @kit-ty-kate]
  * Add a test showing the remote and branch names of a git repository extracted by `opam source` [#6992 @kit-ty-kate]
  * Add a test showing the order of install actions for each relevant commands [#6864 @kit-ty-kate]
  * Add a test showing some of the internal steps of `opam init` [#6957 @kit-ty-kate]
  * Add tests for `.install` `root` and `rootexec` fields [#6938 @rjbou]
  * Add a test showing the git commands called by `opam source` [#6955 @sporkl]

### Engine

## Github Actions
  * The Hygiene workflow has been upgraded to Ubuntu 26.04 [#7052 @kit-ty-kate]
  * auto-cancel unreachable jobs in PRs [#7147 @kit-ty-kate]
  * Always start CI runs by an `apt update` [#7156 @kit-ty-kate]
  * Update the opam-repository SHA to the latest commit [#7145 @kit-ty-kate]

## Doc
  * Update the documentation about the latest opam release [#7150 @kit-ty-kate]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state
  * `OpamSwitchState.update_sys_packages` (new in 2.6.0) was removed in favour of the already existing `update_package_metadata` or `update_pin` functions [#7158 @kit-ty-kate]

## opam-solver

## opam-format

## opam-core
  * `OpamStd.Map.update`: was changed to the stdlib version which has a more flexible API and has better performances [#7130 @NathanReb - fix #4915]
  * `OpamStd.Map.union`: was changed to the stdlib version which has a more flexible API and has better performances [#7170, @NathanReb]
  * `OpamStd.Map.strict_union`: added as a replacement for the previous custom `union` API in `OpamStd` but based on `Stdlib`'s `union` for better performances [#7170, @NathanReb]
