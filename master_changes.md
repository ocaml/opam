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

  * Fix a bug triggering unnecessary reinstallation of packages not directly involved in the solution [#7154 @NathanReb]
  * Fix a bug where `opam install --depext-only` would try to install depexts of packages that needed to be removed or upgraded [#7166, @NathanReb]

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
  * When fetching a git repository (i.e. `--dev-repo` or a package with git url), the resulting git branch is now deterministically named `main` instead of taking the system's `init.defaultBranch` [#6992 @kit-ty-kate]
  * `opam source` no longer sets opam-specific git configuration options [#6955 @sporkl]
  * `opam source` now uses VCS `clone` when possible [#6955 @sporkl]

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
  * Add support for using git repositories owned by another local user [#6980 @kit-ty-kate - fix #6963]
  * Use `/dev/null` on both Unix and Windows when setting `GIT_CONFIG_*` (NUL is not accepted in Git-for-Windows 2.56.0.windows.1) [#7085 @kit-ty-kate]

## Build
  * Upgrade the autoconf generated files (`configure`) to autoconf 2.72 [#7052 @kit-ty-kate]
  * Upgrade to cudf 0.11\~rc1 [#7145 @kit-ty-kate]
  * Upgrade to checkseum 0.5.4 [#7172 @kit-ty-kate]
  * Upgrade to decompress 1.6.1 [#7172 @kit-ty-kate]

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
  * Remove unnecessary set operations in `OpamSwitchSate.universe` [#7154 @NathanReb]

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
  * Add tests checking installation and source for mercurial repositories [#6955 @sporkl]
  * Add tests checking installation and source for darcs repositories [#6955 @sporkl]

### Engine

## Github Actions
  * The Hygiene workflow has been upgraded to Ubuntu 26.04 [#7052 @kit-ty-kate]
  * auto-cancel unreachable jobs in PRs [#7147 @kit-ty-kate]
  * Always start CI runs by an `apt update` [#7156 @kit-ty-kate]
  * Update the opam-repository SHA to the latest commit [#7145 @kit-ty-kate]
  * Add a test showing opam using a git repository owned by root [#6980 @kit-ty-kate]

## Doc
  * Update the documentation about the latest opam release [#7150 @kit-ty-kate]
  * Document the Git environment variables in the FAQ [#7187 @kit-ty-kate @MisterDA - fix #7152]

## Security fixes

# API updates
## opam-client

## opam-repository
  * `OpamGit`: git calls now will all carry git config `safe.directory=.` (current directory) [#6980 @kit-ty-kate - fix #6963]

## opam-state
  * `OpamSwitchState.update_sys_packages` (new in 2.6.0) was removed in favour of the already existing `update_package_metadata` or `update_pin` functions [#7158 @kit-ty-kate]
  * `OpamGit.env` was added [#6992 @kit-ty-kate]
  * `OpamGit`: git is now always called with the `GIT_CONFIG_GLOBAL` and `GIT_CONFIG_SYSTEM` environment variables set to `/dev/null` [#6992 @kit-ty-kate]
  * `OpamRepository.pull{,shared}_tree`: add an optional `?for_source` argument [#6955 @sporkl]
  * `OpamRepositoryBackend.S.pull_url`: add an optional `?for_source` argument [#6955 @sporkl]
  * `OpamRepositoryPath` was moved to `opam-format` [#6917 @rjbou]
  * `OpamRepositoryRoot` was added [#6680 @kit-ty-kate @rjbou]
  * `OpamTar`: add module to manipulate tar gz archive. It handles only files, not directories [#6945 @kit-ty-kate @rjbou]
  * `OpamRepositoryCommand.update_with_auto_upgrade`, `OpamUpdate.repository`: no longer call an external process to create an archive [#6945 @kit-ty-kate]
  * `OpamTar`: add `patch` function to patch files in an tar gz archive [#6625 @rjbou]
  * `OpamTar.create`: add `?flat` argument to do not integrate the target root directory in the archive [#6625 @rjbou]
  * `OpamTar.create`: add `?except_vcs` argument exclude VCS files from archive creation [#6625 @rjbou]
  * `OpamTar`: when an archive is opened, the first step is to check and normalise canonical paths (no `/../`, remove `./`, etc.) [#6625 @rjbou]
  * `OpamRepositoryRoot`: add `remove_prefix` and `remove_prefix_dir` [#6625 @rjbou]
  * `OpamRepositoryRoot`: add `read_file` that reads an `OpamFile.t` using its pp from the archive [#6625 @rjbou]
  * `OpamRepositoryBackend.get_diff`: now computes the diff between two repository roots (dir, archive), instead of only dirs [#6625 @rjbou]
  * `OpamRepositoryRoot`: add `Tgz` module for tar gz archive repository root support [#6625 @rjbou @kit-ty-kate]
  * `OpamVCS.init`: add an optional `?for_source` argument [#6955 @sporkl]
  * `OpamVCS.clone`: add new VCS `clone` function [#6955 @sporkl]

## opam-state
  * `OpamStateConfig.t`: replace `no_depexts` fields that contains disabling informations by `depexts` field that returns if the depexts mechanism is enabled. This field is automatically update by global config value in `OpamStateConfig.load_defaults` [#6489 @rjbou]
  * `OpamStateConfig.options_fun`: replace `no_depexts` argument by `depexts` [#6489 @rjbou]
  * `OpamRepositoryState.load_opams_from_diff` track added packages to avoid removing version-equivalent packages [#6774 @arozovyk fix #6754]
  * `OpamGlobalState.all_installed_versions`: was added [#6818 @dra27]
  * `OpamGlobalState.installed_versions`: was removed [#6818 @dra27]
  * `OpamStateTypes.global_state`: add field `lock` that contains the global lock (not config one) [#6839 @rjbou]
  * `OpamStateTypes`: add `os_family` type that was defined and used internally in `OpamSysInteract` [#6489 @rjbou]
  * `OpamSysInteract`: add `disable_depexts_note` to be used to display a note to disable depexts [#6489 @rjbou]
  * `OpamSysInteract`: add some os families helpers `string_of_os_family`, `equal_os_family`, `same_os_family` [#6489 @rjbou]
  * `OpamSysInteract`: add `available_packages` and `installed_packages` to be computed separately, redefine `packages_status` accordingly. These funct-ions are now no-op if the given system packages set is empty.  [#6489 @arozovyk]
  * `OpamGlobalState`: add `is_root_read_only` to check if we are in sandboxed environment [#6489 @rjbou]
  * `OpamSwitchState`: add `update_sys_packages` to update depexts status of a set of packages. [#6489 @arozovyk]
  * `OpamSysInteract`: add `available_packages` and `installed_packages` to be computed separately, redefine `packages_status` accordingly [#6489 @arozovyk]
  * `OpamStateTypes`: add available system package status field `repos_syspkgs_available` (and its type `repo_syspkgs_available`) in `repos_state` for all the depexts declared in repo's packages. The new field is also added to the cache. [#6489 @arozovyk @rjbou]
  * `OpamRepositoryState.load`: load repo's available system packages [#6489 @arozovyk]
  * `OpamFileTools`: add `opams_depexts` to consolidate depexts extraction logic from individual opam files and package maps [#6489 @arozovyk]
  * `OpamUpdate.download_package_source`: add an optional `?for_source` argument [#6955 @sporkl]
  * `OpamUpdate`: add `update_sys_available_cache` to update the system package availability cache in repository state [#6489 @arozovyk]
  * `OpamUpdate.get_sys_available`: factorize depexts availability computation logic from `OpamUpdate.repositories` [#6489 @arozovyk]
  * `OpamRepositoryState`: add `syspkgs_available` that returns the stored depext availability status in repository state [#6489 @rjbou]
  * `OpamSysInteract`: add `available_packages_and_family` that returns availability status and the os family [#6489 @rjbou]
  * `OpamRepositoryState.load_opams_from_dir`: now sorts files and directories read from disk before processing them [#6941 @rjbou]
  * `OpamFileTools`: add new `lint_repo_package` that lints a file from a repository root [#6625 @rjbou]
  * `OpamRepositoryState`: add `load_opams_from_tgz` [#6625 @rjbou]
  * `OpamRepositoryState`: add `load_opams` that operates from a repository root [#6625 @rjbou]
  * `OpamStateTypes.repos_state`: remove `repos_tmp` field [#6625 @kit-ty-kate @rjbou]

## opam-solver

## opam-format

## opam-core
  * `OpamStd.Map.update`: was changed to the stdlib version which has a more flexible API and has better performances [#7130 @NathanReb - fix #4915]
  * `OpamStd.Map.union`: was changed to the stdlib version which has a more flexible API and has better performances [#7170, @NathanReb]
  * `OpamStd.Map.strict_union`: added as a replacement for the previous custom `union` API in `OpamStd` but based on `Stdlib`'s `union` for better performances [#7170, @NathanReb]
