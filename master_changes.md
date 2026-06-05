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
  * Update Kate's email address [#6808 @kit-ty-kate]
  * Remove unnecessary uses of `chdir` [#6910 @NathanReb]

## Plugins

## Init
  * ✘ Display an appropriate error message when the file given to `opam init --config` does not exist or is in a VCS. This changes the behaviour for VCS local urls that was previously retrieved. [#5979 @kit-ty-kate - fix #5971]

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI
  * Read full lines when asking for user input when `TERM=dumb` [#6829 @arvidj - fix #6828]
  * Fix a typo in the note telling users about new a depexts bypass [#6489 @rjbou @kit-ty-kate]
  * Opam files parsing error now prints the origin repository of the failing opam file if relevant [#6971 @rjbou]
  * Add support for terminal resizing detection on Solaris/Illumos [#6933 @kit-ty-kate]

## Switch

## Config
  * Fix `opam config subst` so it works with absolute paths [#6936 @NathanReb - fix #6925]

## Pin

## List

## Show
  * Improve performance of `opam show` by reading switch selection only once instead of once per package-version [#6818 @dra27]

## Var/Option

## Update / Upgrade
  * Fixed the bug occuring on version-equivalent package rename (i.e `pkg.00 -> pkg.0`) leading to the package being completely removed. [#6774 @arozovyk fix #6754]
  * Compute the list of available depexts on `opam update` [#6489 @arozovyk - fix #6461]
  * Update depexts availability repository state cache when running `opam update --depexts` [#6489 @arozovyk - fix #6461]
  * Display status message while loading system package availability during `opam update` [#6489 @arozovyk - fix #6461]
  * `opam update` now supports updating a repository that changed a file to a directory of the same name and vice versa [#6915 @rjbou @arozovyk - fix #3830]
  * Add some debug logging for opam files loading from repository (dir vs diff) [#6941 @rjbou]

## Tree

## Exec

## Source

## Lint

## Repository
  * No longer call tar tool to create archives, use tar library instead [#6945 @kit-ty-kate]
  * [BUG] Do not fail on directories named `opam` when scanning the `packages` directory of a repository during `opam repo add` or `opam init` (worked on subsequent `opam update`) [#6941 @kit-ty-kate @rjbou]
  * Speedup repository operations on certain file-systems (e.g. NTFS on Windows or IO constrained machines) by changing its storage in the opam root from plain directory to archive, for HTTP repositories, or non-VCS one if `OPAMREPOSITORYTARRING` is enabled [#6625 @rjbou @kit-ty-kate @arozovyk - fix #5346 #5741 #5648 #5484 #5559 #3050 #6974]

## Lock
  * [BUG] Fix `undefined variable` error when a lock file filter contains an undefined variables: fail gracefully with strict mode, continue and default the variable to false on normal mode [#6947 @rjbou - fix #6946]

## Clean

## Env

## Opamfile
  * The `url` file now only supports the legacy opam 1.2 fields [#6827 @kit-ty-kate]
  * Filter fields in .install files containing destinations with `..` or absolute filepaths as parse errors [#6897 @kit-ty-kate]

## External dependencies
  * Restore the distribution detection on Gentoo [#6886 @kit-ty-kate - fix #6887]
  * Add support for single-quoted values of the /etc/os-release file [#6886 @kit-ty-kate - fix #6887]
  * Fix a string injection from the depexts field to nix-build, when `os-family=nixos` [#6894 @RyanGibb]

## Format upgrade
  * Fix switch and repo format upgrade on Windows. A block occurred because the global lock fd was reopened instead of using the one already opened.  [#6839 @rjbou]
  * Add opam root format upgrade conditional mechanism for hard upgrades [#6949 @rjbou]
  * Stop the opam 1.2 to 2.0 repository upgrade process from downloading packages without checksums to add a non-trusted md5 [#6978 @kit-ty-kate]
  * Add conditional upgrade to opam root 2.6. It is triggered by an already present repository archive (from `OPAMREPOSITORYTARRING` usage, or 2.1 opamroot) [#6625 @rjbou]

## Sandbox
  * Allow the macOS sandbox to write in the `/var/folders/` and `/var/db/mds/` directories as it is required by some of macOS core tools [#4797 @kit-ty-kate - fix #4389 #6460]

## VCS
  * Darcs no longer fall back to “Num Patches” as “Weak Hash” has been supported since 2016 [#6866 @toastal]

## Build
  * Fix Windows build on MSYS2 [#6862 @Firobe]
  * opam no longer depends on `cmdliner` [#6755 @kit-ty-kate - fix #6425]
  * Clean variables before calling make on different projects to avoid clashes [#6769 @kit-ty-kate]
  * Add the upcoming OCaml 5.5 (trunk) support when using dune's dev profile [#6670 @kit-ty-kate]
  * Update the download-if-missing patch to 3.1.0 [#6772 @kit-ty-kate]
  * Harden the Makefile's inline shell scripts [#6751 @kit-ty-kate]
  * Add lower-bounds constraints to the dependencies that have none (`ocamlgraph` >= 1.8.8, `jsonm` >= 1.0.2, `swhid_core` >= 0.1, `uutf` >= 1.0.3) [#6878 @kit-ty-kate]
  * Require `spdx_licenses` >= 1.4.0 to ensure compatibility with SPDX v3 syntax [#6878 @kit-ty-kate]
  * Remove support for building opam with OCaml 4.08, 4.09 and 4.10 [#6879 @kit-ty-kate]
  * Upgrade to ocaml-tar.3.5.0 [#6976 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Fix the placement of the vendored archives in the release tarball [#6765 @kit-ty-kate - fix #6762]
  * Fix the Windows build [#6769 @kit-ty-kate]
  * Harden the Makefile's inline shell scripts [#6751 @kit-ty-kate]
  * Fix the release script not being a noop on re-runs [#6903 @kit-ty-kate]

## Install script
  * Add `2.5.2` to the installers [#7017 @kit-ty-kate]
  * Add `2.5.1` to the installers [#6902 @rjbou]
  * Add `2.5.0~alpha1` to the installers [#6748 @kit-ty-kate]
  * Add `2.5.0~beta1` to the installers [#6795 @kit-ty-kate]
  * Add `2.5.0~rc1` to the installers [#6802 @kit-ty-kate]
  * Fix apparmor profile remplacement option [#6760 @rjbou]
  * Use `install` instead of `mv`+`chmod`+`chown` [#6760 @rjbou]
  * Clean apparmor temporary file [#6760 @rjbou]
  * Use variables instead of plain paths [#6760 @rjbou]
  * Reword apparmor message when user need to check the profile [#6760 @rjbou]
  * Add `2.5.0` to the installers [#6817 @kit-ty-kate]
  * Fix the AppArmor support when installing in `/usr/bin` [#6823 @kit-ty-kate - fix #6820]

## Admin
 * `opam admin upgrade --clear-cache` is now a no-op as it is no longer needed [#6978 @kit-ty-kate]

## Opam installer

## State

## Opam file format

## Solver

## Client
  * Improved depexts handling by caching system package availability during update, avoiding redundant system checks at install time. [#6489 @arozovyk - fix #6461]

## Shell
  * added return 0 for zsh/bash/sh/fish to prevent leaking error codes [#6930 @ta2005]

## Internal
  * Improve cache-loading performance when using OCaml >= 5.4 by using `Gc.ramp_up` [#6515 @dra27]
  * Make OpamStd.String.compare_case allocation free [#6515 @dra27]
  * Add a helper script to help generate the configure file on platforms without autoconf 2.71 [#6878 @kit-ty-kate]
  * Fix a rare potential GC corruption in `OpamStubs.uname` [#6880 @avsm @kit-ty-kate @andrew]
  * Fix a rare potential GC corruption in `OpamStubs.enumRegistry` on Windows [#6882 @kit-ty-kate]

## Internal: Unix

## Internal: Windows

## Test
  * lib/patchDiff: no longer print unecessary information after patch [#6915 @rjbou]
  * lib/patchDiff: Ensure a more consistent output accross Unix and Windows platforms [#6915 @kit-ty-kate]
  * lib/patchdiff: add dir-file transformations tests [#6915 @rjbou]
  * Add the `lib-tests` target to the main Makefile [#6928 @kit-ty-kate]
  * Add `opamUnit` as a basic unit test framework [#6953 @NathanReb]
  * Add unit tests for `OpamFilename.starts_with` and `dir_starts_with` in `tests/lib/core` [#6953 @NathanReb]
  * Add unit test for `OpamSystem.real_path` [#7011 @rjbou]

## Benchmarks
  * Add an even larger real-world diff to benchmark `opam update` [#6567 @kit-ty-kate]

## Reftests
### Tests
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
  * Add tests for `.install` fields handling [#6956 @rjbou]

### Engine
  * Add `http-server` to launch a minimal http server [#6939 @rjbou]
  * Add to automatic path subsitutions lines that contains `packages` [#6625 @rjbou]
  * Add to automatic path substitutions lines that contains switch installation paths [#6956 @rjbou]

## Github Actions
  * Add OCaml 5.4 to the test matrix [#6732 @kit-ty-kate]
  * Ensure `curl`'s exit status to be non-zero on failure [#6684 @kit-ty-kate]
  * Add OCaml trunk to the test matrix [#6684 #6670 @kit-ty-kate]
  * Rename the hygiene script for `shell/install.sh` to install-check [#6768 @kit-ty-kate]
  * Remove the unconditional Windows binary uploads on PRs [#6771 @kit-ty-kate]
  * Bump the `actions/checkout` to version 6 [#6811 @kit-ty-kate]
  * Bump `actions/cache` to version 5 [#6835 @kit-ty-kate]
  * Regenerate the cache when `OPAM_TEST_REPO_SHA` is changed [#6832 #6821 @kit-ty-kate]
  * Trigger the depexts CI when OpamSysPoll is modified [#6886 @kit-ty-kate]
  * Speedup macOS builds by stopping testing alternative solvers on macOS [#6889 @kit-ty-kate]
  * Disable testing conf-clang-format in favour of conf-fts on Alpine [#6888 @kit-ty-kate]
  * Upgrade to use opam 2.5.1 [#6904 @kit-ty-kate]
  * depexts: Always use the latest 'stable' version of each distribution [#6905 @kit-ty-kate]
  * depexts: Always use the already installed ocaml package via ocaml-system [#6905 @kit-ty-kate]
  * Trigger CI upon changes in `doc/` [#6927 @kit-ty-kate - fix #6810]
  * Install `micro_httpd` in test scripts [#6939 @rjbou]
  * Fix ubuntu depext job: remove install of no longer available system packages  'ocaml-compiler-libs' [#6960 @rjbou]
  * Fix opensuse depexts job : force resolution in case of conflict (ocaml/ncurses) [#6960 @rjbou]
  * Bump the opam-repository commits sha [#6976 @kit-ty-kate]
  * Fix the archlinux depexts run [#6976 @kit-ty-kate]
  * Fix the cygwin backend basic tests [#6979 @kit-ty-kate]
  * Fix undeterministic tests [#6978 @kit-ty-kate]
  * Bump `actions/cache` to version 6 [#6983 @kit-ty-kate]
  * Fix Doc & Depends jobs when a opam root format upgrade is needed [#6998 #7002 @rjbou]
  * Upgrade the CI to OCaml 5.5 and 4.14.4 [#6988 @kit-ty-kate]
  * Fix the Cygwin 5.5 build by requiring cygwin gcc 13.4 as 14.4 breaks C++ support [#7007 @kit-ty-kate]

## Doc
  * Update the packaging page to recommend using static archives for releases [#6973 @mseri]
  * Add spacing between two words in `--locked` man section [#6806 @yosefAlsuhaibani]
  * Update the Install page with the new opam 2.5.0 release [#6821 @kit-ty-kate]
  * Mention more explicitely that raw fields are an option [@raphael-proust]
  * Correct configure instruction in README [#6858 @gridbugs @kit-ty-kate]
  * Improve visibility of `depopts` filter note [#6920 @ccoulombel - fix #5367]
  * Call man2html only on actual man pages [#6807 @kit-ty-kate]
  * Replace the API example by a link to the API [#6809 @kit-ty-kate - fix #6637]

## Security fixes
  * Invalidate .install fields containing destination filepath trying to escape their scope [#6897 @kit-ty-kate]
  * Fix a bug that allowed a package to install files anywhere on the system using a symlink to an external directory without warning the user and asking for their permission: CVE-2026-57825. [#7005 @NathanReb]

# API updates
## opam-client
  * `OpamArg`: add `build_options_no_depexts` getter to retrieve the value of the given flag  [#6489 @rjbou]
  * `OpamArg{,Tools}.cli2_6` was added [#6978 @kit-ty-kate]
  * `OpamClientConfig.opam_init`: replace `no_depexts` argument by `depexts` [#6489 @rjbou]
  * `OpamSolution` remove the heuristic of recomputing depexts of additional (pinned) packages. [#6489 @arozovyk]
  * `OpamClient` update the system package status check for dependencies during `opam install --deps-only`, including support for pinned packages; also update this in `OpamAuxCommands.autopin` [#6489 @arozovyk]
  * `OpamSolution.get_depexts` remove no longer needed `recover` option that was used with `--depext-only` option  [#6489 @arozovyk]
  * `OpamConfigCommand.subst` now takes a `filename` instead of a `basename` [#6936 @NathanReb]

## opam-repository
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
  * `OpamFile.Descr` was moved to `OpamFile.Descr_legacy` and a simpler `OpamFile.Descr` module was created only containing non-IO functions removing the outdated `descr` file support [#6827 @kit-ty-kate]
  * `OpamFile.URL` was moved to `OpamFile.URL_legacy` and a simpler `OpamFile.URL` module was created only containing non-IO functions removing the outdated `url` file support [#6827 @kit-ty-kate]
  * `OpamFile.Descr.of_legacy`: was added [#6827 @kit-ty-kate]
  * `OpamFile.URL.of_legacy`: was added [#6827 @kit-ty-kate]
  * `OpamFile`: allow dummy filenames to be added a prefix and still be detected as a dummy filename [#6913 @rjbou]
  * `OpamSysPkg`: add `availability_mode` type to indicate the availability of system packages on a given system [#6489 @arozovyk]
  * `OpamSysPkg`: add `equal_availability_mode` function [#6489 @arozovyk]
  * `OpamTypes`: change `result` type name to `solver_result` to avoid conflicts with Stdlib [#6885 @rjbou]
  * `OpamPathName` was added [#6917 @rjbou]
  * `OpamRepositoryPathName` was added [#6917 @rjbou]
  * `OpamRepositoryPath` was moved from `opam-repository` [#6917 @rjbou]
  * `OpamRepositoryPath.{root,repo,packages_dir,packages,opam,files,descr,url}: have been moved to a new `OpamRepositoryPath.Make` functor [#6680 @rjbou @kit-ty-kate]
  * `OpamFilter.expand_interpolations_in_file`: changed argument type from `basename` to `filename` [#6910 @NathanReb]
  * `OpamFile.OPAM.print_errors`: now prints the repository if the informations is available [#6971 @rjbou]
  * `OpamFile.*.read_from_string`: add optional `?loc` string argument to propagate location information when available (path on disk, archive, etc.), and add logging with level 3 that displays it [#6625 @rjbou]
  * `OpamFile.*`: add `safe_read_from_string` [#6625 @rjbou]
  * `OpamRepositoryPath.tar`: renamed to `repo_tarring`, deprecated, no longer used [#6625 @rjbou]

## opam-core
  * `OpamCmdliner` was added. It is accessible through a new `opam-core.cmdliner` sub-library [#6755 @kit-ty-kate]
  * `OpamUrl`: rename and expose `local_path` as `looks_like_local_path` [#5979 @kit-ty-kate]
  * `OpamCompat.Gc.ramp_up`: was added [#6515 @dra27]
  * `OpamCompat.Int.min`: was added [#6515 @kit-ty-kate]
  * `OpamStd.String.compare_case`: is now allocation free [#6515 @dra27]
  * `OpamVersionCompare.{compare,equal}`: are now allocation free [#6515 @dra27]
  * `OpamCompat.Filename`: was removed [#6879 @kit-ty-kate]
  * `OpamCompat.List.fold_left_map`: was removed [#6879 @kit-ty-kate]
  * `OpamCompat.MAP.filter_map`: was removed [#6879 @kit-ty-kate]
  * `OpamCompat.Map.add_to_list`: was added [#6818 @dra27]
  * `OpamPatch` was created [#6934 @rjbou]
  * `OpamCompat.Sys.sigwinch` was added [#6933 @kit-ty-kate]
  * `OpamSystem`: add `is_dir_read_only` [#6489 @rjbou]
  * `OpamSystem.*patch` were moved to `OpamPatch` [#6934 @rjbou]
  * `OpamFilename`: add `is_dir_read_only` [#6489 @rjbou]
  * `OpamFilename.might_escape`: ensure / is detected as a file separator when called with `~sep:Unspecified` on Windows [#6897 @kit-ty-kate]
  * `OpamFilename.Unix` was added abstracting over `/` separated paths regardless of the current system [#6914 @rjbou @kit-ty-kate]
  * `OpamFilename.in_dir`: removed [#6910 @NathanReb]
  * `OpamSystem.in_tmp_dir`: removed [#6910 @NathanReb]
  * `OpamSystem.in_dir`: removed [#6910 @NathanReb]
  * `OpamSystem.chdir`: removed [#6910 @NathanReb]
  * `OpamSystem.{command,commands,read_command_output}`: add a `?dir: dirname` optional arg to launch the command in a specific directory [#6910 @NathanReb]
  * `OpamFilename.Unix`: add `starts_with` [#6945 @rjbou]
  * `OpamCompat.Seq`: add `to_dispenser` [#6945 @kit-ty-kate]
  * `OpamSystem.directories_with_links`, `OpamSystem.rec_files`, `OpamFilename.rec_files`: add optional `?except_vcs` that default to false to exclude VCS directories [#6625 @kit-ty-kate @rjbou]
  * `OpamFilename.make_tar_gz{_job}`: rename `make_tar_gz_job` into `make_tar_gz` as it no longer need an external process call [#6945 @kit-ty-kate]
  * `OpamFilename.{,dir_}starts_with`: Fix a bug where `foo/bar` would be considered a prefix of `foo/bar-baz` [#6953 @NathanReb - fix #6948]
  * `OpamFilename.{,dir_}starts_with`: `/` and `\` are now equivalent on Windows [#6953 @NathanReb]
  * `OpamFilename.starts_with`: `starts_with "a/b" "a/b"` no longer returns `true` [#6953 @NathanReb]
  * `OpamSystem`: add `open_in` and `open_in_bin` that checks is the file is a directory and raise `Sys_error` in that case [#6941 @rjbou]
  * `OpamFilename.{open_in,open_in_bin}`: now errors when the filename is a directory, in all platforms [#6941 @rjbou]
  * `OpamFilename.Unix`: add `basename` and `dirname` [#6625 @rjbou]
  * `OpamFilename.Unix.Dir`: add `basename` and `dirname` [#6625 @rjbou]
  * `OpamFilename.Unix`: add `remove_prefix` [#6625 @rjbou]
  * `OpamFilename.Unix`: add `to_relative_canonical` [#6625 @kit-ty-kate @rjbou]
  * `OpamFilename.Unix`: add `root_dir` [#6625 @rjbou]
  * `OpamFilename.Unix.{to_dir,to_filename}`: now translate `/` into filesystem directory separator [#6625 @rjbou]
  * `OpamPatch.patch`: no longer patch a file on disk, but take as argument a filesystem abstraction `FS_ABSTR` that delivers the needed functions [#6625 @rjbou]
  * `OpamPatch.parse_patch`: no longer take `~dir` the directory to translate the patch in as argument, it now takes `~translate` argument that is a string option (directory option), if we want to perform a translation in that directory [#6625 @rjbou]
  * `OpamSystem.real_path`: fix a bug where paths after a non existent directory where not resolve [#7011 @kit-ty-kate - fix #7010]
