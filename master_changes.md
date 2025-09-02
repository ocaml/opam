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
  * Add cli version 2.5 [#6709 @kit-ty-kate]
  * Add mechanism for the `OPAMAUTOANSWER` environment variable (for internal use only) [#6709 @kit-ty-kate]

## Plugins

## Init
  * Remove `getconf` from the list of required runtime tools, which allows `opam init` to work out-of-the-box on Haiku [#6634 @kit-ty-kate - fix #6632]

## Config report

## Actions

## Install
  * More fine grained error message in case of bad hash or missing extra-files error (and remove raw fatal error) [#6696 @rjbou]
  * Do not ignore extra-files whose name is invalid and fail early in that case [#6679 @rjbou @kit-ty-kate]
  * BUG: Fix `opam install pkg --depext-only` exit with code 0 instead of 20 (not found) [#6516 @rjbou - fix #6488]

## Build (package)

## Remove
  * [BUG] Fix `opam remove --force` launching commands in current directory [#6672 @rjbou - fix #6570]

## UI
  * Show the invalid character when detecting an erroneous package name [#6638 @lefessan - fix #6396]
  * Handle non-displayable characters when detecting an erroneous package name or version [#6640 @kit-ty-kate]
  * Remove duplicated directory separator when displaying some rare filenames [#6703 @rjbou]
  * The name of missing or erroneous extra-files are not displayed instead of their path in error messages [#6679 @kit-ty-kate @rjbou]

## Switch

## Config

## Pin

## List

## Show

## Var/Option
  * Make the computation of `pkg:opamfile` match its specification [#6659 @kit-ty-kate - fix #5346]
  * Make global option `default-invariant` modifiable [#6730 @rjbou]

## Update / Upgrade
  * Fix the false-positive mismatch debug warning during `opam update` when faced with nested extra-files on Windows [#6715 @kit-ty-kate]
  * Implement incremental opam file loading to process only changed files during repository updates and repository state loading [#6614 @arozovyk - fix #5824]

## Tree

## Exec

## Source
  * Better error message, especially in case of `Failure` [#6696 @rjbou]
  * Raise a warning instead of an error when an item of `extra-files` is missing [#6696 @rjbou]
  * Do not ignore extra-files whose name is invalid and raise a warning in that case [#6679 @rjbou @kit-ty-kate]

## Lint

## Repository

## Lock

## Clean

## Env

## Opamfile

## External dependencies

## Format upgrade
  * Complete upgrade mechanism to permit on the fly upgrade and write upgrade from repo and switch level [#6416 @rjbou]

## Sandbox

## VCS
  * Check the status of git submodules when checking if a repository is up-to-date [#6132 @kit-ty-kate]

## Build
  * Update the dependency constraint on `patch` to now require its stable version [#6663 @kit-ty-kate]
  * Update the download-if-missing dependencies to their latest version (re.1.14.0, dune.3.20.2, menhir.20250903) [#6700 @kit-ty-kate]
  * Remove `seq` from the list of packages to download-if-missing as it is no longer a dependency of `re` [#6700 @kit-ty-kate]
  * `./configure --enable-static` is now supported on OpenBSD [#6705 @flumf]
  * Add missing constraints to avoid cmdliner 2.0.0 [#6707 @kit-ty-kate]
  * Add patch library dependency to opam-state [#6614 @arozovyk]

## Infrastructure

## Release scripts
  * The OpenBSD binary now a full static binary [#6705 @flumf @kit-ty-kate - fix #6241]

## Install script
  * Add 2.4.1 to the install scripts [#6617 @kit-ty-kate]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client
  * [NEW] Fetch shared archive sources without checksums [#6627 @psafont - fix #5638]

## Shell

## Internal
  * Replace every polymorphic uses of `List.mem` by a version that doesn't use `Repr.equal` [#6644 @kit-ty-kate]
  * Simplify the `src_ext/update-sources.sh` script [#6701 @kit-ty-kate]
  * Homogeneise verbose command output between sandboxed and non sandboxed one [#6675 @rjbou]
  * Add the `install-pin-depends`, `ignore-pin-depends`, `proceed-actions` and `switch-clean-up` named questions [#6611 @kit-ty-kate @rjbou]
  * Add logging for file reads and writes [#6679 @rjbou]

## Internal: Unix

## Internal: Windows

## Test
  * Display file management log when debug test are enabled (negative values) [#6673 @rjbou]

## Benchmarks
  * Benchmark `opam update` [#6681 @arozovyk]

## Reftests
### Tests
  * Add a test for `opam switch link` to make sure it doesn't remove previous switches [#6450 @kit-ty-kate]
  * Add a test showing the error message when faced with invalid characters in package names [#6638 @kit-ty-kate]
  * Add a test for shared fetch without checksum [#6627 @rjbou]
  * Add a test for shared fetch without checksum, and for VCS shared fetch (not handled) [#6627 @rjbou]
  * Add a test showing the error message when faced with an UTF-8 character in the package version [#6640 @kit-ty-kate]
  * Remove `getconf` filtering [#6671 @rjbou]
  * Update/homogenise escaping `BASEDIR` using `printf` [#6671 @rjbou]
  * Homogenise here document usage [#6671 @rjbou]
  * Add a test showing that `opam upgrade` allows downgrades when necessary [#6690 @kit-ty-kate]
  * Add a test showing the behaviour of `opam tree` on local packages that happen to be already pinned [#6688 @kit-ty-kate]
  * Add a test showing the behaviour of version pins when one of the dependencies isn't up-to-date [#6691 @kit-ty-kate]
  * Add a test showing the behaviour of pre-defined variables in command hooks [#6659 @rjbou]
  * Add a test showing the behaviour of `opam var <pkg>:opamfile` [#6659 @kit-ty-kate]
  * Add a test to show homogeneity of outputs on verbose between sandboxed and non sandboxed commands (with `-vv`) [#6675 @rjbou]
  * Update `sed-cmd` reftest reftest [#6675 @rjbou]
  * Add a test showing the behaviour of nested extra-files [#6715 @kit-ty-kate]
  * Add opam file loading tests to `update.test` to demonstrate current behaviour of loading full repository instead of only changed files. [#6614 @arozovyk @rjbou @kit-ty-kate]
  * Fix `env.test` in cases where calling `env` inside of a script outputs a `__CF_USER_TEXT_ENCODING` environment variable that isn't present in `sh -c env` [#6719 @kit-ty-kate]
  * Add complete tests for the `pin-depends` feature [#6611 @rjbou]
  * Complete `action-disk.test` with the behaviour of `extra-files` [#6679 @rjbou]
  * Complete `action-disk.test` with the behaviour of `extra-source` [#6679 @rjbou]
  * Add a test showing the behaviour of opam when faced with outdated git submodule in its local cache [#6153 @kit-ty-kate]
  * Add reftest for `--depext-only` option [#6516 @rjbou]
  * Add a test for `opam remove --force` [#6672 @rjbou]

### Engine
  * Fix gcc < 14.3 bug on mingw i686 [#6624 @kit-ty-kate]
  * Fix support for removing local link directories [#6450 @kit-ty-kate]
  * Bump `OPAM_REPO_SHA` in the github action workflows to allow patch 3.0.0 [#6663 @kit-ty-kate]
  * Allow `sed-cmd` to parse even if no space is after the command [#6675 @rjbou]
  * Harden the regexp used for substituting variable checksums [#6710 @kit-ty-kate]
  * Add the `unset` builtin [#6708 @kit-ty-kate]

## Github Actions
  * bump `actions/checkout` from 4 to 5 [#6643 @kit-ty-kate]
  * Fix the nixos depexts tests (git is now already installed in the nix docker image) [#6652 @kit-ty-kate]
  * Ensure every part of the scripts are run with `set -ue` [#6648 @kit-ty-kate]
  * Only run the `get-changed-files` action when in a PR [#6582 @kit-ty-kate]
  * Add a CI job to test reverse dependencies of opam. Track and report dependency and build failures, hard-failing only on maintained packages. [#6394 @rjbou @arozovyk]
  * Enhance changed files job dependant handling [#6394 @rjbou]
  * Fix macOS builds by installing rsync [#6656 @kit-ty-kate]
  * Use local pin to correctly detect packages dev repo branch in reverse dependency test job [#6655 @arozovyk]
  * Filter false positives in dependency test job using `--coinstallable-with` [#6655 @arozovyk]
  * Improve the revdeps test by ignoring non-released packages [#6657 @kit-ty-kate]
  * Avoid re-testing already tested repositories when testing the revdeps [#6657 @kit-ty-kate]
  * Fix duplication logic in revdeps script [#6666 @arozovyk]
  * Remove patch dependency in depext actions [#6676 @rjbou]
  * Bump opam binary used in depexts actions to 2.4.1 [#6676 @rjbou]
  * Check `src_ext/update-sources.sh` using shellcheck [#6701 @kit-ty-kate]

## Doc
  * Update the installation documentation with the release of opam 2.4.1 [#6620 @kit-ty-kate]
  * Swapped the use of sha384 for sha512 for the release tarball in the installation documentation [#6620 @kit-ty-kate]
  * Improve the `opam pin` man page by being more explicit about which arguments are optional [#6631 @kit-ty-kate]
  * Fix URL to Software Heritage [#6650 @gahr]
  * Clarify conditions in subsection titles in the Packaging page [#6653 @jmid]
  * Upgrade the deprecated md5 `checksum` example to sha256 [#6653 @jmid]
  * Add mention of `opam admin compare-versions` in the Manual. [#6596 @mbarbin]
  * Update release documentation to add a step updating test repository hash and version number in reverse dependecies test script [#6364 @arozovyk]
  * Correct the docstrings for `OpamPath.Switch.Overlay.*` [#6660 @kit-ty-kate]

## Security fixes

# API updates
## opam-client
  * `OpamClientConfig.opam_init`: now takes an optional `auto_answer` argument [#6709 @kit-ty-kate]

## opam-repository
  * `OpamLocal.rsync_*`: Change the return type from `OpamFilename.*` to `unit` [#6658 @kit-ty-kate]
  * `OpamRepository.update`: changed the `'Changes` return type to include `Patch.operation list` of changes. [#6614 @arozovyk]
  * `OpamRepositoryBackend.update` type : include the `Patch.t list` in `Update_patch` variant [#6614 @arozovyk]
  * `OpamRepositoryBackend.get_diff`: include `Patch.t list` in the return type (along with `filename`) [#6614 @arozovyk]

## opam-state
  * `OpamSwitchState.files`: was removed [#6662 @kit-ty-kate]
  * `OpamSwitchState.overlay_opam_file`: was added [6679 @rjbou]
  * `OpamRepositoryState` add `load_opams_from_diff` to update package definitions based on file change operations (diff) [#6614 @arozovyk]
  * `OpamRepositoryState.get_repo_files`: was added [#6679 @kit-ty-kate @rjbou]

## opam-solver

## opam-format
  * `OpamFile.OPAM.get_metadata_dir`: was removed [#6679 @kit-ty-kate]
  * `OpamFile.OPAM.get_extra_files`: no longer takes a named `repos_roots` argument and instead takes a named `get_repo_files` argument. It also now returns the content of the files instead of their path [#6679 @kit-ty-kate @rjbou]
  * `OpamFormula.equal_relop`: was added [#6644 @kit-ty-kate]
  * `OpamTypesBase.{action,pkg_flag,simple_arg,arg,filter,command}_equal`: were added [#6644 @kit-ty-kate]
  * `OpamVariable.variable_contents_equal`: was added [#6644 @kit-ty-kate]
  * `OpamFormula`: add `equal` function for `OpamFormula.t` [#6730 @rjbou]

## opam-core
  * `OpamConsole.confirm`: now takes an optional `name` argument [#6709 @kit-ty-kate]
  * `OpamConsole.log`: does not keep log messages before initialization if the code is ran through a library [#6487 @kit-ty-kate]
  * `OpamCoreConfig.auto_answer`: field and arguments were added [#6709 @kit-ty-kate]
  * `OpamCoreConfig.{answer,anwser_is,answer_is_yes}`: now take a `name` labeled argument [#6709 @kit-ty-kate]
  * `OpamCoreConfig.in_opam`: was added [#6487 @kit-ty-kate]
  * `OpamSystem.cpu_count`: now uses a C binding instead of system utilities to get the number of cores of the current machine [#6634 @kit-ty-kate]
  * `OpamSystem.is_reg_dir`: is now exposed, which returns `true` only if its parameter is a directory, exists and is not a symlink. It returns `false` otherwise [#6450 @kit-ty-kate]
  * `OpamCompat.List.fold_left_map`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.List.equal`: was added [#6644 @kit-ty-kate]
  * `OpamCompat.Map.filter_map`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.MAP`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.Pair.equal`: was added [#6644 @kit-ty-kate]
  * `OpamCompat.String.{starts_with,ends_with,for_all,fold_left}`: were added [#6442 @kit-ty-kate]
  * `OpamHash.check_string`: was added [#6661 @kit-ty-kate]
  * `OpamHash.equal_kind`: was added [#6644 @kit-ty-kate]
  * `OpamStd.Config.auto_answer`: was added [#6709 @kit-ty-kate]
  * `OpamStd.List.fold_left_map`: was moved to `OpamCompat.List.fold_left_map` [#6442 @kit-ty-kate]
  * `OpamStd.List.{cons,find_opt,filter_map}`: were removed. Use `Stdlib.List` instead. [#6442 @kit-ty-kate]
  * `OpamStd.List.mem`: was added, having as argument the equality function [#6644 @kit-ty-kate]
  * `OpamStd.Op.{(@@),(|>)}`: were removed. Use `Stdlib` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Option.{map,iter,compare,equal,to_string,some}`: were removed. Use `Stdlib.Option` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Map.filter_map`: is now the implementation from `Stdlib.Map` when using OCaml >= 4.11 [#6442 @kit-ty-kate]
  * `OpamStd.Map.{find_opt,choose_opt,fold,map,mapi}`: are now the implementation from `Stdlib.Map` [#6442 @kit-ty-kate]
  * `OpamStd.Set.{map,choose_opt,fold}`: are now the implementation from `Stdlib.Set` [#6442 @kit-ty-kate]
  * `OpamStd.String.contains_char`: was removed. Use `Stdlib.String.contains` instead. [#6442 @kit-ty-kate]
  * `OpamStd.String.map`: was removed. Use `Stdlib.String.map` instead. [#6442 @kit-ty-kate]
  * `OpamStd.String.{starts_with,ends_with,for_all,fold_left}`: were moved to `OpamCompat.String` [#6442 @kit-ty-kate]
  * `OpamFilename.create`: deduplicate the directory separator character when the basename starts with one [#6703 @rjbou]
  * `OpamFilename`: add `parse_patch` that preprocesses and parses a patch file. [#6614 @arozovyk]
  * `OpamSystem`: add lower-level `parse_patch` that preprocesses and parses a patch file. [#6614 @arozovyk]
  * `OpamFilename.patch`: use variants to make the input either `Filename.t` or reuse `Patch.diffs` directly. Remove the `?preprocess` argument since the preprocess logic is moved to the `OpamFilename.parse_patch` function that is called only in `OpamVCS` (mirroring the previous logic). [#6614 @arozovyk]
  * `OpamSystem.patch`: change the signature to work directly with `Patch.diffs` (implementation is now the previously `internal_patch` function), parsing is now done separately. [#6614 @arozovyk]
  * `OpamCompat.Lazy`: add `map_val` [#6679 @rjbou]
