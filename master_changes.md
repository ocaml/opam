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
  * Remove `getconf` from the list of required runtime tools, which allows `opam init` to work out-of-the-box on Haiku [#6634 @kit-ty-kate - fix #6632]

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI
  * Show the invalid character when detecting an erroneous package name [#6638 @lefessan - fix #6396]
  * Handle non-displayable characters when detecting an erroneous package name or version [#6640 @kit-ty-kate]

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
  * Complete upgrade mechanism to permit on the fly upgrade and write upgrade from repo and switch level [#6416 @rjbou]

## Sandbox

## VCS

## Build
  * Update the dependency constraint on `patch` to now require its stable version [#6663 @kit-ty-kate]

## Infrastructure

## Release scripts

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

## Internal: Unix

## Internal: Windows

## Test

## Benchmarks

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

### Engine
  * Fix gcc < 14.3 bug on mingw i686 [#6624 @kit-ty-kate]
  * Fix support for removing local link directories [#6450 @kit-ty-kate]
  * Bump `OPAM_REPO_SHA` in the github action workflows to allow patch 3.0.0 [#6663 @kit-ty-kate]

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

## opam-repository
  * `OpamLocal.rsync_*`: Change the return type from `OpamFilename.*` to `unit` [#6658 @kit-ty-kate]

## opam-state
  * `OpamSwitchState.files`: was removed [#6662 @kit-ty-kate]

## opam-solver

## opam-format

## opam-core
  * `OpamConsole.log`: does not keep log messages before initialization if the code is ran through a library [#6487 @kit-ty-kate]
  * `OpamCoreConfig.in_opam`: was added [#6487 @kit-ty-kate]
  * `OpamSystem.cpu_count`: now uses a C binding instead of system utilities to get the number of cores of the current machine [#6634 @kit-ty-kate]
  * `OpamSystem.is_reg_dir`: is now exposed, which returns `true` only if its parameter is a directory, exists and is not a symlink. It returns `false` otherwise [#6450 @kit-ty-kate]
  * `OpamCompat.List.fold_left_map`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.Map.filter_map`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.MAP`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.String.{starts_with,ends_with,for_all,fold_left}`: were added [#6442 @kit-ty-kate]
  * `OpamHash.check_string`: was added [#6661 @kit-ty-kate]
  * `OpamStd.List.fold_left_map`: was moved to `OpamCompat.List.fold_left_map` [#6442 @kit-ty-kate]
  * `OpamStd.List.{cons,find_opt,filter_map}`: were removed. Use `Stdlib.List` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Op.{(@@),(|>)}`: were removed. Use `Stdlib` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Option.{map,iter,compare,equal,to_string,some}`: were removed. Use `Stdlib.Option` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Map.filter_map`: is now the implementation from `Stdlib.Map` when using OCaml >= 4.11 [#6442 @kit-ty-kate]
  * `OpamStd.Map.{find_opt,choose_opt,fold,map,mapi}`: are now the implementation from `Stdlib.Map` [#6442 @kit-ty-kate]
  * `OpamStd.Set.{map,choose_opt,fold}`: are now the implementation from `Stdlib.Set` [#6442 @kit-ty-kate]
  * `OpamStd.String.contains_char`: was removed. Use `Stdlib.String.contains` instead. [#6442 @kit-ty-kate]
  * `OpamStd.String.map`: was removed. Use `Stdlib.String.map` instead. [#6442 @kit-ty-kate]
  * `OpamStd.String.{starts_with,ends_with,for_all,fold_left}`: were moved to `OpamCompat.String` [#6442 @kit-ty-kate]
