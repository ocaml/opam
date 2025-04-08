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
  * Add 2.4.1 to the install scripts [#6617 @kit-ty-kate]

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
  * Update the installation documentation with the release of opam 2.4.1 [#6620 @kit-ty-kate]
  * Swapped the use of sha384 for sha512 for the release tarball in the installation documentation [#6620 @kit-ty-kate]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state
  * `OpamStateConfig`: Make the `?lock_kind` parameters non-optional to avoid breaking the library users after they upgrade their opam root [#5488 @kit-ty-kate]
  * `OpamSwitchState.did_you_mean`: was added, returning a hint string when package names are misspelled [#6434 @arozovyk]
  * `OpamSwitchState.load_selections`: Make the `?lock_kind` parameter non-optional to avoid breaking the library users after they upgrade their opam root [#5488 @kit-ty-kate]

## opam-solver

## opam-format

## opam-core
  * `OpamCompat.String.{spellcheck}`: (along with all its dependencies, which are not exposed for now) was added [#6434 @arozovyk]
  * `OpamConsole`: Replace `black` text style (unused and not very readable) by `gray` [#6358 @kit-ty-kate]
  * `OpamConsole.pause`: Ensure the function always prints a newline character at the end [#6376 @kit-ty-kate]
  * `OpamFilename.patch`: now returns `exn option` instead of `exn option OpamProcess.job` and no longer calls the system GNU Patch [#5892 @kit-ty-kate]
  * `OpamFilename.patch`: a named-parameter `~allow_unclean` was added [#5892 @kit-ty-kate]
  * `OpamHash.all_kinds`: was added, which returns the list of all possible values of `OpamHash.kind` [#5960 @kit-ty-kate]
  * `OpamStd.List.split`: Improve performance [#6210 @kit-ty-kate]
  * `OpamStd.Option.equal_some`: was added, which tests equality of an option with a value [#6381 @kit-ty-kate]
  * `OpamStd.Sys.{get_terminal_columns,uname,getconf,guess_shell_compat}`: Harden the process calls to account for failures [#6230 @kit-ty-kate - fix #6215]
  * `OpamStd.Sys.getconf`: was removed, replaced by `get_long_bit` [#6217 @kit-ty-kate]
  * `OpamStd.Sys.get_long_bit`: was added, which returns the output of the `getconf LONG_BIT` command [#6217 @kit-ty-kate]
  * `OpamStd.Sys.uname`: now returns the memoized result of the `uname` function from the C standard library [#6217 @kit-ty-kate]
  * `OpamStd.Sys.get_freebsd_version`: was added, which returns the output of the `uname -U` command [#6217 @kit-ty-kate]
  * `OpamStubs.get_stdout_ws_col`: new Unix-only function returning the number of columns of the current terminal window [#6244 @kit-ty-kate]
  * `OpamSystem`: add `is_archive_from_string` that does the same than `is_archive` but without looking at the file, only analysing the string (extension) [#6219 @rjbou]
  * `OpamSystem.get_files`: was exposed which returns the list of files (without prefix) inside the given directory [#5892 @kit-ty-kate]
  * `OpamSystem.remove_dir`: do not fail with an exception when directory is a symbolic link [#6276 @btjorge @rjbou - fix #6275]
  * `OpamSystem.patch`: now returns `exn option` instead of `exn option OpamProcess.job` and no longer calls the system GNU Patch [#5892 @kit-ty-kate]
  * `OpamSystem.patch`: a named-parameter `~allow_unclean` was added [#5892 @kit-ty-kate]
  * `OpamSystem.patch`: do not remove the original patch file if called with `~preprocess:false` [#5892 @kit-ty-kate]
  * `OpamParallel.*.{map,reduce,iter}`: Run `Gc.compact` when the main process is waiting for the children processes for the first time [#5396 @kkeundotnet]
  * `OpamSystem`, `OpamFilename`: add `with_tmp_file` and `with_tmp_file_job` function, that create a file name in temporary directory and removes it at the end of the call [#6036 @rjbou]
