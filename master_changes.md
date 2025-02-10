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
  * Bump the version of opam to 2.4.0~alpha1~dev [#6204 @kit-ty-kate]

## Global CLI
  * Add cli version 2.4 [#6268 @mbarbin @rjbou]

## Plugins

## Init
  * [BUG] Fix the detection of `ZDOTDIR` when using `zsh` [#6299 @acasta-yhliu - fix #6281]

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI
  * [BUG] Fix the detection of the current terminal size [#6244 @kit-ty-kate - fix #6243]
  * [BUG] Ensure the output of opam commands using a column style UI stay consistent accross environment by setting the number of columns to 80 if stdout is not a tty and if the `COLUMNS` env variable is not set [#6244 @kit-ty-kate]
  * Improve the messages when a package is not up-to-date on opam upgrade [#6272 @kit-ty-kate - fix #6270]
  * Use a non-underline uppercase character to denotate the default when asking a question [#6289 @hannesm @kit-ty-kate - fix #6288]
  * Do not pre-write the answer to questions with the default anwser [#6376 @kit-ty-kate]

## Switch
  * [BUG] Fix `opam switch remove <dir>` failure when it is a linked switch [#6276 @btjorge - fix #6275]
  * [BUG] Fix `opam switch list-available` when given several arguments [#6318 @kit-ty-kate]
  * [BUG] Correctly handle `pkg.version` pattern in `opam switch list-available` [#6186 @arozovyk - fix #6152]

## Config

## Pin
  * [NEW] Make it so pin list display the current revision of a pinned repository in a new column [#6274 @desumn - fix #5533]
  * [BUG] Stop double pin of packages located in ./opam/opam [#6343 @kit-ty-kate - fix #6342]
  * Don't ask confirmation when pinning an unknown package (absent from repositories) [#6309 @kit-ty-kate @rjbou - fix #3199]
  * [BUG] Do not ask to install pin-depends twice [#6375 @kit-ty-kate - fix #6374]

## List

## Show
  * Display the version number of packages flagged with avoid-version/deprecated gray [#6358 @kit-ty-kate - fix #6354]

## Var/Option

## Update / Upgrade
  * [BUG] Do not show the not-up-to-date message with packages tagged with avoid-version [#6273 @kit-ty-kate - fix #6271]
  * [BUG] Fix a regression on `opam upgrade <package>` upgrading unrelated packages [#6373 @AltGr]
  * [BUG] Fix a regression on `opam upgrade --all <uninstalled-pkg>` not upgrading the whole switch [#6373 @kit-ty-kate]

## Tree

## Exec

## Source
  * [NEW] Add options `--require-checksums` and `--no-checksums` to harmonise with `opam install` [#5563 @rjbou]

## Lint

## Repository
  * Accurately tag `curl` download command when loaded from global config file [#6270 @rjbou]
  * Remove wget support for Software Heritage fallback [#6036 @rjbou - fix #5721]
  * [BUG] Fix SWH liveness check [#6036 @rjbou - fix #5721]
  * Update SWH API request [#6036 @rjbou]
  * Rework SWH fallback to have a more correct archive retrieval and more fine grained error handling [#6036 @rjbou - fix #5721]
  * Check that the repositories given to `opam repository remove` actually exist [#5014 @kit-ty-kate - fixes #5012]

## Lock

## Clean

## Env
  * Add the `OPAMSOLVERTOLERANCE` environment variable to allow users to fix solver timeouts for good [#5510 @kit-ty-kate - fix #3230]

## Opamfile

## External dependencies
  * Add apt-rpm/ALTLinux family support for depext [#6207 @RiderALT]
  * Lookup at `gpatch` before `patch` on macOS now that both homebrew and macports expose `gpatch` as `gpatch` since Homebrew/homebrew-core#174687 [#6255 @kit-ty-kate]
  * Relax lookup on OpenBSD to consider all installed packages [#6362 @semarie]
  * Speedup the detection of available system packages with pacman and brew [#6324 @kit-ty-kate]

## Format upgrade

## Sandbox
  * Respect the `DUNE_CACHE_ROOT` environment variable if it exists [#6326 @smorimoto]
  * Fix sandboxing support in NixOS [#6333 @kit-ty-kate]

## VCS

## Build
  * Upgrade to opam-file-format 2.2.0~alpha1 [#6321 @kit-ty-kate]
  * Add menhir to the list of vendored packages [#6321 @kit-ty-kate]
  * Upgrade the vendored spdx\_licenses to 1.3.0 [#6370 @kit-ty-kate - fix #6369]
  * Bump the vendored dune to 3.16.1 [#6192 @kit-ty-kate]
  * Fix compilation on macOS with OCaml 5.3 by bumping the vendored mccs to 1.1+19 [#6192 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Simplify the making of stripped binaries by introducing the `make opam-stripped` target [#6208 @kit-ty-kate]
  * Upgrade the Alpine Linux container where the Linux release binaries are built from Alpine 3.13 to 3.20 [#6237 @kit-ty-kate]
  * Make the release script produce a Linux/riscv64 binary [#6237 @kit-ty-kate]
  * Upgrade the FreeBSD qemu image where the FreeBSD binary is built from FreeBSD 13.2 to 14.1 [#6237 @kit-ty-kate]
  * Upgrade the OpenBSD qemu image where the OpenBSD binary is built from OpenBSD 7.4 to 7.6 [#6237 @kit-ty-kate]
  * Simplify and improve the reliability of the one-click release script by switching away from a passwordless setup [#6237 @kit-ty-kate]

## Install script
  * Add 2.3.0\~alpha1 to the install scripts [#6203 @kit-ty-kate]
  * Add 2.3.0\~beta1 to the install scripts [#6238 @rjbou]
  * Add opam 2.3.0\~beta2 to the install scripts [#6262 @kit-ty-kate]
  * Add opam 2.3.0\~rc1 to the install scripts [#6282 @kit-ty-kate]
  * Add opam 2.3.0 to the install scripts [#6293 @kit-ty-kate]

## Admin
  * ◈ Add `opam admin compare-versions` to compare package versions for sanity checks [#6197 @mbarbin]
  * [BUG] Fix `opam admin check` in the presence of the `with-dev-setup` variable [#6331 @kit-ty-kate - fix #6329]
  * ✘ The `-i`/`--ignore-test-doc` argument has been removed from `opam admin check` [#6335 @kit-ty-kate]
  * ✘ `opam admin check` now sets `with-test` and `with-doc` to `false` instead of `true` [#6335 @kit-ty-kate]

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell
  * Make `shell/install.sh` more robust, using shellcheck [#6313 @ElectreAAS @kit-ty-kate]

## Internal
  * Make `curl` the default download tool instead of `wget` on macOS [#6304 @kit-ty-kate]
  * download tool: Use fetch on DragonFlyBSD and ftp on NetBSD [#6305 @kit-ty-kate]
  * Prefer curl over any other download tools on every systems, if available [#6305 @kit-ty-kate]
  * Avoid issues when using wget2 where the requested url might return an html page instead of the expected content [#6303 @kit-ty-kate]
  * Ensure each repositories stored in repos-config is associated with an URL [#6249 @kit-ty-kate]
  * Run `Gc.compact` in OpamParallel, when the main process is waiting for the children processes for the first time [#5396 @kkeundotnet]

## Internal: Unix
  * Use a C stub to call the `uname` function from the C standard library instead of calling the `uname` POSIX command [#6217 @kit-ty-kate]

## Internal: Windows

## Test

## Benchmarks
  * Add benchmarks for `opam show` [#6212 @kit-ty-kate]
  * Add benchmarks for `OpamStd.String.split` [#6212 @kit-ty-kate]

## Reftests
### Tests
  * Add switch removal test: failure on removal linked switch [#6276 @btjorge]
  * Extend the tests on opam admin to include packages using builtin global variables [#6331 @kit-ty-kate]
  * Extend the tests on opam admin check by including all the arguments [#6331 @kit-ty-kate @rjbou]
  * Add double pinning test in case of opam/opam opam file [#6343 @rjbou]
  * Make sure `download.test` does not fail due to a checksum collision in the download cache [#6378 @kit-ty-kate]
  * Add a test showing the behaviour of `opam upgrade` with packages flagged with `avoid-version`/`deprecated` [#6273 @kit-ty-kate]
  * Add a test showing the behaviour when a pin depend is unpinned [#6380 @rjbou]
  * Add a test to ensure `opam upgrade <pkg>` will not upgrade unrelated things [#6373 @kit-ty-kate]

### Engine

## Github Actions
  * Add a doc generation job under linux [#5349 @rjbou]
  * Update the github action scripts now that homebrew renamed the GNU patch binary to gpatch [#6296 @kit-ty-kate]
  * Add branch scheme `username/branch` for opam-rt specific branch to use [#6274 @rjbou]
  * Check `shell/install.sh` using `shellcheck` [#6313 @kit-ty-kate]
  * Fix the alpine depexts test [#6363 @kit-ty-kate]
  * Speedup the gentoo depexts test [#6363 @kit-ty-kate]
  * Add OCaml 5.3 to the build matrix [#6192 @kit-ty-kate]
  * Add OCaml 5.3/MSVC to the build matrix [#6192 @kit-ty-kate]
  * Add a test making sure `opam init` works in the absence of `OPAMROOT` [#5663 @kit-ty-kate]

## Doc
  * Update the command to install opam to point to the new simplified url on opam.ocaml.org [#6226 @kit-ty-kate]
  * Fix debian manual url fragment [#6231 @RyanGibb]
  * Change example of non-letter in version ordering [#6252 @gridbugs]
  * Remove redundant `+` in version BNF definition (it is already present in `identchar`) [#6252 @rjbou]
  * mli documentation: fix code blocks [#6150 @rjbou]
  * mli documentation: fix code blocks, references [#6150 @rjbou]
  * mli documentation: fix code blocks, references, add `@raise` tags [#6150 @rjbou]
  * Unhide `OpamProcess` functions [#6150 @rjbou]
  * Fix a typo in the default man page [#6267 @fccm2]
  * Point users to ways to override the default opam root location in the opam init manpage [#6251 @kit-ty-kate]
  * Manual: Document the stamp field from repo files [#6306 @kit-ty-kate]
  * Fix typo in pin edit man page doc [#6315 @shym]
  * Clarify documentation for `enable` pseudo-variable [#5659 @gridbugs]
  * Manual: add information when flags (`avoid-version`, `deprecated`) were introduced [#6320 @hannesm]
  * Add winget command for installing opam [#6338 @tobil4sk]
  * Fix broken link to non-existing archlinux community repo [#6361 @juergenhoetzel]
  * Document the meaning of colored version numbers in the manpage of `opam show` [#6358 @kit-ty-kate]
  * Add an explanation of how plugins work to the manual [#5627 @kit-ty-kate]

## Security fixes

# API updates
## opam-client
  * `OpamArg.InvalidCLI`: export exception [#6150 @rjbou]
  * `OpamArg`: export `require_checksums` and `no_checksums`, that are shared with `build_options` [#5563 @rjbou]
  * `OpamRepositoryCommand.switch_repos`: expose the function [#5014 @kit-ty-kate]

## opam-repository
  * `OpamDownload.get_output`: fix `wget` option for `POST` requests [#6036 @rjbou]
  * `OpamDownload.get_output`: use long form for `curl` `POST` request option [#6036 @rjbou]
  * `OpamDownload.download`: more fine grained HTTP request error code detection for curl [#6036 @rjbou]

## opam-state

## opam-solver

## opam-format
  * `OpamFormula.string_of_relop`: export function [#6197 @mbarbin]
  * `OpamFormula.all_relop`: a list of all operators [#6197 @mbarbin]
  * `OpamFile.Repos_config.t`: change the type to not allow repositories without an URL [#6249 @kit-ty-kate]

## opam-core
  * `OpamConsole`: Replace `black` text style (unused and not very readable) by `gray` [#6358 @kit-ty-kate]
  * `OpamConsole.pause`: Ensure the function always prints a newline character at the end [#6376 @kit-ty-kate]
  * `OpamStd.List.split`: Improve performance [#6210 @kit-ty-kate]
  * `OpamStd.Sys.{get_terminal_columns,uname,getconf,guess_shell_compat}`: Harden the process calls to account for failures [#6230 @kit-ty-kate - fix #6215]
  * `OpamStd.Sys.getconf`: was removed, replaced by `get_long_bit` [#6217 @kit-ty-kate]
  * `OpamStd.Sys.get_long_bit`: was added, which returns the output of the `getconf LONG_BIT` command [#6217 @kit-ty-kate]
  * `OpamStd.Sys.uname`: now returns the memoized result of the `uname` function from the C standard library [#6217 @kit-ty-kate]
  * `OpamStd.Sys.get_freebsd_version`: was added, which returns the output of the `uname -U` command [#6217 @kit-ty-kate]
  * `OpamStubs.get_stdout_ws_col`: new Unix-only function returning the number of columns of the current terminal window [#6244 @kit-ty-kate]
  * `OpamSystem`: add `is_archive_from_string` that does the same than `is_archive` but without looking at the file, only analysing the string (extension) [#6219 @rjbou]
  * `OpamSystem.remove_dir`: do not fail with an exception when directory is a symbolic link [#6276 @btjorge @rjbou - fix #6275]
  * `OpamParallel.*.{map,reduce,iter}`: Run `Gc.compact` when the main process is waiting for the children processes for the first time [#5396 @kkeundotnet]
  * `OpamSystem`, `OpamFilename`: add `with_tmp_file` and `with_tmp_file_job` function, that create a file name in temporary directory and removes it at the end of the call [#6036 @rjbou]
