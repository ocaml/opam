Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  * Add default cli mechanism: deprecated options are accepted (in the major version) if no cli is specified [#4575 @rjbou]
  * Add `opam config` deprecated subcommands in the default cli  [#4575 @rjbou - fix #4503]
  * Add cli versioning for opam environment variables [#4606 @rjbou]
  * Deprecated `build-doc`, `build-test`, `make` [#4581 @rjbou]
  * Add cli versioning for enums of flags with predefined enums [#4606 @rjbou]
  * Ensure the symlink for a plugin is maintained on each invocation [#4621 @dra27 - partially fixes #4619]

## Init
  * Introduce a `default-invariant` config field, restore the 2.0 semantics for
    `default-compiler` [#4607 @AltGr]
  * Fix default invariant with no system compiler [#4644 @AltGr - fix #4640]

## Config report
  * Fix `Not_found` (config file) error [#4570 @rjbou]
  * Print variables of installed compilers and their (installed) dependencies [#4570 @rjbou]

## Install
  * Don't patch twice [#4529 @rjbou]
  * With `--deps-only`, set dependencies as root packages [#4964 @rjbou - fix #4502]
  * Keep global lock only if root format upgrade is performed [#4612 @rjbou - fix #4597]
  * Improve installation times by only tracking files listed in `.install` instead of the whole switch prefix when there are no `install:` instructions (and no preinstall commands) [#4494 @kit-ty-kate @rjbou - fix #4422]

## Remove
  *

## Switch
  * Don't exclude base packages from rebuilds (made some sense in opam 2.0
    with base packages but doesn't make sense with 2.1 switch invariants) [#4569 @dra27]
  * Don't refer to base packages in messages any more [#4623 @dra27 - fixes #4572]

## Pin
  * Don't look for lock files for pin depends [#4511 @rjbou - fix #4505]
  * Fetch sources when pinning an already pinned package with a different url when using working directory [#4542 @rjbou - fix #4484]
  * Don't ask for confirmation for pinning base packages (similarly makes no
    sense with 2.1 switch invariants) [#4571 @dra27]
  * Fix version pin source retrieving: mustn't error if archive opam file is malformed [#4580 @rjbou]

## List
  * --silent renamed to --check [#4595 @dra27 - fix #4323]

## Show
  * Include doc field in opam-show [#4567 @dra27 - partially fix #4565]

## Var
  *

## Option
  *

## Lint
  * Fix W59 & E60 with conf flag handling (no url required) [#4550 @rjbou - fix #4549]
  * Fix W59 & E60 with VCS urls, don't check upstream if url has VCS backend [#4635 @rjbou]
  * Add E67 checksum specified with non archive url [#4635 @rjbou]

## Lock
  * Don't write lock file with `--read-only', `--safe`, and `--dryrun` [#4562 @rjbou - fix #4320]
  * Make consistent with `opam install`, on local pin always take last opam file even if uncomitted [#4562 @rjbou - fix #4320]

## Opamfile
  * Fix `features` parser [#4507 @rjbou]
  * Rename `hidden-version` to `avoid-version` [#4527 @dra27]
  * Fix rewriting with preserved format empty field error [#4634 @rjbou - fix #4628]

## External dependencies
  * Handle macport variants [#4509 @rjbou - fix #4297]
  * Always upgrade all the installed packages when installing a new package on Archlinux [#4556 @kit-ty-kate]
  * Handle some additional environment variables (`OPAMASSUMEDEPEXTS`, `OPAMNODEPEXTS`) [#4587 @AltGr]
  * Improve messages to hint that answering `no` doesn't abort installation [@AltGr]
  * Improve messages to hint that answering `no` doesn't abort installation [#4591 @AltGr]

## Sandbox
  * Fix the conflict with the environment variable name used by dune [#4535 @smorimoto - fix ocaml/dune#4166]
  * Kill builds on Ctrl-C with bubblewrap [#4530 @kit-ty-kate - fix #4400]
  * Linux: mount existing TMPDIR read-only, re-bind `$TMPDIR` to a separate tmpfs [#4589 @AltGr]
  * Fix the sandbox check [#4589 @AltGr]
  * Fix sandbox script shell mistake that made `PWD` read-write on remove actions [#4589 @AltGr]
  * Port bwrap improvements to sandbox_exec [#4589 @AltGr]
  * Fix realpath use for macos, partial revert of #4589 [#4609 @AltGr]

## Repository management
  *

## VCS
  *

## Build
  * Fix opam-devel's tests on platforms without openssl, GNU-diff and a system-wide ocaml [#4500 @kit-ty-kate]
  * Use dune to run reftests [#4376 @emillon]
  * Restrict `extlib` and `dose` version [#4517 @kit-ty-kate]
  * Restrict to opam-file-format 2.1.2 [#4495 @rjbou]
  * Switch to newer version of MCCS (based on newer GLPK) for src_ext [#4559 @AltGr]
  * Bump dune version to 2.8.2 [#4592 @AltGr]
  * Bump the minimal dune requirement to dune 1.11 [#4437 @dra27 @kit-ty-kate]
  * 4.12 compatibility [#4437 @dra27 @kit-ty-kate]
  * Cold compiler updated to 4.12 [#4616 @dra27]
  * Fix build from source when a dune-project file is presented in the parent directory [#4545 @kit-ty-kate]
  * Fix build from source when a dune-project file is presented in the parent directory [#4545 @kit-ty-kate - fix #4537]

## Infrastructure
  * Release scripts: switch to OCaml 4.10.2 by default, add macos/arm64 builds by default [#4559 @AltGr]
  * Release script: add default cli version check on full archive build [#4575 @rjbou]

## Admin
  *

## Opam installer
  *

## State
  * Rename state.cache to include the OpamVersion.magic() string. All .cache files are deleted if any
    cache file is written to, allowing multiple versions of the library to co-exist without constantly
    regenerating it [#4642 @dra27 - fix #4554]

# Opam file format
  *

## Solver
  * Fix Cudf preprocessing [#4534 #4627 @AltGr - fix #4624]
  * Allow to upgrade to a hidden-version package if a hidden-version package is already installed [#4525 @kit-ty-kate]
  * Add support for a few select criteria useful to CI to the 0install solver: `+count[version-lag,solution]` to always choose the oldest version available, `+removed` to not try to keep installed packages [#4631 @kit-ty-kate]
  * Add a --with-0install-solver option to the configure script to enable the 'builtin-0install' solver [#4646 @kit-ty-kate]

## Client
  * ✘ Environment variables initialised only at opam client launch, no more via libraries [#4606 @rjbou]

## Internal
  * Generalise `mk_tristate_opt` to `mk_state_opt` [#4575 @rjbou]
  * Fix `mk_state_opt` and rename to `mk_enum_opt` [#4626 @rjbou]
  * Fix `opam exec` on native Windows when calling cygwin executables [#4588 @AltGr]
  * Fix temporary file with a too long name causing errors on Windows [#4590 @AltGr]
  * CLI: Add flag deprecation and replacement helper [#4595 @rjbou]
  * Win32 Console: fix VT100 support [#3897 @dra27]
  * Tidied the opam files [#4620 @dra27]
  * Externalise cli versioning tools from `OpamArg` into `OpamArgTools` [#4606 @rjbou]
  * Each library defines its own environment variables, that fills the config record [#4606 @rjbou]
  * Harden cygpath wrapper [#4625 @dra27]
  * Reset the plugin symlinks when the root is upgraded [#4641 @dra27 - partial fix for #4619]

## Test
  * Make the reference tests dune-friendly [#4376 @emillon]
  * Rewrite the very old tests and unify them with the newer ones [#4504 @AltGr]
  * Add patch an substitution tests [#4545 @rjbou]
  * Fix configure check in github actions [#4593 @rjbou]
  * GHA: Add default cli check in hygiene job [#4575 @rjbou]
  * GHA: Fix opam-rt on macos, set ocaml-system as switch compiler [#4610 @dra27 @rjbou]
  * GHA: Ignore opam-rt pin depends, opam libs are already pinned locally [#4610 @AltGr @rjbou]
  * GHA: The bootstrap cache also depends on the precise version! [#4618 @dra27]
  * GHA: fix opam-rt specific PR branch use [#4606 @rjbou]
  * Add switch creation tests: (dead)locking and switch defitinion at action time [#4612 @rjbou]
  * Remove debug information from reftest [#4612 @rjbou]
  * Add preserved format test [#4634 @rjbou]

## Shell
  *

## Doc
  * Install page: add OSX arm64 [#4506 @eth-arm]
  * Document the default build environment variables [#4496 @kit-ty-kate]
  * Remove useless span tag in manual [#4513 @dannywillems]
  * Fix typo [#4637 @UnixJunkie]
