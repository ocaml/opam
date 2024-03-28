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
  * Bump the version number after the release of 2.2.0~beta1 [#5785 @kit-ty-kate]

## Global CLI

## Plugins

## Init
  * Add rsync package to internal Cygwin packages list (enables local pinning and is used by the VCS backends [#5808 @dra27]
  * Recommend enabling Developer Mode on Windows [#5831 @dra27]
  * Disable ACL in Cygwin internal install to avoid permission mismatch errors [#5796 @kit-ty-kate - fix #5781]
  * Add `sys-pkg-manager-cmd` as an accepted field in opamrc files [#5847 @rjbou - fix #5844]
  * Fix `git-location` handling in init config file [#5848 @rjbou - fix #5845]
  * Fix MSYS2 support [#5843 @rjbou - fix #5683]
  * Test if file exists before sourcing in fish + powershell [#5864 @ElectreAAS]
  * Replace the dependency on GNU patch by a strict dependency on git [#5400 @kit-ty-kate - fix #3433 #3782 #3639]
  * Properly test if "we're in interactive mode" instead of "in a tty" in fish script [#5866 @ElectreAAS]
  * Make the computation of the init default `sys-ocaml-*` eval variables on Windows faster, no more depending on Cygwin [#5829 @dra27 @rjbou]
  * Simplify computation of OCaml init default `sys-ocaml-*` eval variables on Unix [#5829 @dra27]
  * Add a init OCaml `sys-ocaml-system` eval variable [#5829 @dra27]
  * Mark the internal cygwin installation as recommended [#5903 @kit-ty-kate]

## Config report

## Actions

## Install

## Remove

## Switch

## Config

## Pin

## List

## Show

## Var/Option
  * Disable Software Heritage fallbacks by default [#5899 @kit-ty-kate]

## Update / Upgrade

## Tree

## Exec

## Source

## Lint

## Repository

## Lock

## Clean

## Env
  * Fix shell detection on Windows when opam is called via Cygwin's /usr/bin/env even though cmd/powershell is used [#5797 @kit-ty-kate]
  * Fix incorrect deduplication of environment variables on update. Effect was that FOO += "" would occlude the value of FOO in the environment [#5837 @dra27]
  * Fix regression from #5356 on the detection of out-of-date environment variables. As part of a refactoring, a filter predicate got inverted [#5837 @dra27]
  * Unixify Windows paths in init shells scripts (sh, bash, zsh, fish & tsh) [#5797 @rjbou]

## Opamfile
  * Hijack the `%{?val_if_true:val_if_false}%` syntax to support extending the variables of packages with + in their name [#5840 @kit-ty-kate]

## External dependencies
 * Add support for Wolfi OS, treat it like Apline family as it uses apk too [#5878 @xnox]

## Format upgrade
  * Handle init OCaml `sys-ocaml-*` eval variables during format upgrade from 2.0 -> 2.1 -> 2.2 [#5829 @dra27]

## Sandbox
  * Mark the user temporary directory (as returned by `getconf DARWIN_USER_TEMP_DIR`) as writable when TMPDIR is not defined on macOS [#5780 @ElectreAAS]

## VCS

## Build
  * Do not check for cppo in the configure script (not used directly anymore since #5498) [#5794 @kit-ty-kate]
  * Upgrade vendored cmdliner to 1.2.0 [#5797 @kit-ty-kate]
  * Add winsymlinks:native to the CYGWIN environment variable when installing a package on Windows [#5793 @kit-ty-kate - fix #5782]
  * Upgrade the vendored dune to 3.14.0 [#5869 @kit-ty-kate]
  * Upgrade the vendored re to 1.11.0 [#5869 @dra27]
  * Upgrade the vendored ocamlgraph to 2.1.0 [#5869 @dra27]
  * Upgrade the vendored opam-file-format to 2.1.6 [#5869 @dra27]
  * Allow to compile opam when the environment contains unicode characters on Windows [#5880 @kit-ty-kate - fix #5861]
  * Upgrade the vendored dune to 3.14.2 [#5880 @kit-ty-kate]

## Infrastructure
  * Fix depexts CI workflow and ensure all workflows run on master push [#5788 @dra27]
  * Update src_ext/Makefile.dune along with src_ext/Makefile.sources [#5869 @dra27]

## Release scripts

## Install script
  * Add support for doas as an alternative to sudo [#5820 @kit-ty-kate - fix #5792]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell
  * Quote all the paths to OPAMROOT when creating the init scripts on Unix in case OPAMROOT contains spaces, backslashes or special characters [#5841 @kit-ty-kate - fix #5804]

## Internal

## Internal: Windows
  * Ensure that the system critical error dialog is disabled when opam starts [#5828 @dra27]
  * Fix loading git location at init [#5843 @rjbou]
  * Remove use of deprecated function SHGetFolderPath and use SHGetKnownFolderPath instead [#5862 @kit-ty-kate]
  * Improve performance by only calling OpamStubs.getPathToSystem once [#5862 @dra27]

## Test

## Benchmarks
  * Benchmark OpamSystem.read [#5900 @kit-ty-kate]

## Reftests
### Tests
  * Add init scripts tests [#5864 @rjbou]
  * Add test for init OCaml predefined eval variables and their format upgrade [#5829 @rjbou]
  + Add a test showing the current behaviour of opam with variable expansion, in particular when the package contains pluses [#5840 @kit-ty-kate]

### Engine

## Github Actions
  * Update checkout action to v4 [#5851 @rjbou]
  * Test OCaml 5.1.1 [#5869 @dra27]
  * Fix the cache key [#5869 @dra27]

## Doc
  * Fix a typo in the documentation of `opam lint --recursive` [#5812 @Khady]
  * Fix the documentation of opam lint --warnings [#5818 @kit-ty-kate]
  * Fix a dead link to SPDX license expressions spec [#5849 @kit-ty-kate - fix #5846]
  * Fix missing spaces in `opam --help` [#5850 @sorawee].
  * Manual: add missing 'since opam 2.2' annotation when mentionning with-dev-setup [#5885 @kit-ty-kate]
  * Installation: update badges for Ubuntu and Fedora to newer versions [#5905 @AldanTanneo]
  * Manual: update regarding `pkg+` variables new syntax [#5840 @kit-ty-kate]

## Security fixes

# API updates
## opam-client
  * `OpamClient.windows_checks`: On existing cygwin install, permit to detect msys2 and store `os-distribution=msys2` in `global-variables` config file field [#5843 @rjbou]
  * `OpamClient.windows_checks`: When updating config file for msys2, resolve `pacman` path and store it in `sys-pkg-manager-cmd` for msys2 [#5843 @rjbou]
  * `OpamArg.apply_global_options`: load MSYS2 Cygwin binary path too [#5843 @rjbou]

## opam-repository

## opam-state
  * `OpamEnv.env_expansion`: Fix detection of out-of-date environment variables, a filter predicate was inverted [#5837 @dra27]
  * `OpamSysInteract.Cygwin.check_install`: add `variant` argument to permit checking that it is an Cygwin-like install if it is set to true, keep checking that it is a strictly Cygwin install if false [#5843 @rjbou]
  * `OpamSysInteract.Cygwin.check_install`: look for `cygcheck.exe` in `usr/bin` also as MSYS2 doesn't have "bin" [#5843 @rjbou]
  * `OpamGlobalState.load_config`: load MSYS2 Cygwin binary path too at config file loading [#5843 @rjbou]
  * `OpamEnv`: add `sys_ocaml_eval_variables` value, moved `OpamInitDefaults` as it is also needed in `OpamFormatUpgrade` too [#5829 @rjbou @kit-ty-kate]

## opam-solver

## opam-format
  * `OpamFile.InitConfig`: add `sys-pkg-manager-cmd` field [#5847 @rjbou]
  * `OpamTypesBase`: add `filter_ident_of_string_interp` that is used for parsing variables in string interpolation like `filter_ident_of_string` but permits the parsing of '%{?pkg+:var:}%' syntax [#5840 @rjbou]
  * `OpamTypesBase.filter_ident_of_string_interp`: add `accept` optional argument to be able to raise an error when several pluses are in the package name without using the new syntax, like `%{pkg+++:var}%`
  * `OpamFilter`: add `extract_variables_from_string` to retrieve string of variables, and exposes it [#5840 @rjbou]

## opam-core
  * `OpamStd.Sys`: add `is_cygwin_variant_cygcheck` that returns true if in path `cygcheck` is from a Cygwin or MSYS2 installation [#5843 @rjbou]
  * `OpamStd.Env.cyg_env`: takes the environment to cygify, usually `OpamStd.Env.raw_env` [#5829 @dra27]
