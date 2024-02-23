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
  * Test if file exists before sourcing in fish + powershell [#5802 @ElectreAAS]

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

## External dependencies

## Format upgrade

## Sandbox
  * Mark the user temporary directory (as returned by `getconf DARWIN_USER_TEMP_DIR`) as writable when TMPDIR is not defined on macOS [#5780 @ElectreAAS]

## VCS

## Build
  * Do not check for cppo in the configure script (not used directly anymore since #5498) [#5794 @kit-ty-kate]
  * Upgrade vendored cmdliner to 1.2.0 [#5797 @kit-ty-kate]
  * Add winsymlinks:native to the CYGWIN environment variable when installing a package on Windows [#5793 @kit-ty-kate - fix #5782]

## Infrastructure
  * Fix depexts CI workflow and ensure all workflows run on master push [#5788 @dra27]

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

## Test

## Reftests
### Tests

### Engine

## Github Actions
  * Update checkout action to v4 [#5851 @rjbou]

## Doc
  * Fix a typo in the documentation of `opam lint --recursive` [#5812 @Khady]
  * Fix the documentation of opam lint --warnings [#5818 @kit-ty-kate]
  * Fix a dead link to SPDX license expressions spec [#5849 @kit-ty-kate - fix #5846]
  * Fix missing spaces in `opam --help` [#5850 @sorawee].

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state
  * `OpamEnv.env_expansion`: Fix detection of out-of-date environment variables, a filter predicate was inverted [#5837 @dra27]

## opam-solver

## opam-format
  * `OpamFile.InitConfig`: add `sys-pkg-manager-cmd` field [#5847 @rjbou]

## opam-core
