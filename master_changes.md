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

### Engine
 * Fix gcc < 14.3 bug on mingw i686 [#6624 @kit-ty-kate]
  * Fix support for removing local link directories [#6450 @kit-ty-kate]

## Github Actions
  * bump `actions/checkout` from 4 to 5 [#6643 @kit-ty-kate]

## Doc
  * Update the installation documentation with the release of opam 2.4.1 [#6620 @kit-ty-kate]
  * Swapped the use of sha384 for sha512 for the release tarball in the installation documentation [#6620 @kit-ty-kate]
  * Improve the `opam pin` man page by being more explicit about which arguments are optional [#6631 @kit-ty-kate]
  * Fix URL to Software Heritage [#6650 @gahr]

* Add mention of `opam admin compare-versions` in the Manual. [#6596 @mbarbin]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamConsole.log`: does not keep log messages before initialization if the code is ran through a library [#6487 @kit-ty-kate]
  * `OpamCoreConfig.in_opam`: was added [#6487 @kit-ty-kate]
  * `OpamSystem.cpu_count`: now uses a C binding instead of system utilities to get the number of cores of the current machine [#6634 @kit-ty-kate]
  * `OpamSystem.is_reg_dir`: is now exposed, which returns `true` only if its parameter is a directory, exists and is not a symlink. It returns `false` otherwise [#6450 @kit-ty-kate]
