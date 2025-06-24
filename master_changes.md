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
  * Use coreutils' `sha512sum` instead of perl's `shasum` utility when using `./configure --with-cygwin-setup` [#6566 @kit-ty-kate - fix #6557]

## Infrastructure

## Release scripts
  * Switch the host of qemu-base-images from gitlab to github [#6510 @kit-ty-kate]
  * Speedup the initial clone of qemu-base-images when missing [#6510 @kit-ty-kate]
  * Update some of the platforms the prebuilt binaries are built on to Alpine 2.21, FreeBSD 14.3, OpenBSD 7.7 and NetBSD 10.1 [#6510 @kit-ty-kate]

## Install script
  * Add opam 2.4.0~beta1 [#6559 @kit-ty-kate]

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
  * cli versioning: untie output from current major version [#6045 @rjbou]
  * Set `opam-version` to 2.2 for some conflict message tests based on opam repository to stabilise their output [#6045 @rjbou]
  * [BUG]: head -c is not posix compliant. Use cut -b instead. [#5989 @madroach]
  * Add bad cudf package name encoding (dose3 lib) [#6055 @rjbou]
  * Add test for filter operators in opam file [#5642 @rjbou]
  * Update init test to make it no repo [#5327 @rjbou]
  * Add a test in admin cache for hash cache [#6103 @rjbou]
  * Add admin cache test [#6068 @rjbou]
  * env: Add a test for `build-env` overwrites build env opam environment variables [#5377 @rjbou]
  * clean: Add to check cleaning of sources directories [#5474 @rjbou]
  * Add reftest for `--verbose-on` option [#5682 @rjbou]
  * Add a test for --deps-only setting direct dependencies as root packages [#6125 @rjbou]
  * Add a test file for `opam install --check` [#6121 @kit-ty-kate]
  * Add reinstall test for delayed removal of packages [#6139 @rjbou]
  * Add a test showing the behaviour of `opam list --latests-only` [#5375 @kit-ty-kate]
  * Add a test filtering mechanism [#6105 @Keryan-dev]
  * Add a package fetching test [#6146 @rjbou]
  * Add a test showing the behaviour of `opam switch list-available` [#6098 @kit-ty-kate]
  * Add a test for git packages with submodules [#6132 @kit-ty-kate]
  * Add basic test for `install --check` [#6122 @rjbou]
  * Add a sandbox usage test [#6165 @rjbou]

### Engine

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
