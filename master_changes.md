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
  * Fix `opam switch <version>` where all compilers of that version are `avoid-version` [#6571 @rjbou - fix #6563]

## Config

## Pin

## List

## Show

## Var/Option

## Update / Upgrade
  * Ignore VCS directories when diffing during updates [#6575 @kit-ty-kate - fix #6560]

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
  * Do not remove the generated patch file during updates when debug-mode is on [#6575 @kit-ty-kate]

## Internal: Unix

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests
  * Add test for switch creation `opam switch <version>` where all compilers are flagged `avoid-version` [#6571 @rjbou]
  * Add a test showing the behaviour of opam repository when adding and updating local VCS repositories forced as non-VCS [#6575 @kit-ty-kate]

### Engine
  * `gawk` was re-added to the base fedora images [#6473 @kit-ty-kate]

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
  * `OpamSystem`: Add `get_files_except_vcs` [#6575 @kit-ty-kate]
