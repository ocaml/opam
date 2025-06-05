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
  * Bump the version to `2.4.0~alpha3~dev` [#6512 @kit-ty-kate]

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
  * [BUG] When using `--deps-only`, no longer take into account the simulated pin information. This is hit when a package `pkg` is already installed and `opam install ./pkg --deps` is called, if there is a conflict between installed `pkg` dependencies and local `pkg` declaration, the conflict is not seen and the already installed `pkg` is kept. [#6530 @rjbou - fix #6529]

## List

## Show

## Var/Option

## Update / Upgrade
  * Fix a stack overflow when updating repositories with large files [#6527 @kit-ty-kate - fix #6513]
  * Fix a failure when updating a repository which adds a line at the end of a file without final newline character [#6527 @kit-ty-kate - fix hannesm/patch#28]

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
  * Bump the downloaded-if-missing dune to 3.19.0, cppo to 1.8.0, ocamlgraph to 2.2.0, uutf to 1.0.4 and patch to 3.0.0~beta1 [#6527 @kit-ty-kate]
  * Allows `./configure --without-dune` to build with OCaml 5.4 [#6527 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Add opam `2.4.0~alpha2` to the install script [#6511 @kit-ty-kate]

## Install script

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
  * Embed Cygwin `setup.exe` executable to use as a fallback in case of cygwin.com network issues [#6523 @rjbou - fix #6498]

## Test

## Benchmarks

## Reftests
### Tests
  * Add some related pin tests: fetching, reinstall trigger & simulated pin (deps-only) [#6530 @rjbou]

### Engine

## Github Actions
  * Bump the opam-repository sha to avoid a checksum breakage [#6524 @kit-ty-kate]
  * Bump the version of opam used to setup the depexts tests [#6524 @kit-ty-kate]

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
