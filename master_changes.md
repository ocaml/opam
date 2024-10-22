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
  * Bump the version to 2.3.0\~beta2\~dev [#6240 @kit-ty-kate]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI
  * [BUG] Fix the detection of the current terminal size [#6244 @kit-ty-kate - fix #6243]
  * [BUG] Ensure the output of opam commands using a column style UI stay consistent accross environment by setting the number of columns to 80 if stdout is not a tty and if the `COLUMNS` env variable is not set [#6244 @kit-ty-kate]

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
  * Upgrade the Alpine Linux container where the Linux release binaries are built from Alpine 3.13 to 3.20 [#6237 @kit-ty-kate]
  * Make the release script produce a Linux/riscv64 binary [#6237 @kit-ty-kate]
  * Upgrade the FreeBSD qemu image where the FreeBSD binary is built from FreeBSD 13.2 to 14.1 [#6237 @kit-ty-kate]
  * Upgrade the OpenBSD qemu image where the OpenBSD binary is built from OpenBSD 7.4 to 7.6 [#6237 @kit-ty-kate]
  * Simplify and improve the reliability of the one-click release script by switching away from a passwordless setup [#6237 @kit-ty-kate]

## Install script

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests

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
  * `OpamStubs.get_stdout_ws_col`: new Unix-only function returning the number of columns of the current terminal window [#6244 @kit-ty-kate]
