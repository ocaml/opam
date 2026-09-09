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
  * Bump version to `2.7.0~alpha1~dev` [#7064 @kit-ty-kate]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)
  * Fix empty argument handling when calling a Cygwin binary [#7128 @kit-ty-kate - fix #6714]
  * Fix handling of arguments containing a backslash followed by double-quote when calling a Cygwin binary [#7128 @kit-ty-kate]
  * Fix handling of arguments containing a double-quote as first character when calling a Cygwin binary [#7128 @kit-ty-kate]

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
  * Upgrade the autoconf generated files (`configure`) to autoconf 2.72 [#7052 @kit-ty-kate]

## Infrastructure

## Release scripts

## Install script
  * Add opam 2.6.0\~rc1 to the install scripts [#7135 @kit-ty-kate]

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
  * Add a test checking that empty arguments are correctly handled [#7128 @kit-ty-kate]

### Engine

## Github Actions
  * The Hygiene workflow has been upgraded to Ubuntu 26.04 [#7052 @kit-ty-kate]

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamProcess.create_process_env`: Fix empty argument handling when calling a Cygwin binary [#7128 @kit-ty-kate - fix #6714]
  * Fix handling of arguments containing a backslash followed by double-quote when calling a Cygwin binary [#7128 @kit-ty-kate]
  * Fix handling of arguments containing a double-quote as first character when calling a Cygwin binary [#7128 @kit-ty-kate]
