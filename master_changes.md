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

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

## Remove

## Switch

## Config

## Pin
  * [BUG] Fix first retrieval of local VCS pin done as local path [#6221 @rjbou - fix #5809]

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
  * Bump the requirement for dune to 2.8 [#6204 @kit-ty-kate]
  * Bump the vendored version of dune to 3.16.0, cppo to 1.7.0 and extlib to 1.8.0 [#6223 @kit-ty-kate]
  * Fix compilation with OCaml 5.3 when using the vendored extlib by updating to the 5.3 compatible version (e.g. `make cold` or `./configure --with-vendored-deps`) [#6223 @kit-ty-kate]
  * Fix the compilation of opam on Windows with OCaml >= 5.0 again [#6216 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Fix the release script after the bump of dune lang to 2.6 [#6204 @kit-ty-kate]
  * Fix the release script after the introduction of opam\_core\_stubs [#6204 @kit-ty-kate]
  * Improve the release script by ignoring interactive questions asked by the FreeBSD package manager [#6204 @kit-ty-kate]
  * Simplify the making of stripped binaries by introducing the `make opam-stripped` target [#6208 @kit-ty-kate]

## Install script
  * Add 2.3.0\~alpha1 to the install scripts [#6203 @kit-ty-kate]

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
  * Move pin test to pin-legacy [#6135 @rjbou]
  * More exhaustive test for pin command: test different behaviour and cli options [#6135 @rjbou]
  * pin: add a test for erroneous first fetch done as local path on local VCS pinned packages [#6221 @rjbou]
  * Add cache test for installed packages cache update after an action failure [#6213 @kit-ty-kate @rjbou]

### Engine
  * Update print file function [#6233 @rjbou]
  * Add `opam-cache` command, to display internal cache content in reftest [#6233 @rjbou]

## Github Actions
  * Add OCaml 5.2.0 to the build matrix [#6216 @kit-ty-kate]
  * Allow to have more than one OCaml default version to run all jobs and add 5.2 to the list of default versions together with 4.14 [#6216 @kit-ty-kate]
  * Bump 4.14 to the latest patch version (4.14.2) [#6216 @kit-ty-kate]

## Doc
  * Update the command to install opam to point to the new simplified url on opam.ocaml.org [#6226 @kit-ty-kate]
  * Fix debian manual url fragment [#6231 @RyanGibb]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state
  * `OpamSwitchState.Installed_cache`: export `load` function [#6233 @rjbou]

## opam-solver

## opam-format

## opam-core
  * `OpamStd.Sys.{get_terminal_columns,uname,getconf,guess_shell_compat}`: Harden the process calls to account for failures [#6230 @kit-ty-kate - fix #6215]
  * `OpamStd.Sys.{uname,getconf}`: now accepts only one argument as parameter, as per their documentation [#6230 @kit-ty-kate]
