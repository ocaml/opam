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
  * Add cli version 2.4 [#6268 @mbarbin @rjbou]

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
  * Improve the messages when a package is not up-to-date on opam upgrade [#6272 @kit-ty-kate - fix #6270]
  * Use a non-underline uppercase character to denotate the default when asking a question [#6289 @hannesm @kit-ty-kate - fix #6288]

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
  * Add apt-rpm/ALTLinux family support for depext [#6207 @RiderALT]

## Format upgrade

## Sandbox

## VCS

## Build

## Infrastructure

## Release scripts
  * Simplify the making of stripped binaries by introducing the `make opam-stripped` target [#6208 @kit-ty-kate]
  * Upgrade the Alpine Linux container where the Linux release binaries are built from Alpine 3.13 to 3.20 [#6237 @kit-ty-kate]
  * Make the release script produce a Linux/riscv64 binary [#6237 @kit-ty-kate]
  * Upgrade the FreeBSD qemu image where the FreeBSD binary is built from FreeBSD 13.2 to 14.1 [#6237 @kit-ty-kate]
  * Upgrade the OpenBSD qemu image where the OpenBSD binary is built from OpenBSD 7.4 to 7.6 [#6237 @kit-ty-kate]
  * Simplify and improve the reliability of the one-click release script by switching away from a passwordless setup [#6237 @kit-ty-kate]

## Install script
  * Add 2.3.0\~alpha1 to the install scripts [#6203 @kit-ty-kate]
  * Add 2.3.0\~beta1 to the install scripts [#6238 @rjbou]
  * Add opam 2.3.0\~beta2 to the install scripts [#6262 @kit-ty-kate]
  * Add opam 2.3.0\~rc1 to the install scripts [#6282 @kit-ty-kate]

## Admin
  * ◈ Add `opam admin compare-versions` to compare package versions for sanity checks [#6197 @mbarbin]

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
  * Add a doc generation job under linux [#5349 @rjbou]

## Doc
  * Update the command to install opam to point to the new simplified url on opam.ocaml.org [#6226 @kit-ty-kate]
  * Fix debian manual url fragment [#6231 @RyanGibb]
  * Change example of non-letter in version ordering [#6252 @gridbugs]
  * Remove redundant `+` in version BNF definition (it is already present in `identchar`) [#6252 @rjbou]
  * mli documentation: fix code blocks [#6150 @rjbou]
  * mli documentation: fix code blocks, references [#6150 @rjbou]
  * mli documentation: fix code blocks, references, add `@raise` tags [#6150 @rjbou]
  * Unhide `OpamProcess` functions [#6150 @rjbou]
  * Fix a typo in the default man page [#6267 @fccm2]
  * Point users to ways to override the default opam root location in the opam init manpage [#6251 @kit-ty-kate]

## Security fixes

# API updates
## opam-client
  * `OpamArg.InvalidCLI`: export exception [#6150 @rjbou]

## opam-repository

## opam-state

## opam-solver

## opam-format
  * `OpamFormula.string_of_relop`: export function [#6197 @mbarbin]
  * `OpamFormula.all_relop`: a list of all operators [#6197 @mbarbin]

## opam-core
  * `OpamStd.Sys.{get_terminal_columns,uname,getconf,guess_shell_compat}`: Harden the process calls to account for failures [#6230 @kit-ty-kate - fix #6215]
  * `OpamStd.Sys.{uname,getconf}`: now accepts only one argument as parameter, as per their documentation [#6230 @kit-ty-kate]
  * `OpamStubs.get_stdout_ws_col`: new Unix-only function returning the number of columns of the current terminal window [#6244 @kit-ty-kate]
  * `OpamSystem`: add `is_archive_from_string` that does the same than `is_archive` but without looking at the file, only analysing the string (extension) [#6219 @rjbou]
