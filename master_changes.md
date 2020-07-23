Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Global CLI
  * `--help/--version` documented in wrong section for aliases [#4317 @dra27]
  * `opam lock --help` missing common information {#4317 @dra27]
  * ◈ `--cli` / `OPAMCLI` option added [#4316 @dra27]
  * ✘ `--yes` passed to all commands, and plugins [#4316 @dra27]

## Init
  * On init, check availability of sandbox and propose to disable [#4284 @rjbou - fix #4089]

## Upgrade
  *

## Install
  *

## Remove
  *

## Switch
  * Support -n as an alias of --no-action (to match opam-pin) [#4324 @dra27]

## Pin
  *

## List
  * <field> form no longer advertised as valid for --columns [#4322 @dra27]

## Show
  *

## Var
  *

## Option
  *

## Lint
  *

## Lock
  * Support -d as alias of --direct-only (to match plugin) [#4319 @dra27]

## External dependencies
  *

## Sandbox
  *

## Repository management
  *

## VCS
  * Add error message in case git repo is empty [#4303 @rjbou - fix #3905]

## Build
  *

## Infrastructure
  *

## Admin
  * <field> form no longer advertised as valid for --columns in list [#4322 @dra27]

## Opam installer
  *

## Opam file
  *

## Solver
  * Don't penalise packages with more recent 'hidden-versions' [#4312 @AltGr]

## Internal
  * Optimise package name comparison [#4328 @AltGr - fix #4245]

## Test
  *

## Doc
  *
