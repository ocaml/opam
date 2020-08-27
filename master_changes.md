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
  *

## Upgrade
  *

## Install
  *

## Remove
  *

## Switch
  * Support -n as an alias of --no-action (to match opam-pin) [#4324 @dra27]

## Pin
  * ◈ Add `pin scan` subcommand to list available pins [#4285 @rjbou]
  * ◈ Add `--normalise` option to print a normalised list when scanning, that can be taken by `opam pin add` [#4285 @rjbou]
  * `OpamCommand.pin` refactor, including adding `OpamClient.PIN.url_pins` to pin a list of package with url  [#4285 #4301 @rjbou]
  * ◈ Add `with-version` option to set the pinned package version [#4301 @rjbou]
  * `OpamPinCommand.source_pin', for new package confirmation, don't check that no opam file is given as argument [#4301 @rjbou]

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


## Solver
  * Don't penalise packages with more recent 'hidden-versions' [#4312 @AltGr]

## Client
  * Provide all functions in the client library [#4329 @AltGr]

## Internal
  * Process: don't display status line if not verbose, and status line disabled [#4285 @rjbou]
  * Optimise package name comparison [#4328 @AltGr - fix #4245]

## Test
  *

## Doc
  *
