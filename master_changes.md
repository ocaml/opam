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
  * Bump to 2.2.0~alpha2~dev [#5603 @rjbou]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Remove

## Switch

## Config

## Pin

## List

## Show

## Var/Option
  * Handle package variable syntax in parse update regexp [#4903 @rjbou - fix #4489]
  * Error with more accurate message in case of package/self variable wrongly given as argument [#4903 @rjbou - fix #4489]

## Update / Upgrade

## Exec

## Source

## Lint
  * E29: The conflicts field's filter does not support package variables [#5535 @kit-ty-kate]

## Repository

## Lock

## Clean

## Opamfile

## External dependencies

## Format upgrade

## Sandbox

## VCS

## Build
  * Run autoupdate to silence autogen warnings [#5555 @MisterDA]
  * Update bootstrap to use FlexDLL 0.43 from ocaml/flexdll [#5579 @MisterDA]

## Infrastructure

## Release scripts

## Admin
  * When linting, clean output when stdout is not tty [#5594 @rjbou]

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Windows

## Test

## Reftests
### Tests
  * Admin: add a full test [#5385 @rjbou]
  * Lint
    * E29: Add conflicts test and simplify W41 to no more trigger E29 [#5535 @rjbou]

### Engine

## Github Actions
  * Fix hygiene scripts for install check: it was missing a conversion `~` -> `-` for dev version [#5588 @rjbou]
  * Add configure autoupdate test [#5555 @rjbou]
  * Fix cli version master check, introduced by #5555 [#5598 @rjbou]

## Doc

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamConsole.carriage_delete`: no-op when not tty out [#5595 @rjbou]
  * `OpamSystem.make_command`: Remove the warning triggered when the command contained spaces [#5596 @rjbou - fix #5163]
