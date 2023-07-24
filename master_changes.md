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
  * Update Kate email [#5609 @rjbou]
  * Fix author heading in manpage [#5609 @rjbou]

## Global CLI

## Plugins

## Init
  * Permit internal Cygwin install on Windows [#5545 @rjbou @dra27]
  * Add `--no-cygwin-setup`, `--cygwin-internal-install`, `--cygwin-local-install` and `--cygwin-location <path>` experimental flags available only on Windows to permit non-interactive Cygwin configuration [#5545 @rjbou]

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
  * configure: Ensure a complementary (32bit on 64bit platforms and 64bit on 32bit platforms) C compiler is installed on Windows [#5522 @kit-ty-kate]

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
  * `OpaminitDefault`: add `required_packages_for_cygwin` packages tool list [#5545 @rjbou]
  * `OpamClient.init`: now propose to install internal Cygwin install [#5545 @rjbou]
  * `OpamSolution.get_depext`: do not confirm in case of internal Cygwin install [#5545 @rjbou]
  * `OpamClient.init`: add optional `cygwin_setup` argument to permit non interactive setup [#5545 @rjbou]
  * `OpamCommands.init`: add cygwin setup flags [#5545 @rjbou]

## opam-repository

## opam-state
  * `OpamSysinteract.Cygwin`: add `install` that performs a Cygwin install in opam internals [#5545 @rjbou @dra27]
  * `OpamSysInteract.Cygwin`: add `is_internal` [#5545 @rjbou]
  * `OpamSysInteract.install`: on Cygwin, upgrade automatically packages, and select local cache [#5545 @rjbou]

## opam-solver

## opam-format

## opam-core
  * `OpamConsole.carriage_delete`: no-op when not tty out [#5595 @rjbou]
  * `OpamSystem.make_command`: Remove the warning triggered when the command contained spaces [#5596 @rjbou - fix #5163]
