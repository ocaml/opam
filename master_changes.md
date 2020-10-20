Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  * Bump version to '2.1.0~beta3' [#4351 @AltGr]

## Global CLI
  * Fix hooks broken by 371963a6b [#4386 @lefessan]

## Init
  * Fix sandbox check with not yet set opam environment variables [#4370 @rjbou - fix #4368]

## Config Upgrade
  *

## Install
  * The stdout of `pre-` and `post-session` hooks is now propagated to the user [#4382 @AltGr - fix #4359]

## Remove
  *

## Switch
  * Fix `--update-invariant` when removing or changing package name [#4360 @AltGr - fix #4353]

## Pin
  * Url pin: fix opamfile format upgrade [#4366 @rjbou - fix #4365]
  * don't save the pin with `--show` [#4367 @rjbou - fix #4348]

## List
  *

## Show
  *

## Var
  *

## Option

## Lint
  *

## Lock
  *

## External dependencies
  * Add support for NetBSD and DragonFlyBSD [#4396 @kit-ty-kate]
  * Fix OpenBSD, FreeBSD and Gentoo: Allow short names and full name paths for ports-based systems [#4396 @kit-ty-kate]

## Sandbox
  *

## Test
  *

## Repository management
  *

## VCS
  *

## Build
  * Update opam file to 2.0 [#4371 @AltGr]

## Infrastructure
  *

## Admin
  * Use the archive caches when running `opam admin cache` [#4384 @AltGr - fix #4352]

## Opam installer
  *

## Solver
  * Fix missing conflict message when trying to remove required packages [#4362 @AltGr]

## Client
  *

## Internal
  * ActionGraph: removal postponing, protect against addition of cycles [#4358 @AltGr - fix #4357]
  * Initialise random [#4391 @rjbou]
  * Fix CLI debug log printed without taking into account debug sections [#4391 @rjbou]

## Test
  * Ensure that a cold `dune runtest` works [#4375 @emillon]
  * Use dune "expected" convention for patcher test [#4395 @emillon]

## Doc
  *
