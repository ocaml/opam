Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  *

## Plugins
  *

## Init
  * Check for gpatch instead of patch on NetBSD and DragonFlyBSD [#5893 @kit-ty-kate]

## Config report
  *

## Install
  * Better recognize depexts on Gentoo, NetBSD, OpenBSD [#5065 @mndrix]

## Remove
  *

## Switch

## Pin
  *

## List

## Show

## Var/Option

## Lint
  *

## Repository
  * Warn if `GNU patch` is not detected during a repository update [#5893 @kit-ty-kate]

## Lock
  *

## Opamfile
  *

## External dependencies

## Sandbox

## Repository management
  *

## VCS
  *

## Build
  * Upgrade the vendored dune to 3.14.0 [#5869 @kit-ty-kate]

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State

# Opam file format
  *

## Solver
  *

## Client

## Internal
  * Warn if `GNU patch` is not detected when a patch is applied [#5893 @kit-ty-kate]
  * Use `gpatch` by default instead of `patch` on NetBSD and DragonFlyBSD [#5893 @kit-ty-kate]
  * Use `gpatch` if it exists and is detected as GNU patch when `patch` is not `GNU patch` [#5893 @kit-ty-kate]

## Test
  *

## Reftests
### Tests
### Engine

## Github Actions

## Shell

## Doc

## Security fixes
  *

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamSystem.patch` now displays a warning when GNU patch is not detected and looks for both patch and gpatch as a backup option depending on the OS [#5893 @kit-ty-kate]
