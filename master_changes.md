Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Init
  *

## Upgrade
  *

## Install
  *

## Remove
  *

## Switch
  * Confirmation on non-compiler switch invariant: not on dryrun, Y by default [#4289 @AltGr]

## Pin
  * Fix pin kind automatic detection consistency [#4300 @rjbou]
    * With `opam pin target`, when opam file is not versioned and at root, vcs-pin the package instead of path-pin
    * With `opam pin add nv target`, take opam file even if not versioned

## List
  *

## Show
  *

## Var
  *

## Option
  *

## Lint
  *

## External dependencies
  * Fix non-interactive mode on OpenSuse [#4293 @kit-ty-kate]

## Sandbox
  *

## Repository management
  *

## VCS
  * Add error message in case git repo is empty [#4303 @rjbou - fix #3905]

## Build
  * src-ext: bump topkg to 1.0.2 and dune to 2.6.2, with a second compiler built in case main one is < 4.07.0 (dune restriction) [#4294 @dra27]
  * Bump to 2.1.0~alpha3 version [#4299 @rjbou]

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## Opam file
  *

## Solver
  * Allow Z3 backend to return sub-optimal solutions on timeout, add `OPAMSOLVERALLOWSUBOPTIMAL` environment variable [#4289 @AltGr]
  * Add an optional solver relying on opam-0install-cudf [#4240 @kit-ty-kate]

## Internal
  *

## Test
  *

## Doc
  *
