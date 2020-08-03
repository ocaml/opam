Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Build
  * Bump to 2.1.0~alpha3 version [#4299 @rjbou]

# External dependencies
  * Fix non-interactive mode on OpenSuse [#4293 @kit-ty-kate]


## Switch
  * Confirmation on non-compiler switch invariant: not on dryrun, Y by default [#4289 @AltGr]

## Solver
  * Allow Z3 backend to return sub-optimal solutions on timeout, add `OPAMSOLVERALLOWSUBOPTIMAL` environment variable [#4289 @AltGr]

