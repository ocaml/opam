Working version changelog, used as a base for the changelog and the release
note.

## Admin
  * Fix admin cache synchronisation message [#4193 @rjbou - fix #4167]

## Init
  * Remove m4 from the list of recommended tools [#4184 @kit-ty-kate]

## Upgrade
  * Fix atoms formula restriction with `--all` [#4221 @rjbou - fix #4218]

## Build
  * Opam file build using dune, removal of opam-%.install makefile target [#4178 @rjbou - fix #4173]
  * Use version var in opam file instead of equal current version number in opamlib dependencies [#4178 @rjbou]

## Install
  * Add `_build` to rsync exclusion list [#4230 @rjobou - fix #4195]
  * Recursive opam file lookup: ignore `_build` [#4230 @rjbou]

## Switch
  * Fix Not_found with `opam switch create . --deps` [#4151 @AltGr]

## Pin
  * Don't keep unpinned package version if it exists in repo [#4073 @rjbou - fix #3630]

## Depext
  * Fix performance issue of depext under Docker/debian [#4165 @AltGr]
  * Refactor package status [#4152 @rjbou]
  * Add Macport support [#4152 @rjbou]

## Remove
  * Fix autoremove env var handling [#4219 @rjbou - fix #4217]
