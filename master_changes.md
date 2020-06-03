Working version changelog, used as a base for the changelog and the release
note.

## Init
  * Remove m4 from the list of recommended tools [#4184 @kit-ty-kate]

## Build
  * Opam file build using dune, removal of opam-%.install makefile target [#4178 @rjbou - fix #4173]
  * Use version var in opam file instead of equal current version number in opamlib dependencies [#4178 @rjbou]

## Switch
  * Fix Not_found with `opam switch create . --deps` [#4151 @AltGr]

## Pin
  * Don't keep unpinned package version if it exists in repo [#4073 @rjbou - fix #3630]

## Depext
  * Fix performance issue of depext under Docker/debian [#4165 @AltGr]
  * Refactor package status [#4152 @rjbou]
  * Add Macport support [#4152 @rjbou]
