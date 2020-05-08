Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘

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

## Show
  * ✘ Display error message for all not found packages [#4179 @rjbou - fix #4164]
  * ✘ Keep package order given via cli [#4179 @rjbou - fix #4163]
  * `--sort`` apply to with all options, not only `--just-file` [#4179 @rjbou]


## Depext
  * Fix performance issue of depext under Docker/debian [#4165 @AltGr]
  * Refactor package status [#4152 @rjbou]
  * Add Macport support [#4152 @rjbou]

## Remove
  * Fix autoremove env var handling [#4219 @rjbou - fix #4217]

## Repository management
  * Fix temp files repository cleaning [#4197 @rjbou]

## Internal
  * Disable chrono when timestamps are disables [#4206 @rjbou]

## Test
  * Add show cram test [#4206 @rjbou]
  * Add envrionnement variable handling on cram test [#4206 @rjbou]

## Doc
  * add doc/warning for  Filename.rmdir_cleanup [#4197 @rjbou]

## Var
  * Not found message show scope [#4192 @rjbou]
  * No scope needed for variable display [#4192 @rjbou - fix #4183]
  * Fix package variable resolution [#4192 @rjbou - fix #4182]
