Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘

## Admin
  * Fix admin cache synchronisation message [#4193 @rjbou - fix #4167]

## Init
  * Remove m4 from the list of recommended tools [#4184 @kit-ty-kate]
  * Fix config solver field ignored [#4243 @rjbou - fix #4241]

## Upgrade
  * Fix atoms formula restriction with `--all` [#4221 @rjbou - fix #4218]

## Build
  * Opam file build using dune, removal of opam-%.install makefile target [#4178 @rjbou - fix #4173]
  * Use version var in opam file instead of equal current version number in opamlib dependencies [#4178 @rjbou]
  * ext: fix extlib url [#4248 @rjbou]

## Install
  * Add `_build` to rsync exclusion list [#4230 @rjobou - fix #4195]
  * Recursive opam file lookup: ignore `_build` [#4230 @rjbou]

## Switch
  * Fix Not_found with `opam switch create . --deps` [#4151 @AltGr]
  * Package Var: resolve self `name` variable for orphan packages [#4228 @rjbou - fix #4224]
  * ✘ Reject (shell) character on switch names [#4237 @rjbou - fix #4231]
  * Add missing depext to unavailable reasons [#4194 @rjbou - fix #4176]


## Pin
  * Add depext handling on new pinned packages [#4194 @rjbou - fix #4189]

## Option
  * Fix messages advertising a command in an obsolete format [#4194 @rjbou]

## Config
  * Add switch depext-bypass as modifiable field [#4194 @rjbou - fix #4177]

## List
  * Add --no-depexts option to disable depexts packages unavailability [#4194 @rjbou - fix #4205]
  * Warn if packages are not listed because of depexts unavailaibilty [#4194 @rjbou - fix #4205]

## Pin
  * Don't keep unpinned package version if it exists in repo [#4073 @rjbou - fix #3630]

## Show
  * ✘ Display error message for all not found packages [#4179 @rjbou - fix #4164]
  * ✘ Keep package order given via cli [#4179 @rjbou - fix #4163]
  * `--sort`` apply to with all options, not only `--just-file` [#4179 @rjbou]


## Depext
  * Fix arch query [#4200 @rjbou]
  * Add message when adding a package to `depext-bypass` [#4194 @rjbou]
  * Fix performance issue of depext under Docker/debian [#4165 @AltGr]
  * Refactor package status [#4152 #4200 @rjbou]
  * Add environment variables handling [#4200 @rjbou]
  * Add Macport support [#4152 @rjbou]
  * Homebrew: add no auto update env var for install, accept `pkgname` and `pkgnam@version` on query [#4200 @rjbou]
  * Force LC_ALL=C for query commands [#4200 @rjbou]
  * Fix install command dryrun [#4200 @rjbou]


## Env
  * Fix `OPAMSWITCH` empty string setting, consider as unset [#4237 @rjbou]

## Remove
  * Fix autoremove env var handling [#4219 @rjbou - fix #4217]

## Repository management
  * Fix temp files repository cleaning [#4197 @rjbou]

## Opam installer
  * For paths, remove use of empty switch in favor of a context-less module [#4237 @rjbou]

## Internal
  * Disable chrono when timestamps are disables [#4206 @rjbou]
  * Expose some functionality in the `OpamAction`, `OpamPath` and `OpamSwitchState`
    modules for use without a `switch` value [#4147 @timberston]
    *Path: introduce a functor to permit replicating switch layout in different contexts

## Test
  * Add show cram test [#4206 @rjbou]
  * Add envrionnement variable handling on cram test [#4206 @rjbou]

## Doc
  * add doc/warning for  Filename.rmdir_cleanup [#4197 @rjbou]

## Var
  * Not found message show scope [#4192 @rjbou]
  * No scope needed for variable display [#4192 @rjbou - fix #4183]
  * Fix package variable resolution [#4192 @rjbou - fix #4182]

## Infrastructure
  * Use OCaml 4.09.1 for the make cold target [#4257 @dra27]

## Opam file
  * Fix mismatching extra files detection [#4198 @rjbou]
