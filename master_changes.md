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
  * Add `opam 2.4.0~rc1` to the install scripts [#6583 @kit-ty-kate]
  * Bump the version number to `2.5.0~alpha1~dev` [#6584 @kit-ty-kate]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI

## Switch

## Config

## Pin

## List

## Show

## Var/Option

## Update / Upgrade

## Tree

## Exec

## Source

## Lint

## Repository

## Lock

## Clean

## Env

## Opamfile

## External dependencies

## Format upgrade

## Sandbox

## VCS

## Build

## Infrastructure

## Release scripts

## Install script
  * Add 2.4.1 to the install scripts [#6617 @kit-ty-kate]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Unix

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests

### Engine
 * Fix gcc < 14.3 bug on mingw i686 [#6624 @kit-ty-kate]
  * Fix support for removing local link directories [#6450 @kit-ty-kate]

## Github Actions

## Doc
  * Update the installation documentation with the release of opam 2.4.1 [#6620 @kit-ty-kate]
  * Swapped the use of sha384 for sha512 for the release tarball in the installation documentation [#6620 @kit-ty-kate]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
  * `OpamCompat.List.fold_left_map`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.Map.filter_map`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.MAP`: was added [#6442 @kit-ty-kate]
  * `OpamCompat.String.{starts_with,ends_with,for_all,fold_left}`: were added [#6442 @kit-ty-kate]
  * `OpamStd.List.fold_left_map`: was moved to `OpamCompat.List.fold_left_map` [#6442 @kit-ty-kate]
  * `OpamStd.List.{cons,find_opt,filter_map}`: were removed. Use `Stdlib.List` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Op.{(@@),(|>)}`: were removed. Use `Stdlib` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Option.{map,iter,compare,equal,to_string,some}`: were removed. Use `Stdlib.Option` instead. [#6442 @kit-ty-kate]
  * `OpamStd.Map.{find_opt,choose_opt,fold,map,mapi}`: are now the implementation from `Stdlib.Map` [#6442 @kit-ty-kate]
  * `OpamCompat.Map.filter_map`: is now the implementation from `Stdlib.Map` when using OCaml >= 4.11 [#6442 @kit-ty-kate]
  * `OpamStd.Set.{map,choose_opt,fold}`: are now the implementation from `Stdlib.Set` [#6442 @kit-ty-kate]
  * `OpamStd.String.map`: was removed. Use `Stdlib.String.map` instead. [#6442 @kit-ty-kate]
  * `OpamStd.String.{starts_with,ends_with,for_all,fold_left}`: were moved to `OpamCompat.String` [#6442 @kit-ty-kate]
  * `OpamConsole.log`: does not keep log messages before initialization if the code is ran through a library [#6487 @kit-ty-kate]
  * `OpamCoreConfig.in_opam`: was added [#6487 @kit-ty-kate]
  * `OpamSystem.is_reg_dir`: is now exposed, which returns `true` only if its parameter is a directory, exists and is not a symlink. It returns `false` otherwise [#6450 @kit-ty-kate]
