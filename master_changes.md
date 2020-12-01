Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  * Bump version to `2.1.0~beta3` [#4351 @AltGr]
  * Bump to version `2.1.0~beta4` [#4413 @rjbou - fix #4408]

## Global CLI
  * Fix hooks broken by 371963a6b [#4386 @lefessan]

## Init
  * Fix sandbox check with not yet set opam environment variables [#4370 @rjbou - fix #4368]

## Config Upgrade
  *

## Install
  * The stdout of `pre-` and `post-session` hooks is now propagated to the user [#4382 @AltGr - fix #4359]
  * `post-install` hooks are allowed to modify or remove installed files, the but not add new ones. Those changes are integrated in changes file [#4388 @lefessan]

## Remove
  * Fix `opam remove --autoremove <PKG>` to not autoremove unrelated packages [#4369 @AltGr - fix #4250 #4332]
  * Fix cases where `opam remove -a` could trigger conflicts in the presence of orphan packages [#4369 @AltGr - fix #4250 #4332]

## Switch
  * Fix `--update-invariant` when removing or changing package name [#4360 @AltGr - fix #4353]
  * Fix updates of the invariant with `--update-invariant` [#4431 @AltGr]

## Pin
  * Url pin: fix opamfile format upgrade [#4366 @rjbou - fix #4365]
  * Don't save the pin with `--show` [#4367 @rjbou - fix #4348]
  * When several pins are needed, do their fetching in parallel [#4399 @rjbou - fix #4315]
  * Don't cleanup vcs pin source directory [#4399 @rjbou]
  * Working dir: fix exclude local switch path [#4433 @rjbou]

## List
  *

## Show
  *

## Var
  * Add `opamfile-loc` as a package variable, containing the location of installed package opam file [#4402 @rjbou]

## Option

## Lint
  *

## Lock
  *

## Opamfile
  * Fix handling of filename-encoded pkgname in opam files [#4401 @AltGr - fix ocaml-opam/opam-publish#107]

## External dependencies
  * Add support for NetBSD and DragonFlyBSD [#4396 @kit-ty-kate]
  * Fix OpenBSD, FreeBSD and Gentoo: Allow short names and full name paths for ports-based systems [#4396 @kit-ty-kate]
  * Handle the case where `os-family=ubuntu` as `os-family=debian` [#4441 @alan-j-hu]
  
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
  * Makefile: Add rule `custom-libinstall` for `opam-custom-install` use [#4401 @AltGr]
  * opam-client lib: fix dependency constraints for cmdliner, extlib [#4410 @AltGr]
  * Makefile: Fix missing spaces between `DUNE_ARGS` and `DUNE_PROMOTE_ARG`. [#4458 @nbraud - fix #4457]

## Infrastructure
  *

## Admin
  * Use the archive caches when running `opam admin cache` [#4384 @AltGr - fix #4352]
  * Fix explosion of `opam admin check --cycles` on repositories with huge cliques [#4392 @AltGr]

## Opam installer
  *

## State
  * 

# Opam file format
  * Update opam-format lib to opam-file-format end position and new type definition [#4298 @rjbou]

## Solver
  * Fix missing conflict message when trying to remove required packages [#4362 @AltGr]
  * Fix the Z3 backend for upgrades [#4393 @AltGr]

## Client
  *

## Internal
  * ActionGraph: removal postponing, protect against addition of cycles [#4358 @AltGr - fix #4357]
  * Initialise random [#4391 @rjbou]
  * Fix CLI debug log printed without taking into account debug sections [#4391 @rjbou]
  * Internal caches: use size checks from Marshal [#4430 @AltGr]
  * openssl invocation: Fix permission denied fallback [#4449 @Blaisorblade - fix #4448]

## Test
  * Ensure that a cold `dune runtest` works [#4375 @emillon]
  * Use dune "expected" convention for patcher test [#4395 @emillon]

## Doc
  * Update ùaintainer name [#4456 @nbraud]
  * Specify url syntaxe in usage/opam pin [#4460 @rjbou - fix #4459]
