Working version changelog, used as a base for the changelog and the release
note.
Prefixes used to help generate release notes, changes, and blog posts:
* ✘ Possibly scripts breaking changes
* ◈ New option/command/subcommand
* [BUG] for bug fixes
* [NEW] for new features (not a command itself)

## Version
  *

## Global CLI
  *

## Plugins
  *

## Init
  *

## Config report
  *

## Install
  *

## Remove
  *

## Switch
  *

## Pin
  *

## List
  *

## Show
  *

## Var
  *

## Option
  *

## Lint
  * W68: add warning for missing license field [#4766 @kit-ty-kate - partial fix #4598]

## Lock
  *

## Opamfile
  *

## External dependencies
  * Set DEBIAN_FRONTEND=noninteractive for unsafe-yes confirmation level [#4735 @dra27 - partially fix #4731] [2.1.0~rc2 #4739]
  * Fix depext alpine tagged repositories handling [#4763 @rjbou] [2.1.0~rc2 #4758]

## Format upgrade
  * Fix format upgrade when there is missing local switches in the config file [#4763 @rjbou - fix #4713] [2.1.0~rc2 #4715]
  * Fix not recorded local switch handling, with format upgrade [#4763 @rjbou] [2.1.0~rc2 #4715]
  * Set opam root version to 2.1 [#4763 @rjbou] [2.1.0~rc2 #4715]
  * Fix 2.1~alpha2 to 2.1 format upgrade with reinit [#4763 @rjbou - fix #4748] [2.1.0~rc2 #4750]
  * Fix bypass-check handling on reinit [#4750 @rjbou] [#4763 @rjbou] [2.1.0~rc2 #4750 #4756]

## Sandbox
  * Add default TMPDIR value for sandbox checker [#4784 @rjbou - fix #4783]

## Repository management
  *

## VCS
  *

## Build
  * Bump src_exts and fix build compat with Dune 2.9.0 [#4752 @dra27]
  * Upgrade to dose3 >= 6.1 and vendor dose3 7.0.0 [#4760 @kit-ty-kate]
  * Change minimum required OCaml to 4.03.0 [#4770 @dra27]
  * Change minimum required Dune to 2.0 [#4770 @dra27]

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State
  *

# Opam file format
  *

## Solver
  * [BUG] Remove z3 debug output [#4723 @rjbou - fix #4717] [2.1.0~rc2 #4720]

## Client
  *

## Internal
  * Add license and lowerbounds to opam files [#4714 @kit-ty-kate]
  * Bump version to 2.2.0~alpha~dev [#4725 @dra27]

## Test

## Reftests
  * opam root version: add local switch cases [#4763 @rjbou] [2.1.0~rc2 #4715]
  * opam root version: add reinit test casess [#4763 @rjbou] [2.1.0~rc2 #4750]
  * Add `opam-cat` to normalise opam file printing [#4763 @rjbou @dra27] [2.1.0~rc2 #4715]

## Github Actions
  * Add solver backends compile test [#4723 @rjbou] [2.1.0~rc2 #4720]
  * Fix ocaml link (http -> https) [#4729 @rjbou]
  * Separate code from install workflow [#4773 @rjbou]
  * Specify whitelist of changed files to launch workflow [#473 @rjbou]
  * Update changelog checker list [#4773 @rjbou]
  * Launch main hygiene job on configure/src_ext changes [#4773 @rjbou]

## Shell
  * fish: fix deprecated redirection syntax `^` [#4736 @vzaliva]

## Doc
  *

## Security fixes
  *
