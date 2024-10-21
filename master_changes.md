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
  * Bump the version of opam to 2.4.0~alpha1~dev [#6204 @kit-ty-kate]

## Global CLI

## Plugins

## Init

## Config report

## Actions

## Install

## Build (package)

## Remove

## Switch
  * Put back support for switch creation with packages argument and
    `--packages` option with cli 2.0, and a specific error message for cli 2.1
    [#4853 @rjbou - fix #4843]
  * Ensure setenv can use package variables defined during the build [#4841 @dra27]
  * [BUG] Fix `set-invariant: default repos were loaded instead of switch repos [#4866 @rjbou]
  * Add support for `opam switch -` (go to previous non-local switch) [#4910 @kit-ty-kate - fix 4866]
  * On loading, check for executable external files if they are in `PATH`, and warn if not the case [#4932 @rjbou - fix #4923]
  * When inferring a 2.1+ switch invariant from 2.0 base packages, don't filter out pinned packages as that causes very wide invariants for pinned compiler packages [#5176 @dra27 - fix #4501]
  * Really install invariant formula if not installed in switch [#5188 @rjbou]
  * On import, check that installed pinned packages changed, reinstall if so [#5181 @rjbou - fix #5173]
  * [BUG] Enforce extra-source to have a checksum when using "opam switch export --freeze" [#5418 @kit-ty-kate]
  * [BUG] Failing switch reinstall partially delete switch layout [#5475 @rjbou - fix #5347]

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
  * Simplify the making of stripped binaries by introducing the `make opam-stripped` target [#6208 @kit-ty-kate]

## Install script
  * Add 2.3.0\~alpha1 to the install scripts [#6203 @kit-ty-kate]
  * Add 2.3.0\~beta1 to the install scripts [#6238 @rjbou]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client

## Shell

## Internal

## Internal: Windows

## Test

## Benchmarks

## Reftests
### Tests
  * Add switch-invariant test [#4866 @rjbou]
  * opam root version: add local switch cases [#4763 @rjbou] [2.1.0~rc2 #4715]
  * opam root version: add reinit test casess [#4763 @rjbou] [2.1.0~rc2 #4750]
  * Port opam-rt tests: orphans, dep-cycles, reinstall, and big-upgrade [#4979 @AltGr]
  * Add & update env tests [#4861 #4841 #4974 #5203 @rjbou @dra27 @AltGr]
  * Add remove test [#5004 @AltGr]
  * List:
    * Add some simple tests for the "opam list" command [#5006 @kit-ty-kate]
    * Update list with co-instabillity [#5024 @AltGr]
    * Add a usecase with faulty dependencies computation [#5329 @rjbou]
  * Add clean test for untracked option [#4915 @rjbou]
  * Harmonise some repo hash to reduce opam repository checkout [#5031 @AltGr]
  * Add repo optim enable/disable test [#5015 @rjbou]
  * Add lint test [#4967 @rjbou]
  * Add lock test [#4963 @rjbou]
  * Add working dir/inplace/assume-built test [#5081 @rjbou]
  * Fix github url: `git://` form no more handled [#5097 @rjbou]
  * Add source test [#5101 @rjbou]
  * Add upgrade (and update) test [#5106 @rjbou]
  * Update var-option test with no switch examples [#5025]
  * Escape for cmdliner.1.1.1 output change [#5131 @rjbou]
  * Add deprectaed flag test [#4523 @kit-ty-kate]
  * Add deps-only, install formula [#4975 @AltGr]
  * Update opam root version test:
    * to escape `OPAMROOTVERSION` sed, it matches generated hexa temporary directory names [#5007 @AltGr #5301 @rjbou]
    * several improvments: add repo config check, update generator [#5304 @rjbou]
  * Add json output test [#5143 @rjbou]
    * Add tree json output [#5303 @cannorin @rjbou]
  * Add test for opam file write with format preserved bug in #4936, fixed in #4941 [#4159 @rjbou]
  * Add test for switch upgrade from 2.0 root, with pinned compiler [#5176 @rjbou @kit-ty-kate]
  * Add switch import (for pinned packages) test [#5181 @rjbou]
  * Add `--with-tools` test [#5160 @rjbou]
  * Add a series of reftests showing empty conflict messages [#5253 @kit-ty-kate]
  * Fix the reftests under some heavy parallel hardwear [#5262 @kit-ty-kate]
  * Add some tests for --best-effort to avoid further regressions when trying to install specific versions of packages [@5261 @kit-ty-kate]
  * Add unhelpful conflict error message test [#5270 @kit-ty-kate]
  * Add rebuild test [#5258 @rjbou]
  * Add test for opam tree command [#5171 @cannorin]
  * Update and reintegrate pin & depext test `pin.unix` in `pin` test, with test environment, there is no more need to have it only on unix [#5268 @rjbou @kit-ty-kate]
  * Add a reftest testing for system package manager failure [#5257 @kit-ty-kate]
  * Add autopin test including deps-only, dev-deps, depexts; instrument depext handling to allow depext reftesting [#5236 @AltGr]
  * Add test for init configuration with opamrc [#5315 @rjbou]
  * Test opam pin remove <pkg>.<version> [#5325 @kit-ty-kate]
  * Add a test checking that reinstalling a non-installed package is equivalent to installing it [#5228 @kit-ty-kate]
  * Add a test showing that we still get the reason for installing a package when using opam reinstall on non-installed packages [#5229 @kit-ty-kate]
  * Add a windows test to check case insensitive environment variable handling [#5356 @dra27]
  * Fix the reftests on OCaml 5.0 [#5402 @kit-ty-kate]
  * Add switch reinstall test with failures [#5475 @rjbou]

### Engine

## Github Actions

## Doc
  * Update the command to install opam to point to the new simplified url on opam.ocaml.org [#6226 @kit-ty-kate]
  * Fix debian manual url fragment [#6231 @RyanGibb]

## Security fixes

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
