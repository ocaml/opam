Working version changelog, used as a base for the changelog and the release
note.
Possibly scripts breaking changes are prefixed with ✘.
New option/command/subcommand are prefixed with ◈.

## Version
  *

## Global CLI
  *

## Plugins
  *

## Init

## Config report
  *

## Install
  *

## Remove
  *

## Switch

## Pin
  *

## List

## Show

## Var/Option

## Lint
  *

## Lock
  *

## Opamfile
  *

## External dependencies

## Sandbox

## Repository management
  *

## VCS
  *

## Build

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State

# Opam file format
  *

## Solver
  *

## Client

## Internal
  *

## Test
  *

## Reftests
### Tests
### Engine
  * Add switch-invariant test [#4866 @rjbou]
  * opam root version: add local switch cases [#4763 @rjbou] [2.1.0~rc2 #4715]
  * opam root version: add reinit test casess [#4763 @rjbou] [2.1.0~rc2 #4750]
  * Add & update env tests [#4861 #4841 @rjbou @dra27]
  * Port opam-rt tests: orphans, dep-cycles, reinstall, and big-upgrade [#4979 @AltGr]
  * Add & update env tests [#4861 #4841 #4974 @rjbou @dra27 @AltGr]
  * Add remove test [#5004 @AltGr]
  * Add some simple tests for the "opam list" command [#5006 @kit-ty-kate]
  * Add clean test for untracked option [#4915 @rjbou]
  * Harmonise some repo hash to reduce opam repository checkout [#5031 @AltGr]
### Engine
  * Add `opam-cat` to normalise opam file printing [#4763 @rjbou @dra27] [2.1.0~rc2 #4715]
  * Fix meld reftest: open only with failing ones [#4913 @rjbou]
  * Add `BASEDIR` to environement [#4913 @rjbou]
  * Replace opam bin path [#4913 @rjbou]
  * Add `grep -v` command [#4913 @rjbou]
  * Apply grep & seds on file order [#4913 @rjbou]
  * Precise `OPAMTMP` regexp, `hexa` instead of `'alphanum` to avoid confusion with `BASEDIR` [#4913 @rjbou]
  * Hackish way to have several replacement in a single line [#4913 @rjbou]
  * Substitution in regexp pattern (for environment variables) [#4913 @rjbou]
  * Substitution for opam-cat content [#4913 @rjbou]
  * Allow one char package name on repo [#4966 @AltGr]
  * Remove opam output beginning with `###` [#4966 @AltGr]
  * Add `<pin:path>` header to specify incomplete opam files to pin, it is updated from a template in reftest run (no lint errors) [#4966 @rjbou]
  * Unescape output [#4966 @rjbou]
  * Clean outputs from opam error reporting block [#4966 @rjbou]
  * Avoid diff when the repo is too old [#4979 @AltGr]
  * Escape regexps characters in string replacements primitives [#5009 @kit-ty-kate]
  * Automatically update default repo when adding a package file [#5004 @AltGr]
  * Make all the tests work on macOS/arm64 [#5019 @kit-ty-kate]
  * Add unix only tests handling [#5031 @AltGr]

## Github Actions
  * Add solver backends compile test [#4723 @rjbou] [2.1.0~rc2 #4720]
  * Fix ocaml link (http -> https) [#4729 @rjbou]
  * Separate code from install workflow [#4773 @rjbou]
  * Specify whitelist of changed files to launch workflow [#473 @rjbou]
  * Update changelog checker list [#4773 @rjbou]
  * Launch main hygiene job on configure/src_ext changes [#4773 @rjbou]
  * Add opam.ocaml.org cache to reach disappearing archive [#4865 @rjbou]
  * Update ocaml version frm 4.11.2 to  4.12.0 (because of macos failure) [#4865 @rjbou]
  * Add a depext checkup, launched only is `OpamSysInteract` is changed [#4788 @rjbou]
  * Arrange scripts directory [#4922 @rjbou]
  * Run ci on tests changes [#4966 @rjbou]

## Shell
  * fish: fix deprecated redirection syntax `^` [#4736 @vzaliva]

## Doc

## Security fixes
  *

# API updates
## opam-client

## opam-repository

## opam-state

## opam-solver

## opam-format

## opam-core
