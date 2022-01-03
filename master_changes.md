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
  * Some optimisations to 'opam list --installable' queries combined with other filters [#4882 @AltGr]
  * Improve performance of some opam list combination (e.g. --available --installable) [#4999 @kit-ty-kate]
  * Improve performance of opam list --conflicts-with when combined with other filters [#4999 @kit-ty-kate]

## Show
  * Improve performance of opam show by 300% when the package to show is given explicitly or unique [#4998 @kit-ty-kate - fix #4997 and partially #4172]

## Var
  *

## Option
  *

## Lint
  *

## Lock
  *

## Opamfile
  *

## External dependencies
  * Stop zypper from upgrading packages on updates on OpenSUSE [#4978 @kit-ty-kate]

## Sandbox
  *

## Repository management
  *

## VCS
  *

## Build
  *

## Infrastructure
  *

## Admin
  *

## Opam installer
  *

## State
  * Actually allow multiple state caches to co-exist [#4934 @dra27 - fix #4554 properly this time]

# Opam file format
  *

## Solver
  *

## Client
  *

## Internal
  *

## Test
  *

## Reftests
### Tests
  * Port opam-rt tests: orphans, dep-cycles, reinstall, and big-upgrade [#4979 @AltGr]
  * Add & update env tests [#4861 #4841 #4974 @rjbou @dra27 @AltGr]
  * Add remove test [#5004 @AltGr]
  * Add some simple tests for the "opam list" command [#5006 @kit-ty-kate]
  * Add clean test for untracked option [#4915 @rjbou]
  * Harmonise some repo hash to reduce opam repository checkout [#5031 @AltGr]
  * Add repo optim enable/disable test [#5015 @rjbou]
  * Update list with co-instabillity [#5024 @AltGr]
### Engine
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
  * Fix github url: `git://` form no more handled [#5097 @rjbou]
  * Escape regexps characters in string replacements primitives [#5009 @kit-ty-kate]
  * Automatically update default repo when adding a package file [#5004 @AltGr]
  * Replace vars on the right-hand of exports [#5024 @AltGr]

## Doc
  * Add github `git://` protocol deprecation note [#5097 @rjbou]

## Security fixes
  *

# API updates
## opam-client
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamCLIVersion` [#4918 @rjbou]
  * `OpamConfigCommand`: add a labelled argument `no_switch` to `exec` [#4957 @kit-ty-kate]
  * `OpamClient`: fix `update_with_init_config`, when ``jobs` was set in `init_config`, it dropped rest of `config` update [#5056 @rjbou]
  * Add an optional argument to `OpamArg.mk_subdoc` for extra default elements: `?extra_defaults:(validity * string * string) list` [#4910 @kit-ty-kate]
  * Add `OpamSwitchCommand.previous_switch` [#4910 @kit-ty-kate]
## opam-repository
  * `OpamRepositoryConfig`: add in config record `repo_tarring` field and as an argument to config functions, and a new constructor `REPOSITORYTARRING` in `E` environment module and its access function [#5015 @rjbou]
## opam-state
## opam-solver
  * `OpamCudf`: Change type of `conflict_case.Conflict_cycle` (`string list list` to `Cudf.package action list list`) and `cycle_conflict`, `string_of_explanations`, `conflict_explanations_raw` types accordingly [#4039 @gasche]
  * `OpamCudf`: add `conflict_cycles` [#4039 @gasche]
  * `OpamCudf`: add `trim_universe` [#5024 @AltGr]
  * `OpamSolver.cudf_versions_map`: no more takes a package set as argument, compute whole packages (repo + installed) and take accounet of invariant [#5024 @AltGr]
  * `OpamSolver.load_cudf_universe`: change staging of `add_invariant` [#5024 @AltGr]
  * `OpamSolver.coinstallable_subset`: add `add_inaviant` optional argument [#5024 @AltGr]
## opam-format
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamSysPkg` and `OpamVariable` [#4918 @rjbou]
  * Add OpamPackage.Version.default returning the version number used when no version is given for a package [#4949 @kit-ty-kate]
  * Add `OpamPath.Switch.man_dirs` [#4915 @rjbou]
  * `OpamFile.Config`: order list of installed switches according their last use, update `with_switch` accordingly, and add `previous_switch` [#4910 @AltGr]
## opam-core
  * OpamSystem: avoid calling Unix.environment at top level [#4789 @hannesm]
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamFilename`, `OpamHash`, `OpamStd`, `OpamStd`, `OpamUrl`, and `OpamVersion` [#4918 @rjbou]
  * `OpamHash`: add `sort` from strongest to weakest kind
  * `OpamSystem.real_path`: Remove the double chdir trick on OCaml >= 4.13.0 [#4961 @kit-ty-kate]
