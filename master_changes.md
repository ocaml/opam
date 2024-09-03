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
  * Bump version to 2.3.0~alpha~dev [#6045 @rjbou]
  * Bump opam-root-version to 2.2 [#5980 @kit-ty-kate]

## Global CLI
  * Add cli version 2.3 [#6045 #6151 @rjbou]

## Plugins

## Init
  * Suppress all the Windows menus when running with `opam init -ya` [#6034 @dra27]

## Config report
  * Always list all the repositories regardless of whether or not a switch is currently set [#6116 @kit-ty-kate]
  * Make `opam config report` return the actual invariant syntax expected by `--invariant` [#5619 @kit-ty-kate - fixes #5491]

## Actions
  * Add support for wget2 [#6104 @kit-ty-kate]

## Install
  * Fix package name display for no agreement conflicts [#6055 @rjbou - fix #6030]
  * Make fetching an archive from cache add missing symlinks [#6068 @kit-ty-kate - fix #6064]
  * [BUG] Fix `opam install --deps-only` set direct dependencies as root packages [#6125 @rjbou]
  * [BUG] Fix `opam install --check pkg` when pkg depends on a non-existing package [#6121 @kit-ty-kate]
  * Disable shallow clone by default except for opam repositories [#6146 @kit-ty-kate - fix #6145]
  * Improve performance of `opam install --check` [#6122 @kit-ty-kate]
  * Make `opam install --check` check if all dependencies are installed recursively [#6122 @kit-ty-kate - fix #6097]

## Build (package)
  * ◈ Add `--verbose-on` option to enable verbose mode on specified package names [#5682 @desumn @rjbou]
  * Remove unnecessary copies/move when fetching archives [#5018 @kit-ty-kate @rjbou]

## Remove

## Switch
  * ◈ Add `opam switch import --deps-only` option to install only dependencies of root package at import [#5388 @rjbou - fix #5200]
  * [BUG] Make accepted `--repos` URLs on creation consistent with `opam repository` [#6091 @Keryan-dev - fix #4673]
  * ◈ opam switch list-available will not display compilers flagged with avoid-version/deprecated unless --all is given [#6098 @kit-ty-kate - fix #6089]
  * Make `opam switch set-invariant` return the actual invariant syntax expected by `--invariant` [#5619 @kit-ty-kate - fixes #5491]

## Config

## Pin

## List
  * ◈ Add a new `--latests-only` option to only list the latest packages [#5375 @kit-ty-kate]
  * Speedup `opam list` on options that do not use availibility information [#5317 @kit-ty-kate - fix #5314]

## Show

## Var/Option
  * Fix the value of the 'arch' variable when the current OS is 32bit on a 64bit machine [#5950 @kit-ty-kate - fix #5949]

## Update / Upgrade

## Tree

## Exec

## Source

## Lint
  * Add E70 to check `extra-files:` duplicated fields [#5561 @rjbou]
  * Add E71 to check if the same checksum algorithm is used several times for a given url in `url` section [#5561 @rjbou]
  * Add E72 to check if the same checksum algorithm is used several times for a given url in `extra-sources` section [#5561 @rjbou]
  * Add E73 to check that paths in `extra-files:` are not escapable [#5561 @rjbou]
  * Update W59 (no checksum in `url`) to always display a warning, untying it from `--check-upstream` [#5561 @rjbou]

## Repository
 * Mitigate curl/curl#13845 by falling back from --write-out to --fail if exit code 43 is returned by curl [#6168 @dra27 - fix #6120]

## Lock

## Clean

## Env
  * Make the shell environment update hint easier to copy/paste [#6159 @kit-ty-kate - fix #6158]

## Opamfile
  * Make all writes atomic [#5489 @kit-ty-kate]

## External dependencies
 * Always pass --no-version-check and --no-write-registry to Cygwin setup [#6046 @dra27]
 * Use --quiet-mode noinput for the internal Cygwin installation (which is definitely a fully-specified command line) and --quiet-mode unattended for external Cygwin installations (in case the user does need to select something, e.g. a mirror) [#6046 @dra27]
  * [BUG] Fix apt/debian lookup for installed packages [#6054 @rjbou]

## Format upgrade

## Sandbox

## VCS
  * Fail when git submodule fails to update instead of showing a warning and ignoring the error [#6132 @kit-ty-kate - fix #6131]

## Build
 * Synchronise opam-core.opam with opam-repository changes [#6043 @dra27]
  * Unset OPAM_SWITCH_PREFIX when using make cold [#5534 @kit-ty-kate]
  * Bump the vendored opam-0install-cudf to 0.5.0 [#6130 @kit-ty-kate]
  * Require opam-0install-cudf >= 0.5.0 [#6130 @kit-ty-kate]
  * Bump the vendored mccs to 1.1+18 [#6170 @kit-ty-kate]
  * Fix Windows builds with OCaml >= 5.0 [#6189 @kit-ty-kate - fix #6148]

## Infrastructure

 * Bump opam version used in the depext CI from 2.1.0 to 2.1.6 [#6074 @RyanGibb]

## Release scripts
  * Add the missing mccs and dune archives to the opam-full-<version>.tar.gz archive [#6066 @kit-ty-kate]
  * Ensure the configure file stays as it is in the tag, in the opam-full-<version>.tar.gz archive [#6066 @kit-ty-kate]
  * Exclude the .git directory from the release archive when using GNU tar [#6066 @kit-ty-kate]
  * Ensure non-existing %.cache target fail with a fatal error [#6066 @kit-ty-kate]
  * Remove opam 2.1 support from the release script [#6084 #6175 @kit-ty-kate]

## Install script
  * Provide a shell/install.ps1 PowerShell script to install opam on Windows [#5906 @kit-ty-kate @dra27]
  * Add opam 2.2.0 to the install scripts [#6062 @kit-ty-kate]
  * Add opam 2.2.1 to the install scripts [#6173 @kit-ty-kate]

## Admin
  * Change hash cache location from `~/.cache` to `<opamroot>/download-cache/hash-cache` [#6103 @rjbou]
  * Make `opam admin cache` add missing symlinks [#6068 @kit-ty-kate - fix #6064]

## Opam installer

## State

## Opam file format

## Solver
  * Add support for unordered criteria with the `builtin-0install` solver [#6130 @kit-ty-kate]
  * Add support for the `-changed` criteria with the `builtin-0install` solver, to make the solver prefer to keep packages installed at their current version [#6130 @kit-ty-kate]
  * Add support for the `-count[avoid-version,solution]` criteria with the `builtin-0install` solver, to avoid packages marked with `avoid-version` flag [#6130 @kit-ty-kate]
  * The default criteria for the `builtin-0install` solver changed from empty to `-changed,-count[avoid-version,solution]` [#6130 @kit-ty-kate]
  * The upgrade and fixup criteria for the `builtin-0install` solver changed from empty to `-count[avoid-version,solution]` [#6130 @kit-ty-kate]

## Client

## Shell

## Internal
  * Stop using polymorphic comparison when comparing `OpamTypes.switch_selections` [#6102 @kit-ty-kate]
  * Remove the meta opam packages opam and opam-admin [#6115 @kit-ty-kate]
  * Reduce allocations in OpamVersionCompare [#6144 @talex5]
  * Speedup OpamVersionCompare by 25% by removing the unused handling of epoch [#5518 @kit-ty-kate]
  * Fix error in `OpamSystem.transform_patch` - patches were only applied when debugging [#6182 @dra27 regression since #3449]

## Internal: Windows

## Test

## Benchmarks
  * Make the benchmark setup process faster and the benchmark itself more stable [#6094 @kit-ty-kate]
  * Add a benchmark showing the current performance of OpamVersionCompare [#6078 @kit-ty-kate]
  * Add a benchmark for `opam install --check` [#6123 @kit-ty-kate]
  * Add a benchmark for `opam list --installed` on non-installed packages [#6149 @kit-ty-kate]

## Reftests
### Tests
  * cli versioning: untie output from current major version [#6045 @rjbou]
  * Set `opam-version` to 2.2 for some conflict message tests based on opam repository to stabilise their output [#6045 @rjbou]
  * [BUG]: head -c is not posix compliant. Use cut -b instead. [#5989 @madroach]
  * Add bad cudf package name encoding (dose3 lib) [#6055 @rjbou]
  * Add test for filter operators in opam file [#5642 @rjbou]
  * Update init test to make it no repo [#5327 @rjbou]
  * Add a test in admin cache for hash cache [#6103 @rjbou]
  * Add admin cache test [#6068 @rjbou]
  * env: Add a test for `build-env` overwrites build env opam environment variables [#5377 @rjbou]
  * clean: Add to check cleaning of sources directories [#5474 @rjbou]
  * Add reftest for `--verbose-on` option [#5682 @rjbou]
  * Add a test for --deps-only setting direct dependencies as root packages [#6125 @rjbou]
  * Add a test file for `opam install --check` [#6121 @kit-ty-kate]
  * Add reinstall test for delayed removal of packages [#6139 @rjbou]
  * Add a test showing the behaviour of `opam list --latests-only` [#5375 @kit-ty-kate]
  * Add a test filtering mechanism [#6105 @Keryan-dev]
  * Add a package fetching test [#6146 @rjbou]
  * Add a test showing the behaviour of `opam switch list-available` [#6098 @kit-ty-kate]
  * Add a test for git packages with submodules [#6132 @kit-ty-kate]
  * Add basic test for `install --check` [#6122 @rjbou]
  * lint: add an additional test case for W37 [#5561 @rjbou]
  * lint: update W37 to test other urls scheme [#5561 @rjbou]
  * lint: update W37 to test other url schemes [#5561 @rjbou]
  * lint: add E70 test [#5561 @rjbou]
  * lint: add E71 test [#5561 @rjbou]
  * lint: add E72 test [#5561 @rjbou]
  * lint: add E73 test [#5561 @rjbou]
  * lint: add more test cases for E59: special cases (conf, git url), with and without option `--with-check-upstream` [#5561 @rjbou]
  * lint: add E70 test [#5561 @rjbou]
  * lint: add E71 test [#5561 @rjbou]
  * lint: add E72 test [#5561 @rjbou]
  * lint: add E73 test [#5561 @rjbou]
  * lint: add more test cases for E59: special cases (conf, git url), with and without option `--with-check-upstream` [#5561 @rjbou]
  * lint: add more test cases for W59: special cases (conf, git url), with and without `--with-check-upstream` [#5561 @rjbou]

### Engine
  * Add a test filtering mechanism [#6105 @Keryan-dev]
  * Add a test filter on N0REP0 first line [#6105 @Keryan-dev]
  * Add a makefile target `quick-test` to launch only `N0REP0` tests [#6105 @Keryan-dev]
  * Speedup `make reftest-gen` [#6155 @kit-ty-kate]
  * Fix some json output automatic replacement (duration and path on Windows) [#6184 @rjbou]
  * Add test for reftest syntax [#6184 @rjbou]
  * Add some readme file [#6184 @rjbou]

## Github Actions
  * Depexts: replace centos docker with almalinux to fake a centos [#6079 @rjbou]
  * Depexts: fix conf package install check [#6079 @rjbou]
  * Depexts: specify packages to test per distribution [#6079 @rjbou]
  * Depexts: add update depexts check [#6079 @rjbou]
  * Depexts: move parts to docker build image, for caching [#6079 @rjbou]
  * Depexts: set version for conf packages to check [#6079 @rjbou]
  * Depexts: add package to test containing `os-version` in filter [#6079 @rjbou]
  * Depexts: fix opensuse job [#6079 @rjbou]
  * Use actions/cache instead of our own fork ocaml-opam/cache [#6081 @rjbou]
  * Update action cache to v4 [#6081 @rjbou]
  * Update action checkout to v4 [#6081 @rjbou]
  * Update action upload-artifact to v4 [#6081 @rjbou]
  * preamble: Allow local git submodules (ignore CVE-2022-39253) [#6132 @kit-ty-kate]

## Doc
  * Remove the ppa from the installation instructions on Ubuntu [#5988 @kit-ty-kate - fix #5987]
  * Fix pinning instructions in readme [#5946 @rjbou - fix #5945]
  * Add a brief note about version ordering and an OCaml REPL example [#6119 @mbarbin]
  * Update the installation documentation after the release of opam 2.2 [#6138 @kit-ty-kate]
  * Fix formatting of inline code in `Environment update portability` section [#6141 @shonfeder]
  * Add some missing environment variable added during a build [#5363 @kit-ty-kate @rjbou]

## Security fixes

# API updates
## opam-client
  * `OpamSwitchCommand.import`: add optional `?deps_only` argument to install only dependencies of root packages [#5388 @rjbou]
  * `OpamArg.build_options`: add `--verbose-on` flag [#5682 @desumn @rjbou]
  * `OpamClientConfig.build_options`: add `verbose_on` field [#5682 @desumn]
  * `OpamClientConfig.E`, `OpamArg.environment_variables`: and `OPAMVERBOSEON` support [#5682 @desumn @rjbou]
  * `OpamListCommand.selector`: Add `NotFlag` selector [#6098 @kit-ty-kate]

## opam-repository
 * `OpamRepository.fetch_from_cache`: when an archive is found, add a symlink (or copy) for the ones found in opam file but not in cache [#6068 @kit-ty-kate]
 * `?full_fetch` is now `true` by default instead of `false` [#6146 @kit-ty-kate - fix #6145]

## opam-state
 * `OpamStateConfig.opamroot_with_provenance`: restore previous behaviour to `OpamStateConfig.opamroot` for compatibility with third party code [#6047 @dra27]
  * `OpamSwitchState.{,reverse_}dependencies`: make `unavailable` a non-optional argument to enforce speedups when availability information is not needed [#5317 @kit-ty-kate]

## opam-solver
 * `OpamCudfCriteria`, `OpamBuiltinZ3.Syntax`: Move `OpamBuiltinZ3.Syntax` into a dedicated module `OpamCudfCriteria` [#6130 @kit-ty-kate]
 * `OpamSolver.dependency_graph`: make `unavailable` a non-optional argument to enforce speedups when availability information is not needed [#5317 @kit-ty-kate]

## opam-format
  * Add `OpamTypesBase.switch_selections_{compare,equal}`: proper comparison functions for `OpamTypes.switch_selections` [#6102 @kit-ty-kate]
  * `OpamFormula`: add `exists` [#5317 @kit-ty-kate]
  * `OpamTypes.universe`: make `u_available` and `u_attrs` lazy to speedup actions that do not require availiblity information [#5317 @kit-ty-kate - fix #5314]
  * `OpamFormula`: add some missing comparison functions for `relop`, `version_constraint` and `atom` (`compare_relop`, `compare_version_constraint` and `compare_atom` respectively) [#6122 @kit-ty-kate]

## opam-core
  * `OpamStd.Env`: add `env_string_list` for parsing string list environment variables (comma separated) [#5682 @desumn]
  * `OpamHash`: export `compare_kind` [#5561 @rjbou]
  * `OpamFilename`: add `might_escape` to check if a path is escapable, ie contains `<sep>..<sep>` [#5561 @rjbou]
  * Add `OpamStd.Sys.getconf` [#5950 @kit-ty-kate]
