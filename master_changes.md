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
  *

## Global CLI
  * Fix typo in error message for opam var [#4786 @kit-ty-kate - fix #4785]
  * Add cli 2.2 handling [#4853 @rjbou]
  * --no-depexts is the default in CLI 2.0 mode [#4908 @dra27]

## Plugins
  *

## Init
  * Run the sandbox check in the temporary directory [#4787 @dra27 - fix #4783]

## Config report
  *

## Install
  *

## Remove
  *

## Switch
  * Put back support for switch creation with packages argument and
    `--packages` option with cli 2.0, and a specific error message for cli 2.1
    [#4853 @rjbou - fix #4843]
  * Ensure setenv can use package variables defined during the build [#4841 @dra27]
  * [BUG] Fix `set-invariant: default repos were loaded instead of switch repos [#4866 @rjbou]

## Pin
  * Switch the default version when undefined from ~dev to dev [#4949 @kit-ty-kate]
  * ◈ New option `opam pin --current` to fix a package in its current state (avoiding pending reinstallations or removals from the repository) [#4973 @AltGr - fix #4970]

## List
  * Some optimisations to 'opam list --installable' queries combined with other filters [@altgr]

## Show
  * Add `depexts` to default printer [#4898 @rjbou]
  * Make `opam show --list-files <pkg>` fail with not found when `<pkg>` is not installed [#4956 @kit-ty-kate - fix #4930]

## Var
  *

## Option
  *

## Exec
  * [NEW] Add `opam exec --no-switch` [#4957 @kit-ty-kate - fix #4951]

## Lint
  * W68: add warning for missing license field [#4766 @kit-ty-kate - partial fix #4598]
  * W62: use the spdx_licenses library to check for valid licenses. This allows to use compound expressions such as "MIT AND (GPL-2.0-only OR LGPL-2.0-only)", as well as user defined licenses e.g. "LicenseRef-my-custom-license" [#4768 @kit-ty-kate - fixes #4598]

## Repository
  * When several checksums are specified, instead of adding in the cache only the archive by first checksum, name by best one and link others to this archive [#4696 rjbou]

## Lock
  *

## Opamfile
  *

## External dependencies
  * Set `DEBIAN_FRONTEND=noninteractive` for unsafe-yes confirmation level [#4735 @dra27 - partially fix #4731] [2.1.0~rc2 #4739]
  * Fix depext alpine tagged repositories handling [#4763 @rjbou] [2.1.0~rc2 #4758]
  * Homebrew: Add support for casks and full-names [#4801 @kit-ty-kate]
  * Disable the detection of available packages on RHEL-based distributions.
    This fixes an issue on RHEL-based distributions where yum list used to detect available
    and installed packages would wait for user input without showing any output and/or fail
    in some cases [#4791 @kit-ty-kate - fixes #4790]
  * Archlinux: handle virtual package detection [#4831 @rjbou - partial fix #4759]
  * Fallback on dnf if yum does not exist on RHEL-based systems [#4825 @kit-ty-kate]
  * Stop zypper from upgrading packages on updates on OpenSUSE [#4978 @kit-ty-kate]

## Format upgrade
  * Fix format upgrade when there is missing local switches in the config file [#4763 @rjbou - fix #4713] [2.1.0~rc2 #4715]
  * Fix not recorded local switch handling, with format upgrade [#4763 @rjbou] [2.1.0~rc2 #4715]
  * Set opam root version to 2.1 [#4763 @rjbou] [2.1.0~rc2 #4715]
  * Fix 2.1~alpha2 to 2.1 format upgrade with reinit [#4763 @rjbou - fix #4748] [2.1.0~rc2 #4750]
  * Fix bypass-check handling on reinit [#4750 @rjbou] [#4763 @rjbou] [2.1.0~rc2 #4750 #4756]

## Sandbox
  * Sync the behaviour of the macOS sandbox script with Linux's: /tmp is now ready-only [#4719 @kit-ty-kate]
  * Always mount every directories under / on Linux [#4795 @kit-ty-kate]
  * Get rid of OPAM_USER_PATH_RO (never used on macOS and no longer needed on Linux) [#4795 @kit-ty-kate]
  * Print error message if command doesn't exist [#4971 @kit-ty-kat - fix #4112]

## Repository
  * Don't display global message when `this-switch` is given [#4899 @rjbou - fix #4889]
  * Set the priority of user-set archive-mirrors higher than the repositories'.
    This allows opam-repository to use the default opam.ocaml.org cache and be more resilient to changed/force-pushed or unavailable archives. [#4830 @kit-ty-kate - fixes #4411]

## VCS
  * Pass --depth=1 to git-fetch in the Git repo backend [#4442 @dra27]
  * Use 4.08's unnamed functor arguments to silence warning 67 [#4775 @dra27]
  * git: disable colored output [#4884 @rjbou]

## Build
  * Bump src_exts and fix build compat with Dune 2.9.0 [#4752 @dra27]
  * Upgrade to dose3 >= 6.1 and vendor dose3 7.0.0 [#4760 @kit-ty-kate]
  * Change minimum required OCaml to 4.03.0 [#4770 @dra27]
  * Change minimum required Dune to 2.0 [#4770 @dra27]
  * Change minimum required OCaml to 4.08.0 for everything except opam-core, opam-format and opam-installer [#4775 @dra27]
  * Fix the cold target in presence of an older OCaml compiler version on macOS [#4802 @kit-ty-kate - fix #4801]
  * Harden the check for a C++ compiler [#4776 @dra27 - fix #3843]
  * Add `--without-dune` to configure to force compiling vendored Dune [#4776 @dra27]
  * Use `--without-dune` in `make cold` to avoid picking up external Dune [#4776 @dra27 - fix #3987]
  * Add `--with-vendored-deps` to replace `make lib-ext` instruction [#4776 @dra27 - fix #4772]
  * Fix vendored build on mingw-w64 with g++ 11.2 [#4835 @dra27]
  * Switch to vendored build if spdx_licenses is missing [#4842 @dra27]
  * Check versions of findlib packages in configure [#4842 @dra27]
  * Fix dose3 download url since gforge is gone [#4870 @avsm]
  * Update bootstap ocaml to 4.12.1 to integrate mingw fix [#4927 @rjbou]

## Infrastructure
  *

## Admin
  * ✘ `opam admin cache` now ignores all already present cache files. Option
    `--check-all` restores the previous behaviour of validating all checksums.
  * [BUG] Fix repo-upgrade internal error [#4965 @AltGr]

## Opam installer
  *

## State
  * Handle empty environment variable updates - missed cherry-pick from 2.0 [#4840 @dra27]
  * Repository state: stop scanning directory once opam file is found [#4847 @rgrinberg]
  * Fix reverting environment additions to PATH-like variables when several dirs added at once [#4861 @dra27]
  * Actually allow multiple state caches to co-exist [#4934 @dra27 - fix #4554 properly this time]

## Opam file format
  *

## Solver
  * [BUG] Remove z3 debug output [#4723 @rjbou - fix #4717] [2.1.0~rc2 #4720]
  * Fix and improve the Z3 solver backend [#4880 @altgr]
  * Refactored, fixed, improved and optimised the z3 solver backend [#4878 @altgr]

## Client
  * Check whether the repository might need updating more often [#4935 @kit-ty-kate]

## Internal
  * Add license and lowerbounds to opam files [#4714 @kit-ty-kate]
  * Bump version to 2.2.0~alpha~dev [#4725 @dra27]
  * Add specific comparison function on several module (that includes `OpamStd.ABSTRACT`) [#4918 @rjbou]
  * Homogeneise is_archive tar & zip: if file exists check magic number, otherwise check extension [#4964 @rjbou]
  * [BUG] Remove windows double printing on commands and their output [#4940 @rjbou]

## Internal: Windows
  * Support MSYS2: treat MSYS2 and Cygwin as equivalent [#4813 @jonahbeckford]
  * Process control: close stdin by default for Windows subprocesses and on all platforms for the download command [#4615 @dra27]
  * [BUG] handle converted variables correctly when no_undef_expand is true [#4811 @timbertson]
  * [BUG] check Unix.has_symlink before using Unix.symlink [#4962 @jonahbeckford]

## Test
  * Update crowbar with compare functions [#4918 @rjbou]

## Reftests
### Tests
  * Add switch-invariant test [#4866 @rjbou]
  * opam root version: add local switch cases [#4763 @rjbou] [2.1.0~rc2 #4715]
  * opam root version: add reinit test casess [#4763 @rjbou] [2.1.0~rc2 #4750]
  * Add & update env tests [#4861 #4841 @rjbou @dra27]
  * Port opam-rt tests: orphans, dep-cycles, reinstall, and big-upgrade [#4979 @AltGr]
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
  * Standardise `macOS` use [#4782 @kit-ty-kate]
  * Fix `span` tag in mannual [#4855 @rjbou - fix #4848]
  * Add `avoid-version` doc [#4896 @AltGR - fix #4864]
  * Document custom licenses [#4863 @kit-ty-kate - fix #4862]

## Security fixes
  *

# API updates
## opam-client
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamCLIVersion` [#4918 @rjbou]
  * `OpamConfigCommand`: add a labelled argument `no_switch` to `exec` [#4957 @kit-ty-kate]
## opam-repository
## opam-state
## opam-solver
## opam-format
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamSysPkg` and `OpamVariable` [#4918 @rjbou]
  * Add OpamPackage.Version.default returning the version number used when no version is given for a package [#4949 @kit-ty-kate]
## opam-core
  * OpamSystem: avoid calling Unix.environment at top level [#4789 @hannesm]
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamFilename`, `OpamHash`, `OpamStd`, `OpamStd`, `OpamUrl`, and `OpamVersion` [#4918 @rjbou]
  * `OpamHash`: add `sort` from strongest to weakest kind

