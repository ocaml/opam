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
  * `--no-depexts` is the default in CLI 2.0 mode [#4908 @dra27]
  * [BUG] Fix behaviour on closed stdout/stderr [#4901 @altgr - fix #4216]
  * Add `OPAMREPOSITORYTARRING` environment variable to enable repository tarring optimisation, it is disabled by default because it is an optimisation only on some os/configurations [#5015 @rjbou]
  * Refresh the actions list output, now sorted by action/package rather than dependency [#5045 @kit-ty-kate @AltGr - fix #5041]
  * Put back the actions summary as part of confirmation question [#5045 @AltGr]
  * Error report display: print action name [#5045 @AltGr]
  * Refactored depext-related questions, with a flat menu instead of nested y/n questions [#5053 @AltGr - fix #5026]
    * Fix removal of interactive special characters is output is not tty [#5155 @rjbou]
  * [BUG] Fix default cli handling for simple flags [#5099 @rjbou]
  * Add `experimental` flags handling [#5099 @rjbou]
  * [BUG] Fix `OPAMCURL` and `OPAMFETCH` value setting [#5111 @rjbou - fix #5108]
  * [BUG] Fix display of pinned packages in action list [#5079 @rjbou]
  * [BUG] Fix spaces in root and switch dirs [#5203 @jonahbeckford]
  * Use menu for init setup [#5057 @AltGr; #5217 @dra27]

## Plugins
  *

## Init
  * Run the sandbox check in the temporary directory [#4787 @dra27 - fix #4783]
  * [BUG] Fix `opam init` and `opam init --reinit` when the `jobs` variable has been set in the opamrc or the current config. [#5056 @rjbou]

## Config report
  * [BUG] Don't fail is no switch is set [#5198 @rjbou]

## Actions
  *  Add a `'Fetch` action with several packages: one node to download once and prepare source once for packages that share same archive [#4893 @rjbou - fix #3741]
  * Add subpath on actions listing urls [#4876 @rjbou]

## Install
  * Make the status of pinned packages more explicit during installation [#4987 @kit-ty-kate - fix #4925]
  * Better recognize depexts on Gentoo, NetBSD, OpenBSD [#5065 @mndrix]
  * Reimplement deps-only [#4975 @AltGr]
    * Fix conflict handling [#5136 @AltGr]
  * ◈ Add `--formula` option to specify a formula to install [#4975 @AltGr]
  * [BUG] Prevent `.changes` files from being updated during dry-run [#5144 @na4zagin3 - fix #5132]
  * Log a summary of recorded `.changes` as a `ACTION` trace log to help debug #4419 [#5144 @na4zagin3]
  * ◈ Add `--with-tools` option to install recommended development tools from opam file (as `with-test`/`with-doc`), and its environment variable `OPAMWITHTOOLS` [#5016 @rjbou]

## Remove
  *
  * Fix message when running `opam remove` on an unavailable package [@AltGr - fix #4890]
  * Fix removal of root packages with `-a` and an optional dependency explicitely specified [@AltGr - fix #4727]

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

## Pin
  * Switch the default version when undefined from ~dev to dev [#4949 @kit-ty-kate]
  * ◈ New option `opam pin --current` to fix a package in its current state (avoiding pending reinstallations or removals from the repository) [#4973 @AltGr - fix #4970]
  * [BUG] Fix using `--working-dir` with non pinned packages: it was not downloading sources as they were remove from package that need sources [#5082 @rjbou - fix #5060]
  * [NEW] Reactivate subpath and recursive pinning `--recursive` and `--subpath` [#4876 @rjbou]
  * scan: show subpaths [#4876 @rjbou]
  * [BUG] Fix windows path for subpath, by introducing their own type in `OpamFilename` [#4876 @rjbou]
  * [BUG] Fix repin of locked pins when there is no change in lock file [#5079 @rjbou - fix #4313]
  * [BUG] Fix `opam install ./file.opam` lock pinning [#5148 @rjbou - fix #4313]
  * [BUG] Fix origin opam file retrieval when opam originate from locked file [#5079 @rjbou - fix #4936]

## List
  * Some optimisations to 'opam list --installable' queries combined with other filters [#4882 @altgr - fix #4311]
  * Improve performance of some opam list combination (e.g. --available --installable) [#4999 @kit-ty-kate]
  * Improve performance of opam list --conflicts-with when combined with other filters [#4999 @kit-ty-kate]
  * Fix coinstallability filter corner case [#5024 @AltGr]

## Show
  * Add `depexts` to default printer [#4898 @rjbou]
  * Make `opam show --list-files <pkg>` fail with not found when `<pkg>` is not installed [#4956 @kit-ty-kate - fix #4930]
  * Improve performance of opam show by 300% when the package to show is given explicitly or unique [#4998 @kit-ty-kate - fix #4997 and partially #4172]

## Var/Option
  * Don't error when displaying if switch is not set [#5027 @rjbou - fix #5025]
  * Try to set a variable with option `--switch <sw>` fails instead of writing a wrong `switch-config` file [#5027 @rjbou]
  * When a field is defined in switch and global scope, try to determine the scope also by checking switch selection [#5027 @rjbou]
## Var
  * Resolve and use global config and environment variable before polling system informations (os, os-family, etc.) [#4892 @rjbou - fix #4883]

## Update / Upgrade
  * [BUG] if a package is pinned from a locked file, it is automatically updated/upgraded accordingly a lock file (same extension) [#5080 @rjbou]

## Exec
  * [NEW] Add `opam exec --no-switch` [#4957 @kit-ty-kate - fix #4951]

## Source
  * [BUG] Fix directory display in dev mode [#5102 @rjbou]
  * Download source even if no switch is set [#4850 @rjbou @zapashcanon - fix #4809]
  * [NEW] Add `--no-switch` option [#4850 @rjbou - fix #4858]

## Lint
  * W68: add warning for missing license field [#4766 @kit-ty-kate - partial fix #4598]
  * W62: use the spdx_licenses library to check for valid licenses. This allows to use compound expressions such as "MIT AND (GPL-2.0-only OR LGPL-2.0-only)", as well as user defined licenses e.g. "LicenseRef-my-custom-license" [#4768 @kit-ty-kate - fixes #4598]
  * E57 (capital on synopsis) not trigger W47 (empty descr) [#5070 @rjbou]
  * [BUG] Fix linting packages from repository with tarred repositories, the file in temporary repository was no more avaiable when lint is done [#5068 @rjbou]
  * Update repository package filename display [#5068 @rjbou]
  * E67: check checksums only for vcs urls [#4960 @rjbou]

## Repository
  * When several checksums are specified, instead of adding in the cache only the archive by first checksum, name by best one and link others to this archive [#4696 rjbou]
  * Update opam repository man doc regarding removal of the last repository in a switch [#4435 - fixes #4381]
  * Don't display global message when `this-switch` is given [#4899 @rjbou - fix #4889]
  * Set the priority of user-set archive-mirrors higher than the repositories'.
    This allows opam-repository to use the default opam.ocaml.org cache and be more resilient to changed/force-pushed or unavailable archives. [#4830 @kit-ty-kate - fixes #4411]
  * Repository tarring "optimisation" no more needed, removed in favor of a plain directory. It still can be used with environment variable `OPAMREPOSITORYTARRING`.  [#5015 @kit-ty-kate @rjbou @AltGr - fix #4586]
    * Fix loading a plain repository froma tarred one [#5109 @rjbou]
  * Avoid reloading repository contents when the repo has no changes [#5043 @Armael]

## Lock
  * Fix lock generation of multiple interdependent packages [#4993 @AltGr]

## Clean
  * [NEW] Add `--untracked` option to remove interactively untracked files [{4915 @rjbou - fix #4831]

## Opamfile
  * Fix substring errors in preserved_format [#4941 @rjbou - fix #4936]
  * Add `with-tools` variable for recommended tools [#5016 @rjbou]
  * Add `x-locked` extension fields for overlay internal use, it stores if the files originate from a locked file, if so its extension [#5080 @rjbou]
  * Set `depext-bypass` parsing with depth 1, no needed brakcet if single package [#5154 @rjbou]

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
  * Increase verbose logging of command to 4 [#5151 @rjbou]
  * [BUG] Avoid to loop eternally when `depext-runs-installs` is false in a script [#5156 @rjbou]

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

## VCS
  * Pass --depth=1 to git-fetch in the Git repo backend [#4442 @dra27]
  * Use 4.08's unnamed functor arguments to silence warning 67 [#4775 @dra27]
  * git: disable colored output [#4884 @rjbou]
  * Check if a source is up to date with subpath [#4876 @rjbou]

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
  * Update bootstrap ocaml to 4.12.1 to integrate mingw fix [#4927 @rjbou]
  * Update bootstrap to use `-j` for Unix (Windows already does) [#4988 @dra27]
  * Update cold compiler to 4.13 [#5017 @dra27]
  * Bring the autogen script from ocaml/ocaml to be compatible with non-ubuntu-patched autoconf [#5090 @kit-ty-kate #5093 @dra27]
  * configure: Use gmake instead of make on Unix systems (fixes BSDs) [#5090 @kit-ty-kate]
  * Patch AltGr/ocaml-mccs#36 in the src_ext build to fix Cygwin32 [#5094 @dra27]
  * Silence warning 70 [#5104 @dra27]
  * Add `jsonm` (and `uutf`) dependency [#5098 @rjbou - fix #5085]
  * Bump opam-file-format to 2.1.4 [#5117 @kit-ty-kate - fix #5116]
  * Add `sha` dependency [#5042 @kit-ty-kate]
  * Add a 'test' target [#5129 @kit-ty-kate @mehdid - partially fixes #5058]
  * Add `with-tools` handling in selection process [#5016 @rjbou]
  * Bump cudf to 0.10 [#5195 @kit-ty-kate]

## Infrastructure
  * Fix caching of Cygwin compiler on AppVeyor [#4988 @dra27]
  * Small update to GHA scripts [#5055 @dra27]
  * Adapt Windows CI to new safe.directory setting [#5119 @dra27]
  * Bid a fond farewell to AppVeyor, with thanks for 5100+ CI builds [#5134 @dra27]

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
  * Don’t rebuild packages when updating dependencies or availablity, unless the current state needs to be changed [#5118 @kit-ty-kate - fix #4647]
  * Rebuild packages when removing or adding the "plugin" flag [#5118 @kit-ty-kate]

## Opam file format
  *

## Solver
  * [BUG] Remove z3 debug output [#4723 @rjbou - fix #4717] [2.1.0~rc2 #4720]
  * Fix and improve the Z3 solver backend [#4880 @altgr]
  * Refactored, fixed, improved and optimised the z3 solver backend [#4878 @altgr]
  * Add an explanation for "no longer available" packages [#4969 @AltGr]
  * Orphan packages are now handled at the solver level instead of a pre-processing phase, better ensuring consistency [#4969 @altgr]
    * Clean debug code [#5182 @rjbou]
  * Make the 0install solver non-optional [#4909 @kit-ty-kate]
  * Optimised reverse dependencies calculation [#5005 @AltGr]
  * Enable cudf preprocessing for (co)insallability calculation, resulting in a x20 speedup [@AltGr]
  * Make sure that `--best-effort` only installs root package versions that where requested [#4796 @LasseBlaauwbroek]
  * Ask users to report errors when no explanations are given to them [#4981 @kit-ty-kate]
  * Add bultin support for the 'deprecated' flag.  Any packages flagged with deprecated would be avoided by the solver unless there is no other choice (e.g. some user wants to install package a which depends on b which is deprecated) If it is installed, show up a note after installation notifying the user that the package is deprecated. [#4523 @kit-ty-kate]

## Client
  * Check whether the repository might need updating more often [#4935 @kit-ty-kate]
  * ✘ It is no longer possible to process actions on packages that depend on a package that was removed upstream [#4969 @altgr]
  * Fix (at least some of the) empty conflict explanations [#4982 @kit-ty-kate]
  * Fix json double printing [#5143 @rjbou]

## Internal
  * Add license and lowerbounds to opam files [#4714 @kit-ty-kate]
  * Bump version to 2.2.0~alpha~dev [#4725 @dra27]
  * Add specific comparison function on several module (that includes `OpamStd.ABSTRACT`) [#4918 @rjbou]
  * Homogeneise is_archive tar & zip: if file exists check magic number, otherwise check extension [#4964 @rjbou]
  * [BUG] Remove windows double printing on commands and their output [#4940 @rjbou]
  * OpamParallel, MakeGraph(_).to_json: fix incorrect use of List.assoc [#5038 @Armael]
  * [BUG] Fix display of command when parallelised [#5091 @rjbou]
  * Add some debug log to OpamCudf.extract_explanations to help debug #4373 [#4981 @kit-ty-kate]
  * Make SHA computation faster by using ocaml-sha [#5042 @kit-ty-kate]
  * Make OpamConfigCommand.global_allowed_fields fully lazy [#5162 @LasseBlaauwbroek]
  * Overhaul Windows C stubs and update for Unicode [#5190 @dra27]
  * Unify constructors for powershell hosts [#5203 @dra27]
  * Fix lazy compilation of regular expression in OpamFormula.atom_of_string [#5211 @dra27]

## Internal: Windows
  * Support MSYS2: treat MSYS2 and Cygwin as equivalent [#4813 @jonahbeckford]
  * Process control: close stdin by default for Windows subprocesses and on all platforms for the download command [#4615 @dra27]
  * [BUG] handle converted variables correctly when no_undef_expand is true [#4811 @timbertson]
  * [BUG] check Unix.has_symlink before using Unix.symlink [#4962 @jonahbeckford]
  * OpamCudf: provide machine-readable information on conflicts caused by cycles [#4039 @gasche]
  * Remove memoization from `best_effort ()` to allow for multiple different settings during the same session (useful for libaray users) [#4805 @LasseBlaauwbroek]
  * [BUG] Catch `EACCES` in lock function [#4948 @oandrieu - fix #4944]
  * Permissions: chmod+unlink before copy [#4827 @jonahbeckford @dra27]
  * Support MSYS2: two-phase rsync on MSYS2 to allow MSYS2's behavior of copying rather than symlinking [#4817 @jonahbeckford]
  * Environment: translate PATH from Windows to Unix during opam env. [#4844 @jonahbeckford]

## Test
  * Update crowbar with compare functions [#4918 @rjbou]

## Reftests
### Tests
  * Add switch-invariant test [#4866 @rjbou]
  * opam root version: add local switch cases [#4763 @rjbou] [2.1.0~rc2 #4715]
  * opam root version: add reinit test casess [#4763 @rjbou] [2.1.0~rc2 #4750]
  * Port opam-rt tests: orphans, dep-cycles, reinstall, and big-upgrade [#4979 @AltGr]
  * Add & update env tests [#4861 #4841 #4974 #5203 @rjbou @dra27 @AltGr]
  * Add remove test [#5004 @AltGr]
  * Add some simple tests for the "opam list" command [#5006 @kit-ty-kate]
  * Add clean test for untracked option [#4915 @rjbou]
  * Harmonise some repo hash to reduce opam repository checkout [#5031 @AltGr]
  * Add repo optim enable/disable test [#5015 @rjbou]
  * Update list with co-instabillity [#5024 @AltGr]
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
  * Update opam root version test do escape `OPAMROOTVERSION` sed, it matches generated hexa temporary directory names [#5007 @AltGr]
  * Add json output test [#5143 @rjbou]
  * Add test for opam file write with format preserved bug in #4936, fixed in #4941 [#4159 @rjbou]
  * Add test for switch upgrade from 2.0 root, with pinned compiler [#5176 @rjbou @kit-ty-kate]
  * Add switch import (for pinned packages) test [#5181 @rjbou]
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
  * Add switch-set test [#4910 @kit-ty-kate]
  * Replace vars on the right-hand of exports [#5024 @AltGr]
  * Add `json-cat` printer, with some automatic remplacements [#5143 @rjbou]
  * Add some tests showing how --working-dir behaves on updated dependency constraints [#5179 @kit-ty-kate]
  * Add config (report) test [#4892 @rjbou]

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
  * GHA: Fix caching for the "test" job [#5090 @dra27 @kit-ty-kate]
  * Add gentoo depext test [#5067 @rjbou]
  * Add more constraint path for launch of workflow [#5067 @rjbou]
  * Upgrade packages for sovler jobs, in case depext changed [#5010 @rjbou]
  * Fix github safe directory issues in depext workflow [#5153 @rjbou]
  * Update repo hash in depext workflow [#5153 @rjbou]
  * Fix the archlinux script [#5218 @kit-ty-kate]

## Shell
  * fish: fix deprecated redirection syntax `^` [#4736 @vzaliva]
  * dash: recognize dash as a POSIX shell for opam env [#4816 @jonahbeckford]
  * pwsh,powershell: use $env: for opam env [#4816 @jonahbeckford]
  * command prompt: use SET for opam env [#4816 @jonahbeckford]

## Doc
  * Standardise `macOS` use [#4782 @kit-ty-kate]
  * Fix `span` tag in mannual [#4855 @rjbou - fix #4848]
  * Add `avoid-version` doc [#4896 @AltGR - fix #4864]
  * Document custom licenses [#4863 @kit-ty-kate - fix #4862]
  * Add OpenBSD & FreeBSD in the precompiled binaries list [#5001 @mndrix]
  * install.md: fix brew instructions, spelling [#4421 @johnwhitington]
  * document the options of OpamSolver.dependencies [#5040 @gasche @Armael]
  * Add github `git://` protocol deprecation note [#5097 @rjbou]
  * Add src_ext/HACKING.md [#5095 @dra27]
  * Fix URL for the developer manual in README.md [#5165 @omnisci3nce]
  * Update package versions for Ubuntu in distributions list
  * Fix typo in External Solvers docs [#5167 @metanivek]
  * Fix URL for opam-publish in README.md [#5168 @cnmade]
  * Fix typo in `OpamArg` [@hannesm #5175]
  * Fix `OpamAction.install_package` documentation [#5215 @rjbou - fix #5207]

## Security fixes
  *

# API updates
## opam-client
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamCLIVersion` [#4918 @rjbou]
  * `OpamConfigCommand`: add a labelled argument `no_switch` to `exec` [#4957 @kit-ty-kate]
  * `OpamClient`: fix `update_with_init_config`, when ``jobs` was set in `init_config`, it dropped rest of `config` update [#5056 @rjbou]
  * Add an optional argument to `OpamArg.mk_subdoc` for extra default elements: `?extra_defaults:(validity * string * string) list` [#4910 @kit-ty-kate]
  * Add `OpamSwitchCommand.previous_switch` [#4910 @kit-ty-kate]
  * `OpamClient`: `requested` argument moved from `name_package_set` to `package_set`, to precise installed packages with `--best-effort` [#4796 @LasseBlaauwbroek]
  * `OpamConfigCommand`: `set_opt_switch`, `set_var_switch`, `options_list_switch`, and `var_list_switch` now raise configuration error exception (50) if no switch is found [#5027 @rjbou]
  * `OpamArgs`, `OpamArgTools`: add `experimental` optional argument to `cli_from` and replace `default` by `option:['experimental | 'ëefault]` for `cli_between`, to handle experimental features [#5099 @rjbou]
  * OpamAction: `prepare_package_source` can now take any switch state (previously required `rw`) [#4850 @rjbou]
  * `OpamClient`: handle formula on several functions, adding a `formula` labelled or optional argument  (`upgrade_t`, `compute_upgrade_t`, `upgrade`, `fixup`, `install_t`, `install`, `remove_t`, and `remove`) [#4975 @AltGr]
  * `OpamSolution`: add `print_requested` to print actions reasons [#4975 @AltGr]
  * `OpamSolution.apply`: take an optional argument `skip`, to avoid filtering solution beforehand [#4975 @AltGr]
  * `OpamAction`: add `?tools` filtering argument in `build_package`, `install_package` [#5016 @rjbou]
  * `OpamListCommand`: add `?tools` filtering argument in `dependency_toggles` [#5016 @rjbou]
  * `OpamPinCommand`, `OpamClient`, `OpamAuxCommands`: use `OpamStateTypes` pin record types [#5080 @rjbou]
  `OpamPinCommand.fetch_all_pins`: return the list of well fetched pins instead of fetched urls [#5080 @rjbou]
  * `OpamAuxCommand`: add `?locked` (and handle lock file then) argument to `name_and_dir_of_opam_file`, `opams_of_dir`, `opams_of_dir_w_target`, `resolve_locals`, `autopin`, and `simulate_autopin` [#5080 @rjbou]
  * `OpamClient.PIN`: change `?locked:bool` argument into `string`, to have lock extension name [#5080 @rjbou]

## opam-repository
  * `OpamRepositoryConfig`: add in config record `repo_tarring` field and as an argument to config functions, and a new constructor `REPOSITORYTARRING` in `E` environment module and its access function [#5015 @rjbou]
  * New download functions for shared source, old ones kept [#4893 @rjbou]
  * `OpamClient.filter_unpinned_locally` now display a warning of skipped packages instead of debug log [#5083 @rjbou]
  * `OpamSolution.parallel_apply`: fix sources_needed package set, now integrate requested but not locally pinned packages [#5082 @rjbou]
  * Add `?subpath` to `OpamRepository.fetch_dev_packages`, `OpamVCS.is_up_to_date` and vcs specific functions in `OpamDarcs`, `OpamHG`, and `OpamGit` [#4876 @rjbou]
  * `OpamRepositoryConfig.E`: add `curl_t` and `fetch_t` to get their respective environement vairbales value dynamically, without lazyness. It is used in `opamClient.InitDefaults`, that can be called at topelevel [#5111 @rjbou]
  * `OpamRepository.update`: Return a change state result of the repo update [#5043 @Armael]

## opam-state
  * `OpamSwitchState.universe`: `requested` argument moved from `name_package_set` to `package_set`, to precise installed packages with `--best-effort` [#4796 @LasseBlaauwbroek]
  * `OpamSwitchState.universe`: add a chrono for universe loading [#4975 @AltGr]
  * `OpamSwitchState.universe`: set to false unresolved variables used in constraint, and warn [#5141 @rjbou - fix #5139]
  * `OpamStateConfig`: add with-tools support ; i.e. add `E.withtools`, add `with_tools` in config record [#5016 @rjbou]
  * `OpamPackageVar`: add `?tools` filtering argument in `filter_depends_formula`, `all_depends` [#5016 @rjbou]
  * `OpamSwitchState`: add `?tools` filtering argument in `universe` [#5016 @rjbou]
  * `OpamStateTypes`: Add record types for to pin and pinned packages informations (in order to avoid n-uplet with `n` growing) ; `name_and_file`, `name_and_file_w_url`, `nameopt_and_file`, `nameopt_and_file_w_url`, and `pinned_opam` [#5080 @rjbou]
  * `OpamPinned`: use pin record types [#5080 @rjbou]
  * `OpamPinned`: add `?locked:string` (and handle lock file then) argument to `files_in_source`, and `name_of_opam_filename` [#5080 @rjbou]
  * `OpamPinned`: when looking at opam files, keep (and return) information about its locked origin [#5080 @rjbou]
  * `OpamUpdate.pinned_package`: use locked information to automatically update from locked file if present, if `?autolock` is given to true [#5080 @rjbou]

  * Add optional argument `?env:(variable_contents option Lazy.t * string) OpamVariable.Map.t` to `OpamSysPoll` and `OpamSysInteract` functions. It is used to get syspolling variables from the environment first. [#4892 @rjbou]
## opam-solver
  * `OpamCudf`: Change type of `conflict_case.Conflict_cycle` (`string list list` to `Cudf.package action list list`) and `cycle_conflict`, `string_of_explanations`, `conflict_explanations_raw` types accordingly [#4039 @gasche]
  * `OpamCudf`: add `conflict_cycles` [#4039 @gasche]
  * `OpamCudf`: add `trim_universe` [#5024 @AltGr]
  * `OpamSolver.cudf_versions_map`: no more takes a package set as argument, compute whole packages (repo + installed) and take accounet of invariant [#5024 @AltGr]
  * `OpamSolver.load_cudf_universe`: change staging of `add_invariant` [#5024 @AltGr]
  * `OpamSolver.coinstallable_subset`: add `add_invariant` optional argument [#5024 @AltGr]
  * `OpamSolver.installable`: use `installable_subset` that uses `coinstallable_subset` [#5024 @kit_ty_kate]
  * `OpamSolver.explicit`: when adding fetch nodes, add shared source ones. Change of `sources_needed` argument type [#4893 @rjbou]
  * `OpamActionGraph.to_aligned_strings`: add `explicit` optional argument to print action name in utf8 [#5045 @AltGr]
  * `OpamSolver.print_solution`: change output format [#5045 @AltGr]
  * `OpamSolver`, `OpamCudf`: Several changes to handle installation from a formula [#4975 @AltGr]
  * `OpamCudf`: add `trim_universe`, `opam_deprequest_package_name`, and `opam_deprequest_package` [#4975 @AltGr]
  * `OpamCudf.print_solution`: add optional `skip`, to avoid filtering solution beforehand [#4975 @AltGr]
  * `OpamCudf.filter_solution`: can do not remove recursively actions with optional `~recursive:true` [#4975 @AltGr]

## opam-format
  * Exposed `with_*` functions in `OpamFile.Dot_install` [#5169 @panglesd]
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamSysPkg` and `OpamVariable` [#4918 @rjbou]
  * Add OpamPackage.Version.default returning the version number used when no version is given for a package [#4949 @kit-ty-kate]
  * Add `OpamPath.Switch.man_dirs` [#4915 @rjbou]
  * `OpamFile.Config`: order list of installed switches according their last use, update `with_switch` accordingly, and add `previous_switch` [#4910 @AltGr]
  * Change ``Fetch` action to take several packages, in order to handle shared fetching of packages [#4893 @rjbou]
  * `OpamFile.OPAM.to_string_with_preserved_format`: handle substring errors [#4941 @rjbou - fix #4936]
  * `OpamFile.OPAM.effective_part` and `OpamFile.OPAM.effectively_equal` now take an optional `?modulo_state:bool` parameter, that if `true`, eliminates the fields relying on the state of the switch (depends, available, …). This is `false` by default. [#5118 @kit-ty-kate]
  * `OpamTypes`: `request.wish_install` now takes a formula instead of  a conjunction [#4975 @AltGr]
  * `OpamFilter`: add `?tools` filtering argument in `filter_deps` [#5016 @rjbou]
  * `OpamFile.OPAM`: Add `locked`, file origin and extension, in the record with its modifiers/getter [#5080 @rjbou]

## opam-core
  * OpamSystem: avoid calling Unix.environment at top level [#4789 @hannesm]
  * `OpamStd.ABSTRACT`: add `compare` and `equal`, that added those functions to `OpamFilename`, `OpamHash`, `OpamStd`, `OpamStd`, `OpamUrl`, and `OpamVersion` [#4918 @rjbou]
  * `OpamHash`: add `sort` from strongest to weakest kind
  * `OpamSystem.real_path`: Remove the double chdir trick on OCaml >= 4.13.0 [#4961 @kit-ty-kate]
  * `OpamProcess.wait_one`: display command in verbose mode for finished found process [#5091 @rjbou]
  * `OpamStd.Config.E`: add a `REMOVED` variant to allow removing completely an environment variable handling [#5112 @rjbou]
  * `OpamHash`: add `is_null`
  * `OpamStd.Sys`: add `get_windows_executable_variant` to use instead of `is_cygwin_variant` [#4817 @jonahbeckford]
  * `OpamSystem.copy_dir`: two-pass `rsync` copy for `MSYS2`, to handle symlinks [#4817 @jonahbeckford]
  * `OpamSHA`: use now only `sha`, some function removed (`shaxxx`, `shaxxx_bytes`, etc.) [#5042 @kit-ty-kate]
  * `OpamCoreConfig.r`: remove openssl related config: `use_openssl` parameter & config field, and `OPAMUSEOPENSSL` environment variable [#5042 @kit-ty-kate]
  * `OpamFilename`: add a `SubPath` submodule to handle multi-platform subpath specifications. It has an effect on a lot of functions signatures [#4876 @rjbou]
  * `OpamDirTrack`: Add `to_summary_string` to summarise changes [#5144 @na4zagin3]
  * `OpamJson`: use `Jsonm` and add an `of_string` function [#5142 @rjbou]
  * `OpamStd.Config.E`: add `value_t` to allow getting environment variable value dynamically [#5111 @rjbou]
  * `OpamCompat.Unix`: add `realpath` for ocaml < 4.13, and use it in `OpamSystem` [#5152 @rjbou]
  * `OpamCompat`: add `Lazy` module and `Lazy.map` function [#5176 @dra27]
  * `OpamConsole.menu`: add `noninteractive` option to choose a different default when output is not a tty [#5156 @rjbou]
  * `OpamStd.Sys`: add `all_shells` list of all supported shells [#5217 @dra27]
