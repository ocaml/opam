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
  * Bump version to 2.2.0~beta3~dev [#5917 @kit-ty-kate]
  * Bump the version number after the release of 2.2.0~beta1 [#5785 @kit-ty-kate]
  * Upgrade the opam-root-version to 2.2~beta [#5904 @kit-ty-kate]

## Global CLI
  * Fix a typo in the variable description returned by "opam var" [#5961 @jmid]
  * Out-of-the-box UTF-8 paged --help on Windows [#5970 @kit-ty-kate]
  * ✘ Display lock hold/release messages on stderr instead of stdout [#5999 @kit-ty-kate - fix #5990]
  * stop hiding the Windows specific arguments of opam init on non-Windows platforms [#6003 @rjbou @kit-ty-kate]

## Plugins

## Init
  * ◈ New option `opam init --cygwin-extra-packages=CYGWIN_PKGS --cygwin-internal-install`, to specify additional packages for internal Cygwin [#5930, #5964 @moyodiallo - fix #5834]
  * Skip Git-for-Windows menu if the Git binary resolved in PATH is Git-for-Windows [#5963 @dra27 - fix #5835]
  * Enhance the Git menu by warning if the user appears to need to restart the shell to pick up PATH changes [#5963 @dra27]
  * Include Git for Windows installations in the list of possibilities where the user instructed Git-for-Windows setup not to update PATH [#5963 @dra27]
  * [BUG] Fail if `--git-location` points to a directory not containing git [#6000 @dra27]
  * Redirect the opam root to `C:\opamroot-xxx` when the opam root contains spaces on Windows [#5457 @rjbou @dra27]

## Config report

## Actions

## Install
  * Fix performance regression when calling opam install --deps-only on an already installed package [#5908 @kit-ty-kate - fix #5817]

## Remove

## Switch
  * Allow to parse opam 2.1 switch import files containing extra-files [#5943 @kit-ty-kate - fix #5941]

## Config
  * Move last-env `OPAM_LAST_ENV` files outside the switch to be in the `opam root` [#5962 @moyodiallo - fix #5823]

## Pin

## List

## Show

## Var/Option

## Update / Upgrade
  * [BUG] Stop triggering "Undefined filter variable variable" warning for `?variable` [#5983 @dra27]
  * Display extractions in the status bar [#5977 @dra27]
  * Display a note when reloading a repository [#5977 @kit-ty-kate]

## Tree
  * [BUG] Fix `opam tree --with-*` assigning the `with-*` variables to unrequested packages [#5919 @kit-ty-kate @rjbou - fix #5755]
  * [BUG] Fix combinations of `opam tree --with-*` and `--no-switch` [#5919 @kit-ty-kate @rjbou - fix #5920]

## Exec

## Source
  * Fix extraction of tarballs on Windows which contain symlinks both when those symlinks can't be created or if they point to files which don't exist [#5953 @dra27]

## Lint
  * W41: Relax warning 41 not to trigger on uses of package variables which are guarded by a package:installed filter [#5927 @dra27]
  * W41: Tighten w.r.t depends & depopts [#5927 @dra27]

## Repository
  * Fix download URLs containing invalid characters on Windows (e.g. the ? character in `?full_index=1`) [#5921 @dra27]
  * [BUG] Fix curl failures - the progress meter can become interleaved with the status code on Windows [#5984 @dra27 @kit-ty-kate @rjbou]

## Lock

## Clean

## Env
  * [BUG] Fix reverting of environment variables, principally on Windows [#5935 @dra27 fix #5838]
  * [BUG] Fix splitting environment variables [#5935 @dra27]
  * [BUG] When opam creates an empty variable then appends/prepends a value, ensure no additional separator is added [#5935 @dra27 - fix #5925]
  * [BUG] Fix `x-env-path-rewrite` splitting of values when reverting [#5935 @dra27 - fix #5838]
  * [BUG] Rework the logic of := and =: so that an empty entry is correctly preserved on multiple updates [#5935 @dra27 - fix #5926]
  * [BUG] Fix incorrect reverting of `=+` and `=:` [#5935 @dra27 - fix #5926]
  * For the `Cygwin` internal operator, don't allow `make.exe` to become shadowed [#5996 @dra27]
  * [BUG] Fix incorrect quoting rule for `PKG_CONFIG_PATH` [#5972 @dra27 - partial fix for #5923]
  * [BUG] Do not special case the rewriting rule for the PKG_CONFIG_PATH environment variable [#6002 @kit-ty-kate - fix #5923]

## Opamfile

## External dependencies
  * Pass --symlink-type native to Cygwin setup if symlinks are available [#5830 @dra27]
  * Pass --no-version-check to Cygwin setup (suppresses a message box if setup needs updating) [#5830 @dra27]
  * Pass --quiet-mode noinput to stop the user interrupting the setup GUI [#5830 @dra27]
  * Always pass --no-write-registry to the Cygwin installer, not just on first installation [#5995 @dra27]
  * os-distribution is now by default calculated from cygpath for Cygwin and MSYS2, instead of needing to be set by opam init [#6000 @dra27]
  * Cygwin setup is now always downloaded and updated both for internal and external Cygwin installations [#6000 @dra27]

## Format upgrade
  * Handle init OCaml `sys-ocaml-*` eval variables during format upgrade from 2.0 -> 2.1 -> 2.2 [#5829 @dra27]
  * Reset the "jobs" config variable when upgrading from opam 2.1 to 2.2, instead of 2.0 to 2.1 [#5904 @kit-ty-kate - fix #5816]

## Sandbox
## VCS

## Build
  * Upgrade vendored cmdliner to 1.3.0 [#5970 @kit-ty-kate]
  * Remove unused/untested Makefile targets lib-pkg [#5494 @kit-ty-kate]
  * Upgrade vendored OCaml compiler to 4.14.2 [#5976 @kit-ty-kate]

## Infrastructure
  * Ensure GNU coreutils available on the macOS 14 CI runners [#5938 @dra27]
  * Update the opam-repository testing sha to include the new compiler packages in opam-repository [#5998 @dra27]

## Release scripts
  * Upgrade the OCaml compiler used for releases to 4.14.2 [#5976 @kit-ty-kate]

## Install script

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client
  * Fix rounding error when displaying the timestamp in debug mode [#5912 @kit-ty-kate - fix #5910]
  * Overhaul Windows `opam init` to determine Unix and Git configuration simultaneously, and to detect from Cygwin, Git and MSYS2 from all the known package managers and shells [#6000 @dra27]

## Shell

## Internal
  * Noisy code refactor renaming `OpamClient.git_for_windows_check` to `OpamClient.git_for_windows` [#5997 @dra27]

## Internal: Windows
  * Set the console to use UTF-8 on Windows using SetConsoleCP and SetConsoleOutputCP [#5970 @kit-ty-kate]
  * Harden the CRLF stripping when using cygcheck [#5993 @dra27]

## Test

## Benchmarks
  * Benchmark opam install --deps-only of an already installed package [#5909 @kit-ty-kate]

## Reftests
### Tests
  * tree: add a test for packages that have variables in their transitive dependencies [#5919 @rjbou]
  * tree: add test for `opam tree pkg --with-test --no-switch` [#5919 @rjbou]
  * Update opam root version test with root version bump [#5904 @rjbou]
  * env tests: use `sort` to increase stability of the `opam env` output [#5935 @dra27 @rjbou]
  * env.win32: add mixed slashes test [#5935 @dra27]
  * env.win32: add test for environment revert not working correctly for Unix-like variables on Windows [#5935 @dra27]
  * env.win32: add regression test for reverting additions to PATH-like variables [#5935 @dra27]
  * env tests: add regression test for append/prepend operators to empty environment variables [#5925, #5935 @dra27]
  * env.win32: add regression test for handling the empty entry in PATH-like variables [#5926, #5935 @dra27]
  * lint: add W41 examples [#5927 @dra27]

### Engine
  * Add `sort` command [#5935 @dra27]

## Github Actions

## Doc

## Security fixes

# API updates
## opam-client
  * `OpamClient.init` and `OpamClient.reinit`: now can have additional cygwin packages to install [#5930 @moyodiallo]
  * Expose `OpamSolution.print_depext_msg` [#5994 @dra27]
  * Extracted `OpamSolution.install_sys_packages` from `OpamSolution.install_depexts` [#5994 @dra27]
  * `OpamInitDefaults.required_packages_for_cygwin`: no longer includes git; as the need to add that is computed in `OpamClient` [#6000 @dra27]
  * `OpamClientConfig.opam_init`: add `original_root_dir` argument that contains the original roo directory before redirection [#5457 @rjbou]
  * `OpamClientConfig.opam_init`: add `root_from` argument that contains the origin of used root[#5457 @dra27]

## opam-repository
  * `OpamDownload.download_command`: separate output from stdout and stderr [#5984 @kit-ty-kate]

## opam-state
  * `OpamEnv.cygwin_non_shadowed_programs`: exposes the list of executables (not including git) which should always come from Cygwin [#6000 @dra27]
  * `opamSysInteract.Cygwin.install`: de-label `packages` argument [#6000 @dra27]
  * `OpamSysInteract.Cygwin.check_install` renamed to `analyse_install` which now also returns whether the installation found was MSYS2 or Cygwin [#6000 @dra27]
  * `OpamStateConfig.r`, `OpamStateConfig.init`: add `original_root_dir` field to config record and argument that contains the original root directory before redirection [#5457 @rjbou]
  * `OpamStateConfig.r`, `OpamStateConfig.init`: add `root_from` field to config record and argument that contains the origin of used root[#5457 @dra27]

## opam-solver

## opam-format
  * `OpamPath`: remove `OpamPath.Switch.last_env` function in favor to `OpamPath.last_env` as the files are no more stored in switch directory [#5962 @moyodiallo - fix #5823]
  * `OpamFilter.map_up`: correct handling of FDefined [#5983 @dra27]
  * `OpamFilter.fold_down_left`: correct handling of FDefined and FUndef [#5983 @dra27]
  * `OpamPath`: add `redirected` the file name of redirected opam root [#5457 @rjbou]

## opam-core
  * `OpamStd.String`: add `split_quoted` that preserves quoted separator [#5935 @dra27]
  * `OpamSystem.copy_dir` and `OpamSystem.mv` may display a warning on Windows if an invalid symlink (e.g. an LXSS Junction) is found [#5953 @dra27]
  * `OpamStubs.getVersionInfo`: on Windows, retrives the version information block of an executable/library [#5963 @dra27]
  * `OpamStubs.readRegistry`: on Windows, complements `OpamStubs.writeRegistry` [#5963 @dra27]
  * `OpamStubs.get_initial_environment`: on Windows, returns the pristine environment for new shells [#5963 @dra27]
  * `OpamConsole`: Add `formatted_errmsg` [#5999 @kit-ty-kate]
  * `OpamConsole.menu` now supports up to 35 menu items [#5992 @dra27]
  * `OpamStd.Sys.resolve_command`: extracted the logic from `OpamSystem.resolve_command`, without the default environment handling from OpamProcess. [#5991 @dra27]
  * `OpamStd.Sys.resolve_in_path`: split the logic of `OpamStd.Sys.resolve_command` to allow searching for an arbitrary file in the search path [#5991 @dra27]
  * `OpamProcess.run_background`: name the stderr output file have the .err extension when cmd_stdout is given [#5984 @kit-ty-kate]
  * [BUG]: fix incorrect recursion case in `OpamSystem.mk_temp_dir` [#6005 @dra27]
  * `OpamStubs.enumRegistry`: on Windows, retrieves all the values of a given type from a registry key, with their names [#6000 @dra27]
  * `OpamCompat`: add `Seq.find_map` from OCaml 4.14 [#6000 @dra27]
  * `OpamStd.Sys.{get_windows_executable_variant,get_cygwin_variant,is_cygwin_variant}`: renamed `~cygbin` to `?search_in_path` with a change in semantics so that it acts as though the directory was simply the first entry in PATH [#6000 @dra27]
  * `OpamConsole.Symbols`: add `collision` symbol [#5457 @dra27]
  * `OpamSystem`: add `mk_unique_dir` that returns an unique directory name as `mk_temp_dir` but not in temporary directory [#5457 @dra27]
