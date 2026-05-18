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
  * Bump version to 2.6.0~alpha1~dev [#6749 @rjbou]

## Global CLI
  * Update Kate's email address [#6808 @kit-ty-kate]
  * Remove unnecessary uses of `chdir` [#6910 @NathanReb]

## Plugins

## Init
  * ✘ Display an appropriate error message when the file given to `opam init --config` does not exist or is in a VCS. This changes the behaviour for VCS local urls that was previously retrieved. [#5979 @kit-ty-kate - fix #5971]

## Config report

## Actions

## Install

## Build (package)

## Remove

## UI
  * Read full lines when asking for user input when `TERM=dumb` [#6829 @arvidj - fix #6828]
  * Fix a typo in the note telling users about new a depexts bypass [#6489 @rjbou @kit-ty-kate]

## Switch

## Config

## Pin

## List

## Show
  * Improve performance of `opam show` by reading switch selection only once instead of once per package-version [#6818 @dra27]

## Var/Option

## Update / Upgrade
  * Fixed the bug occuring on version-equivalent package rename (i.e `pkg.00 -> pkg.0`) leading to the package being completely removed. [#6774 @arozovyk fix #6754]
  * Compute the list of available depexts on `opam update` [#6489 @arozovyk - fix #6461]
  * Update depexts availability repository state cache when running `opam update --depexts` [#6489 @arozovyk - fix #6461]
  * Display status message while loading system package availability during `opam update` [#6489 @arozovyk - fix #6461]
  * `opam update` now supports updating a repository that changed a file to a directory of the same name and vice versa [#6915 @rjbou @arozovyk - fix #3830]

## Tree

## Exec

## Source

## Lint

## Repository

## Lock

## Clean

  * Use the [dead_code_analyzer](https://github.com/LexiFi/dead_code_analyzer) to remove unused exported values [#6954 @fantazio]

## Env

## Opamfile
  * The `url` file now only supports the legacy opam 1.2 fields [#6827 @kit-ty-kate]
  * Filter fields in .install files containing destinations with `..` or absolute filepaths as parse errors [#6897 @kit-ty-kate]

## External dependencies
  * Restore the distribution detection on Gentoo [#6886 @kit-ty-kate - fix #6887]
  * Add support for single-quoted values of the /etc/os-release file [#6886 @kit-ty-kate - fix #6887]
  * Fix a string injection from the depexts field to nix-build, when `os-family=nixos` [#6894 @RyanGibb]

## Format upgrade
  * Fix switch and repo format upgrade on Windows. A block occurred because the global lock fd was reopened instead of using the one already opened.  [#6839 @rjbou]

## Sandbox
  * Allow the macOS sandbox to write in the `/var/folders/` and `/var/db/mds/` directories as it is required by some of macOS core tools [#4797 @kit-ty-kate - fix #4389 #6460]

## VCS

## Build
  * opam no longer depends on `cmdliner` [#6755 @kit-ty-kate - fix #6425]
  * Clean variables before calling make on different projects to avoid clashes [#6769 @kit-ty-kate]
  * Add the upcoming OCaml 5.5 (trunk) support when using dune's dev profile [#6670 @kit-ty-kate]
  * Update the download-if-missing patch to 3.1.0 [#6772 @kit-ty-kate]
  * Harden the Makefile's inline shell scripts [#6751 @kit-ty-kate]
  * Add lower-bounds constraints to the dependencies that have none (`ocamlgraph` >= 1.8.8, `jsonm` >= 1.0.2, `swhid_core` >= 0.1, `uutf` >= 1.0.3) [#6878 @kit-ty-kate]
  * Require `spdx_licenses` >= 1.4.0 to ensure compatibility with SPDX v3 syntax [#6878 @kit-ty-kate]
  * Remove support for building opam with OCaml 4.08, 4.09 and 4.10 [#6879 @kit-ty-kate]

## Infrastructure

## Release scripts
  * Fix the placement of the vendored archives in the release tarball [#6765 @kit-ty-kate - fix #6762]
  * Fix the Windows build [#6769 @kit-ty-kate]
  * Harden the Makefile's inline shell scripts [#6751 @kit-ty-kate]
  * Fix the release script not being a noop on re-runs [#6903 @kit-ty-kate]

## Install script
  * Add `2.5.1` to the installers [#6902 @rjbou]
  * Add `2.5.0~alpha1` to the installers [#6748 @kit-ty-kate]
  * Add `2.5.0~beta1` to the installers [#6795 @kit-ty-kate]
  * Add `2.5.0~rc1` to the installers [#6802 @kit-ty-kate]
  * Fix apparmor profile remplacement option [#6760 @rjbou]
  * Use `install` instead of `mv`+`chmod`+`chown` [#6760 @rjbou]
  * Clean apparmor temporary file [#6760 @rjbou]
  * Use variables instead of plain paths [#6760 @rjbou]
  * Reword apparmor message when user need to check the profile [#6760 @rjbou]
  * Add `2.5.0` to the installers [#6817 @kit-ty-kate]
  * Fix the AppArmor support when installing in `/usr/bin` [#6823 @kit-ty-kate - fix #6820]

## Admin

## Opam installer

## State

## Opam file format

## Solver

## Client
  * Improved depexts handling by caching system package availability during update, avoiding redundant system checks at install time. [#6489 @arozovyk - fix #6461]

## Shell

## Internal
  * Improve cache-loading performance when using OCaml >= 5.4 by using `Gc.ramp_up` [#6515 @dra27]
  * Make OpamStd.String.compare_case allocation free [#6515 @dra27]
  * Add a helper script to help generate the configure file on platforms without autoconf 2.71 [#6878 @kit-ty-kate]
  * Fix a rare potential GC corruption in `OpamStubs.uname` [#6880 @avsm @kit-ty-kate @andrew]
  * Fix a rare potential GC corruption in `OpamStubs.enumRegistry` on Windows [#6882 @kit-ty-kate]

## Internal: Unix

## Internal: Windows

## Test
  * lib/patchDiff: no longer print unecessary information after patch [#6915 @rjbou]
  * lib/patchDiff: Ensure a more consistent output accross Unix and Windows platforms [#6915 @kit-ty-kate]
  * lib/patchdiff: add dir-file transformations tests [#6915 @rjbou]
  * Add the `lib-tests` target to the main Makefile [#6928 @kit-ty-kate]

## Benchmarks
  * Add an even larger real-world diff to benchmark `opam update` [#6567 @kit-ty-kate]

## Reftests
### Tests
  *  Add test cases to `update.test` for version-equivalent renames [#6774 @arozovyk fix #6754]
  * Fix a failure when two hashes start with the same two characters [#6793 @kit-ty-kate]
  * Add a test showing the behaviour of `opam init --config` when the file given does not exist [#5979 @kit-ty-kate @rjbou]
  * Add a test for switch link when a local switch is already present [#6860 @rjbou]
  * Add more tests for depexts behaviour with unknown family types [#6489 @arozovyk]
  * Add disabled depexts tests [#6489 @rjbou]
  * Add depexts tests with debug section that demostrate system availability polling [#6489 @arozovyk]
  * Add a test showing the behaviour of .install files containing destination filepath trying to escape their scope [#6897 @rjbou @kit-ty-kate]
  * Add a test showing that `opam install ./` will leave packages pinned if
    aborted or failed [#6922 @NathanReb]
  * Add test for update in repository that changes directories to files and vice versa [#6915 @rjbou]
  * Add an http repository test [#6939 @rjbou]

### Engine
  * Add `http-server` to launch a minimal http server [#6939 @rjbou]

## Github Actions
  * Add OCaml 5.4 to the test matrix [#6732 @kit-ty-kate]
  * Ensure `curl`'s exit status to be non-zero on failure [#6684 @kit-ty-kate]
  * Add OCaml trunk to the test matrix [#6684 #6670 @kit-ty-kate]
  * Rename the hygiene script for `shell/install.sh` to install-check [#6768 @kit-ty-kate]
  * Remove the unconditional Windows binary uploads on PRs [#6771 @kit-ty-kate]
  * Bump the `actions/checkout` to version 6 [#6811 @kit-ty-kate]
  * Bump `actions/cache` to version 5 [#6835 @kit-ty-kate]
  * Regenerate the cache when `OPAM_TEST_REPO_SHA` is changed [#6832 #6821 @kit-ty-kate]
  * Trigger the depexts CI when OpamSysPoll is modified [#6886 @kit-ty-kate]
  * Speedup macOS builds by stopping testing alternative solvers on macOS [#6889 @kit-ty-kate]
  * Disable testing conf-clang-format in favour of conf-fts on Alpine [#6888 @kit-ty-kate]
  * Upgrade to use opam 2.5.1 [#6904 @kit-ty-kate]
  * depexts: Always use the latest 'stable' version of each distribution [#6905 @kit-ty-kate]
  * depexts: Always use the already installed ocaml package via ocaml-system [#6905 @kit-ty-kate]
  * Trigger CI upon changes in `doc/` [#6927 @kit-ty-kate - fix #6810]
  * Install `micro_httpd` in test scripts [#6939 @rjbou]

## Doc
  * Add spacing between two words in `--locked` man section [#6806 @yosefAlsuhaibani]
  * Update the Install page with the new opam 2.5.0 release [#6821 @kit-ty-kate]
  * Mention more explicitely that raw fields are an option [@raphael-proust]
  * Correct configure instruction in README [#6858 @gridbugs @kit-ty-kate]
  * Improve visibility of `depopts` filter note [#6920 @ccoulombel - fix #5367]
  * Call man2html only on actual man pages [#6807 @kit-ty-kate]
  * Replace the API example by a link to the API [#6809 @kit-ty-kate - fix #6637]

## Security fixes
  * Invalidate .install fields containing destination filepath trying to escape their scope [#6897 @kit-ty-kate]

# API updates
## opam-client
  * `OpamArg`: add `build_options_no_depexts` getter to retrieve the value of the given flag  [#6489 @rjbou]
  * `OpamClientConfig.opam_init`: replace `no_depexts` argument by `depexts` [#6489 @rjbou]
  * `OpamSolution` remove the heuristic of recomputing depexts of additional (pinned) packages. [#6489 @arozovyk]
  * `OpamClient` update the system package status check for dependencies during `opam install --deps-only`, including support for pinned packages; also update this in `OpamAuxCommands.autopin` [#6489 @arozovyk]
  * `OpamSolution.get_depexts` remove no longer needed `recover` option that was used with `--depext-only` option  [#6489 @arozovyk]
  * `OpamAdminCheck`: remove `installability_check`, `cycle_check`, and `get_obsolete` [#6954 @fantazio]
  * `OpamArg`: remove `cli2_5`, `escape_path`, `name_list`, `param_list`, `atom_list`, `nonempty_atom_list`, `locked`, `package_with_version`, `atom_or_local`, `atom_or_dir`, `opamlist_columns`, and `scrubbed_environment_variables` [#6954 @fantazio]
  * `OpamAuxCommands`: remove `name_and_dir_of_opam_file`, and `resolve_locals` [#6954 @fantazio]
  * `OpamCliMain`: remove `check_and_run_external_commands`, `main_catch_all`, `json_out`, and `run` [#6954 @fantazio]
  * `OpamClient`: remove `reinstall_t`, `upgrade_t`, and `PIN.post_pin_action` [#6954 @fantazio]
  * `OpamClientConfig`: remove `search_files` [#6954 @fantazio]
  * `OpamConfigCommand`: remove `parse_whole` [#6954 @fantazio]
  * `OpamInitDefaults`: remove `default_compiler`, and `eval_variables` [#6954 @fantazio]
  * `OpamListCommand`: remove `field_of_string` [#6954 @fantazio]
  * `OpamCommand`: remove `update_global_selection` [#6954 @fantazio]
  * `OpamSolution`: remove `eq_atom`, and `sum` [#6954 @fantazio]

## opam-repository
  * `OpamRepositoryPath` was moved to `opam-format` [#6917 @rjbou]
  * `OpamRepositoryRoot` was added [#6680 @kit-ty-kate @rjbou]
  * `OpamRepository`: remove `find_backend` [#6954 @fantazio]
  * `OpamRepositoryBackend`: remove `to_json`, `compare`, and `check_digest` [#6954 @fantazio]
  * `OpamRepositoryConfig`: remove `E.curl`, and `E.fetch` [#6954 @fantazio]

## opam-state
  * `OpamStateConfig.t`: replace `no_depexts` fields that contains disabling informations by `depexts` field that returns if the depexts mechanism is enabled. This field is automatically update by global config value in `OpamStateConfig.load_defaults` [#6489 @rjbou]
  * `OpamStateConfig.options_fun`: replace `no_depexts` argument by `depexts` [#6489 @rjbou]
  * `OpamRepositoryState.load_opams_from_diff` track added packages to avoid removing version-equivalent packages [#6774 @arozovyk fix #6754]
  * `OpamGlobalState.all_installed_versions`: was added [#6818 @dra27]
  * `OpamGlobalState.installed_versions`: was removed [#6818 @dra27]
  * `OpamStateTypes.global_state`: add field `lock` that contains the global lock (not config one) [#6839 @rjbou]
  * `OpamStateTypes`: add `os_family` type that was defined and used internally in `OpamSysInteract` [#6489 @rjbou]
  * `OpamSysInteract`: add `disable_depexts_note` to be used to display a note to disable depexts [#6489 @rjbou]
  * `OpamSysInteract`: add some os families helpers `string_of_os_family`, `equal_os_family`, `same_os_family` [#6489 @rjbou]
  * `OpamSysInteract`: add `available_packages` and `installed_packages` to be computed separately, redefine `packages_status` accordingly. These funct-ions are now no-op if the given system packages set is empty.  [#6489 @arozovyk]
  * `OpamGlobalState`: add `is_root_read_only` to check if we are in sandboxed environment [#6489 @rjbou]
  * `OpamSwitchState`: add `update_sys_packages` to update depexts status of a set of packages. [#6489 @arozovyk]
  * `OpamSysInteract`: add `available_packages` and `installed_packages` to be computed separately, redefine `packages_status` accordingly [#6489 @arozovyk]
  * `OpamStateTypes`: add available system package status field `repos_syspkgs_available` (and its type `repo_syspkgs_available`) in `repos_state` for all the depexts declared in repo's packages. The new field is also added to the cache. [#6489 @arozovyk @rjbou]
  * `OpamRepositoryState.load`: load repo's available system packages [#6489 @arozovyk]
  * `OpamFileTools`: add `opams_depexts` to consolidate depexts extraction logic from individual opam files and package maps [#6489 @arozovyk]
  * `OpamUpdate`: add `update_sys_available_cache` to update the system package availability cache in repository state [#6489 @arozovyk]
  * `OpamUpdate.get_sys_available`: factorize depexts availability computation logic from `OpamUpdate.repositories` [#6489 @arozovyk]
  * `OpamRepositoryState`: add `syspkgs_available` that returns the stored depext availability status in repository state [#6489 @rjbou]
  * `OpamSysInteract`: add `available_packages_and_family` that returns availability status and the os family [#6489 @rjbou]
  * `OpamEnv`: remove `get_opam`, `get_opam_raw`, `cygwin_non_shadowed_programs`, `path`, `update_user_setup`, `write_static_init_scripts`, and `clear_dynamic_init_scripts` [#6954 @fantazio]
  * `OpamFileTools`: remove `lint_string` [#6954 @fantazio]
  * `OpamFormatUpgrade`: remove `latest_version` [#6954 @fantazio]
  * `OpamGlobalState`: remove `all_installed`, and `unlock` [#6954 @fantazio]
  * `OpamRepositoryState`: remove `load_repo`, and `cleanup` [#6954 @fantazio]
  * `OpamScript`: remove `prompt` [#6954 @fantazio]
  * `OpamStateConfig`: remove `safe_load`, and `load_config_root` [#6954 @fantazio]
  * `OpamSwitchState`: remove `get_conflicts_t`, `unlock`, `descr`, `descr_opt`, and `dev_packages` [#6954 @fantazio]
  * `OpamUpdate`: remove `dev_package`, `pinned_packages, and `pinned_package` [#6954 @fantazio]


## opam-solver
  * `OpamCudf`: remove `Package.equal`, `Package.compare`, `Package.to_json`, `Package.of_json`, `diff`, `check_request`, `get_final_universe`, `actions_of_diff`, `remove`, `uninstall_all`, `install`, `remove_all_uninstalled_versions_but`, `opam_invariant_package_name`, `opam_deprequest_package_name`, `unavailable_package`, `is_unavailable_package`, `string_of_vpkgs`, `string_of_explanation`, `conflict_cycles`, `string_of_atom`, `string_of_request`, `string_of_universe`, `string_of_packages`, `packages`, `to_cudf`, and `Json` [#6954 @fantazio]
  * `OpamSolver`: remove `empty_universe`, `string_of_request`, `solution_to_json`, `solution_of_json`, `cudf_versions_map`, `check_for_conflicts`, and `coinstallability_check` [#6954 @fantazio]

## opam-format
  * `OpamFile.Descr` was moved to `OpamFile.Descr_legacy` and a simpler `OpamFile.Descr` module was created only containing non-IO functions removing the outdated `descr` file support [#6827 @kit-ty-kate]
  * `OpamFile.URL` was moved to `OpamFile.URL_legacy` and a simpler `OpamFile.URL` module was created only containing non-IO functions removing the outdated `url` file support [#6827 @kit-ty-kate]
  * `OpamFile.Descr.of_legacy`: was added [#6827 @kit-ty-kate]
  * `OpamFile.URL.of_legacy`: was added [#6827 @kit-ty-kate]
  * `OpamFile`: allow dummy filenames to be added a prefix and still be detected as a dummy filename [#6913 @rjbou]
  * `OpamSysPkg`: add `availability_mode` type to indicate the availability of system packages on a given system [#6489 @arozovyk]
  * `OpamSysPkg`: add `equal_availability_mode` function [#6489 @arozovyk]
  * `OpamTypes`: change `result` type name to `solver_result` to avoid conflicts with Stdlib [#6885 @rjbou]
  * `OpamPathName` was added [#6917 @rjbou]
  * `OpamRepositoryPathName` was added [#6917 @rjbou]
  * `OpamRepositoryPath` was moved from `opam-repository` [#6917 @rjbou]
  * `OpamRepositoryPath.{root,repo,packages_dir,packages,opam,files,descr,url}: have been moved to a new `OpamRepositoryPath.Make` functor [#6680 @rjbou @kit-ty-kate]
  * `OpamFilter.expand_interpolations_in_file`: changed argument type from `basename` to `filename` [#6910 @NathanReb]
  * `OpamFile.Config`: remove `with_best_effort_prefix`, `with_solver`, and `with_dl_tool` [#6954 @fantazio]
  * `OpaFile.InitConfig`: remove `opam_version`, `with_jobs`, `with_dl_jobs`, `with_dl_cache`, `with_solver_criteria`, `with_solver`, and `with_global_variables` [#6954 @fantazio]
  * `OpamFile.Descr_legacy`: remove `of_string`, and `full` [#6954 @fantazio]
  * `OpamFile.URL`: remove `with_mirrors`, `with_swhid`, `with_subpath`, and `with_subpath_opt` [#6954 @fantazio]
  * `OpamFile.Environment`: remove `read`, `read_from_channel`, and `read_from_string` [#6954 @fantazio]
  * `OpamFile.Comp`: remove `create_preinstalled`, `opam_version`, `name`, `src`, `configure`, `make`, `make`, `env`, `tags`, `with_src`, `with_patches`, `with_configure`, `with_make`, `with_build`, and `with_packages` [#6954 @fantazio]
  * `OpamFile.Dot_installed`: remove `with_bin`, `with_sbin`, `with_lib`, `with_toplevel`, `with_stublibs`, `with_share`, `with_share_root`, `with_etc`, `with_doc`, `with_man`, `with_libexec`, `with_lib_root`, `with_libexec_root`, and `with_misc` [#6954 @fantazio]
  * `OpamFile.Dot_config: remove `variables` [#6954 @fantazio]
  * `OpamFile.Report`: remove `browse`, `with_browse`, `with_upstream`, `with_announce`, and `with_stamp_opt` [#6954 @fantazio]
  * `OpamFile.Syntax`: remove `pp_channel`, `to_channel`, `to_string`, and `to_string_with_preserved_format` [#6954 @fantazio]
  * `OpamFilter`: remove `string_interp_regex`, `eval_to_string`, `ident_value`, `ident_bool`, `expand_interpolations_in_file_full`, and `gen_filter_formula` [#6954 @fantazio]
  * `OpamFormat`: remove `value_pos`, `V.simple_arg`, `V.group`, `V.map_group`, `V.filter_ident`, `V.package_atom`, `I.file`, `I.item`, and `I.extract_field` [#6954 @fantazio]
  * `OpamFormula`: remove `compare_relop`, `compare_version_constraint`, `string_of_disjunction`, `string_of_cnf`, `string_of_dnf`, `compare`, `compare_nc`, `formula_to_cnf`, `dnf_of_formula`, `simplify_ineq_formula`, `to_conjunction`, `of_conjunction`, `to_disjunction`, and `of_disjunction` [#6954 @fantazio]
  * `OpamPath`: remove `backup`, `plugins`, `plugin`, `Switch.meta_dirname`, `Switch.extra_file`, `Switch.Default.lib_dir`, `Switch.Default.stublibs`, `Switch.Default.toplevel`, `Switch.Default.doc_dir`, `Switch.Default.share_dir`, `Switch.Default.etc_dir`, `Switch.Default.man_dir`, `Switch.Default.man_dirs`, `Switch.Default.sbin`, `Switch.DefaultF.doc_dir`, `Switch.DefaultF.etc_dir`, and `Switch.DefaultF.man_dirs` [#6954 @fantazio]
  * `OpamPp`: remove `ignore` [#6954 @fantazio]
  * `OpamSysPkg`: remove `string_of_status`, and `string_of_to_install` [#6954 @fantazio]
  * `OpamTypesBase`: remove `map_atomic_action`, `map_highlevel_action`, `map_concrete_action`, `nullify_pos_map`, `pos_best`, `iter_success`, `env_update`, `switch_selections_compare`, `simple_arg_equal`, and `arg_equal` [#6954 @fantazio]

## opam-core
  * `OpamCmdliner` was added. It is accessible through a new `opam-core.cmdliner` sub-library [#6755 @kit-ty-kate]
  * `OpamUrl`: rename and expose `local_path` as `looks_like_local_path` [#5979 @kit-ty-kate]
  * `OpamCompat.Gc.ramp_up`: was added [#6515 @dra27]
  * `OpamCompat.Int.min`: was added [#6515 @kit-ty-kate]
  * `OpamStd.String.compare_case`: is now allocation free [#6515 @dra27]
  * `OpamVersionCompare.{compare,equal}`: are now allocation free [#6515 @dra27]
  * `OpamCompat.Filename`: was removed [#6879 @kit-ty-kate]
  * `OpamCompat.List.fold_left_map`: was removed [#6879 @kit-ty-kate]
  * `OpamCompat.MAP.filter_map`: was removed [#6879 @kit-ty-kate]
  * `OpamCompat.Map.add_to_list`: was added [#6818 @dra27]
  * `OpamPatch` was created [#6934 @rjbou]
  * `OpamSystem`: add `is_dir_read_only` [#6489 @rjbou]
  * `OpamSystem.*patch` were moved to `OpamPatch` [#6934 @rjbou]
  * `OpamFilename`: add `is_dir_read_only` [#6489 @rjbou]
  * `OpamFilename.might_escape`: ensure / is detected as a file separator when called with `~sep:Unspecified` on Windows [#6897 @kit-ty-kate]
  * `OpamFilename.Unix` was added abstracting over `/` separated paths regardless of the current system [#6914 @rjbou @kit-ty-kate]
  * `OpamFilename.in_dir`: removed [#6910 @NathanReb]
  * `OpamSystem.in_tmp_dir`: removed [#6910 @NathanReb]
  * `OpamSystem.in_dir`: removed [#6910 @NathanReb]
  * `OpamSystem.chdir`: removed [#6910 @NathanReb]
  * `OpamSystem.{command,commands,read_command_output}`: add a `?dir: dirname` optional arg to launch the command in a specific directory [#6910 @NathanReb]
  * `Cmdliner_msg`: remove `pp_try_help` [#6954 @fantazio]
  * `Cmdliner_trie`: remove `is_empty` [#6954 @fantazio]
  * `OpamCmdliner.Manpage`: remove `title`, `t`, `s_name`, `s_synopsis`, `s_environment_intro`, `s_see_also`, `s_none`, and `print` [#6954 @fantazio]
  * `OpamCmdliner.Term`: remove `app`, `map`, `product`, `Syntax`, `term_result`, `term_result'`, `cli_parse_result`, `cli_parse_result'`, `main_name`, `with_used_args`, `exit_info`, `default_exits`, `default_error_exits`, `env_info`, `name`, `eval_choice`, `eval_peek_opts`, `exit_status_success`, `exit_status_cli_error`, `exit_status_internal_error`, `exit_status_of_result`, `exit_status_of_status_result`, `exit`, `exit_status`, `pure`, and `man_format` [#6954 @fantazio]
  * `OpamCmdliner.Cmd`: remove `Exit.ok`, `Exit.some_error`, `Exit.cli_error`, `Exit.internal_error`, `Exit.info`, `Exit.info_code`, `Exit.defaults`, `Env.var`, `Env.info`, `eval`, `eval'`, `eval_result`, `eval_result'`, `eval_exit`, `eval_value'`, and `eval_peek_opts` [#6954 @fantazio]
  * `OpamCmdliner.Arg`: remove `conv`, `conv'`, `conv_parser`, `conv_printer`, `conv_docv`, `parser_of_kind_of_string`, `some'`, `pos_left`, `last`, `bool`, `char`, `nativeint`, `int32`, `int64`, `float` `file`, `dir`, `non_dir_file`, `array`, `t2`, `t3`, `t4`, `doc_quote`, `doc_alts`, `converter`, `pconv`, `env`, and `env_var` [#6954 @fantazio]
  * `OpamConsole`: remove `color`, `utf8_extended`, `acolor`, `acolor_w`, `Symbols.latin_capital_letter_o_with_stroke`, and `Tree.get_default_symbols` [#6954 @fantazio]
  * `OpamCoreConfig`: remove `E.confirmlevel`, `E.yes`, `set`, and `setk` [#6954 @fantazio]
  * `OpamDirTrack`: remove `is_empty`, and `string_of_change` [#6954 @fantazio]
  * `OpamFilename`: remove `env_of_list`, `to_list_dir`, `with_open_out_bin`, `with_tmp_file`, `with_tmp_file_job`, `with_contents`, `copy_in`, `extract_generic_file`, `with_flock_write_then_read`, `Attribute.to_string_list`, and `Attribute.of_string_list` [#6954 @fantazio]
  * `OpamHash`: remove `md5`, `sha256`, and `sha512` [#6954 @fantazio]
  * `OpamProcess`: remove `is_verbose`, and `Job.seq_map` [#6954 @fantazio]
  * `OpamSHA`: remove `sha1_file`, `sha256_file`, `sha512_file`, `sha1_string`, `sha256_string`, and `sha512_string` [#6954 @fantazio]
  * `OpamSystem`: remove `verbose_for_base_commands`, `get_files`, `files_with_links`, `directories_with_links`, `lock_max`, `register_printer`, and `classify_executable` [#6954 @fantazio]
  * `OpamVersion`: remove `major`, `git`, and `message` [#6954 @fantazio]
  * `OpamVersionCompare`: remove `equal` [#6954 @fantazio]
