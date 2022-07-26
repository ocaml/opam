(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase
open Cmdliner
open OpamStd.Op

include OpamArgTools


(** Utils *)

let when_enum =
  [ "always", `Always; "never", `Never; "auto", `Auto ]
  |> List.map (fun (s,v) -> cli_original, s, v)

let confirm_enum = [
  cli_from cli2_1, "ask", `ask;
  cli_from cli2_1, "no", `all_no;
  cli_from cli2_1, "yes", `all_yes;
  cli_from cli2_1, "unsafe-yes", `unsafe_yes;
]

(* Windows directory separators need to be escaped for manpages *)
let dir_sep, escape_path =
  match Filename.dir_sep with
  | "\\" ->
    let esc = "\\\\" in
    esc,
    fun p ->
      OpamStd.List.concat_map esc (fun x -> x)
        (OpamStd.String.split_delim p '\\')
  | ds -> ds, fun x -> x


(** Opam environment variables *)

(* Environment variables that need to be initialised before config init, see
   [OpamCliMain.run]. *)
let preinit_environment_variables =
  let open OpamStd.Config in
  let core =
    let open OpamCoreConfig.E in [
      "DEBUG", (fun v -> DEBUG (env_int v)),
      "see options `--debug' and `--debug-level'.";
      "YES", (fun v -> YES (env_bool v)),
      "see options `--yes' and `--confirm-level`. \
       $(b,OPAMYES) has priority over $(b,OPAMNO) and is \
       ignored if $(b,OPAMCONFIRMLEVEL) is set.";
    ] in
  let client =
    let open OpamClientConfig.E in [
      "CLI", (fun v -> CLI (env_string v)), "see option `--cli'.";
      "NOSELFUPGRADE",(fun v -> NOSELFUPGRADE (env_string v)),
      "see option `--no-self-upgrade'";
      "ROOTISOK", (fun v -> ROOTISOK (env_bool v)),
      "don't complain when running as root.";
    ] in
  core @ client

let preinit_opam_env_variables, doc_opam_env_variables_pre =
  let preinit () =
    OpamStd.Config.E.updates @@
    List.map (fun (var, cons, _doc) -> cons var)
      preinit_environment_variables
  in
  let doc =
    List.map (fun (var, _cons, doc) ->
        `P (Printf.sprintf "$(i,OPAM%s) %s" var doc))
      preinit_environment_variables
  in
  preinit, doc

(* Environment variables with their doc and their validity OPAMVAR_var and
   OPAMPACKAGE_var are defined and documented static in [help_sections].
*)
let environment_variables =
  let open OpamStd.Config in
  let core =
    let open OpamCoreConfig.E in [
      "COLOR", cli_original, (fun v -> COLOR (env_when v)),
      "when set to $(i,always) or $(i,never), sets a default value for the \
       `--color' option.";
      "CONFIRMLEVEL", cli_from cli2_1, (fun v -> CONFIRMLEVEL (env_answer v)),
      "see option `--confirm-level`. \
       $(b,OPAMCONFIRMLEVEL) has priority over $(b,OPAMYES) \
       and $(b,OPAMNO).";
      "DEBUGSECTIONS", cli_from cli2_1, (fun v -> DEBUGSECTIONS (env_sections v)),
      "if set, limits debug messages to the space-separated list of \
       sections. Sections can optionally have a specific debug level (for \
       example, $(b,CLIENT:2) or $(b,CLIENT CUDF:2)), but otherwise use \
       `--debug-level'.";
      "ERRLOGLEN", cli_original, (fun v -> ERRLOGLEN (env_int v)),
      "sets the number of log lines printed when a sub-process fails. 0 to \
       print all.";
      "KEEPLOGS", cli_original, (fun v -> KEEPLOGS (env_bool v)),
      "tells opam to not remove some temporary command logs and some \
       backups. This skips some finalisers and may also help to get more \
       reliable backtraces.";
      "LOGS", cli_original, (fun v -> LOGS (env_string v)),
      ("$(i,logdir) sets log directory, default is a temporary directory in \
       " ^ (if Sys.win32 then "%TEMP%" else "/tmp"));
      "MERGEOUT", cli_original, (fun v -> MERGEOUT (env_bool v)),
      "merge process outputs, stderr on stdout.";
      "NO", cli_original, (fun v -> NO (env_bool v)),
      "answer no to any question asked, see options `--no` and `--confirm-level`. \
       $(b,OPAMNO) is ignored if either $(b,OPAMCONFIRMLEVEL) or $(b,OPAMYES) \
       is set.";
      "PRECISETRACKING", cli_original, (fun v -> PRECISETRACKING (env_bool v)),
      "fine grain tracking of directories.";
      "SAFE", cli_original, (fun v -> SAFE (env_bool v)),
      "see option `--safe'.";
      "STATUSLINE", cli_original, (fun v -> STATUSLINE (env_when v)),
      ("display a dynamic status line showing what's currently going on on \
        the terminal. (one of "^string_of_enum when_enum^")");
      "USEOPENSSL", cli_between cli2_0 cli2_2, (fun _v -> OpamStd.Config.E.REMOVED),
      "force openssl use for hash computing.";
      "UTF8", cli_original, (fun v -> UTF8 (env_when_ext v)),
      (Printf.sprintf "use UTF8 characters in output (one of %s). By default \
                       `auto', which is determined from the locale)."
         (string_of_enum when_enum));
      "UTF8MSGS", cli_original, (fun v -> UTF8MSGS (env_bool v)),
      "use extended UTF8 characters (camels) in opam messages. Implies \
       $(i,OPAMUTF8). This is set by default on macOS only.";
      "VERBOSE", cli_original, (fun v -> VERBOSE (env_level v)),
      "see option `--verbose'.";
    ] in
  let format =
    let open OpamFormatConfig.E in [
      "ALLPARENS", cli_original, (fun v -> ALLPARENS (env_bool v)),
      "surround all filters with parenthesis.";
      "SKIPVERSIONCHECKS", cli_original,
      (fun v -> SKIPVERSIONCHECKS (env_bool v)),
      "bypasses some version checks. Unsafe, for compatibility testing only.";
      "STRICT", cli_original, (fun v -> STRICT (env_bool v)),
      "fail on inconsistencies (file reading, switch import, etc.).";
    ] in
  let solver =
    let open OpamSolverConfig.E in [
      "BESTEFFORT", cli_original, (fun v -> BESTEFFORT (env_bool v)),
      "see option `--best-effort'.";
      "BESTEFFORTPREFIXCRITERIA", cli_original,
      (fun v -> BESTEFFORTPREFIXCRITERIA (env_string v)),
      "sets the string that must be prepended to the criteria when the \
       `--best-effort' option is set, and is expected to maximise the \
       `opam-query' property in the solution.";
      "CRITERIA", cli_original, (fun v -> CRITERIA (env_string v)),
      "specifies user $(i,preferences) for dependency solving. The default \
       value depends on the solver version, use `config report' to know the \
       current setting. See also option --criteria.";
      "CUDFFILE", cli_original, (fun v -> CUDFFILE (env_string v)),
      "save the cudf graph to $(i,file)-actions-explicit.dot.";
      "CUDFTRIM", cli_original, (fun v -> CUDFTRIM (env_string v)),
      "controls the filtering of unrelated packages during CUDF preprocessing.";
      "DIGDEPTH", cli_original, (fun v -> DIGDEPTH (env_int v)),
      "defines how aggressive the lookup for conflicts during CUDF \
       preprocessing is.";
      "EXTERNALSOLVER", cli_original, (fun v -> EXTERNALSOLVER (env_string v)),
      "see option `--solver'.";
      "FIXUPCRITERIA", cli_original, (fun v -> FIXUPCRITERIA (env_string v)),
      "same as $(i,OPAMUPGRADECRITERIA), but specific to fixup.";
      "NOASPCUD", cli_original, (fun v -> NOASPCUD (env_bool v)),
      "Deprecated.";
      "PREPRO", cli_original, (fun v -> PREPRO (env_bool v)),
      "set this to false to disable CUDF preprocessing. Less efficient, but \
       might help debugging solver issue.";
      "SOLVERALLOWSUBOPTIMAL", cli_from cli2_1,
      (fun v -> SOLVERALLOWSUBOPTIMAL (env_bool v)),
      "(default `true') allows some solvers to still return a solution when \
       they reach timeout; while the solution remains assured to be \
       consistent, there is no guarantee in this case that it fits the \
       expected optimisation criteria. If `true', opam willcontinue with a \
       warning, if `false' a timeout is an error. Currently only \
       the builtin-z3 backend handles this degraded case.";
      "SOLVERTIMEOUT", cli_original, (fun v -> SOLVERTIMEOUT (env_float v)),
      (Printf.sprintf
         "change the time allowance of the solver. Default is %.1f, set to 0 \
          for unlimited. Note that all solvers may not support this option."
         (OpamStd.Option.default 0. OpamSolverConfig.(default.solver_timeout)));
      "UPGRADECRITERIA", cli_original,
      (fun v -> UPGRADECRITERIA (env_string v)),
      "specifies user $(i,preferences) for dependency solving when performing \
       an upgrade. Overrides $(i,OPAMCRITERIA) in upgrades if both are set. \
       See also option --criteria.";
      "USEINTERNALSOLVER", cli_original,
      (fun v -> USEINTERNALSOLVER (env_bool v)),
      "see option `--use-internal-solver'.";
      "VERSIONLAGPOWER", cli_original, (fun v -> VERSIONLAGPOWER (env_int v)),
      "do not use.";
    ] in
  let repository =
    let open OpamRepositoryConfig.E in [
      "CURL", cli_original, (fun v -> CURL (env_string v)),
      "can be used to select a given 'curl' program. See $(i,OPAMFETCH) for \
       more options.";
      "FETCH", cli_original, (fun v -> FETCH (env_string v)),
      "specifies how to download files: either `wget', `curl' or a custom \
       command where variables $(b,%{url}%), $(b,%{out}%), $(b,%{retry}%), \
       $(b,%{compress}%) and $(b,%{checksum}%) will be replaced. Overrides the \
       'download-command' value from the main config file.";
      "NOCHECKSUMS", cli_original, (fun v -> NOCHECKSUMS (env_bool v)),
      "enables option --no-checksums when available.";
      "REPOSITORYTARRING", cli_from cli2_2,
      (fun b -> REPOSITORYTARRING (env_bool b)),
      "internally store the repositories as tar.gz files. This can be much \
       faster on filesystems that don't cope well with scanning large trees \
       but have good caching in /tmp. However this is slower in the \
       general case.";
      "REQUIRECHECKSUMS", cli_original,
      (fun v -> REQUIRECHECKSUMS (env_bool v)),
      "Enables option `--require-checksums' when available \
       (e.g. for `opam install').";
      "RETRIES", cli_original, (fun v -> RETRIES (env_int v)),
      "sets the number of tries before failing downloads.";
      "VALIDATIONHOOK", cli_original, (fun v -> VALIDATIONHOOK (env_string v)),
      "if set, uses the `%{hook%}' command to validate \
       an opam repository update.";
    ] in
  let state =
    let open OpamStateConfig.E in [
      "BUILDDOC", cli_between cli2_0 cli2_1,
      (fun v -> BUILDDOC (env_bool v)), "see option `--build-doc'.";
      "BUILDTEST", cli_between cli2_0 cli2_1,
      (fun v -> BUILDTEST (env_bool v)), "see option `--build-test'.";
      "DOWNLOADJOBS", cli_original, (fun v -> DOWNLOADJOBS (env_int v)),
      "sets the maximum number of simultaneous downloads.";
      "DRYRUN", cli_original, (fun v -> DRYRUN (env_bool v)),
      "see option `--dry-run'.";
      "IGNORECONSTRAINTS", cli_original,
      (fun v -> IGNORECONSTRAINTS (env_string v)),
      "see install option `--ignore-constraints-on'.";
      "JOBS", cli_original, (fun v -> JOBS (env_int v)),
      "sets the maximum number of parallel workers to run.";
      "LOCKED", cli_original, (fun v -> LOCKED (env_string v)),
      "combination of `--locked' and `--lock-suffix' options.";
      "MAKECMD", cli_original, (fun v -> MAKECMD (env_string v)),
      "set the system make command to use.";
      "NODEPEXTS", cli_from cli2_1, (fun v -> NODEPEXTS (env_bool v)),
      "disables system dependencies handling, see option `--no-depexts'.";
      "NOENVNOTICE", cli_original, (fun v -> NOENVNOTICE (env_bool v)),
      "Internal.";
      "ROOT", cli_original, (fun v -> ROOT (env_string v)),
      "see option `--root'. This is automatically set \
       by `opam env --root=DIR --set-root'.";
      "SWITCH", cli_original, (fun v -> SWITCH (env_string v)),
      "see option `--switch'. Automatically set \
       by `opam env --switch=SWITCH --set-switch'.";
      "UNLOCKBASE", cli_original, (fun v -> UNLOCKBASE (env_bool v)),
      "see install option `--unlock-base'.";
      "WITHTOOLS", cli_from cli2_2, (fun v -> WITHTOOLS (env_bool v)),
      "see install option `--with-tools'.";
      "WITHDOC", cli_original, (fun v -> WITHDOC (env_bool v)),
      "see install option `--with-doc'.";
      "WITHTEST", cli_original, (fun v -> WITHTEST (env_bool v)),
      "see install option `--with-test.";
    ] in
  let client =
    let open OpamClientConfig.E in [
      "ASSUMEDEPEXTS", cli_from cli2_1, (fun v -> ASSUMEDEPEXTS (env_bool v)),
      "see option `--assume-depexts'.";
      "AUTOREMOVE", cli_original, (fun v -> AUTOREMOVE (env_bool v)),
      "see remove option `--auto-remove'.";
      "DROPWORKINGDIR", cli_from cli2_1, (fun v -> DROPWORKINGDIR (env_bool v)),
      "overrides packages previously updated with $(b,--working-dir) on \
       update. Without this variable set, opam would keep them unchanged \
       unless explicitly named on the command-line.";
      "EDITOR", cli_original, (fun v -> EDITOR (env_string v)),
      "sets the editor to use for opam file editing, overrides $(i,\\$EDITOR) \
       and $(i,\\$VISUAL).";
      "FAKE", cli_original, (fun v -> FAKE (env_bool v)),
      "see option `--fake'.";
      "IGNOREPINDEPENDS", cli_original,
      (fun v -> IGNOREPINDEPENDS (env_bool v)),
      "see option `--ignore-pin-depends'.";
      "INPLACEBUILD", cli_original, (fun v -> INPLACEBUILD (env_bool v)),
      "see option `--inplace-build'.";
      "JSON", cli_original, (fun v -> JSON (env_string v)),
      "log json output to the given file \
       (use character `%' to index the files).";
      "KEEPBUILDDIR", cli_original, (fun v -> KEEPBUILDDIR (env_bool v)),
      "see install option `--keep-build-dir'.";
      "NOAUTOUPGRADE", cli_original, (fun v -> NOAUTOUPGRADE (env_bool v)),
      "disables automatic internal upgrade of repositories in an earlier \
       format to the current one, on 'update' or 'init'.";
      "NOAGGREGATE", cli_original, (fun v -> NOAGGREGATE (env_bool v)),
      "with `opam admin check', don't aggregate packages.";
      "PINKINDAUTO", cli_original, (fun v -> PINKINDAUTO (env_bool v)),
      "sets whether version control systems should be detected when pinning \
       to a local path. Enabled by default since 1.3.0.";
      "REUSEBUILDDIR", cli_original, (fun v -> REUSEBUILDDIR (env_bool v)),
      "see option `--reuse-build-dir'.";
      "SHOW", cli_original, (fun v -> SHOW (env_bool v)),
      "see option `--show'.";
      "SKIPUPDATE", cli_original, (fun v -> SKIPUPDATE (env_bool v)),
      "see option `--skip-updates'.";
      "STATS", cli_original, (fun v -> STATS (env_bool v)),
      "display stats at the end of command.";
      "WORKINGDIR", cli_original, (fun v -> WORKINGDIR (env_bool v)),
      "see option `--working-dir'.";
    ] in
  core @ format @ solver @ repository @ state @ client

let scrubbed_environment_variables =
  let f (name, validity, _, _) =
    if is_original_cli validity then
      None
    else
      Some ("OPAM" ^ name)
  in
    OpamStd.List.filter_map f environment_variables

let doc_opam_env_variables, init_opam_env_variabes =
  env_with_cli environment_variables

(** Help sections common to all commands *)

let global_option_section = Manpage.s_common_options
let help_sections cli =
  [
    `S global_option_section;
    `P "These options are common to all commands.";

    `S Manpage.s_environment;
    `P "Opam makes use of the environment variables listed here. Boolean \
        variables should be set to \"0\", \"no\", \"false\" or the empty \
        string to disable, \"1\", \"yes\" or \"true\" to enable.";
  ] @
  List.sort compare (doc_opam_env_variables_pre @ doc_opam_env_variables cli)
  @ [
    `P "$(i,OPAMVAR_var) overrides the contents of the variable $(i,var)  when \
        substituting `%{var}%` strings in `opam` files.";
    `P "$(i,OPAMVAR_package_var) overrides the contents of the variable \
        $(i,package:var) when substituting `%{package:var}%` strings in \
        `opam` files.";

    `S "CLI VERSION";
    `P "All scripts and programmatic invocations of opam should use `--cli' in \
        order to ensure that they work seamlessly with future versions of the \
        opam client. Additionally, blog posts or other documentation can \
        benefit, as it prevents information from becoming stale.";
    `P (Printf.sprintf
          "Although opam only supports roots ($(i,~%s.opam%s)) for the current \
           version, it does provide backwards compatibility for its \
           command-line interface." dir_sep dir_sep);
    `P "Since CLI version support was only added in opam 2.1, use $(i,OPAMCLI) \
        to select 2.0 support (as opam 2.0 will just ignore it), \
        and `--cli=2.1' for 2.1 (or later) versions, since an environment variable \
        controlling the parsing of syntax is brittle. To this end, opam \
        displays a warning if $(i,OPAMCLI) specifies a valid version other \
        than 2.0, and also if `--cli=2.0' is specified.";
    `P "The command-line version is selected by using the `--cli' option or \
        the $(i,OPAMCLI) environment variable. `--cli' may be specified more\
        than once, where the last instance takes precedence. $(i,OPAMCLI) is \
        only inspected if `--cli' is not given.";

    `S Manpage.s_exit_status;
    `P "As an exception to the following, the `exec' command returns 127 if the \
        command was not found or couldn't be executed, and the command's exit \
        value otherwise."
  ] @
  List.map (fun (reason, code) ->
      `I (string_of_int code, match reason with
        | `Success ->
          "Success, or true for boolean queries."
        | `False ->
          "False. Returned when a boolean return value is expected, e.g. when \
           running with $(b,--check), or for queries like $(b,opam lint)."
        | `Bad_arguments ->
          "Bad command-line arguments, or command-line arguments pointing to \
           an invalid context (e.g. file not following the expected format)."
        | `Not_found ->
          "Not found. You requested something (package, version, repository, \
           etc.) that couldn't be found."
        | `Aborted ->
          "Aborted. The operation required confirmation, which wasn't given."
        | `Locked ->
          "Could not acquire the locks required for the operation."
        | `No_solution ->
          "There is no solution to the user request. This can be caused by \
           asking to install two incompatible packages, for example."
        | `File_error ->
          "Error in package definition, or other metadata files. Using \
           $(b,--strict) raises this error more often."
        | `Package_operation_error ->
          "Package script error. Some package operations were unsuccessful. \
           This may be an error in the packages or an incompatibility with \
           your system. This can be a partial error."
        | `Sync_error ->
          "Sync error. Could not fetch some remotes from the network. This can \
           be a partial error."
        | `Configuration_error ->
          "Configuration error. Opam or system configuration doesn't allow \
           operation, and needs fixing."
        | `Solver_failure ->
          "Solver failure. The solver failed to return a sound answer. It can \
           be due to a broken external solver, or an error in solver \
           configuration."
        | `Internal_error ->
          "Internal error. Something went wrong, likely due to a bug in opam \
           itself."
        | `User_interrupt ->
          "User interrupt. SIGINT was received, generally due to the user \
           pressing Ctrl-C."
        ))
    OpamStd.Sys.exit_codes
  @ [
    `S "FURTHER DOCUMENTATION";
    `P (Printf.sprintf "See https://opam.ocaml.org/doc.");

    `S Manpage.s_authors;
    `P "Vincent Bernardoff <vb@luminar.eu.org>"; `Noblank;
    `P "Raja Boujbel       <raja.boujbel@ocamlpro.com>"; `Noblank;
    `P "Roberto Di Cosmo   <roberto@dicosmo.org>"; `Noblank;
    `P "Thomas Gazagnaire  <thomas@gazagnaire.org>"; `Noblank;
    `P "Louis Gesbert      <louis.gesbert@ocamlpro.com>"; `Noblank;
    `P "Fabrice Le Fessant <Fabrice.Le_fessant@inria.fr>"; `Noblank;
    `P "Anil Madhavapeddy  <anil@recoil.org>"; `Noblank;
    `P "Guillem Rieu       <guillem.rieu@ocamlpro.com>"; `Noblank;
    `P "Ralf Treinen       <ralf.treinen@pps.jussieu.fr>"; `Noblank;
    `P "Frederic Tuong     <tuong@users.gforge.inria.fr>";

    `S Manpage.s_bugs;
    `P "Check bug reports at https://github.com/ocaml/opam/issues.";
  ]


(** Global options *)

type global_options = {
  debug_level: int option;
  verbose: int;
  quiet : bool;
  color : OpamStd.Config.when_ option;
  opt_switch : string option;
  confirm_level : OpamStd.Config.answer option;
  yes: bool option;
  strict : bool;
  opt_root : dirname option;
  git_version : bool;
  external_solver : string option;
  use_internal_solver : bool;
  cudf_file : string option;
  solver_preferences : string option;
  best_effort : bool;
  safe_mode : bool;
  json : string option;
  no_auto_upgrade : bool;
  working_dir : bool;
  ignore_pin_depends : bool;
  cli : OpamCLIVersion.t;
}

(* The --cli passed by cmdliner is ignored (it's only there for --help) *)
let create_global_options
    git_version debug debug_level verbose quiet color opt_switch
    yes confirm_level
    strict
    opt_root external_solver use_internal_solver
    cudf_file solver_preferences best_effort safe_mode json no_auto_upgrade
    working_dir ignore_pin_depends
    d_no_aspcud _ =
  if d_no_aspcud then
    OpamConsole.warning
      "Option %s is deprecated, ignoring it."
      (OpamConsole.colorise `bold "--no-aspcud");
  let debug_level = OpamStd.Option.Op.(
      debug_level >>+ fun () -> if debug then Some 1 else None
    ) in
  let get_last l = match List.rev l with [] -> None | x::_ -> Some x in
  let yes = get_last yes in
  let confirm_level = get_last confirm_level in
  let verbose = List.length verbose in
  let cli = OpamCLIVersion.current in
  { git_version; debug_level; verbose; quiet; color; opt_switch; confirm_level; yes;
    strict; opt_root; external_solver; use_internal_solver;
    cudf_file; solver_preferences; best_effort; safe_mode; json;
    no_auto_upgrade; working_dir; ignore_pin_depends; cli }

let apply_global_options cli o =
  if o.git_version then (
    begin match OpamGitVersion.version with
      | None   -> ()
      | Some v -> OpamConsole.msg "%s\n" v
    end;
    OpamStd.Sys.exit_because `Success
  );
  let open OpamStd.Option.Op in
  let flag f = if f then Some true else None in
  let some x = match x with None -> None | some -> Some some in
  let solver =
    if o.use_internal_solver then
      Some (lazy (OpamCudfSolver.get_solver ~internal:true
                    OpamCudfSolver.default_solver_selection))
    else
      o.external_solver >>| fun s -> lazy (OpamCudfSolver.solver_of_string s)
  in
  let solver_prefs = o.solver_preferences >>| fun p -> lazy (Some p) in
  let yes = OpamStd.Option.(map some o.yes) in
  init_opam_env_variabes cli;
  OpamClientConfig.opam_init
    (* - format options - *)
    ?strict:(flag o.strict)
    (* ?skip_version_checks:bool *)
    (* ?all_parens:bool *)
    (* - core options - *)
    ?debug_level:(if o.safe_mode then Some 0 else o.debug_level)
    ?verbose_level:(if o.quiet then Some 0 else
                    if o.verbose = 0 then None else Some o.verbose)
    ?color:o.color
    (* ?utf8:[ `Extended | `Always | `Never | `Auto ] *)
    (* ?disp_status_line:[ `Always | `Never | `Auto ] *)
    ?confirm_level:o.confirm_level
    ?yes
    ?safe_mode:(flag o.safe_mode)
    (* ?lock_retries:int *)
    (* ?log_dir:OpamTypes.dirname *)
    (* ?keep_log_dir:bool *)
    (* - repository options - *)
    (* ?download_tool:(OpamTypes.arg list * dl_tool_kind) Lazy.t *)
    (* ?retries:int *)
    (* ?force_checksums:bool option *)
    (* - solver options *)
    ?cudf_file:(some o.cudf_file)
    ?solver
    ?best_effort:(flag o.best_effort)
    ?solver_preferences_default:solver_prefs
    ?solver_preferences_upgrade:solver_prefs
    ?solver_preferences_fixup:solver_prefs
    (* ?solver_preferences_best_effort_prefix: *)
    (* - state options - *)
    ?root_dir:o.opt_root
    ?current_switch:(o.opt_switch >>| OpamSwitch.of_string)
    ?switch_from:(o.opt_switch >>| fun _ -> `Command_line)
    (* ?jobs: int *)
    (* ?dl_jobs: int *)
    (* ?keep_build_dir:bool *)
    (* ?build_test:bool *)
    (* ?build_doc:bool *)
    (* ?show:bool *)
    (* ?dryrun:bool *)
    (* ?fake:bool *)
    (* ?makecmd:string Lazy.t *)
    (* ?ignore_constraints_on:name_set *)
    (* ?skip_dev_update:bool *)
    ?json_out:OpamStd.Option.Op.(o.json >>| function "" -> None | s -> Some s)
    (* ?root_is_ok:bool *)
    ?no_auto_upgrade:(flag o.no_auto_upgrade)
    (* - client options - *)
    ?working_dir:(flag o.working_dir)
    ?ignore_pin_depends:(flag o.ignore_pin_depends)
    (* ?print_stats:bool *)
    (* ?sync_archives:bool *)
    (* ?pin_kind_auto:bool *)
    (* ?autoremove:bool *)
    (* ?editor:string *)
    ~cli:o.cli
    ();
  if OpamClientConfig.(!r.json_out <> None) then (
    OpamJson.append "opam-version" (`String OpamVersion.(to_string (full ())));
    OpamJson.append "command-line"
      (`A (List.map (fun s -> `String s) (Array.to_list Sys.argv)))
  )


(** Build options *)

type build_options = {
  keep_build_dir: bool;
  reuse_build_dir: bool;
  inplace_build : bool;
  make          : string option;
  no_checksums  : bool;
  req_checksums : bool;
  build_test    : bool;
  build_doc     : bool;
  with_tools      : bool;
  show          : bool;
  dryrun        : bool;
  fake          : bool;
  skip_update   : bool;
  jobs          : int option;
  ignore_constraints_on: name list option;
  unlock_base   : bool;
  locked        : bool;
  lock_suffix   : string;
  assume_depexts: bool;
  no_depexts    : bool;
}

let create_build_options
    keep_build_dir reuse_build_dir inplace_build make no_checksums
    req_checksums build_test build_doc with_tools show dryrun skip_update
    fake jobs ignore_constraints_on unlock_base locked lock_suffix
    assume_depexts no_depexts
    =
  {
    keep_build_dir; reuse_build_dir; inplace_build; make; no_checksums;
    req_checksums; build_test; build_doc; with_tools; show; dryrun; skip_update;
    fake; jobs; ignore_constraints_on; unlock_base; locked; lock_suffix;
    assume_depexts; no_depexts;
  }

let apply_build_options cli b =
  let open OpamStd.Option.Op in
  let flag f = if f then Some true else None in
  OpamRepositoryConfig.update
    (* ?download_tool:(OpamTypes.arg list * dl_tool_kind) Lazy.t *)
    (* ?retries:int *)
    ?force_checksums:(if b.req_checksums then Some (Some true)
                      else if b.no_checksums then Some (Some false)
                      else None)
    ();
  OpamStateConfig.update
    (* ?root: -- handled globally *)
    ?jobs:(b.jobs >>| fun j -> lazy j)
    (* ?dl_jobs:int *)
    (* ?no_base_packages:(flag o.no_base_packages) -- handled globally *)
    ?build_test:(flag b.build_test)
    ?build_doc:(flag b.build_doc)
    ?with_tools:(flag b.with_tools)
    ?dryrun:(flag b.dryrun)
    ?makecmd:(b.make >>| fun m -> lazy m)
    ?ignore_constraints_on:
      (b.ignore_constraints_on >>|
       OpamPackage.Name.Set.of_list)
    ?unlock_base:(flag b.unlock_base)
    ?locked:(if b.locked then Some (Some b.lock_suffix) else None)
    ?no_depexts:(flag (b.no_depexts || OpamCLIVersion.Op.(cli @= cli2_0)))
    ();
  OpamClientConfig.update
    ?keep_build_dir:(flag b.keep_build_dir)
    ?reuse_build_dir:(flag b.reuse_build_dir)
    ?inplace_build:(flag b.inplace_build)
    ?show:(flag b.show)
    ?fake:(flag b.fake)
    ?skip_dev_update:(flag b.skip_update)
    ?assume_depexts:(flag (b.assume_depexts || b.no_depexts))
    ~scrubbed_environment_variables
    ()


(** Converters *)

let pr_str = Format.pp_print_string

let repository_name =
  let parse str = `Ok (OpamRepositoryName.of_string str) in
  let print ppf name = pr_str ppf (OpamRepositoryName.to_string name) in
  parse, print

let url =
  let parse str =
    match OpamUrl.parse_opt ~from_file:false str with
    | Some url -> `Ok url
    | None -> `Error ("malformed url "^str)
  in
  let print ppf url = pr_str ppf (OpamUrl.to_string url) in
  parse, print

let filename =
  let parse str = `Ok (OpamFilename.of_string str) in
  let print ppf filename = pr_str ppf (OpamFilename.to_string filename) in
  parse, print

let existing_filename_or_dash =
  let parse str =
    if str = "-" then `Ok None
    else
      let f = OpamFilename.of_string str in
      if OpamFilename.exists f then `Ok (Some f)
      else
        `Error (Printf.sprintf "File %s not found" (OpamFilename.to_string f))
  in
  let print ppf filename =
    pr_str ppf OpamStd.Option.Op.((filename >>| OpamFilename.to_string) +! "-") in
  parse, print

let dirname =
  let parse str = `Ok (OpamFilename.Dir.of_string str) in
  let print ppf dir = pr_str ppf (escape_path (OpamFilename.prettify_dir dir)) in
  parse, print

let existing_filename_dirname_or_dash =
  let parse str =
    if str = "-" then `Ok None else
    match OpamFilename.opt_file (OpamFilename.of_string str) with
    | Some f -> `Ok (Some (OpamFilename.F f))
    | None -> match OpamFilename.opt_dir (OpamFilename.Dir.of_string str) with
      | Some d -> `Ok (Some (OpamFilename.D d))
      | None ->
        `Error (Printf.sprintf "File or directory %s not found" str)
  in
  let print ppf gf =
    pr_str ppf @@ match gf with
    | None -> "-"
    | Some (OpamFilename.D d) -> OpamFilename.Dir.to_string d
    | Some (OpamFilename.F f) -> OpamFilename.to_string f
  in
  parse, print

let subpath_conv =
  let parse str =
    `Ok (OpamFilename.SubPath.of_string str)
  in
  let print ppf sb = pr_str ppf (OpamFilename.SubPath.to_string sb) in
  parse, print

let package_name =
  let parse str =
    try `Ok (OpamPackage.Name.of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf pkg = pr_str ppf (OpamPackage.Name.to_string pkg) in
  parse, print

let package_version =
  let parse str =
    try `Ok (OpamPackage.Version.of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf ver = pr_str ppf (OpamPackage.Version.to_string ver) in
  parse, print

let positive_integer : int Arg.converter =
  let (parser, printer) = Arg.int in
  let parser s =
    match parser s with
    | `Error _ -> `Error "expected a strictly positive integer"
    | `Ok n as r -> if n <= 0
      then `Error "expected a positive integer"
      else r in
  (parser, printer)

(* name * version option *)
let package =
  let parse str =
    let re = Re.(compile @@ seq [
        bos;
        group @@ rep1 @@ diff any (set ">=<.!");
        opt @@ seq [ set ".="; group @@ rep1 any ];
        eos;
      ]) in
    try
      let sub = Re.exec re str in
      let name = OpamPackage.Name.of_string (Re.Group.get sub 1) in
      let version_opt =
        try Some (OpamPackage.Version.of_string (Re.Group.get sub 2))
        with Not_found -> None in
      `Ok (name, version_opt)
    with Not_found | Failure _ -> `Error "bad package format"
  in
  let print ppf (name, version_opt) =
    match version_opt with
    | None -> pr_str ppf (OpamPackage.Name.to_string name)
    | Some v -> pr_str ppf (OpamPackage.Name.to_string name ^"."^
                            OpamPackage.Version.to_string v)
  in
  parse, print

let package_with_version =
  let parse str =
    match fst package str with
    | `Ok (n, Some v) -> `Ok (OpamPackage.create n v)
    | `Ok (_, None) -> `Error "missing package version"
    | `Error e -> `Error e
  in
  let print ppf nv = pr_str ppf (OpamPackage.to_string nv) in
  parse, print

(* name * version constraint *)
let atom =
  let parse str =
    try `Ok (OpamFormula.atom_of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf atom =
    pr_str ppf (OpamFormula.short_string_of_atom atom) in
  parse, print

let atom_or_local =
  let parse str =
    if OpamStd.String.contains ~sub:Filename.dir_sep str ||
       OpamStd.String.starts_with ~prefix:"." str
    then
      if OpamFilename.(exists (of_string str)) then
        `Ok (`Filename (OpamFilename.of_string str))
      else if  OpamFilename.(exists_dir (Dir.of_string str)) then
        `Ok (`Dirname (OpamFilename.Dir.of_string str))
      else
        `Error (Printf.sprintf
                  "Not a valid package specification or existing file or \
                   directory: %s" str)
    else match fst atom str with
      | `Ok at -> `Ok (`Atom at)
      | `Error e -> `Error e
  in
  let print ppf = function
    | `Filename f -> pr_str ppf (OpamFilename.to_string f)
    | `Dirname d -> pr_str ppf (OpamFilename.Dir.to_string d)
    | `Atom a -> snd atom ppf a
  in
  parse, print

let atom_or_dir =
  let parse str = match fst atom_or_local str with
    | `Ok (`Filename _) ->
      `Error (Printf.sprintf
                "Not a valid package specification or existing directory: %s"
                str)
    | `Ok (`Atom _ | `Dirname _ as atom_or_dir) -> `Ok (atom_or_dir)
    | `Error e -> `Error e
  in
  let print ppf = snd atom_or_local ppf in
  parse, print

let dep_formula =
  let module OpamParser = OpamParser.FullPos in
  let module OpamPrinter = OpamPrinter.FullPos in
  let pp = OpamFormat.V.(package_formula `Conj (constraints version)) in
  let parse str =
    try
      let v = OpamParser.value_from_string str "<command-line>" in
      `Ok (OpamPp.parse pp ~pos:pos_null v)
    with e -> OpamStd.Exn.fatal e; `Error (Printexc.to_string e)
  in
  let print ppf f =
    pr_str ppf (OpamPrinter.value (OpamPp.print pp f))
  in
  parse, print

let variable_bindings =
  let parse str =
    try
      OpamStd.String.split str ',' |>
      List.map (fun s -> match OpamStd.String.cut_at s '=' with
          | Some (a, b) -> OpamVariable.of_string a, b
          | None -> Printf.ksprintf failwith "%S is not a binding" s) |>
      fun bnds -> `Ok bnds
    with Failure e -> `Error e
  in
  let print ppf x =
    List.map
      (fun (a,b) -> Printf.sprintf "%s=%s" (OpamVariable.to_string a) b) x |>
    String.concat "," |>
    pr_str ppf
  in
  parse, print

let warn_selector =
  let parse str =
    let sep = Re.(compile (set "+-")) in
    let sel = Re.(compile @@
                  seq [bos; group (rep1 digit);
                       opt @@ seq [str ".."; group (rep1 digit)];
                       eos]) in
    let rec seq i j =
      if i = j then [i]
      else if i < j then i :: seq (i+1) j
      else j :: seq (j+1) i
    in
    let rec aux acc = function
      | `Delim d :: `Text n :: r ->
        let nums =
          let g = Re.exec sel n in
          let i = int_of_string (Re.Group.get g 1) in
          try seq i (int_of_string (Re.Group.get g 2))
          with Not_found -> [i]
        in
        let enabled = Re.Group.get d 0 = "+" in
        let acc = List.fold_left (fun acc n -> (n, enabled) :: acc) acc nums in
        aux acc r
      | [] -> acc
      | _ -> raise Not_found
    in
    try `Ok (List.rev (aux [] (Re.split_full sep str)))
    with Not_found ->
      `Error "Expected a warning string, e.g. '--warn=-10..21+12-36'"
  in
  let print ppf warns =
    pr_str ppf @@
    OpamStd.List.concat_map "" (fun (num,enable) ->
        Printf.sprintf "%c%d" (if enable then '+' else '-') num)
      warns
  in
  parse, print

let _selector =
  let parse str =
    let r =
      List.fold_left (fun (plus, minus) elem ->
          match OpamStd.String.sub_at 1 elem with
          | "+" as prefix ->
            (OpamStd.String.remove_prefix ~prefix elem)::plus, minus
          | "-" as prefix ->
            plus, (OpamStd.String.remove_prefix ~prefix elem)::minus
          |  _ ->  elem::plus, minus)
        ([],[]) (OpamStd.String.split str ',')
    in
    `Ok r
  in
  let print ppf (plus,minus) =
    let concat c =
      OpamStd.List.concat_map ~nil:"" "," (fun x -> c^x)
    in
    pr_str ppf @@ Printf.sprintf "%s,%s" (concat "+" plus) (concat "-" minus)
  in
  parse, print

(* unused
let enum_with_default sl: 'a Arg.converter =
  let parse, print = Arg.enum sl in
  let parse s =
    match parse s with
    | `Ok _ as x -> x
    | _ -> `Ok (`default s) in
  parse, print
*)

let opamlist_column =
  let parse str =
    if OpamStd.String.ends_with ~suffix:":" str then
      let fld = OpamStd.String.remove_suffix ~suffix:":" str in
      `Ok (OpamListCommand.Field fld)
    else
    try
      List.find (function (OpamListCommand.Field _), _ -> false
                        | _, name -> name = str)
        OpamListCommand.field_names
      |> fun (f, _) -> `Ok f
    with Not_found ->
      `Error (Printf.sprintf
                "No known printer for column %s. If you meant an opam file \
                 field, use '%s:' instead (with a trailing colon)."
                str str)
  in
  let print ppf field =
    Format.pp_print_string ppf (OpamListCommand.string_of_field field)
  in
  parse, print

let opamlist_columns =
  let field_re =
    (* max paren nesting 1, obviously *)
    Re.(compile @@ seq [
        start;
        group @@ seq [
          rep @@ diff any (set ",(");
          opt @@ seq [char '('; rep (diff any (char ')')); char ')'];
        ];
        alt [char ','; stop];
      ])
  in
  let parse str =
    try
      let rec aux pos =
        if pos = String.length str then [] else
        let g = Re.exec ~pos field_re str in
        Re.Group.get g 1 :: aux (Re.Group.stop g 0)
      in
      let fields = aux 0 in
      List.fold_left (function
          | `Error _ as e -> fun _ -> e
          | `Ok acc -> fun str ->
            match fst opamlist_column str with
            | `Ok f -> `Ok (acc @ [f])
            | `Error _ as e -> e)
        (`Ok []) fields
    with Not_found ->
      `Error (Printf.sprintf "Invalid columns specification: '%s'." str)
  in
  let print ppf cols =
    let rec aux = function
      | x::(_::_) as r ->
        snd opamlist_column ppf x; Format.pp_print_char ppf ','; aux r
      | [x] -> snd opamlist_column ppf x
      | [] -> ()
    in
    aux cols
  in
  parse, print

let term_info ~cli title ~doc ~man =
  let man = man @ help_sections cli in
  Term.info ~sdocs:global_option_section ~docs:Manpage.s_commands
    ~doc ~man title

let mk_command ~cli validity name ~doc ~man =
  mk_command ~cli validity term_info name ~doc ~man

let mk_command_ret ~cli validity name ~doc ~man =
  mk_command_ret ~cli validity term_info name ~doc ~man

let make_command_alias ~cli cmd ?(options="") name =
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s%s)." orig options in
  let man = [
    `S Manpage.s_description;
    `P (Printf.sprintf "$(mname)$(b, %s) is an alias for $(mname)$(b, %s%s)."
          name orig options);
    `P (Printf.sprintf "See $(mname)$(b, %s --help) for details."
          orig);
    `S Manpage.s_options;
  ] @ help_sections cli
  in
  term,
  Term.info name
    ~docs:"COMMAND ALIASES" ~sdocs:global_option_section
    ~doc ~man

let arg_list name doc kind =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(value & pos_all kind [] & doc)

let nonempty_arg_list name doc kind =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(non_empty & pos_all kind [] & doc)


(** Common flags *)

let print_short_flag cli validity =
  mk_flag ~cli validity ["s";"short"]
    "Output raw lists of names, one per line, skipping any details."

let shell_opt cli validity =
  let enum = [
    None,"bash",SH_bash;
    None,"sh",SH_sh;
    None,"csh",SH_csh;
    None,"zsh",SH_zsh;
    None,"fish",SH_fish;
    Some cli2_2,"pwsh",SH_pwsh;
    Some cli2_2,"cmd",SH_win_cmd;
    Some cli2_2,"powershell",SH_win_powershell
  ] |> List.map (fun (c,s,v) -> OpamStd.Option.map_default cli_from cli_original c, s, v)
  in
  mk_enum_opt ~cli validity ["shell"] "SHELL" enum
    (Printf.sprintf
       "Sets the configuration mode for opam environment appropriate for \
        $(docv). One of %s. Guessed from the parent processes and the \\$SHELL \
        variable by default."
       (string_of_enum enum))

let dot_profile_flag cli validity =
  mk_opt ~cli validity ["dot-profile"]
    "FILENAME"
    (Printf.sprintf
      "Name of the configuration file to update instead of \
       $(i,~%s.profile) or $(i,~%s.zshrc) based on shell detection."
      dir_sep dir_sep)
    (Arg.some filename) None

let repo_kind_flag cli validity =
  let main_kinds = [
    "http" , `http;
    "local", `rsync;
    "git"  , `git;
    "darcs", `darcs;
    "hg"   , `hg;
  ] |> List.map (fun (s,v) -> cli_original, s, v)
  in
  let aliases_kinds = [
    "wget" , `http;
    "curl" , `http;
    "rsync", `rsync;
  ] |> List.map (fun (s,v) -> cli_original, s, v)
  in
  mk_enum_opt ~cli validity ["k";"kind"] "KIND" (main_kinds @ aliases_kinds)
    (Printf.sprintf "Specify the kind of the repository to be used (%s)."
       (string_of_enum main_kinds))

let jobs_flag cli validity =
  mk_opt ~cli validity ["j";"jobs"] "JOBS"
    "Set the maximal number of concurrent jobs to use. The default value is \
    calculated from the number of cores. You can also set it using the \
    $(b,\\$OPAMJOBS) environment variable."
    Arg.(some positive_integer) None

let formula_flag cli =
  mk_opt ~cli (cli_from ~experimental:true cli2_2) ["formula"] "FORMULA"
    "Specify a dependency formula to be verified by the solution, in addition \
     to the switch invariant. The format is the same as for expressing \
     dependencies in package definition files, e.g. '\"foo\" {>= \"1.1\"}'"
    dep_formula OpamFormula.Empty

let name_list =
  arg_list "PACKAGES" "List of package names." package_name

let atom_list =
  arg_list "PACKAGES"
    "List of package names, with an optional version or constraint, \
     e.g `pkg', `pkg.1.0' or `pkg>=0.5'."
    atom

let atom_or_local_list =
  arg_list "PACKAGES"
    (Printf.sprintf
      "List of package names, with an optional version or constraint, e.g `pkg', \
       `pkg.1.0' or `pkg>=0.5' ; or files or directory names containing package \
       description, with explicit directory (e.g. `.%sfoo.opam' or `.')" dir_sep)
    atom_or_local

let atom_or_dir_list =
  arg_list "PACKAGES"
    (Printf.sprintf
      "List of package names, with an optional version or constraint, e.g `pkg', \
       `pkg.1.0' or `pkg>=0.5' ; or directory names containing package \
       description, with explicit directory (e.g. `.%ssrcdir' or `.')" dir_sep)
    atom_or_dir

let nonempty_atom_list =
  nonempty_arg_list "PACKAGES"
    "List of package names, with an optional version or constraint, \
     e.g `pkg', `pkg.1.0' or `pkg>=0.5'."
    atom

let param_list =
  arg_list "PARAMS" "List of parameters." Arg.string

(* Options common to all commands *)
let global_options cli =
  let section = global_option_section in
  let git_version =
    mk_flag ~cli cli_original ~section ["git-version"]
      "Print the git version of opam, if set (i.e. you are using a development \
       version), and exit."
  in
  let debug =
    mk_flag ~cli cli_original ~section ["debug"]
      "Print debug message to stderr. \
       This is equivalent to setting $(b,\\$OPAMDEBUG) to \"true\"." in
  let debug_level =
    mk_opt ~cli cli_original ~section ["debug-level"] "LEVEL"
      "Like $(b,--debug), but allows specifying the debug level ($(b,--debug) \
       sets it to 1). Equivalent to setting $(b,\\$OPAMDEBUG) to a positive \
       integer."
      Arg.(some int) None in
  let verbose =
    Arg.(value & flag_all & info ~docs:section ["v";"verbose"] ~doc:
           "Be more verbose. One $(b,-v) shows all package commands, repeat to \
            also display commands called internally (e.g. $(i,tar), $(i,curl), \
            $(i,patch) etc.) Repeating $(i,n) times is equivalent to setting \
            $(b,\\$OPAMVERBOSE) to \"$(i,n)\".") in
  let quiet =
    mk_flag ~cli cli_original ~section ["q";"quiet"] "Disables $(b,--verbose)." in
  let color =
    mk_enum_opt ~cli cli_original ~section ["color"] "WHEN" when_enum
      (Printf.sprintf "Colorize the output. $(docv) must be %s."
         (string_of_enum when_enum)) in
  (* The --cli option is pre-processed, because it has to be able to appear
     before sub-commands. The one here is present only for --help. *)
  let cli_arg =
    mk_opt ~cli cli_original ~section ["cli"] "MAJOR.MINOR"
      "Use the command-line interface syntax and semantics of $(docv). \
       Intended for any persistent use of opam (scripts, blog posts, etc.), \
       any version of opam in the same MAJOR series will behave as for the \
       specified MINOR release. The flag was not available in opam 2.0, so to \
       select the 2.0 CLI, set the $(b,OPAMCLI) environment variable to \
       $(i,2.0) instead of using this parameter."
      Arg.string (OpamCLIVersion.to_string OpamCLIVersion.current) in
  let switch =
    mk_opt ~cli cli_original ~section ["switch"]
      "SWITCH" "Use $(docv) as the current compiler switch. \
                This is equivalent to setting $(b,\\$OPAMSWITCH) to $(i,SWITCH)."
      Arg.(some string) None in
  let yes =
    mk_vflag_all ~cli [
      cli_original, true, ["y";"yes"],
      "Answer yes to all opam yes/no questions without prompting. \
       See also $(b,--confirm-level). \
       This is equivalent to setting $(b,\\$OPAMYES) to \"true\".";
      cli_from cli2_1, false, ["no"],
      "Answer no to all opam yes/no questions without prompting. \
       See also $(b,--confirm-level). \
       This is equivalent to setting $(b,\\$OPAMNO) to \"true\".";
    ]
  in
  let confirm_level =
    mk_enum_opt_all ~cli (cli_from cli2_1) ~section ["confirm-level"] "LEVEL"
      confirm_enum
      (Printf.sprintf
         "Confirmation level, $(docv) must be %s. Can be specified more than \
          once. If $(b,--yes) or $(b,--no) are also given, the value of the \
          last $(b,--confirm-level) is taken into account. This is equivalent \
          to setting $(b, \\$OPAMCONFIRMLEVEL)`."
         (string_of_enum confirm_enum))
  in
  let strict =
    mk_flag ~cli cli_original ~section ["strict"]
      "Fail whenever an error is found in a package definition \
       or a configuration file. The default is to continue silently \
       if possible." in
  let root =
    mk_opt ~cli cli_original ~section ["root"]
      "ROOT" "Use $(docv) as the current root path. \
              This is equivalent to setting $(b,\\$OPAMROOT) to $(i,ROOT)."
      Arg.(some dirname) None in
  let d_no_aspcud =
    mk_flag ~cli (cli_between cli2_0 cli2_1) ~section ["no-aspcud"] "Deprecated"
  in
  let use_internal_solver =
    mk_flag ~cli cli_original ~section ["use-internal-solver"]
      "Disable any external solver, and use the built-in one (this requires \
       that opam has been compiled with a built-in solver). This is equivalent \
       to setting $(b,\\$OPAMNOASPCUD) or $(b,\\$OPAMUSEINTERNALSOLVER)." in
  let external_solver =
    mk_opt ~cli cli_original ~section ["solver"] "CMD"
      (Printf.sprintf
         "Specify the CUDF solver to use for resolving package installation \
          problems. This is either a predefined solver (this version of opam \
          supports %s), or a custom command that should contain the variables \
          %%{input}%%, %%{output}%%, %%{criteria}%%, and optionally \
          %%{timeout}%%. This is equivalent to setting $(b,\\$OPAMEXTERNALSOLVER)."
         (OpamStd.List.concat_map ", "
            (fun (module S : OpamCudfSolver.S) -> S.name)
            (OpamCudfSolver.default_solver_selection)))
      Arg.(some string) None in
  let solver_preferences =
    mk_opt ~cli cli_original ~section ["criteria"] "CRITERIA"
      ("Specify user $(i,preferences) for dependency solving for this run. \
        Overrides both $(b,\\$OPAMCRITERIA) and $(b,\\$OPAMUPGRADECRITERIA). \
        For details on the supported language, and the external solvers available, see \
        $(i, http://opam.ocaml.org/doc/External_solvers.html). \
        A general guide to using solver preferences can be found at \
        $(i,  http://www.dicosmo.org/Articles/usercriteria.pdf).")
      Arg.(some string) None in
  let cudf_file =
    mk_opt ~cli cli_original ~section ["cudf"] "FILENAME"
      "Debug option: Save the CUDF requests sent to the solver to \
       $(docv)-<n>.cudf."
      Arg.(some string) None in
  let best_effort =
    mk_flag ~cli cli_original ~section ["best-effort"]
      "Don't fail if all requested packages can't be installed: try to install \
       as many as possible. Note that not all external solvers may support \
       this option (recent versions of $(i,aspcud) or $(i,mccs) should). This \
       is equivalent to setting $(b,\\$OPAMBESTEFFORT) environment variable."
  in
  let safe_mode =
    mk_flag ~cli cli_original ~section ["readonly"; "safe"]
      "Make sure nothing will be automatically updated or rewritten. Useful \
       for calling from completion scripts, for example. Will fail whenever \
       such an operation is needed ; also avoids waiting for locks, skips \
       interactive questions and overrides the $(b,\\$OPAMDEBUG) variable. \
       This is equivalent to set environment variable $(b,\\$OPAMSAFE)."
  in
  let json_flag =
    mk_opt ~cli cli_original ~section ["json"] "FILENAME"
      "Save the results of the opam run in a computer-readable file. If the \
       filename contains the character `%', it will be replaced by an index \
       that doesn't overwrite an existing file. Similar to setting the \
       $(b,\\$OPAMJSON) variable."
      Arg.(some string) None
  in
  let no_auto_upgrade =
    mk_flag ~cli cli_original ~section ["no-auto-upgrade"]
      "When configuring or updating a repository that is written for an \
       earlier opam version (1.2), opam internally converts it to the current \
       format. This disables this behaviour. Note that repositories should \
       define their format version in a 'repo' file at their root, or they \
       will be assumed to be in the older format. It is, in any case, \
       preferable to upgrade the repositories manually using $(i,opam admin \
       upgrade [--mirror URL]) when possible."
  in
  let working_dir =
    mk_flag ~cli cli_original ~section ["working-dir"; "w"]
      "Whenever updating packages that are bound to a local, \
       version-controlled directory, update to the current working state of \
       their source instead of the last committed state, or the ref they are \
       pointing to. As source directory is copied as it is, if it isn't clean \
       it may result on a opam build failure.\
       This only affects packages explicitly listed on the command-line.\
       It can also be set with $(b,\\$OPAMWORKINGDIR). "
  in
  let ignore_pin_depends =
    mk_flag ~cli cli_original ~section ["ignore-pin-depends"]
      "Ignore extra pins required by packages that get pinned, either manually \
       through $(i,opam pin) or through $(i,opam install DIR). This is \
       equivalent to setting $(b,IGNOREPINDEPENDS=true)."
  in
  Term.(const create_global_options
        $git_version $debug $debug_level $verbose $quiet $color $switch
        $yes $confirm_level
        $strict $root $external_solver
        $use_internal_solver $cudf_file $solver_preferences $best_effort
        $safe_mode $json_flag $no_auto_upgrade $working_dir
        $ignore_pin_depends
        $d_no_aspcud $cli_arg)

(* lock options *)
let locked ?(section=Manpage.s_options) cli =
  mk_flag ~cli cli_original ~section ["locked"]
    "In commands that use opam files found from pinned sources, if a variant \
     of the file with an added .$(i,locked) extension is found (e.g. \
     $(b,foo.opam.locked) besides $(b,foo.opam)), that will be used instead. \
     This is typically useful to offer a more specific set of dependencies \
     and reproduce similar build contexts, hence the name. The $(i, lock)\
     option can be used to generate such files, based on the versions \
     of the dependencies currently installed on the host. This is equivalent \
     to setting the $(b,\\$OPAMLOCKED) environment variable. Note that this \
     option doesn't generally affect already pinned packages."
let lock_suffix ?(section=Manpage.s_options) cli =
  mk_opt ~cli (cli_from cli2_1) ~section ["lock-suffix"] "SUFFIX"
    "Set locked files suffix to $(i,SUFFIX)."
    Arg.(string) ("locked")

(* Options common to all build commands *)
let build_option_section = "PACKAGE BUILD OPTIONS"
let man_build_option_section =
  [
    `S build_option_section;
  ]
let build_options cli =
  let section = build_option_section in
  let keep_build_dir =
    mk_flag ~cli cli_original ~section ["b";"keep-build-dir"]
      "Keep the build directories after compiling packages. \
       This is equivalent to setting $(b,\\$OPAMKEEPBUILDDIR) to \"true\"." in
  let reuse_build_dir =
    mk_flag ~cli cli_original ~section ["reuse-build-dir"]
      "Reuse existing build directories (kept by using $(b,--keep-build-dir)), \
       instead of compiling from a fresh clone of the source. This can be \
       faster, but also lead to failures if the build systems of the packages \
       don't handle upgrades of dependencies well. This is equivalent to \
       setting $(b,\\$OPAMREUSEBUILDDIR) to \"true\"."
  in
  let inplace_build =
    mk_flag ~cli cli_original ~section ["inplace-build"]
      "When compiling a package which has its source bound to a local \
       directory, process the build and install actions directly in that \
       directory, rather than in a clean copy handled by opam. This only \
       affects packages that are explicitly listed on the command-line. \
       This is equivalent to setting $(b,\\$OPAMINPLACEBUILD) to \"true\"."
  in
  let no_checksums =
    mk_flag ~cli cli_original ~section ["no-checksums"]
      "Do not verify the checksum of downloaded archives.\
       This is equivalent to setting $(b,\\$OPAMNOCHECKSUMS) to \"true\"." in
  let req_checksums =
    mk_flag ~cli cli_original ~section ["require-checksums"]
      "Reject the installation of packages that don't provide a checksum for the upstream archives. \
       This is equivalent to setting $(b,\\$OPAMREQUIRECHECKSUMS) to \"true\"." in
  let build_test =
    mk_flag_replaced ~cli ~section [
      cli_between cli2_0 cli2_1 ~replaced:"--with-test", ["build-test"];
      cli_original, ["t";"with-test"];
    ] "Build and $(b,run) the package unit-tests. This only affects packages \
       listed on the command-line. This is equivalent to setting \
       $(b,\\$OPAMWITHTEST) (or the deprecated $(b,\\$OPAMBUILDTEST)) to \
       \"true\"."
  in
  let build_doc =
    mk_flag_replaced ~cli ~section [
      cli_between cli2_0 cli2_1 ~replaced:"--with-doc", ["build-doc"];
      cli_original, ["d";"with-doc"];
    ] "Build the package documentation. This only affects packages listed on \
       the command-line. This is equivalent to setting $(b,\\$OPAMWITHDOC) \
       (or the deprecated $(b,\\$OPAMBUILDDOC)) to \"true\"."
  in
  let with_tools =
    mk_flag ~cli (cli_from cli2_2) ["with-tools"] ~section
      "Include development tools only dependencies."
  in
  let make =
    mk_opt ~cli (cli_between cli2_0 cli2_1
                   ~replaced:"opam config set[-global] make MAKE")
      ~section ["m";"make"] "MAKE"
      "Use $(docv) as the default 'make' command. Has no effect if the \
       $(i,make) variable is defined."
      Arg.(some string) None
  in
  let show =
    mk_flag ~cli cli_original ~section ["show-actions"]
      "Call the solver and display the actions. Don't perform any changes. \
      This is equivalent to setting $(b,\\$OPAMSHOW)." in
  let dryrun =
    mk_flag ~cli cli_original ~section ["dry-run"]
      "Simulate the command, but don't actually perform any changes. This also \
       can be set with environment variable $(b,\\$OPAMDEBUG)." in
  let skip_update =
    mk_flag ~cli cli_original ~section ["skip-updates"]
      "When running an install, upgrade or reinstall on source-pinned \
       packages, they are normally updated from their origin first. This flag \
       disables that behaviour and will keep them to their version in cache. \
       This is equivalent to setting $(b,\\$OPAMSKIPUPDATE)."
  in
  let fake =
    mk_flag ~cli cli_original ~section ["fake"]
      "This option registers the actions into the opam database, without \
       actually performing them. \
       WARNING: This option is dangerous and likely to break your opam \
       environment. You probably want $(b,--dry-run). You've been $(i,warned)."
  in
  let ignore_constraints_on =
    mk_opt ~cli cli_original ~section ["ignore-constraints-on"] "PACKAGES"
      "Forces opam to ignore version constraints on all dependencies to the \
       listed packages. This can be used to test compatibility, but expect \
       builds to break when using this. Note that version constraints on \
       optional dependencies and conflicts are unaffected. This is equivalent \
       to setting $(b,\\$OPAMIGNORECONSTRAINTS)."
      Arg.(some (list package_name)) None ~vopt:(Some [])
  in
  let unlock_base =
    mk_flag_replaced ~cli ~section [
      cli_between cli2_0 cli2_1 ~replaced:"--update-invariant", ["unlock-base"];
      cli_from cli2_1, ["update-invariant"]
    ] "Allow changes to the packages set as switch base (typically, the main \
       compiler). Use with caution. This is equivalent to setting the \
       $(b,\\$OPAMUNLOCKBASE) environment variable"
  in
  let locked = locked cli ~section in
  let lock_suffix = lock_suffix cli ~section in
  let assume_depexts =
    mk_flag ~cli (cli_from cli2_1) ~section ["assume-depexts"]
      "Skip the installation step for any missing system packages, and attempt \
       to proceed with compilation of the opam packages anyway. If the \
       installation is successful, opam won't prompt again about these system \
       packages. Only meaningful if external dependency handling is enabled."
  in
  let no_depexts =
    mk_flag ~cli (cli_from cli2_1) ~section ["no-depexts"]
      "Temporarily disables handling of external dependencies. This can be \
       used if a package is not available on your system package manager, but \
       you installed the required dependency by hand. Implies \
       $(b,--assume-depexts), and stores the exceptions upon success as well."
  in
  Term.(const create_build_options
        $keep_build_dir $reuse_build_dir $inplace_build $make
        $no_checksums $req_checksums $build_test $build_doc $with_tools $show
        $dryrun $skip_update $fake $jobs_flag cli cli_original
        $ignore_constraints_on $unlock_base $locked $lock_suffix
        $assume_depexts $no_depexts)

(* Option common to install commands *)
let assume_built cli =
  mk_flag ~cli cli_original  ["assume-built"]
    "For use on locally-pinned packages: assume they have already \
     been correctly built, and only run their installation \
     instructions, directly from their source directory. This \
     skips the build instructions and can be useful to install \
     packages that are being worked on. Implies $(i,--inplace-build). \
     No locally-pinned packages will be skipped."

(* Options common to all path based/related commands, e.g. (un)pin, upgrade,
   remove, (re)install.
   Disabled *)
(* let recurse _cli = Term.const false *)

let recurse cli =
  mk_flag ~cli (cli_from cli2_2) ["recursive"]
    "Allow recursive lookups of (b,*.opam) files. Cf. $(i,--subpath) also."

(* let subpath _cli = Term.const None *)
let subpath cli =
  mk_opt ~cli (cli_from cli2_2) ["subpath"] "PATH"
    "$(b,*.opam) files are retrieved from the given sub directory instead of \
      top directory. Sources are then taken from the targeted sub directory, \
      internally only this subdirectory is copied/fetched.  It can be combined \
      with $(i,--recursive) to have a recursive lookup on the subpath."
    Arg.(some subpath_conv) None

let package_selection_section = "PACKAGE SELECTION OPTIONS"

let package_selection cli =
  let section = package_selection_section in
  let depends_on =
    mk_opt_all ~cli cli_original ["depends-on"] "PACKAGES" ~section
      "List only packages that depend on one of (comma-separated) $(b,PACKAGES)."
      Arg.(list atom)
  in
  let required_by =
    mk_opt_all ~cli cli_original ["required-by"] "PACKAGES" ~section
      "List only the dependencies of (comma-separated) $(b,PACKAGES)."
      Arg.(list atom)
  in
  let conflicts_with =
    mk_opt_all ~cli cli_original ["conflicts-with"] "PACKAGES" ~section
      "List packages that have declared conflicts with at least one of the \
       given list. This includes conflicts defined from the packages in the \
       list, from the other package, or by a common $(b,conflict-class:) \
       field."
      Arg.(list package_with_version)
  in
  let coinstallable_with =
    mk_opt_all ~cli cli_original ["coinstallable-with"] "PACKAGES" ~section
      "Only list packages that are compatible with all of $(b,PACKAGES)."
      Arg.(list package_with_version)
  in
  let resolve =
    mk_opt_all ~cli cli_original ["resolve"] "PACKAGES" ~section
      "Restrict to a solution to install (comma-separated) $(docv), $(i,i.e.) \
       a consistent set of packages including those. This is subtly different \
       from `--required-by --recursive`, which is more predictable and can't \
       fail, but lists all dependencies independently without ensuring \
       consistency. \
       Without `--installed`, the answer is self-contained and independent of \
       the current installation. With `--installed', it's computed from the \
       set of currently installed packages. \
       `--no-switch` further makes the solution independent from the \
       currently pinned packages, architecture, and compiler version. \
       The combination with `--depopts' is not supported."
      Arg.(list atom)
  in
  let recursive =
    mk_flag ~cli cli_original ["recursive"] ~section
      "With `--depends-on' and `--required-by', display all transitive \
       dependencies rather than just direct dependencies." in
  let depopts =
    mk_flag ~cli cli_original ["depopts"]  ~section
      "Include optional dependencies in dependency requests."
  in
  let nobuild =
    mk_flag ~cli cli_original ["nobuild"]  ~section
      "Exclude build dependencies (they are included by default)."
  in
  let post =
    mk_flag ~cli cli_original ["post"]  ~section
      "Include dependencies tagged as $(i,post)."
  in
  let dev =
    mk_flag ~cli cli_original ["dev"]  ~section
      "Include development packages in dependencies."
  in
  let doc_flag =
    mk_flag ~cli cli_original ["doc";"with-doc"] ~section
      "Include doc-only dependencies."
  in
  let test =
    mk_flag ~cli cli_original ["t";"test";"with-test"] ~section
      "Include test-only dependencies."
  in
  let tools =
    mk_flag ~cli (cli_from cli2_2) ["with-tools"] ~section
      "Include development only dependencies."
  in
  let field_match =
    mk_opt_all ~cli cli_original ["field-match"] "FIELD:PATTERN" ~section
      "Filter packages with a match for $(i,PATTERN) on the given $(i,FIELD)"
      Arg.(pair ~sep:':' string string)
  in
  let has_flag =
    mk_opt_all ~cli cli_original ["has-flag"] "FLAG" ~section
      ("Only include packages which have the given flag set. \
        Package flags are one of: "^
       (OpamStd.List.concat_map " "
          (Printf.sprintf "$(b,%s)" @* string_of_pkg_flag)
          all_package_flags))
      ((fun s -> match pkg_flag_of_string s with
          | Pkgflag_Unknown s ->
            `Error ("Invalid package flag "^s^", must be one of "^
                    OpamStd.List.concat_map " " string_of_pkg_flag
                      all_package_flags)
          | f -> `Ok f),
       fun fmt flag ->
         Format.pp_print_string fmt (string_of_pkg_flag flag))
  in
  let has_tag =
    mk_opt_all ~cli cli_original ["has-tag"] "TAG" ~section
      "Only includes packages which have the given tag set"
      Arg.string
  in
  let filter
      depends_on required_by conflicts_with coinstallable_with resolve recursive
      depopts nobuild post dev doc_flag test tools field_match has_flag has_tag
    =
    let dependency_toggles = {
      OpamListCommand.
      recursive; depopts; build = not nobuild; post; test; tools;
      doc = doc_flag; dev
    } in
    List.map (fun flag -> OpamListCommand.Flag flag) has_flag @
    List.map (fun tag -> OpamListCommand.Tag tag) has_tag @
    List.map (fun (field,patt) ->
        OpamListCommand.Pattern
          ({OpamListCommand.default_pattern_selector with
            OpamListCommand.fields = [field]},
           patt))
      field_match @
    List.map (fun deps ->
        OpamListCommand.Depends_on (dependency_toggles, deps))
      depends_on @
    List.map (fun rdeps ->
        OpamListCommand.Required_by (dependency_toggles, rdeps))
      required_by @
    List.map (fun pkgs ->
        OpamListCommand.Conflicts_with pkgs)
      conflicts_with @
    List.map (fun deps ->
        OpamListCommand.Solution (dependency_toggles, deps))
      resolve @
    List.map (fun pkgs ->
        OpamListCommand.Coinstallable_with (dependency_toggles, pkgs))
      coinstallable_with
  in
  Term.(const filter $
        depends_on $ required_by $ conflicts_with $ coinstallable_with $
        resolve $ recursive $ depopts $ nobuild $ post $ dev $ doc_flag $
        test $ tools $ field_match $ has_flag $ has_tag)

let package_listing_section = "OUTPUT FORMAT OPTIONS"

let package_listing cli =
  let section = package_listing_section in
  let all_versions = mk_flag ~cli cli_original ["all-versions";"V"]
      ~section
      "Normally, when multiple versions of a package match, only one is shown \
       in the output (the installed one, the pinned-to one, or, failing that, \
       the highest one available or the highest one). This flag disables this \
       behaviour and shows all matching versions. This also changes the \
       default display format to include package versions instead of just \
       package names (including when --short is set). This is automatically \
       turned on when a single non-pattern package name is provided on the \
       command-line."
  in
  let print_short =
    mk_flag ~cli cli_original ["short";"s"] ~section
      "Don't print a header, and sets the default columns to $(b,name) only. \
       If you need package versions included, use $(b,--columns=package) \
       instead"
  in
  let sort =
    mk_flag ~cli cli_original ["sort";"S"] ~section
      "Sort the packages in dependency order (i.e. an order in which they \
       could be individually installed.)"
  in
  let columns =
    mk_opt ~cli cli_original ["columns"] "COLUMNS" ~section
      (Printf.sprintf "Select the columns to display among: %s.\n\
                       The default is $(b,name) when $(i,--short) is present \
                       and %s otherwise."
         (OpamStd.List.concat_map ", " (fun (_,f) -> Printf.sprintf "$(b,%s)" f)
            OpamListCommand.raw_field_names)
         (OpamStd.List.concat_map ", "
            (fun f -> Printf.sprintf "$(b,%s)" (OpamListCommand.string_of_field f))
            OpamListCommand.default_list_format))
      Arg.(some & opamlist_columns) None
  in
  let normalise = mk_flag ~cli cli_original ["normalise"] ~section
      "Print the values of opam fields normalised"
  in
  let wrap = mk_flag ~cli cli_original ["wrap"] ~section
      "Wrap long lines, the default being to truncate when displaying on a \
       terminal, or to keep as is otherwise"
  in
  let separator =
    mk_opt ~cli cli_original ["separator"] "STRING" ~section
      "Set the column-separator string"
      Arg.string " "
  in
  let format all_versions short sort columns normalise wrap separator =
  fun ~force_all_versions ->
    let all_versions = force_all_versions || all_versions in
    let columns =
      match columns with
      | Some c -> c
      | None ->
        let cols =
          if short then [OpamListCommand.Name]
          else OpamListCommand.default_list_format
        in
        if all_versions then
          List.map (function
              | OpamListCommand.Name -> OpamListCommand.Package
              | c -> c)
            cols
        else cols
    in
    { OpamListCommand.
      short;
      header = not short;
      columns;
      all_versions;
      wrap = if wrap then Some (`Wrap "\\ ") else Some `Truncate;
      separator;
      value_printer = if normalise then `Normalised else `Normal;
      order = if sort then `Dependency else `Standard;
    }
  in
  Term.(const format $ all_versions $ print_short $ sort $ columns $ normalise $
        wrap $ separator)
