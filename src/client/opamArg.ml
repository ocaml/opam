(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase
open Cmdliner
open OpamStd.Op

(* Global options *)
type global_options = {
  debug_level: int option;
  verbose: int;
  quiet : bool;
  color : [ `Always | `Never | `Auto ] option;
  opt_switch : string option;
  yes : bool;
  strict : bool;
  opt_root : dirname option;
  no_base_packages: bool;
  git_version : bool;
  external_solver : string option;
  use_internal_solver : bool;
  cudf_file : string option;
  solver_preferences : string option;
  safe_mode : bool;
  json : string option;
}

let create_global_options
    git_version debug debug_level verbose quiet color opt_switch yes strict
    opt_root no_base_packages external_solver use_internal_solver
    cudf_file solver_preferences safe_mode json =
  let debug_level = OpamStd.Option.Op.(
      debug_level >>+ fun () -> if debug then Some 1 else None
    ) in
  let verbose = List.length verbose in
  { git_version; debug_level; verbose; quiet; color; opt_switch; yes;
    strict; opt_root; no_base_packages; external_solver; use_internal_solver;
    cudf_file; solver_preferences; safe_mode; json; }

let apply_global_options o =
  if o.git_version then (
    begin match OpamGitVersion.version with
      | None   -> ()
      | Some v -> OpamConsole.msg "%s\n%!" v
    end;
    exit 0
  );
  let open OpamStd.Option.Op in
  let flag f = if f then Some true else None in
  let some x = match x with None -> None | some -> Some some in
  let external_solver =
    if o.use_internal_solver then Some (lazy None) else
      o.external_solver >>| fun s ->
      lazy (
        let args = OpamStd.String.split s ' ' in
        Some (List.map (fun a -> OpamTypes.CString a, None) args)
      )
  in
  let solver_prefs = o.solver_preferences >>| fun p -> lazy p in
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
    ?answer:(some (flag o.yes))
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
    (* ?solver_timeout:float *)
    ?external_solver
    ?solver_preferences_default:(some solver_prefs)
    ?solver_preferences_upgrade:(some solver_prefs)
    ?solver_preferences_fixup:(some solver_prefs)
    (* - state options - *)
    ?root_dir:o.opt_root
    ?current_switch:(o.opt_switch >>| OpamSwitch.of_string)
    ?switch_from:(o.opt_switch >>| fun _ -> `Command_line)
    (* ?jobs: int *)
    (* ?dl_jobs: int *)
    (* ?external_tags:string list *)
    (* ?keep_build_dir:bool *)
    ?no_base_packages:(flag o.no_base_packages)
    (* ?build_test:bool *)
    (* ?build_doc:bool *)
    (* ?show:bool *)
    (* ?dryrun:bool *)
    (* ?fake:bool *)
    (* ?makecmd:string Lazy.t *)
    ?json_out:OpamStd.Option.Op.(o.json >>| function "" -> None | s -> Some s)
    (* - client options - *)
    (* ?print_stats:bool *)
    (* ?sync_archives:bool *)
    (* ?pin_kind_auto:bool *)
    (* ?autoremove:bool *)
    (* ?editor:string *)
    ();
  if OpamStateConfig.(!r.json_out <> None) then (
    OpamJson.append "opam-version" (`String OpamVersion.(to_string (full ())));
    OpamJson.append "command-line"
      (`A (List.map (fun s -> `String s) (Array.to_list Sys.argv)))
  )

(* Build options *)
type build_options = {
  keep_build_dir: bool;
  make          : string option;
  no_checksums  : bool;
  req_checksums : bool;
  build_test    : bool;
  build_doc     : bool;
  show          : bool;
  dryrun        : bool;
  fake          : bool;
  external_tags : string list;
  jobs          : int option;
}

let create_build_options
    keep_build_dir make no_checksums req_checksums build_test
    build_doc show dryrun external_tags fake
    jobs = {
  keep_build_dir; make; no_checksums; req_checksums;
  build_test; build_doc; show; dryrun; external_tags;
  fake; jobs;
}

let apply_build_options b =
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
    ?jobs:OpamStd.Option.Op.(b.jobs >>| fun j -> lazy j)
    (* ?dl_jobs:int *)
    ?external_tags:(match b.external_tags with [] -> None | l -> Some l)
    ?keep_build_dir:(flag b.keep_build_dir)
    (* ?no_base_packages:(flag o.no_base_packages) -- handled globally *)
    ?build_test:(flag b.build_test)
    ?build_doc:(flag b.build_doc)
    ?show:(flag b.show)
    ?dryrun:(flag b.dryrun)
    ?fake:(flag b.fake)
    ?makecmd:OpamStd.Option.Op.(b.make >>| fun m -> lazy m)
    ()

let when_enum = [ "always", `Always; "never", `Never; "auto", `Auto ]

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "ENVIRONMENT VARIABLES";
  `P "OPAM makes use of the environment variables listed here. Boolean \
      variables should be set to \"0\", \"no\", \"false\" or the empty  string \
      to disable, \"1\", \"yes\" or \"true\" to enable.";

  (* Alphabetical order *)
  `P "$(i,OPAMCOLOR), when set to $(i,always) or $(i,never), sets a default \
      value for the --color option.";
  `P ("$(i,OPAMCRITERIA) specifies user $(i,preferences) for dependency \
       solving. The default value depends on the solver version, use `config \
       report` to know the current setting. See also option --criteria");
  `P "$(i,OPAMCURL) can be used to select a given 'curl' program. See \
      $(i,OPAMFETCH) for more options.";
  `P "$(i,OPAMDEBUG) see options `--debug' and `--debug-level'.";
  `P "$(i,OPAMDOWNLOADJOBS) sets the maximum number of simultaneous downloads.";
  `P "$(i,OPAMERRLOGLEN) sets the number of log lines printed when a \
      sub-process fails. 0 to print all.";
  `P "$(i,OPAMEXTERNALSOLVER) see option `--solver'.";
  `P "$(i,OPAMFETCH) specifies how to download files: either `wget', `curl' or \
      a custom command where variables $(b,%{url}%), $(b,%{out}%), \
      $(b,%{retry}%), $(b,%{compress}%) and $(b,%{checksum}%) will \
      be replaced. Overrides the \
      'download-command' value from the main config file.";
  `P "$(i,OPAMJOBS) sets the maximum number of parallel workers to run.";
  `P "$(i,OPAMJSON) log json output to the given file (use character `%' to \
      index the files)";
  `P "$(i,OPAMLOCKRETRIES) sets the number of tries after which OPAM gives up \
      acquiring its lock and fails. <= 0 means infinite wait.";
  `P "$(i,OPAMNO) answer no to any question asked.";
  `P "$(i,OPAMNOASPCUD) see option `--no-aspcud'.";
  `P "$(i,OPAMNOSELFUPGRADE) see option `--no-self-upgrade'.";
  `P "$(i,OPAMPINKINDAUTO) sets whether version control systems should be \
      detected when pinning to a local path. Enabled by default since 1.3.0.";
  `P "$(i,OPAMREQUIRECHECKSUMS) see option `--require-checksums'.";
  `P "$(i,OPAMRETRY) sets the number of tries before failing downloads.";
  `P "$(i,OPAMROOT) see option `--root'. This is automatically set by \
      `opam config env --root=DIR' when DIR is non-default or OPAMROOT is \
      already defined.";
  `P "$(i,OPAMSAFE) see option `--safe'";
  `P "$(i,OPAMSKIPVERSIONCHECKS) bypasses some version checks. Unsafe, for \
      compatibility testing only.";
  `P "$(i,OPAMSOLVERTIMEOUT) change the time allowance of the internal solver.";
  `P ("$(i,OPAMSTATUSLINE) display a dynamic status line showing what's \
       currently going on on the terminal. \
       (one of "^Arg.doc_alts_enum when_enum^")");
  `P "$(i,OPAMSWITCH) see option `--switch'. Automatically set by \
      `opam config env --switch=SWITCH'.";
  `P ("$(i,OPAMUPGRADECRITERIA) specifies user $(i,preferences) for dependency \
       solving when performing an upgrade. Overrides $(i,OPAMCRITERIA) in \
       upgrades if both are set. See also option --criteria");
  `P "$(i,OPAMUSEINTERNALSOLVER) see option `--use-internal-solver'.";
  `P ("$(i,OPAMUTF8) use UTF8 characters in output \
       (one of "^Arg.doc_alts_enum when_enum^
      "). By default `auto', which is determined from the locale).");
  `P "$(i,OPAMUTF8MSGS) use extended UTF8 characters (camels) in OPAM \
      messages. Implies $(i,OPAMUTF8). This is set by default on OSX only.";
  `P "$(i,OPAMVAR_var) overrides the contents of the variable $(i,var)  when \
      substituting `%{var}%` strings in `opam` files.";
  `P "$(i,OPAMVAR_package_var) overrides the contents of the variable \
      $(i,package:var) when substituting `%{package:var}%` strings in \
      `opam` files.";
  `P "$(i,OPAMVERBOSE) see option `--verbose'.";
  `P "$(i,OPAMYES) see option `--yes'.";

  `S "FURTHER DOCUMENTATION";
  `P (Printf.sprintf "See https://opam.ocaml.org.");

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>"; `Noblank;
  `P "Anil Madhavapeddy   <anil@recoil.org>"; `Noblank;
  `P "Fabrice Le Fessant  <Fabrice.Le_fessant@inria.fr>"; `Noblank;
  `P "Frederic Tuong      <tuong@users.gforge.inria.fr>"; `Noblank;
  `P "Louis Gesbert       <louis.gesbert@ocamlpro.com>"; `Noblank;
  `P "Vincent Bernardoff  <vb@luminar.eu.org>"; `Noblank;
  `P "Guillem Rieu        <guillem.rieu@ocamlpro.com>"; `Noblank;
  `P "Roberto Di Cosmo    <roberto@dicosmo.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/ocaml/opam/issues.";
]

(* Converters *)
let pr_str = Format.pp_print_string

let repository_name =
  let parse str = `Ok (OpamRepositoryName.of_string str) in
  let print ppf name = pr_str ppf (OpamRepositoryName.to_string name) in
  parse, print

let url =
  let parse str = `Ok (OpamUrl.parse str) in
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
  let print ppf dir = pr_str ppf (OpamFilename.prettify_dir dir) in
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

let package_name =
  let parse str =
    try `Ok (OpamPackage.Name.of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf pkg = pr_str ppf (OpamPackage.Name.to_string pkg) in
  parse, print

let positive_integer : int Arg.converter =
  let (parser, printer) = Arg.int in
  let parser s =
    match parser s with
    | `Error _ -> `Error "expected a positive integer"
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
        opt @@ seq [ char '.'; group @@ rep1 any ];
        eos;
      ]) in
    try
      let sub = Re.exec re str in
      let name = OpamPackage.Name.of_string (Re.get sub 1) in
      let version_opt =
        try Some (OpamPackage.Version.of_string (Re.get sub 2))
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

(* name * version constraint *)
let atom =
  let parse str =
    let re = Re.(compile @@ seq [
        bos;
        group @@ rep1 @@ diff any (set ">=<.!");
        group @@ alt [ seq [ alt [char '<'; char '>']; opt @@ char '=' ];
                       char '='; char '.'; str "!="; ];
        group @@ rep1 any;
        eos;
      ]) in
    try
      let sub = Re.exec re str in
      let sname = Re.get sub 1 in
      let sop = Re.get sub 2 in
      let sversion = Re.get sub 3 in
      let name = OpamPackage.Name.of_string sname in
      let sop = if sop = "." then "=" else sop in
      let op = OpamFormula.relop_of_string sop in (* may raise Invalid_argument *)
      let version = OpamPackage.Version.of_string sversion in
      `Ok (name, Some (op, version))
    with Not_found | Failure _ | Invalid_argument _ ->
      try `Ok (OpamPackage.Name.of_string str, None)
      with Failure msg -> `Error msg
  in
  let print ppf atom =
    pr_str ppf (OpamFormula.short_string_of_atom atom) in
  parse, print

type 'a default = [> `default of string] as 'a

let enum_with_default sl: 'a Arg.converter =
  let parse, print = Arg.enum sl in
  let parse s =
    match parse s with
    | `Ok _ as x -> x
    | _ -> `Ok (`default s) in
  parse, print

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

let mk_opt ?section ?vopt flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt ?vopt conv default & doc)

let mk_tristate_opt ?section flags value doc =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt (some (enum when_enum)) None & doc)

type 'a subcommand = string * 'a * string list * string

type 'a subcommands = 'a subcommand list

let mk_subdoc ?(defaults=[]) commands =
  let bold s = Printf.sprintf "$(b,%s)" s in
  let it s = Printf.sprintf "$(i,%s)" s in
  `S "COMMANDS" ::
  (List.map (function
       | "", name ->
         `P (Printf.sprintf "Without argument, defaults to %s."
               (bold name))
       | arg, default ->
         `I (it arg, Printf.sprintf "With a %s argument, defaults to %s %s."
               (it arg) (bold default) (it arg))
     ) defaults) @
  List.map (fun (c,_,args,d) ->
      let cmds = bold c ^ " " ^ OpamStd.List.concat_map " " it args in
      `I (cmds, d)
    ) commands @
  [`S "OPTIONS"] (* Ensures options get after commands *)

let mk_subcommands_aux my_enum commands =
  let command =
    let doc = Arg.info ~docv:"COMMAND" [] in
    let commands =
      List.fold_left
        (fun acc (c,f,_,_) -> (c,f) :: acc)
        [] commands in
    Arg.(value & pos 0 (some & my_enum commands) None & doc) in
  let params =
    let doc = Arg.info ~doc:"Optional parameters." [] in
    Arg.(value & pos_right 0 string [] & doc) in
  command, params

let mk_subcommands commands =
  mk_subcommands_aux Arg.enum commands

let mk_subcommands_with_default commands =
  mk_subcommands_aux enum_with_default commands

let bad_subcommand subcommands (command, usersubcommand, userparams) =
  match usersubcommand with
  | None ->
    `Error (false, Printf.sprintf "Missing subcommand. Valid subcommands are %s."
              (OpamStd.Format.pretty_list
                 (List.map (fun (a,_,_,_) -> a) subcommands)))
  | Some (`default cmd) ->
    `Error (true, Printf.sprintf "Invalid %s subcommand %S" command cmd)
  | Some usersubcommand ->
    let exe = Filename.basename Sys.executable_name in
    match List.find_all (fun (_,cmd,_,_) -> cmd = usersubcommand) subcommands with
    | [name, _, args, _doc] ->
      let usage =
        Printf.sprintf "%s %s [OPTION]... %s %s"
          exe command name (String.concat " " args) in
      if List.length userparams < List.length args then
        `Error (false, Printf.sprintf "%s: Missing argument.\nUsage: %s\n"
                  exe usage)
      else
        `Error (false, Printf.sprintf "%s: Too many arguments.\nUsage: %s\n"
                  exe usage)
    | _ ->
      `Error (true, Printf.sprintf "Invalid %s subcommand" command)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~docs:"COMMANDS" ~doc ~man title

let arg_list name doc conv =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(value & pos_all conv [] & doc)

let nonempty_arg_list name doc conv =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(non_empty & pos_all conv [] & doc)

(* Common flags *)
let print_short_flag =
  mk_flag ["s";"short"]
    "Output raw lists of names, one per line, skipping any details."

let installed_roots_flag =
  mk_flag ["installed-roots"] "Display only the installed roots."

let shell_opt =
  let enum = [
      "bash",`bash;
      "sh",`sh;
      "csh",`csh;
      "zsh",`zsh;
      "fish",`fish;
    ] in
  mk_opt ["shell"] "SHELL"
    (Printf.sprintf
       "Sets the configuration mode for OPAM environment appropriate for \
        $(docv). One of %s." (Arg.doc_alts_enum enum))
    (Arg.enum enum) (OpamStd.Sys.guess_shell_compat ())

let dot_profile_flag =
  mk_opt ["dot-profile"]
    "FILENAME" "Name of the configuration file to update instead of \
                $(i,~/.profile) or $(i,~/.zshrc) based on shell detection."
    (Arg.some filename) None

let repo_kind_flag =
  let main_kinds = [
    "http" , `http;
    "local", `rsync;
    "git"  , `git;
    "darcs", `darcs;
    "hg"   , `hg;
  ] in
  let aliases_kinds = [
    "wget" , `http;
    "curl" , `http;
    "rsync", `rsync;
  ] in
  mk_opt ["k";"kind"] "KIND"
    (Printf.sprintf "Specify the kind of the repository to be used (%s)."
       (Arg.doc_alts_enum main_kinds))
    Arg.(some (enum (main_kinds @ aliases_kinds))) None

let jobs_flag =
  mk_opt ["j";"jobs"] "JOBS"
    "Set the maximal number of concurrent jobs to use. You can also set it using \
     the $(b,\\$OPAMJOBS) environment variable."
    Arg.(some positive_integer) None

let pattern_list =
  arg_list "PATTERNS" "List of package patterns." Arg.string

let name_list =
  arg_list "PACKAGES" "List of package names." package_name

let atom_list =
  arg_list "PACKAGES"
    "List of package names, with an optional version or constraint, \
     e.g `pkg', `pkg.1.0' or `pkg>=0.5'."
    atom

let nonempty_atom_list =
  nonempty_arg_list "PACKAGES"
    "List of package names, with an optional version or constraint, \
     e.g `pkg', `pkg.1.0' or `pkg>=0.5'."
    atom

let param_list =
  arg_list "PARAMS" "List of parameters." Arg.string

(* Options common to all commands *)
let global_options =
  let section = global_option_section in
  let git_version =
    mk_flag ~section ["git-version"]
      "Print the git version if it exists and exit." in
  let debug =
    mk_flag ~section ["debug"]
      "Print debug message to stderr. \
       This is equivalent to setting $(b,\\$OPAMDEBUG) to \"true\"." in
  let debug_level =
    mk_opt ~section ["debug-level"] "LEVEL"
      "Like `--debug', but allows specifying the debug level (`--debug' \
       sets it to 1). Equivalent to setting $(b,\\$OPAMDEBUG) to a positive \
       integer."
      Arg.(some int) None in
  let verbose =
    Arg.(value & flag_all & info ~docs:section ["v";"verbose"] ~doc:
           "Be more verbose, show package sub-commands and their output. \
            Repeat to see more. Repeating $(i,n) times is equivalent to \
            setting $(b,\\$OPAMVERBOSE) to \"$(i,n)\".") in
  let quiet =
    mk_flag ~section ["q";"quiet"] "Be quiet when installing a new compiler." in
  let color =
    mk_tristate_opt ~section ["color"] "WHEN"
      (Printf.sprintf "Colorize the output. $(docv) must be %s."
         (Arg.doc_alts_enum when_enum)) in
  let switch =
    mk_opt ~section ["switch"]
      "SWITCH" "Use $(docv) as the current compiler switch. \
                This is equivalent to setting $(b,\\$OPAMSWITCH) to $(i,SWITCH)."
      Arg.(some string) None in
  let yes =
    mk_flag ~section ["y";"yes"]
      "Disable interactive mode and answer yes \
       to all questions that would otherwise be \
       asked to the user. \
       This is equivalent to setting $(b,\\$OPAMYES) to \"true\"." in
  let strict =
    mk_flag ~section ["strict"]
      "Fail whenever an error is found in a package definition \
       or a configuration file. The default is to continue silently \
       if possible." in
  let root =
    mk_opt ~section ["root"]
      "ROOT" "Use $(docv) as the current root path. \
              This is equivalent to setting $(b,\\$OPAMROOT) to $(i,ROOT)."
      Arg.(some dirname) None in
  let no_base_packages =
    mk_flag ~section ["no-base-packages"]
      "Do not install base packages (useful for testing purposes). \
       This is equivalent to setting $(b,\\$OPAMNOBASEPACKAGES) to a non-empty \
       string." in
  let use_internal_solver =
    mk_flag ~section ["no-aspcud"; "use-internal-solver"]
      "Force use of internal heuristics, even if an external solver is \
       available." in
  let external_solver =
    mk_opt ~section ["solver"] "CMD"
      ("Specify the name of the external dependency $(i,solver). \
        The default value is \"aspcud\". \
        Either 'aspcud', 'packup', 'mccs' or a custom command that should \
        contain the variables %{input}%, %{output}% and %{criteria}%")
      Arg.(some string) None in
  let solver_preferences =
    mk_opt ~section ["criteria"] "CRITERIA"
      ("Specify user $(i,preferences) for dependency solving for this run. \
        Overrides both $(b,\\$OPAMCRITERIA) and $(b,\\$OPAMUPGRADECRITERIA). \
        For details on the supported language, and the external solvers available, see \
        $(i,  http://opam.ocaml.org/doc/Specifying_Solver_Preferences.html). \
        A general guide to using solver preferences can be found at \
        $(i,  http://www.dicosmo.org/Articles/usercriteria.pdf).")
      Arg.(some string) None in
  let cudf_file =
    mk_opt ~section ["cudf"] "FILENAME"
      "Debug option: Save the CUDF requests sent to the solver to \
       $(docv)-<n>.cudf."
      Arg.(some string) None in
  let safe_mode =
    mk_flag ~section ["safe"]
      "Make sure nothing will be automatically updated or rewritten. Useful \
       for calling from completion scripts, for example. Will fail whenever \
       such an operation is needed ; also avoids waiting for locks, skips \
       interactive questions and overrides the $(b,\\$OPAMDEBUG) variable."
  in
  let json_flag =
    mk_opt ["json"] "FILENAME"
      "Save the results of the OPAM run in a computer-readable file. If the \
       filename contains the character `%', it will be replaced by an index \
       that doesn't overwrite an existing file. Similar to setting the \
       $(b,\\OPAMJSON) variable."
      Arg.(some string) None
  in
  Term.(pure create_global_options
        $git_version $debug $debug_level $verbose $quiet $color $switch $yes
        $strict $root $no_base_packages $external_solver
        $use_internal_solver $cudf_file $solver_preferences
        $safe_mode $json_flag)

(* Options common to all build commands *)
let build_options =
  let keep_build_dir =
    mk_flag ["b";"keep-build-dir"]
      "Keep the build directory. \
       This is equivalent to setting $(b,\\$OPAMKEEPBUILDDIR) to \"true\"." in
  let no_checksums =
    mk_flag ["no-checksums"]
      "Do not verify the checksum of downloaded archives.\
       This is equivalent to setting $(b,\\$OPAMNOCHECKSUMS) to \"true\"." in
  let req_checksums =
    mk_flag ["require-checksums"]
      "Reject the installation of packages that don't provide a checksum for the upstream archives. \
       This is equivalent to setting $(b,\\$OPAMREQUIRECHECKSUMS) to \"true\"." in
  let build_test =
    mk_flag ["t";"build-test"]
      "Build and $(b,run) the package unit-tests. \
       This is equivalent to setting $(b,\\$OPAMBUILDTEST) to \"true\"." in
  let build_doc =
    mk_flag ["d";"build-doc"]
      "Build the package documentation. \
       This is equivalent to setting $(b,\\$OPAMBUILDDOC) to \"true\"." in
  let make =
    mk_opt ["m";"make"] "MAKE"
      "Use $(docv) as the default 'make' command."
      Arg.(some string) None in
  let show =
    mk_flag ["show-actions"]
      "Call the solver and display the actions. Don't perform any changes." in
  let dryrun =
    mk_flag ["dry-run"]
      "Simulate the command, but don't actually perform any changes." in
  let external_tags =
    mk_opt ["e";"external"] "TAGS"
      "Display the external packages associated to the given tags. \
       This is deprecated, use `opam list --external' instead"
      Arg.(list string) [] in
  let fake =
    mk_flag ["fake"]
      "This option registers the actions into the OPAM database, without \
       actually performing them. \
       WARNING: This option is dangerous and likely to break your OPAM \
       environment. You probably want `--dry-run'. You've been warned." in
  Term.(pure create_build_options
    $keep_build_dir $make $no_checksums $req_checksums $build_test
    $build_doc $show $dryrun $external_tags $fake
    $jobs_flag)
