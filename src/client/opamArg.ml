(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(* Global options *)
type global_options = {
  debug  : bool;
  debug_level: int;
  verbose: bool;
  quiet  : bool;
  color  : bool;
  switch : string option;
  yes    : bool;
  strict : bool;
  root   : dirname;
  no_base_packages: bool;
  git_version     : bool;
  compat_mode_1_0 : bool;
  external_solver : string option;
  use_internal_solver : bool;
  cudf_file : string option;
  solver_preferences : string option;
  no_self_upgrade : bool;
}

let self_upgrade_exe opamroot =
  OpamFilename.OP.(opamroot // "opam", opamroot // "opam.version")

let switch_to_updated_self debug opamroot =
  let updated_self, updated_self_version = self_upgrade_exe opamroot in
  let updated_self_str = OpamFilename.to_string updated_self in
  let updated_self_version_str = OpamFilename.to_string updated_self_version in
  if updated_self_str <> Sys.executable_name &&
     OpamFilename.exists updated_self &&
     OpamFilename.is_exec updated_self &&
     OpamFilename.exists updated_self_version
  then
    let no_version = OpamVersion.of_string "" in
    let update_version =
      try
        let s = OpamSystem.read updated_self_version_str in
        let s =
          let len = String.length s in if len > 0 && s.[len-1] = '\n'
          then String.sub s 0 (len-1) else s in
        OpamVersion.of_string s
      with e -> OpamMisc.fatal e; no_version in
    if update_version = no_version then
      OpamGlobals.error "%s exists but cannot be read, disabling self-upgrade."
        updated_self_version_str
    else if OpamVersion.compare update_version OpamVersion.current <= 0 then
      OpamGlobals.warning "Obsolete OPAM self-upgrade package v.%s found, \
                           not using it (current system version is %s)."
        (OpamVersion.to_string update_version)
        (OpamVersion.to_string OpamVersion.current)
    else (
      if OpamVersion.git <> None then
        OpamGlobals.warning "Using OPAM self-upgrade to %s while the system \
                             OPAM is a development version (%s)"
          (OpamVersion.to_string update_version)
          (OpamVersion.to_string OpamVersion.full);
      (if debug || !OpamGlobals.debug then
         Printf.eprintf "!! %s found, switching to it !!\n%!" updated_self_str;
       let env =
         Array.append
           [|"OPAMNOSELFUPGRADE="^OpamGlobals.self_upgrade_bootstrapping_value|]
           (Unix.environment ()) in
       try
         Unix.execve updated_self_str Sys.argv env
       with e ->
         OpamMisc.fatal e;
         OpamGlobals.error
           "Could'nt run the upgraded opam %s found at %s. \
            Continuing with %s from the system."
           (OpamVersion.to_string update_version)
           updated_self_str
           (OpamVersion.to_string OpamVersion.current)))

let create_global_options
    git_version debug debug_level verbose quiet color switch yes strict root
    no_base_packages compat_mode_1_0 external_solver use_internal_solver
    cudf_file solver_preferences no_self_upgrade =
  let debug, debug_level = match debug, debug_level with
    | _, Some lvl -> true, lvl
    | true, None -> true, 1
    | false, None -> false, 0
  in
  if not (no_self_upgrade) then
    switch_to_updated_self debug root; (* do this asap, don't waste time *)
  { git_version; debug; debug_level; verbose; quiet; color; switch; yes; strict; root;
    no_base_packages; compat_mode_1_0; external_solver; use_internal_solver; cudf_file; solver_preferences;
    no_self_upgrade; }

let apply_global_options o =
  if o.git_version then (
    begin match OpamGitVersion.version with
      | None   -> ()
      | Some v -> OpamGlobals.msg "%s\n%!" v
    end;
    exit 0
  );
  OpamGlobals.debug    := !OpamGlobals.debug || o.debug;
  OpamGlobals.debug_level := max !OpamGlobals.debug_level o.debug_level;
  OpamMisc.debug       := !OpamGlobals.debug;
  OpamGlobals.verbose  := (not o.quiet) && (!OpamGlobals.verbose || o.verbose);
  OpamGlobals.color    := o.color;
  begin match o.switch with
    | None   -> ()
    | Some s -> OpamGlobals.switch := `Command_line s
  end;
  OpamGlobals.root_dir := OpamFilename.Dir.to_string o.root;
  OpamGlobals.yes      := !OpamGlobals.yes || o.yes;
  OpamGlobals.strict   := !OpamGlobals.strict || o.strict;
  OpamGlobals.no_base_packages := !OpamGlobals.no_base_packages || o.no_base_packages;
  OpamGlobals.compat_mode_1_0  := !OpamGlobals.compat_mode_1_0 || o.compat_mode_1_0;
  OpamGlobals.external_solver :=
    OpamMisc.Option.Op.(o.external_solver ++ !OpamGlobals.external_solver);
  OpamGlobals.use_external_solver :=
    !OpamGlobals.use_external_solver && not o.use_internal_solver &&
    !OpamGlobals.external_solver <> Some "";
  OpamGlobals.cudf_file :=
    OpamMisc.Option.Op.(o.cudf_file ++ !OpamGlobals.cudf_file);
  OpamGlobals.no_self_upgrade := !OpamGlobals.no_self_upgrade || o.no_self_upgrade;
  match o.solver_preferences with
  | None -> ()
  | Some prefs ->
    OpamGlobals.solver_preferences :=
      [`Default,prefs; `Upgrade,prefs; `Fixup,prefs]

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
  json          : string option;
}

let create_build_options
    keep_build_dir make no_checksums req_checksums build_test
    build_doc show dryrun external_tags fake
    jobs json = {
  keep_build_dir; make; no_checksums; req_checksums;
  build_test; build_doc; show; dryrun; external_tags;
  fake; jobs; json
}

let json_update = function
  | None   -> ()
  | Some f ->
    let write str = OpamFilename.write (OpamFilename.of_string f) str in
    OpamJson.set_output write

let apply_build_options b =
  OpamGlobals.keep_build_dir := !OpamGlobals.keep_build_dir || b.keep_build_dir;
  OpamGlobals.no_checksums   := !OpamGlobals.no_checksums || b.no_checksums;
  OpamGlobals.req_checksums  := !OpamGlobals.req_checksums || b.req_checksums;
  OpamGlobals.build_test     := !OpamGlobals.build_test || b.build_test;
  OpamGlobals.build_doc      := !OpamGlobals.build_doc || b.build_doc;
  OpamGlobals.show           := !OpamGlobals.show || b.show;
  OpamGlobals.dryrun         := !OpamGlobals.dryrun || b.dryrun;
  OpamGlobals.external_tags  := b.external_tags;
  OpamGlobals.fake           := b.fake;
  json_update b.json;
  OpamGlobals.jobs           :=
    begin match b.jobs with
      | None   -> !OpamGlobals.jobs
      | Some j -> Some j
    end;
  match b.make with
  | None   -> ()
  | Some s -> OpamGlobals.makecmd := (fun () -> s)

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "ENVIRONMENT VARIABLES";
  `P "OPAM makes use of the environment variables listed here. Boolean \
      variables should be set to \"0\", \"no\" of \"false\" to disable, \
      \"1\", \"yes\" or \"true\" to enable.";

  (* Alphabetical order *)
  `P "$(i,OPAMCOLOR), when set to $(i,always) or $(i,never), sets a default \
      value for the --color option.";
  `P ("$(i,OPAMCRITERIA) specifies user $(i,preferences) for dependency solving.\
      The default value is "^OpamGlobals.default_preferences `Default^". \
      See also option --criteria");
  `P "$(i,OPAMCURL) can be used to define an alternative for the 'curl' \
      command-line utility to download files.";
  `P "$(i,OPAMDEBUG) see options `--debug' and `--debug-level'.";
  `P "$(i,OPAMEXTERNALSOLVER) see option `--solver'.";
  `P "$(i,OPAMJOBS) sets the maximum number of parallel workers to run.";
  `P "$(i,OPAMNOASPCUD) see option `--no-aspcud'.";
  `P "$(i,OPAMNOSELFUPGRADE) see option `--no-self-upgrade'.";
  `P "$(i,OPAMROOT) see option `--root'. This is automatically set by \
      `opam config env --root=DIR' when DIR is non-default.";
  `P "$(i,OPAMSOLVERTIMEOUT) change the time allowance of the internal solver.";
  `P "$(i,OPAMSWITCH) see option `--switch'. Automatically set by \
      `opam config env --switch=SWITCH'.";
  `P ("$(i,OPAMUPGRADECRITERIA) specifies user $(i,preferences) for dependency solving \
      when performing an upgrade. Overrides $(i,OPAMCRITERIA) in upgrades if both are set.\
      The default value is "^OpamGlobals.default_preferences `Upgrade^". \
      See also option --criteria");
  `P "$(i,OPAMUTF8MSGS) use nice UTF8 characters in OPAM messages.";
  `P "$(i,OPAMVERBOSE) see option `--verbose'.";
  `P "$(i,OPAMVAR_var) overrides the contents of the variable $(i,var)  when \
      substituting `%{var}%` strings in `opam` files.";
  `P "$(i,OPAMVAR_package_var) overrides the contents of the variable \
      $(i,package:var) when substituting `%{package:var}%` strings in \
      `opam` files.";
  `P "$(i,OPAMUSEINTERNALSOLVER) see option `--use-internal-solver'.";
  `P "$(i,OPAMYES) see option `--yes'.";

  `S "FURTHER DOCUMENTATION";
  `P (Printf.sprintf "See %s." OpamGlobals.default_repository_address);

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

let address =
  let parse str = `Ok (address_of_string str) in
  let print ppf address = pr_str ppf (string_of_address address) in
  parse, print

let filename =
  let parse str = `Ok (OpamFilename.of_string str) in
  let print ppf filename = pr_str ppf (OpamFilename.to_string filename) in
  parse, print

let dirname =
  let parse str = `Ok (OpamFilename.Dir.of_string str) in
  let print ppf dir = pr_str ppf (OpamFilename.prettify_dir dir) in
  parse, print

let compiler =
  let parse str = `Ok (OpamCompiler.of_string str) in
  let print ppf comp = pr_str ppf (OpamCompiler.to_string comp) in
  parse, print

let package_name =
  let parse str =
    try `Ok (OpamPackage.Name.of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf pkg = pr_str ppf (OpamPackage.Name.to_string pkg) in
  parse, print

(* name * version option *)
let package =
  let parse str =
    let re = Re_str.regexp "\\([^>=<.!]+\\)\\(\\.\\(.+\\)\\)?" in
    try
      if not (Re_str.string_match re str 0) then failwith "bad_format";
      let name =
        OpamPackage.Name.of_string (Re_str.matched_group 1 str) in
      let version_opt =
        try Some (OpamPackage.Version.of_string (Re_str.matched_group 3 str))
        with Not_found -> None in
      `Ok (name, version_opt)
    with Failure _ -> `Error "bad package format"
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
    let re = Re_str.regexp "\\([^>=<.!]+\\)\\(>=?\\|<=?\\|=\\|\\.\\|!=\\)\\(.*\\)" in
    try
      if not (Re_str.string_match re str 0) then failwith "no_version";
      let sname = Re_str.matched_group 1 str in
      let sop = Re_str.matched_group 2 str in
      let sversion = Re_str.matched_group 3 str in
      let name = OpamPackage.Name.of_string sname in
      let sop = if sop = "." then "=" else sop in
      let op = OpamFormula.relop_of_string sop in (* may raise Invalid_argument *)
      let version = OpamPackage.Version.of_string sversion in
      `Ok (name, Some (op, version))
    with Failure _ | Invalid_argument _ ->
      `Ok (OpamPackage.Name.of_string str, None)
  in
  let print ppf atom =
    pr_str ppf (OpamFormula.short_string_of_atom atom) in
  parse, print

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

let mk_tristate_opt ?section flags value doc auto default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  let choices =
    Arg.enum [ "always", `Always; "never", `Never; "auto", `Auto ] in
  let arg = Arg.(value & opt choices default & doc) in
  let to_bool = function
    | `Always -> true
    | `Never -> false
    | `Auto -> auto ()
  in
  Term.(pure to_bool $ arg)

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
  List.map (fun (cs,_,args,d) ->
      let cmds = String.concat ", " (List.map bold cs) ^ " " ^
                 String.concat " " (List.map it args) in
      `I (cmds, d)
    ) commands @
  [`S "OPTIONS"] (* Ensures options get after commands *)

let mk_subcommands_aux my_enum commands =
  let command =
    let doc = Arg.info ~docv:"COMMAND" [] in
    let commands =
      List.fold_left
        (fun acc (cs,f,_,_) -> List.map (fun c -> c,f) cs @ acc)
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

let bad_subcommand command subcommands usersubcommand userparams =
  match usersubcommand with
  | None ->
    `Error (false, Printf.sprintf "Missing subcommand. Valid subcommands are %s."
              (OpamMisc.pretty_list
                 (List.flatten (List.map (fun (a,_,_,_) -> a) subcommands))))
  | Some usersubcommand ->
    let name, args =
      match List.find (fun (_,cmd,_,_) -> cmd = usersubcommand) subcommands with
      | name::_, _, args, _doc -> name, args
      | _ -> assert false
    in
    let exe = Filename.basename Sys.executable_name in
    let usage =
      Printf.sprintf "%s %s [OPTION]... %s %s"
        exe command name (String.concat " " args) in
    if List.length userparams < List.length args then
      `Error (false, Printf.sprintf "%s: Missing argument.\nUsage: %s\n"
                exe usage)
    else
      `Error (false, Printf.sprintf "%s: Too many arguments.\nUsage: %s\n"
                exe usage)

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
    "Output the names separated by one whitespace instead of using the usual formatting."

let installed_flag =
  mk_flag ["i";"installed"] "List installed packages only."

let installed_roots_flag =
  mk_flag ["installed-roots"] "Display only the installed roots."

let fish_flag =
  mk_flag ["fish"] "Use fish-compatible mode for configuring OPAM."

let zsh_flag =
  mk_flag ["zsh"] "Use zsh-compatible mode for configuring OPAM."

let csh_flag =
  mk_flag ["csh"] "Use csh-compatible mode for configuring OPAM."

let sh_flag =
  mk_flag ["sh"] "Use sh-compatible mode for configuring OPAM."

let dot_profile_flag =
  mk_opt ["dot-profile"]
    "FILENAME" "Name of the configuration file to update instead of \
                $(i,~/.profile) or $(i,~/.zshrc) based on shell detection."
    (Arg.some filename) None

let repo_kind_flag =
  let kinds = [
    (* main kinds *)
    "http" , `http;
    "local", `local;
    "git"  , `git;
    "darcs"  , `darcs;
    "hg"   , `hg;

    (* aliases *)
    "wget" , `http;
    "curl" , `http;
    "rsync", `local;
  ] in
  mk_opt ["k";"kind"]
    "KIND" "Specify the kind of the repository to be set (the main ones \
            are 'http', 'local', 'git', 'darcs' or 'hg')."
    Arg.(some (enum kinds)) None

let jobs_flag =
  mk_opt ["j";"jobs"] "JOBS"
    "Set the maximal number of concurrent jobs to use. You can also set it using \
     the $(b,\\$OPAMJOBS) environment variable."
    Arg.(some int) None

let pattern_list =
  arg_list "PATTERNS" "List of package patterns." Arg.string

let nonempty_pattern_list =
  nonempty_arg_list "PATTERNS" "List of package patterns." Arg.string

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
    mk_flag ~section ["v";"verbose"]
      "Be more verbose. Show output of all sub-commands. \
       This is equivalent to setting $(b,\\$OPAMVERBOSE) to \"true\"." in
  let quiet =
    mk_flag ~section ["q";"quiet"] "Be quiet when installing a new compiler." in
  let color =
    mk_tristate_opt ~section ["color"] "WHEN"
      "Colorize the output. $(docv) must be `always', `never' or `auto'."
      (fun () -> Unix.isatty Unix.stdout) OpamGlobals.color_tri_state in
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
    mk_opt ~section ["r";"root"]
      "ROOT" "Use $(docv) as the current root path. \
              This is equivalent to setting $(b,\\$OPAMROOT) to $(i,ROOT)."
      dirname (OpamFilename.Dir.of_string OpamGlobals.default_opam_dir) in
  let no_base_packages =
    mk_flag ~section ["no-base-packages"]
      "Do not install base packages (useful for testing purposes). \
       This is equivalent to setting $(b,\\$OPAMNOBASEPACKAGES) to a non-empty \
       string." in
  let compat_mode_1_0 =
    mk_flag ~section ["compat-mode-1.0"]
      "Compatibility mode with OPAM 1.0" in
  let use_internal_solver =
    mk_flag ~section ["no-aspcud"; "use-internal-solver"]
      "Force use of internal heuristics, even if an external solver is available." in
  let external_solver =
    mk_opt ~section ["solver"] "CMD"
      ("Specify the name of the external dependency $(i,solver). \
        The default value is "^OpamGlobals.default_external_solver)
      Arg.(some string) None in
  let solver_preferences =
    mk_opt ~section ["criteria"] "CRITERIA"
      ("Specify user $(i,preferences) for dependency solving for this run. \
        Overrides both $(b,\\$OPAMCRITERIA) and $(b,\\$OPAMUPGRADECRITERIA). \
        For details on the supported language, and the external solvers available, see \
        $(i,  http://opam.ocaml.org/doc/Specifying_Solver_Preferences.html). \
        A general guide to using solver preferences can be found at \
        $(i,  http://www.dicosmo.org/Articles/usercriteria.pdf). \
        The default value is "^OpamGlobals.default_preferences `Upgrade^
       " for upgrades, and "^OpamGlobals.default_preferences `Default^" otherwise.")
      Arg.(some string) None in
  let cudf_file =
    mk_opt ~section ["cudf"] "FILENAME"
      "Debug option: Save the CUDF requests sent to the solver to \
       $(docv)-<n>.cudf."
      Arg.(some string) None in
  let no_self_upgrade =
    mk_flag ~section ["no-self-upgrade"]
      "OPAM will replace itself with a newer binary found \
       at $(b,OPAMROOT/opam) if present. This disables this behaviour." in
  Term.(pure create_global_options
        $git_version $debug $debug_level $verbose $quiet $color $switch $yes
        $strict $root $no_base_packages $compat_mode_1_0 $external_solver
        $use_internal_solver $cudf_file $solver_preferences $no_self_upgrade)

let json_flag =
  mk_opt ["json"] "FILENAME"
    "Save the result output of an OPAM run in a computer-readable file"
    Arg.(some string) None

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
      "Display the external packages associated to the given tags."
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
    $jobs_flag $json_flag)

let init_dot_profile shell dot_profile =
  match dot_profile with
  | Some n -> n
  | None   -> OpamFilename.of_string (OpamMisc.guess_dot_profile shell)

module Client = OpamClient.SafeAPI

type command = unit Term.t * Term.info

(* INIT *)
let init_doc = "Initialize OPAM state."
let init =
  let doc = init_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,init) command creates a fresh client state.  This initializes OPAM \
        configuration in $(i,~/.opam) and configures a default package repository.";
    `P "Once the fresh client has been created, OPAM will ask the user if he wants \
        $(i,~/.profile) (or $i,~/.zshrc, depending on his shell) and $(i,~/.ocamlinit) \
        to be updated. \
        If $(b,--auto-setup) is used, OPAM will modify the configuration files automatically, \
        without asking the user. If $(b,--no-setup) is used, OPAM will *NOT* modify \
        anything outside of $(i,~/.opam).";
    `P "Additional repositories can be added later by using the $(b,opam repository) command.";
    `P "The state of repositories can be synchronized by using $(b,opam update).";
    `P "The user and global configuration files can be setup later by using $(b,opam config setup).";
  ] in
  let jobs = mk_opt ["j";"jobs"] "JOBS" "Number of jobs to use when building packages." Arg.int OpamGlobals.default_jobs in
  let compiler =
    mk_opt ["comp"] "VERSION" "Which compiler version to use." compiler OpamCompiler.system in
  let repo_name =
    let doc = Arg.info ~docv:"NAME" ~doc:"Name of the repository." [] in
    Arg.(value & pos ~rev:true 1 repository_name OpamRepositoryName.default & doc) in
  let repo_address =
    let doc = Arg.info ~docv:"ADDRESS" ~doc:"Address of the repository." [] in
    Arg.(value & pos ~rev:true 0 address OpamRepository.default_address & doc) in
  let no_setup   = mk_flag ["n";"no-setup"]   "Do not update the global and user configuration options to setup OPAM." in
  let auto_setup = mk_flag ["a";"auto-setup"] "Automatically setup all the global and user configuration options for OPAM." in
  let init global_options
      build_options repo_kind repo_name repo_address compiler jobs
      no_setup auto_setup sh csh zsh fish dot_profile_o =
    (* Create the dir in current directory so that it can be made absolute *)
    OpamFilename.mkdir global_options.root;
    apply_global_options global_options;
    apply_build_options build_options;
    let repo_priority = 0 in
    let repo_address, repo_kind2 = parse_url repo_address in
    let repo_kind = OpamMisc.Option.default repo_kind2 repo_kind in
    let repository = {
      repo_root = OpamPath.Repository.create (OpamPath.root ()) repo_name;
      repo_name; repo_kind; repo_address; repo_priority } in
    let update_config =
      if no_setup then `no
      else if auto_setup then `yes
      else `ask in
    let shell =
      if sh then `sh
      else if csh then `csh
      else if zsh then `zsh
      else if fish then `fish
      else OpamMisc.guess_shell_compat () in
    let dot_profile = init_dot_profile shell dot_profile_o in
    Client.init repository compiler ~jobs shell dot_profile update_config in
  Term.(pure init
    $global_options $build_options $repo_kind_flag $repo_name $repo_address $compiler $jobs
    $no_setup $auto_setup $sh_flag $csh_flag $zsh_flag $fish_flag $dot_profile_flag),
  term_info "init" ~doc ~man

(* LIST *)
let list_doc = "Display the list of available packages."
let list =
  let doc = list_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of installed packages, or the list of \
        all the available packages if the $(b,--all) flag is used.";
    `P "Unless the $(b,--short) switch is used, the output format displays one \
        package per line, and each line contains the name of the package, the \
        installed version or -- if the package is not installed, and a short \
        description. In color mode, root packages (eg. manually installed) are \
        underlined.";
    `P "The full description can be obtained by doing $(b,opam info <package>). \
        You can search through the package descriptions using the $(b,opam search) \
        command."
  ] in
  let all =
    mk_flag ["a";"all"]
      "List all the packages which can be installed on the system." in
  let sort = mk_flag ["sort";"S"] "Sort the packages in dependency order." in
  let depends_on =
    let doc = "List only packages that depend on one of $(docv)." in
    Arg.(value & opt (list atom) [] & info ~doc ~docv:"PACKAGES" ["depends-on"])
  in
  let required_by =
    let doc = "List only the dependencies of $(docv)." in
    Arg.(value & opt (list atom) [] & info ~doc ~docv:"PACKAGES" ["required-by"])
  in
  let recursive =
    mk_flag ["recursive"]
      "With `--depends-on' and `--required-by', display all transitive \
       dependencies rather than just direct dependencies." in
  let depopts =
    mk_flag ["depopts"] "Include optional dependencies in dependency requests." in
  let depexts =
    mk_opt ["e";"external"] "TAGS" ~vopt:(Some [])
      "Instead of displaying the packages, display their external dependencies \
       that are associated with any subset of the given $(i,TAGS) (OS, \
       distribution, etc.). \
       Common tags include `debian', `x86', `osx', `homebrew', `source'... \
       Without $(i,TAGS), display the tags and all associated external \
       dependencies."
      Arg.(some & list string) None in
  let list global_options print_short all installed
      installed_roots sort
      depends_on required_by recursive depopts depexts
      packages =
    apply_global_options global_options;
    let filter =
      match all, installed, installed_roots with
      | true,  false, false -> Some `installable
      | false, true,  false -> Some `installed
      | false, _,     true  -> Some `roots
      | false, false, false ->
        if depends_on = [] && required_by = [] && packages = []
        then Some `installed else Some `installable
      | _ -> None
    in
    let order = if sort then `depends else `normal in
    match filter, depends_on, required_by with
    | Some filter, [], depends | Some filter, depends, [] ->
      Client.list
        ~print_short ~filter ~order
        ~exact_name:true ~case_sensitive:false
        ~depends ~reverse_depends:(depends_on <> [])
        ~recursive_depends:recursive
        ~depopts ?depexts
        packages;
      `Ok ()
    | None, _, _ ->
      `Error (true, "Conflicting filters: only one of --all, --installed and \
                     --installed-roots may be given at a time")
    | _ ->
      `Error (true, "Only one of --depends-on and --required-by \
                      may be given at a time")
  in
  Term.ret
    Term.(pure list $global_options
          $print_short_flag $all $installed_flag $installed_roots_flag
          $sort
          $depends_on $required_by $recursive $depopts $depexts
          $pattern_list),
  term_info "list" ~doc ~man

(* SEARCH *)
let search =
  let doc = "Search into the package list." in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of available packages that match one of \
        the package patterns specified as arguments.";
    `P "Unless the $(b,--short) flag is used, the output format is the same as the \
        $(b,opam list) command. It displays one package per line, and each line \
        contains the name of the package, the installed version or -- if the package \
        is not installed, and a short description.";
    `P "The full description can be obtained by doing $(b,opam show <package>).";
  ] in
  let case_sensitive =
    mk_flag ["c";"case-sensitive"] "Force the search in case sensitive mode." in
  let search global_options print_short installed installed_roots case_sensitive pkgs =
    apply_global_options global_options;
    let filter = match installed, installed_roots with
      | _, true -> `roots
      | true, _ -> `installed
      | _       -> `all in
    let order = `normal in
    Client.list ~print_short ~filter ~order
      ~exact_name:false ~case_sensitive pkgs in
  Term.(pure search $global_options
    $print_short_flag $installed_flag $installed_roots_flag $case_sensitive
    $pattern_list),
  term_info "search" ~doc ~man

(* SHOW *)
let show_doc = "Display information about specific packages."
let show =
  let doc = show_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the information block for the selected \
        package(s).";
    `P "The information block consists in the name of the package, \
        the installed version if this package is installed in the currently \
        selected compiler, the list of available (installable) versions, and a \
        complete description.";
    `P "$(b,opam list) can be used to display the list of \
        available packages as well as a short description for each.";
  ] in
  let fields =
    let doc =
      Arg.info
        ~docv:"FIELDS"
        ~doc:"Only display these fields. You can specify multiple fields by separating them with commas."
        ["f";"field"] in
    Arg.(value & opt (list string) [] & doc) in
  let raw =
    mk_flag ["raw"] "Print the raw opam file for this package" in
  let pkg_info global_options fields raw packages =
    apply_global_options global_options;
    Client.info ~fields ~raw_opam:raw packages in
  Term.(pure pkg_info $global_options $fields $raw $nonempty_atom_list),
  term_info "show" ~doc ~man


(* CONFIG *)
let config_doc = "Display configuration options for packages."
let config =
  let doc = config_doc in
  let commands = [
    ["env"]     , `env     , [],
    "Return the environment variables PATH, MANPATH, OCAML_TOPLEVEL_PATH \
     and CAML_LD_LIBRARY_PATH according to the currently selected \
     compiler. The output of this command is meant to be evaluated by a \
     shell, for example by doing $(b,eval `opam config env`).";
    ["setup"]   , `setup   , [],
    "Configure global and user parameters for OPAM. Use $(b, opam config setup) \
     to display more options. Use $(b,--list) to display the current configuration \
     options. You can use this command to automatically update: (i) user-configuration \
     files such as ~/.profile and ~/.ocamlinit; and (ii) global-configaration files \
     controlling which shell scripts are loaded on startup, such as auto-completion. \
     These configuration options can be updated using: $(b,opam config setup --global) \
     to setup the global configuration files stored in $(b,~/.opam/opam-init/) and \
     $(b,opam config setup --user) to setup the user ones. \
     To modify both the global and user configuration, use $(b,opam config setup --all).";
    ["exec"]    , `exec    , ["[--] COMMAND"; "[ARG]..."],
    "Execute $(i,CMD) with the correct environment variables. \
     This command can be used to cross-compile between switches using \
     $(b,opam config exec --switch=SWITCH -- COMMAND ARG1 ... ARGn)";
    ["var"]     , `var     , ["VAR"],
    "Return the value associated with variable $(i,VAR). Package variables can \
     be accessed with the syntax $(i,pkg:var).";
    ["list"]    , `list    , ["[PACKAGE]..."],
    "Without argument, prints a documented list of all available variables. With \
     $(i,PACKAGE), lists all the variables available for these packages.";
    ["subst"]   , `subst   , ["FILE..."],
    "Substitute variables in the given files. The strings $(i,%{var}%) are \
     replaced by the value of variable $(i,var) (see $(b,var)).";
    ["report"]  , `report  , [],
    "Prints a summary of your setup, useful for bug-reports.";
    ["cudf-universe"], `cudf, ["[FILE]"],
    "Outputs the current available package universe in CUDF format.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command uses OPAM state to output information on how to use \
        installed libraries, update the $(b,PATH), and substitute \
        variables used in OPAM packages.";
    `P "Apart from $(b,opam config env), most of these commands are used \
        by OPAM internally, and are of limited interest for the casual \
        user.";
  ] @ mk_subdoc commands in

  let command, params = mk_subcommands commands in
  let all_doc         = "Enable all the global and user configuration options." in
  let global_doc      = "Enable all the global configuration options." in
  let user_doc        = "Enable all the user configuration options." in
  let ocamlinit_doc   = "Modify ~/.ocamlinit to make `#use \"topfind\"` works in the toplevel." in
  let profile_doc     = "Modify ~/.profile (or ~/.zshrc if running zsh) to \
                         setup an OPAM-friendly environment when starting a new shell." in
  let no_complete_doc = "Do not load the auto-completion scripts in the environment." in
  let no_eval_doc     = "Do not install `opam-switch-eval` to switch & eval using a single command." in
  let dot_profile_doc = "Select which configuration file to update (default is ~/.profile)." in
  let list_doc        = "List the current configuration." in
  let sexp_doc        = "Display environment variables as an s-expression" in
  let profile         = mk_flag ["profile"]        profile_doc in
  let ocamlinit       = mk_flag ["ocamlinit"]      ocamlinit_doc in
  let no_complete     = mk_flag ["no-complete"]    no_complete_doc in
  let no_switch_eval  = mk_flag ["no-switch-eval"] no_eval_doc in
  let all             = mk_flag ["a";"all"]        all_doc in
  let user            = mk_flag ["u";"user"]       user_doc in
  let global          = mk_flag ["g";"global"]     global_doc in
  let list            = mk_flag ["l";"list"]       list_doc in
  let sexp            = mk_flag ["sexp"]           sexp_doc in

  let config global_options
      command sh csh zsh fish sexp
      dot_profile_o list all global user
      profile ocamlinit no_complete no_switch_eval
      params =
    apply_global_options global_options;
    let csh, fish =
      match sh, csh, sexp, fish with
      | false, false, false, false ->
        (* No overrides have been provided, so guess which shell is active *)
        (match OpamMisc.guess_shell_compat () with
         | `csh                -> true , false
         | `fish               -> false, true
         | `sh | `bash | `zsh  -> false, false)
      | _ -> csh, fish
    in
    match command, params with
    | Some `env, [] -> `Ok (Client.CONFIG.env ~csh ~sexp ~fish)
    | Some `setup, [] ->
      let user        = all || user in
      let global      = all || global in
      let profile     = user  || profile in
      let ocamlinit   = user  || ocamlinit in
      let complete    = global && not no_complete in
      let switch_eval = global && not no_switch_eval in
      let shell       =
        if sh then `sh
        else if csh then `csh
        else if zsh then `zsh
        else if fish then `fish
        else OpamMisc.guess_shell_compat () in
      let dot_profile = init_dot_profile shell dot_profile_o in
      if list then
        `Ok (Client.CONFIG.setup_list shell dot_profile)
      else if profile || ocamlinit || complete || switch_eval then
        let dot_profile = if profile then Some dot_profile else None in
        let user   = if user then Some { shell; ocamlinit; dot_profile } else None in
        let global = if global then Some { complete; switch_eval } else None in
        `Ok (Client.CONFIG.setup user global)
      else
        `Ok (OpamGlobals.msg
          "usage: opam config setup [options]\n\
           \n\
           Main options\n\
          \    -l, --list           %s\n\
          \    -a, --all            %s\n\
          \    --sh,--csh,--zsh,--fish     Force the configuration mode to a given shell.\n\
           \n\
           User configuration\n\
          \    -u, --user           %s\n\
          \    --ocamlinit          %s\n\
          \    --profile            %s\n\
          \    --dot-profile FILE   %s\n\
           \n\
           Global configuration\n\
          \    -g,--global          %s\n\
          \    --no-complete        %s\n\
          \    --no-switch-eval     %s\n\n"
          list_doc all_doc
          user_doc ocamlinit_doc profile_doc dot_profile_doc
          global_doc no_complete_doc no_eval_doc)
    | Some `exec, (_::_ as c) -> `Ok (Client.CONFIG.exec c)
    | Some `list, [] ->
      `Ok (Client.CONFIG.list (List.map OpamPackage.Name.of_string params))
    | Some `var, [var] ->
      `Ok (Client.CONFIG.variable (OpamVariable.Full.of_string var))
    | Some `subst, (_::_ as files) ->
      `Ok (Client.CONFIG.subst (List.map OpamFilename.Base.of_string files))
    | Some `cudf, params ->
      let opam_state = OpamState.load_state "config-universe" in
      let opam_univ = OpamState.universe opam_state Depends in
      let version_map =
        OpamSolver.cudf_versions_map opam_univ opam_state.OpamState.Types.packages in
      let cudf_univ =
        OpamSolver.load_cudf_universe ~depopts:false opam_univ ~version_map
          opam_univ.u_available in
      let dump oc =
        OpamCudf.dump_universe oc cudf_univ;
        (* Add explicit bindings to retrieve original versions of non-available packages *)
        OpamPackage.Map.iter (fun nv i ->
            if not (OpamPackage.Set.mem nv opam_univ.u_available) then
              Printf.printf "#v2v:%s:%d=%s\n"
                (OpamPackage.name_to_string nv) i (OpamPackage.version_to_string nv)
          ) version_map in
      (match params with
       | [] -> `Ok (dump stdout)
       | [file] -> let oc = open_out file in dump oc; close_out oc; `Ok ()
       | _ -> bad_subcommand "config" commands command params)
    | Some `report, [] -> (
      let print label fmt = Printf.printf ("# %-15s "^^fmt^^"\n") label in
      Printf.printf "# OPAM status report\n";
      let version = OpamVersion.to_string OpamVersion.current in
      let version = match OpamVersion.git with
        | None   -> version
        | Some v -> Printf.sprintf "%s (%s)" version (OpamVersion.to_string v)
      in
      print "opam-version" "%s" version;
      print "self-upgrade" "%s"
        (if OpamGlobals.is_self_upgrade
         then OpamFilename.prettify (fst (self_upgrade_exe (OpamPath.root())))
         else "no");
      print "os" "%s" (OpamGlobals.os_string ());
      print "external-solver" "%b" (OpamCudf.external_solver_available ());
      try
        let state = OpamState.load_state "config-report" in
        let open OpamState.Types in
        let nprint label n =
          if n <> 0 then [Printf.sprintf "%d (%s)" n label]
          else [] in
        print "jobs" "%d" (OpamState.jobs state);
        print "repositories" "%s"
          OpamRepositoryName.Map.(
            let nhttp, nlocal, nvcs =
              fold (fun _ {repo_kind=k} (nhttp, nlocal, nvcs) -> match k with
                  | `http -> nhttp+1, nlocal, nvcs
                  | `local -> nhttp, nlocal+1, nvcs
                  | _ -> nhttp, nlocal, nvcs+1)
                state.repositories (0,0,0) in
            let has_default =
              exists (fun _ {repo_address} ->
                  repo_address = OpamRepository.default_address)
                state.repositories in
            String.concat ", "
              (Printf.sprintf "%d%s (http)" nhttp
                 (if has_default then "*" else "") ::
               nprint "local" nlocal @
               nprint "version-controlled" nvcs)
          );
        print "pinned" "%s"
          OpamPackage.Name.Map.(
            if is_empty state.pinned then "0" else
            let nver, nlocal, nvc =
              fold (fun _ p (nver, nlocal, nvc) -> match p with
                  | Version _ -> nver+1, nlocal, nvc
                  | Local _ -> nver, nlocal+1, nvc
                  | _ -> nver, nlocal, nvc+1)
                state.pinned (0,0,0) in
            String.concat ", "
              (nprint "version" nver @
               nprint "path" nlocal @
               nprint "version control" nvc)
          );
        print "current-switch" "%s%s"
          (OpamSwitch.to_string state.switch)
          (if (OpamFile.Comp.preinstalled
                 (OpamFile.Comp.read
                    (OpamPath.compiler_comp state.root state.compiler)))
           then "*" else "");
        let index_file = OpamFilename.to_string (OpamPath.package_index state.root) in
        let u = Unix.gmtime (Unix.stat index_file).Unix.st_mtime in
        Unix.(print "last-update" "%04d-%02d-%02d %02d:%02d"
              (1900 + u.tm_year) (1 + u.tm_mon) u.tm_mday
              u.tm_hour u.tm_min);
        `Ok ()
      with e -> print "read-state" "%s" (Printexc.to_string e); `Ok ())
    | command, params -> bad_subcommand "config" commands command params
  in

  Term.ret (
    Term.(pure config
          $global_options $command $sh_flag $csh_flag $zsh_flag $fish_flag $sexp
          $dot_profile_flag $list $all $global $user
          $profile $ocamlinit $no_complete $no_switch_eval
          $params)
  ),
  term_info "config" ~doc ~man

(* INSTALL *)
let install_doc = "Install a list of packages."
let install =
  let doc = install_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command installs one or more packages to the currently selected \
        compiler. To install packages for another compiler, you need to switch \
        compilers using $(b,opam switch). You can remove installed packages with \
        $(b,opam remove), and list installed packages with $(b,opam list -i). \
        See $(b,opam pin) as well to understand how to manage package versions.";
    `P "This command makes OPAM use the dependency solver to compute the \
        transitive closure of dependencies to be installed, and will also handle \
        conflicts. If the dependency solver returns more than one \
        solution, OPAM will arbitrarily select the first one. If dependencies \
        are to be installed, OPAM will confirm if the installation should \
        proceed.";
  ] in
  let add_to_roots =
    let root =
      Some true, Arg.info ["set-root"]
        ~doc:"Mark given packages as installed roots. This is the default \
              for newly manually-installed packages." in
    let unroot =
      Some false, Arg.info ["unset-root"]
        ~doc:"Mark given packages as \"installed automatically\"." in
    Arg.(value & vflag None[root; unroot])
  in
  let deps_only =
    Arg.(value & flag & info ["deps-only"]
           ~doc:"Install all its dependencies, but don't actually install the \
                 package.") in
  let install global_options build_options add_to_roots deps_only atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    Client.install atoms add_to_roots deps_only
  in
  Term.(pure install $global_options $build_options
        $add_to_roots $deps_only $nonempty_atom_list),
  term_info "install" ~doc ~man

(* REMOVE *)
let remove_doc = "Remove a list of packages."
let remove =
  let doc = remove_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes (i.e. uninstalls) one or more packages currently \
        installed in the currently selected compiler switch. To remove packages \
        installed in another compiler, you need to switch compilers using \
        $(b,opam switch) or use the $(b,--switch) flag. This command is the \
        inverse of $(b,opam-install).";
  ] in
  let autoremove =
    mk_flag ["a";"auto-remove"]
      "Remove all the packages which have not been explicitly installed and \
       which are not necessary anymore. It is possible to prevent the removal of an \
       already-installed package by running $(b,opam install <pkg>). This flag \
       can also be set using the $(b,\\$OPAMAUTOREMOVE) configuration variable." in
  let force =
    mk_flag ["force"]
      "Execute the remove commands of given packages directly, even if they are \
       not considered installed by OPAM." in
  let remove global_options build_options autoremove force atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    Client.remove ~autoremove ~force atoms in
  Term.(pure remove $global_options $build_options $autoremove $force $atom_list),
  term_info "remove" ~doc ~man

(* REINSTALL *)
let reinstall =
  let doc = "Reinstall a list of packages." in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes the given packages and the ones \
        that depend on them, and reinstalls the same versions."
  ] in
  let reinstall global_options build_options atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    Client.reinstall atoms in
  Term.(pure reinstall $global_options $build_options $nonempty_atom_list),
  term_info "reinstall" ~doc ~man

(* UPDATE *)
let update_doc = "Update the list of available packages."
let update =
  let doc = update_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command updates each repository that has been previously set up \
        by the $(b,opam init) or $(b,opam repository) commands. The list of packages \
        that can be upgraded will be printed out, and the user can use \
        $(b,opam upgrade) to upgrade them.";
  ] in
  let repos_only =
    mk_flag ["R"; "repositories"]
      "Only update repositories, not development packages." in
  let sync =
    mk_flag ["sync-archives"]
      "Always sync the remote archives files. This is not \
       a good idea to enable this, unless your really know \
       what your are doing: this flag will make OPAM try to \
       download the archive files for ALL the available \
       packages." in
  let upgrade =
    mk_flag ["u";"upgrade"]
      "Automatically run $(b,opam upgrade) after the update." in
  let name_list =
    arg_list "NAMES" "List of repository or development package names."
      Arg.string in
  let update global_options jobs json names repos_only sync upgrade =
    apply_global_options global_options;
    json_update json;
    OpamGlobals.sync_archives := sync;
    OpamGlobals.jobs := jobs;
    Client.update ~repos_only names;
    if upgrade then Client.upgrade []
  in
  Term.(pure update $global_options $jobs_flag $json_flag $name_list
        $repos_only $sync $upgrade),
  term_info "update" ~doc ~man

(* UPGRADE *)
let upgrade_doc = "Upgrade the installed package to latest version."
let upgrade =
  let doc = upgrade_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command upgrades the installed packages to their latest available \
        versions. More precisely, this command calls the dependency solver to \
        find a consistent state where $(i,most) of the installed packages are \
        upgraded to their latest versions.";
  ] in
  let fixup =
    mk_flag ["fixup"]
      "Recover from a broken state (eg. missing dependencies, two conflicting \
       packages installed together...). This requires that you have an \
       external solver installed (aspcud, cudf-services.irill.org, ...)" in
  let upgrade global_options build_options fixup atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    if fixup then
      if atoms <> [] then
        `Error (true, Printf.sprintf "--fixup doesn't allow extra arguments")
      else `Ok (Client.fixup ())
    else
      `Ok (Client.upgrade atoms) in
  Term.(ret (pure upgrade $global_options $build_options $fixup $atom_list)),
  term_info "upgrade" ~doc ~man

(* REPOSITORY *)
let repository_doc = "Manage OPAM repositories."
let repository =
  let doc = repository_doc in
  let commands = [
    ["add"]        , `add     , ["NAME"; "ADDRESS"],
    "Add the repository at address $(i,ADDRESS) to the list of repositories \
     used by OPAM, under $(i,NAME). It will have highest priority unless \
     $(b,--priority) is specified.";
    ["remove"]     , `remove  , ["NAME"],
    "Remove the repository $(i,NAME) from the list of repositories used by OPAM.";
    ["list"]       , `list    , [],
    "List all repositories used by OPAM.";
    ["priority"]   , `priority, ["NAME"; "PRIORITY"],
    "Change the priority of repository named $(i,NAME) to $(i,PRIORITY).";
    ["set-url"]    , `set_url, ["NAME"; "ADDRESS"],
    "Change the URL associated with $(i,NAME)";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command is used to manage OPAM repositories. To synchronize OPAM \
        with the last versions of the packages available in remote \
        repositories, use $(b,opam update).";
  ] @ mk_subdoc ~defaults:["","list"] commands in

  let command, params = mk_subcommands commands in
  let priority =
    mk_opt ["p";"priority"]
      "INT" "Set the repository priority (bigger is better)"
      Arg.(some int) None in

  let repository global_options command kind priority short params =
    apply_global_options global_options;
    match command, params with
    | Some `add, [name;address] ->
      let name = OpamRepositoryName.of_string name in
      let address = address_of_string address in
      let address, kind2 = parse_url address in
      let kind = OpamMisc.Option.default kind2 kind in
      `Ok (Client.REPOSITORY.add name kind address ~priority)
    | (None | Some `list), [] ->
      `Ok (Client.REPOSITORY.list ~short)
    | Some `priority, [name; p] ->
      let name = OpamRepositoryName.of_string name in
      let priority =
        try int_of_string p
        with Failure _ -> OpamGlobals.error_and_exit "%s is not an integer." p in
      `Ok (Client.REPOSITORY.priority name ~priority)
    | Some `set_url, [name; address] ->
      let name = OpamRepositoryName.of_string name in
      let url = address_of_string address in
      `Ok (Client.REPOSITORY.set_url name url)
    | Some `remove, [name] ->
      let name = OpamRepositoryName.of_string name in
      `Ok (Client.REPOSITORY.remove name)
    | command, params -> bad_subcommand "repository" commands command params
  in
  Term.ret
    Term.(pure repository $global_options $command $repo_kind_flag $priority
          $print_short_flag $params),
  term_info "repository" ~doc ~man

(* SWITCH *)
let switch_doc = "Manage multiple installation of compilers."
let switch =
  let doc = switch_doc in
  let commands = [
    ["install"]      , `install  , ["SWITCH"],
    "Install the given compiler. The commands fails if the package is \
     already installed (e.g. it will not transparently switch to the \
     installed compiler switch, as with $(b,set)).";
    ["set"]          , `set      , ["SWITCH"],
      "Set the currently active switch, installing it if needed.";
    ["remove"]       , `remove   , ["SWITCH"], "Remove the given compiler.";
    ["export"]       , `export   , ["FILE"],
    "Save the current switch state to a file.";
    ["import"]       , `import   , ["FILE"], "Import a saved switch state.";
    ["reinstall"]    , `reinstall, ["SWITCH"],
    "Reinstall the given compiler switch. This will also reinstall all \
     packages.";
    ["list"]         , `list     , [],
    "List compilers. \
     By default, lists installed and `standard' compilers. Use `--all' to get \
     the list of all installable compilers.\n\
     The first column displays the switch name (if any), the second one \
     the switch state (C = current, I = installed, -- = not installed), \
     the third one the compiler name and the last one the compiler \
     description. To switch to an already installed compiler alias (with \
     state = I), use $(b,opam switch <name>). If you want to use a new \
     compiler <comp>, use $(b,opam switch <comp>): this will download, \
     compile and create a fresh and independent environment where new \
     packages can be installed. If you want to create a new compiler alias \
     (for instance because you already have this compiler version installed), \
     use $(b,opam switch <name> --alias-of <comp>). In case \
     <name> and <comp> are the same, this is equivalent to $(b,opam switch <comp>).";
    ["show"]          , `current  , [], "Show the current compiler.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command allows one to switch between different compiler versions, \
        installing the compiler if $(b,opam switch) is used to switch to that \
        compiler for the first time. The different compiler versions are \
        totally independent from each other, meaning that OPAM maintains a \
        separate state (e.g. list of installed packages...) for each.";
    `P "See the documentation of $(b,opam switch list) to see the compilers which \
        are available, and how to switch or to install a new one."
  ] @ mk_subdoc ~defaults:["","list";"SWITCH","set"] commands in

  let command, params = mk_subcommands_with_default commands in
  let alias_of =
    mk_opt ["A";"alias-of"]
      "COMP" "The name of the compiler description which will be aliased."
      Arg.(some string) None in
  let no_warning =
    mk_flag ["no-warning"]
      "Do not display any warning related to environment variables." in
  let no_switch =
    mk_flag ["no-switch"]
      "Only install the compiler switch, without switching to it. If the compiler \
       switch is already installed, then do nothing." in
  let installed =
    mk_flag ["i";"installed"] "List installed compiler switches only." in
  let all =
    mk_flag ["a";"all"]
      "List all the compilers which can be installed on the system." in

  let switch global_options
      build_options command alias_of print_short installed all
      no_warning no_switch params =
    apply_global_options global_options;
    apply_build_options build_options;
    let mk_comp alias = match alias_of with
      | None      -> OpamCompiler.of_string alias
      | Some comp -> OpamCompiler.of_string comp in
    let warning = not no_warning in
    match command, params with
    | None      , []
    | Some `list, [] ->
      Client.SWITCH.list ~print_short ~installed ~all;
      `Ok ()
    | Some `install, [switch] ->
      Client.SWITCH.install
        ~quiet:global_options.quiet
        ~warning
        ~update_config:(not no_switch)
        (OpamSwitch.of_string switch)
        (mk_comp switch);
      `Ok ()
    | Some `export, [filename] ->
      Client.SWITCH.export
        (if filename = "-" then None else Some (OpamFilename.of_string filename));
      `Ok ()
    | Some `import, [filename] ->
      Client.SWITCH.import
        (if filename = "-" then None else Some (OpamFilename.of_string filename));
      `Ok ()
    | Some `remove, switches ->
      List.iter
        (fun switch -> Client.SWITCH.remove (OpamSwitch.of_string switch))
        switches;
      `Ok ()
    | Some `reinstall, [switch] ->
      Client.SWITCH.reinstall (OpamSwitch.of_string switch);
      `Ok ()
    | Some `current, [] ->
      Client.SWITCH.show ();
      `Ok ()
    | Some `default switch, [] ->
      (match alias_of with
       | None ->
         Client.SWITCH.switch ~quiet:global_options.quiet ~warning
           (OpamSwitch.of_string switch);
         `Ok ()
       | _    ->
         Client.SWITCH.install
           ~quiet:global_options.quiet
           ~warning
           ~update_config:(not no_switch)
           (OpamSwitch.of_string switch)
           (mk_comp switch);
         `Ok ())
    | command, params -> bad_subcommand "switch" commands command params
  in
  Term.(ret (pure switch
             $global_options $build_options $command
             $alias_of $print_short_flag
             $installed $all $no_warning $no_switch $params)),
  term_info "switch" ~doc ~man

(* PIN *)
let pin_doc = "Pin a given package to a specific version or source."
let pin ?(unpin_only=false) () =
  let doc = pin_doc in
  let commands = [
    ["list"]     , `list, [], "Lists pinned packages.";
    ["add"]      , `add  , ["NAME"; "TARGET"],
    "Pins package $(i,NAME) to $(i,TARGET), which may be a version, a path, \
     or a url. \
     $(i,NAME) can be omitted if $(i,TARGET) is a local path containing a \
     package description with a name. $(i,TARGET) can be omitted with \
     `--dev-repo'. \
     Use url syntax or $(b,--kind) to explicitely set the kind of pinning. Git \
     pins may target a specific branch or commit using $(b,#branch) e.g. \
     $(b,git://host/me/pkg#testing). \
     It is possible to create a new package if $(i,NAME) does not exist.";
    ["remove"]   , `remove, ["NAME"],
    "Unpins package $(b,NAME), restoring its definition from the repository, if any.";
    ["edit"]     , `edit, ["NAME"],
    "Opens an editor giving you the opportunity to \
     change the opam file that OPAM will locally use for pinned package \
     $(b,NAME). To simply cange the pinning target, use $(b,add). \
     The chosen editor is determined from environment variables \
     $(b,OPAM_EDITOR), $(b,VISUAL) or $(b,EDITOR), in order.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command allows local customisation of the packages in a given \
        switch. A package can be pinned to a specific upstream version, to \
        a path containing its source, to a version-controlled location or to \
        an URL. An `opam' file found at the root of the pinned source will \
        override the package's opam file from the repository, an `opam' \
        directory will override all its metadata."
  ] @ mk_subdoc ~defaults:["","list"] commands in
  let command, params =
    if unpin_only then
      Term.pure (Some `remove),
      Arg.(value & pos_all string [] & Arg.info [])
    else
      mk_subcommands commands in
  let edit =
    mk_flag ["e";"edit"] "With $(opam pin add), edit the opam file as with \
                          `opam pin edit' after pinning." in
  let kind =
    let help = "Set the kind of pinning. Must be one of version, \
                path, git, hg or darcs." in
    let doc = Arg.info ~docv:"KIND" ~doc:help ["k";"kind"] in
    let kinds = [
      "git"    , `git;
      "darcs"  , `darcs;
      "version", `version;
      "path"   , `local;
      "local"  , `local;
      "rsync"  , `local;
      "hg"     , `hg
    ] in
    Arg.(value & opt (some & enum kinds) None & doc) in
  let no_act =
    mk_flag ["n";"no-action"]
      "Just record the new pinning status, don't prompt for \
       install / reinstall / removal"
  in
  let dev_repo =
    mk_flag ["dev-repo"] "Pin to the package's upstream source for the latest \
                          development version"
  in
  let guess_name path =
    let open OpamFilename.OP in
    let opamf = path / "opam" // "opam" in
    let opamf = if OpamFilename.exists opamf then opamf else path // "opam" in
    if OpamFilename.exists opamf then
      try match OpamFile.OPAM.(name_opt (read opamf)) with
        | Some name -> name
        | None -> raise Not_found
      with e -> OpamMisc.fatal e; raise Not_found
    else raise Not_found
  in
  let pin global_options kind edit no_act dev_repo command params =
    apply_global_options global_options;
    let action = not no_act in
    match command, params with
    | Some `list, [] | None, [] -> `Ok (Client.PIN.list ())
    | Some `remove, [n] ->
      `Ok (Client.PIN.unpin ~action (OpamPackage.Name.of_string n))
    | Some `edit, [n]  ->
      `Ok (Client.PIN.edit ~action (OpamPackage.Name.of_string n))
    | Some `add, [name] when dev_repo ->
      `Ok (Client.PIN.pin (OpamPackage.Name.of_string name) ~edit ~action None)
    | Some `add, [path] when not dev_repo ->
      (try
         let name = guess_name (OpamFilename.Dir.of_string path) in
         let pin_option = pin_option_of_string ~kind:`local path in
         `Ok (Client.PIN.pin name ~edit ~action (Some pin_option))
      with Not_found ->
        `Error (false, Printf.sprintf
                  "No valid package description found at path %s.\n\
                   Please supply at least a package name \
                   (e.g. `opam pin add NAME PATH')"
                  path))
    | Some `add, [name; target] ->
      let name = OpamPackage.Name.of_string name in
      let pin_option = pin_option_of_string ?kind:kind target in
      `Ok (Client.PIN.pin name ~edit ~action (Some pin_option))
    | command, params -> bad_subcommand "pin" commands command params
  in
  Term.ret
    Term.(pure pin
          $global_options $kind $edit $no_act $dev_repo $command $params),
  term_info "pin" ~doc ~man

let source_doc = "Get the source of an OPAM package."
let source =
  let doc = source_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Downloads the source for a given package to a local directory, \
        for developpement, bug fixing or documentation purposes."
  ] in
  let atom =
    Arg.(required & pos 0 (some atom) None & info ~docv:"PACKAGE" []
           ~doc:"A package name with an optional version constraint")
  in
  let dev_repo =
    mk_flag ["dev-repo"]
      "Get the latest version-controlled source rather than the \
       release archive" in
  let pin =
    mk_flag ["pin"]
      "Pin the package to the downloaded source (see `opam pin')." in
  let dir =
    mk_opt ["dir"] "DIR" "The directory where to put the source."
      Arg.(some dirname) None in
  let source global_options atom dev_repo pin dir =
    apply_global_options global_options;
    let open OpamState.Types in
    let t = OpamState.load_state "source" in
    let nv =
      OpamPackage.Set.max_elt
        (OpamPackage.Set.filter (OpamFormula.check atom) t.packages) in
    let dir = match dir with
      | Some d -> d
      | None -> OpamFilename.OP.(OpamFilename.cwd () / OpamPackage.to_string nv)
    in
    if OpamFilename.exists_dir dir then
      OpamGlobals.error_and_exit
        "Directory %s already exists. Please remove it or use option `--dir'"
        (OpamFilename.Dir.to_string dir);
    if dev_repo then (
      match OpamFile.OPAM.dev_repo (OpamState.opam t nv) with
      | None ->
        OpamGlobals.error_and_exit
          "Version-controlled repo for %s unknown \
           (\"dev-repo\" field missing from metadata)"
          (OpamPackage.to_string nv)
      | Some pin ->
        let address = match pin with
          | Git p | Darcs p | Hg p -> p
          | _ ->
            OpamGlobals.error_and_exit "Bad \"dev_repo\" field %S for %s"
              (string_of_pin_option pin) (OpamPackage.to_string nv)
        in
        let kind =
          match repository_kind_of_pin_kind (kind_of_pin_option pin) with
          | Some k -> k
          | None -> assert false
        in
        OpamGlobals.error "%s" (OpamFilename.Dir.to_string dir);
        OpamFilename.mkdir dir;
        match OpamRepository.pull_url kind nv dir None [address] with
        | Not_available u -> OpamGlobals.error_and_exit "%s is not available" u
        | Result _ | Up_to_date _ -> ()
    ) else (
      OpamGlobals.msg "Downloading archive of %s...\n"
        (OpamPackage.to_string nv);
      OpamAction.download_package t nv;
      OpamAction.extract_package t nv;
      OpamFilename.move_dir
        ~src:(OpamPath.Switch.build t.root t.switch nv)
        ~dst:dir;
      OpamGlobals.msg "Successfully extracted to %s\n"
        (OpamFilename.Dir.to_string dir)
    );

    if pin then
      let pin_option =
        pin_option_of_string ~kind:`local (OpamFilename.Dir.to_string dir) in
      Client.PIN.pin (OpamPackage.name nv) (Some pin_option)
  in
  Term.(pure source
        $global_options $atom $dev_repo $pin $dir),
  term_info "source" ~doc ~man

(* HELP *)
let help =
  let doc = "Display help about OPAM and OPAM commands." in
  let man = [
    `S "DESCRIPTION";
    `P "Prints help about OPAM commands.";
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  let help man_format cmds topic = match topic with
    | None       -> `Help (`Pager, None)
    | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
      | `Ok t -> `Help (man_format, Some t) in

  Term.(ret (pure help $Term.man_format $Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default =
  let doc = "source-based OCaml package management" in
  let man = [
    `S "DESCRIPTION";
    `P "OPAM is a package manager for OCaml. It uses the powerful mancoosi \
        tools to handle dependencies, including support for version \
        constraints, optional dependencies, and conflict management.";
    `P "It has support for different remote repositories such as HTTP, rsync, git, \
        darcs and mercurial. It handles multiple OCaml versions concurrently, and is \
        flexible enough to allow you to use your own repositories and packages \
        in addition to the central ones it provides.";
    `P "Use either $(b,opam <command> --help) or $(b,opam help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage global_options =
    apply_global_options global_options;
    OpamGlobals.msg
      "usage: opam [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
       \n\
       The most commonly used opam commands are:\n\
      \    init         %s\n\
      \    list         %s\n\
      \    show         %s\n\
      \    install      %s\n\
      \    remove       %s\n\
      \    update       %s\n\
      \    upgrade      %s\n\
      \    config       %s\n\
      \    repository   %s\n\
      \    switch       %s\n\
      \    pin          %s\n\
       \n\
       See 'opam help <command>' for more information on a specific command.\n"
      init_doc list_doc show_doc install_doc remove_doc update_doc
      upgrade_doc config_doc repository_doc switch_doc pin_doc in
  Term.(pure usage $global_options),
  Term.info "opam"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:global_option_section
    ~doc
    ~man

let make_command_alias cmd ?(options="") name =
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s%s)." orig options in
  let man = [
    `S "DESCRIPTION";
    `P (Printf.sprintf "$(b,$(mname) %s) is an alias for $(b,$(mname) %s%s)."
          name orig options);
    `P (Printf.sprintf "See $(b,$(mname) %s --help) for details."
          orig);
  ] in
  term,
  Term.info name
    ~docs:"COMMAND ALIASES"
    ~doc ~man

let commands = [
  init;
  list; search;
  show; make_command_alias show "info";
  install;
  remove; make_command_alias remove "uninstall";
  reinstall;
  update; upgrade;
  config;
  repository; make_command_alias repository "remote";
  switch;
  pin (); make_command_alias (pin ~unpin_only:true ()) ~options:" remove" "unpin";
  source;
  help;
]

(* Handle git-like plugins *)
let check_and_run_external_commands () =
  let len = Array.length Sys.argv in
  if len > 1 then (
    let opam = Filename.basename Sys.argv.(0) in
    let name = Sys.argv.(1) in
    let command = opam ^ "-" ^ name in
    if
      String.length name > 1
      && name.[0] <> '-'
      && List.for_all (fun (_,info) -> Term.name info <> name) commands
      && OpamSystem.command_exists command
    then
      let args = Array.sub Sys.argv 1 (len - 1) in
      args.(0) <- command;
      Unix.execvp command args
  ) else
    ()

let run default commands =
  Sys.catch_break true;
  let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  try
    check_and_run_external_commands ();
    match Term.eval_choice ~catch:false default commands with
    | `Error _ -> exit 1
    | _        -> exit 0
  with
  | OpamGlobals.Exit 0 -> ()
  | OpamGlobals.Exec (cmd,args,env) ->
    Unix.execvpe cmd args env
  | e                  ->
    if !OpamGlobals.verbose then
      Printf.eprintf "'%s' failed.\n" (String.concat " " (Array.to_list Sys.argv));
    let exit_code = ref 1 in
    begin match e with
      | OpamGlobals.Exit i ->
        exit_code := i;
        if !OpamGlobals.debug && i <> 0 then
          Printf.eprintf "%s" (OpamMisc.pretty_backtrace e)
      | OpamSystem.Internal_error _
      | OpamSystem.Process_error _ ->
        Printf.eprintf "%s\n" (Printexc.to_string e);
        Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
      | Sys.Break -> exit_code := 130
      | Failure msg ->
        Printf.eprintf "Fatal error: %s\n" msg;
        Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
      | _ ->
        Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
        Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    end;
    exit !exit_code
