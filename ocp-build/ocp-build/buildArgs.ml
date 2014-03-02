(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)


(* open BuildBase *)
(* open Stdlib2 *)
(* TODO: we should save the version of ocaml used to build a project,
   so that we can detect changes and ask for a clean before building.
   Can we access the magic used by every compiler ? (we can compile an
   empty file in bytecode and native code) We could cache this
   information using the uniq identifier of the executable (would not
   work with wrappers).
*)

(* TODO
  We could force packages with missing dependencies to still be computed,
  since it is still possible that these missing dependencies are not used
  in a particular compilation scheme.
*)

(* open OcpLang *)
open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions

type subcommand = {
  sub_name : string;
  sub_arg_list : (string * Arg.spec * string) list;
  sub_arg_anon : (string -> unit) option;
  sub_arg_usage : string list;
  sub_action : (unit -> unit);
  sub_help : string;
}


let _ = DebugVerbosity.add_submodules "B" [ "BuildMain" ]

let initial_verbosity =
  try
    Some (int_of_string (Sys.getenv "OCPBUILD_VERBOSITY"))
  with _ -> None

let version = BuildVersion.version


let t0 = MinUnix.gettimeofday ()

let time s f x =
  if !time_arg then
    let t0 = MinUnix.gettimeofday () in
    let y = f x in
    let t1 = MinUnix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x

(* TODO: check which options are still settable *)
let add_external_projects_arg = ref ([] : string list)
let oasis_arg = ref false
let save_project = ref false
let print_conflicts_arg = ref false
let list_projects_arg = ref false
let meta_verbose_arg = ref false
let build_dir_basename_arg = ref "_obuild"
type arch_arg = ArchNone | Arch of string
let arch_arg = ref ArchNone

let tests_arg = ref false
let benchmarks_arg = ref false
let list_byte_targets_arg = ref false
let list_asm_targets_arg = ref false
let configure_arg = ref false

(* if [query_global] is set, we don't load the project
.ocp files and stop immediatly after replying to queries. *)
let query_global = ref false
let query_root_dir = ref false
let query_install_dir = ref (None : string option)
let query_include_dir = ref ([] : string list)
let query_has_package_args = ref ([] : string list)

let delete_orphans_arg = ref DeleteOrphanFilesAndDirectories
let list_installed_arg = ref false
let install_arg = ref false
let uninstall_arg = ref false

let auto_uninstall = ref true









let best_indent = 30
let second_indent = String.make 30 ' '
let rec arg_align list =
  match list with
  [] -> []
  | ("", arg_usage, help) :: tail ->
    ("", arg_usage, help) :: arg_align tail
  | (string, arg_usage, help) :: tail ->
    let help = OcpString.split help '\n' in
    let help =
      match help with
      [] -> []
      | first_line :: other_lines ->
        let first_word, next_words = OcpString.cut_at first_line ' ' in
        let next_words = String.capitalize next_words in
        let len_string = String.length string in
        let len_1 = String.length first_word in
        if len_string + len_1 + 5 > best_indent then
          first_word :: (second_indent ^ next_words) ::
            List.map (fun s -> second_indent ^ s) other_lines

        else
          (first_word ^ (String.make (best_indent - len_string - len_1 - 3) ' ')
          ^ next_words) ::
            List.map (fun s -> second_indent ^ s) other_lines
    in
    (string, arg_usage, String.concat "\n" help) :: arg_align tail

let arg_anon_none s = raise (Arg.Bad s)
let arg_anon s =  targets_arg := s :: !targets_arg








(*
let save_arguments_arg = ref false
let meta_dirnames_arg = ref []


let dont_load_project_arg = ref false

let options_arg_list = BuildOptions.arg_list ()

let init_arg = ref false
let root_arg = ref false
let project_arg = ref false

let root_action () =
  root_arg := true;
  init_arg := true;
  save_arguments_arg := true;
  save_project := true

let oasis_action () =
  oasis_arg := true

let configure_action () =
  configure_arg := true

let project_action () =
  project_arg := true

let build_action () =
  ()

let install_action () =
  install_arg := true

let test_action () =
  tests_arg := true

let clean_action () =
  clean_arg := true

let query_action () =
  query_global := true

let uninstall_action () =
  uninstall_arg := true

exception PrintShortArgList
exception PrintLongArgList

(*
let short_arg_list = [
  "-h", Arg.Unit (fun _ -> raise PrintShortArgList),
  " Print short help";
  "-help", Arg.Unit (fun _ -> raise PrintShortArgList),
  " Print short help";
  "--help", Arg.Unit (fun _ -> raise PrintLongArgList),
  " Print long help (with compatibility options)";
  "-long-help", Arg.Unit (fun _ -> raise PrintLongArgList),
  " Print long help (with compatibility options)";

  "-version", Arg.Unit (fun () ->
    Printf.printf "%s\n%!" BuildVersion.version;
    BuildMisc.clean_exit 0
  ),
  " Print version information";

  "-about", Arg.Unit (fun () ->
    Printf.printf "ocp-build : OCaml Project Builder\n";
    Printf.printf "\tversion: %s\n" BuildVersion.version;
    Printf.printf "\tdescription: %s\n" BuildVersion.description;
    List.iter (fun author ->
      Printf.printf "\tauthor: %s\n" author) BuildVersion.authors;
    Printf.printf "\tlicense: %s\n" BuildVersion.license;
    Printf.printf "%!";
    BuildMisc.clean_exit 0
  ),
  " Print version information";

]


let arg_list = short_arg_list @ [

  "-init", Arg.Unit (fun () ->
    init_arg := true;
    save_arguments_arg := true;
    save_project := true
  ), " Create the ocp-build.root file\n  in the current directory";

  "-root", Arg.Unit (fun () ->
    init_arg := true;
    save_arguments_arg := true;
    save_project := true
  ), " Create the ocp-build.root file\n  in the current directory";

  "-configure", Arg.Unit (fun () ->
    configure_arg := true;
  ),
  " Configure or update project options";

  "-query-has", Arg.String (fun s ->
    query_global := true;
    query_has_package_args := s :: !query_has_package_args
  ),
  "PACKAGE Verify that package is currently installed";

(*
  "-local-only", Arg.Set local_only_arg,
  " Don't scan external directories";
*)

  "-query", Arg.Set query_global, " Query environment";

  "-no-project", Arg.Set dont_load_project_arg, " Don't load project info";

  "-query-root-dir", Arg.Unit (fun _ ->
    query_root_dir := true; query_global := true),
  " Return current root dir";

  "-query-install-dir", Arg.String (fun s ->
    query_install_dir := Some s),
  "PACKAGE print dir where installed";

  "-query-include-dir", Arg.String (fun s ->
    query_include_dir := s :: !query_include_dir),
  "PACKAGE Print dir where compiled";


  "-define", Arg.String (fun s ->
    let (name, valeur) = OcpString.cut_at s '=' in
      match valeur with
        | "true" | "" ->
          BuildOCPVariable.set_global s (BuildOCPVariable.plist_of_bool true)
        | "false" ->
          BuildOCPVariable.set_global s (BuildOCPVariable.plist_of_bool false)
        | _ ->
          BuildOCPVariable.set_global s (BuildOCPVariable.plist_of_string valeur)
  ),
  "OPTION define an initial option";

  "-add-external-project", Arg.String (fun dir ->
    add_external_projects_arg := dir :: !add_external_projects_arg
  ),
  "DIRECTORY Add directory to scanned dirs";

  "-installed", Arg.Set list_installed_arg,
  " List installed packages";





  "-print-incomplete-meta", Arg.Set meta_verbose_arg,
  " Print incomplete META packages";
  "-print-conflicts", Arg.Set print_conflicts_arg,
  " Print conflicts between packages";

  "-add-meta-dir",
  Arg.String (fun s -> meta_dirnames_arg := s :: !meta_dirnames_arg),
  "DIRECTORY Load META files from this directory";

  "-install", Arg.Set install_arg,
  " Install binaries and libraries";

  "-uninstall", Arg.Set uninstall_arg,
  " Uninstall given packages (installed by ocp-build)";

  "-no-auto-uninstall", Arg.Unit (fun () -> auto_uninstall := false),
  " If trying to install already installed packages, fail rather than uninstall them";

  "-clean", Arg.Set clean_arg, " Clean all compiled files and exit";

  "-obuild", Arg.String (fun s -> build_dir_basename_arg := s),
  "DIRECTORY change _obuild directory";
  "-no-sanitize", Arg.Unit (fun () -> delete_orphans_arg := KeepOrphans),
  " Fail rather than remove stale objects from _obuild";

  "-list-ocp-files", Arg.Set list_ocp_files, " List all .ocp files found";
  "-k", Arg.Clear stop_on_error_arg,  " Continue after errors";

  "-tests", Arg.Set tests_arg,
  " Build and run tests";
  "-benchmarks", Arg.Unit (fun () -> tests_arg := true; benchmarks_arg := true),
  " Build and run benchmarks";

  "-list-all-packages", Arg.Set list_projects_arg,
  " List all packages";
  "-list-targets", Arg.Unit (fun _ ->
    list_byte_targets_arg := true;
    list_asm_targets_arg := true),
  " List all targets";
  "-list-byte-targets", Arg.Set list_byte_targets_arg,
  " List bytecode targets";
  "-list-asm-targets", Arg.Set list_asm_targets_arg,
  " List native targets";

  "-time", Arg.Set time_arg,
  " Print timings";

  "-library-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name LibraryPackage
      (File.of_string "."); BuildMisc.clean_exit 0;
  ), "OCP_FILE Auto-generate a .ocp file for a library";

    "-oasis", Arg.Unit oasis_action,
    " Compatibility mode with oasis";

  "-program-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name ProgramPackage
      (File.of_string "."); BuildMisc.clean_exit 0;
  ),
  "OCP_FILE Auto-generate a .ocp file for a program";

]
  @ [
    "", Arg.Unit (fun _ -> ()),
    String.concat "\n" [
      "-------------------------------------------------------------------";
      "Options under this line are used to set options";
      ""
    ]]
  @ options_arg_list
    @
[
    BuildOptions.shortcut_arg "-v" "-verbosity" options_arg_list ;
]

(* @

    [
    "", Arg.Unit (fun _ -> ()),
    String.concat "\n" [
      "-------------------------------------------------------------------";
      "Options under this line are used when checking";
      "OCaml config";
      ""
    ]] @
[
    BuildOptions.shortcut_arg "-asm" "-native" ocaml_arg_list;
    BuildOptions.shortcut_arg "-byte" "-bytecode" ocaml_arg_list;
] @
  ocaml_arg_list
  @ [
    "", Arg.Unit (fun _ -> ()),
    String.concat "\n" [
      "-------------------------------------------------------------------";
      "Options under this line will be saved if you";
      "use \"-save-prefs\".";
      ""
    ];
    BuildOptions.shortcut_arg "-scan" "-autoscan" user_arg_list;
  ]
  @
    user_arg_list
    *)
  @ [
    "", Arg.Unit (fun _ -> ()),
    String.concat "\n" [
      "-------------------------------------------------------------------";
    ]]
*)
*)

(*
let arg_usage =
  String.concat "\n"
    ["ocp-build [options] targets : build OCaml targets";
     "";
     "A project is composed of packages, described by .ocp files";
     "";
     "The following options are available:"
    ]

*)

(*
(*

ocp-build root       (* creates project root directory *)
ocp-build configure  (* configures project *)
ocp-build project    (* query project information *)
ocp-build build      (* build project *)
ocp-build install    (* install packages *)
ocp-build test       (* build test packages and execute them *)
ocp-build clean      (* clean project *)

ocp-build query      (* query environment information *)
ocp-build uninstall  (* uninstall installed packages *)


let arg_map =
  let map = ref StringMap.empty in
  List.iter (fun (arg, action, help) ->
    if arg <> "" then
      map := StringMap.add arg (action, help) !map
  ) arg_list;
  !map


let subcommands_list = ref []
let subcommands_map = ref StringMap.empty

let add_sub_command sub_name sub_help sub_action arg_list arg_anon arg_usage =

  let arg_list = List.map (fun (name, key) ->
      if name = "" then
        (name, Arg.Unit (fun () -> ()), key)
      else
        try
          let (action, help) = StringMap.find key arg_map in
          (name, action, help)
        with Not_found ->
          Printf.eprintf "Warning: argument %S disabled as key %S not found\n%!"
            name key;
          ("", Arg.Unit (fun () -> ()),
           Printf.sprintf "Arg %S disabled" name)
    ) arg_list in

  let arg_usage = String.concat "\n"
      (
        [ "Subcommand:";  "" ] @
        arg_usage @ [ ""; "Available options in this mode:";]) in

  subcommands_list := (sub_name, sub_help) :: !subcommands_list;
  subcommands_map := StringMap.add sub_name
      (sub_action, (arg_list, arg_anon, arg_usage) ) !subcommands_map

let arg_anon s = targets_arg := s :: !targets_arg


let dup s = s,s

let _ =

  add_sub_command "root" "Set project root directory "
    root_action [] arg_anon_none
    [
      "      ocp-build root";
      "";
      "Set the current directory as the project root.";
      "When working on a project, ocp-build always go to the root";
      "of the project, from any sub-directory." ];


  add_sub_command "configure" "Configure project options"
    configure_action [
    "-asm", "-project-asm";
    "-byte", "-project-byte";
    "-autoscan", "-project-autoscan";
    "-digest", "-project-digest";
    "-ocamlfind", "-project-use-ocamlfind";
    "-njobs", "-project-njobs";
    "-verbosity", "-project-verbosity";
    "-no-asm", "-project-no-asm";
    "-no-byte", "-project-no-byte";
    "-no-autoscan", "-project-no-autoscan";
    "-no-digest", "-project-no-digest";
    "-no-ocamlfind", "-project-no-use-ocamlfind";

    "-install-destdir", "-project-install-destdir";
    "-install-bin", "-project-install-bin";
    "-install-lib", "-project-install-lib";
    "-install-doc","-project-install-doc";
    "-install-data", "-project-install-data";
    dup "";
  ] arg_anon_none
    [ "     ocp-build configure [OPTIONS]";
      "";
      "Configure permanent options for this project";
    ];


  add_sub_command "project" "Query project information"
    project_action [
      dup "-print-conflicts";
      dup "-print-incomplete-meta";
    dup "-list-all-packages";
    "-v", "-verbosity";
  ] arg_anon_none
    [ "     ocp-build project [OPTIONS]";
      "";
      "Query information on this project (nothing interesting yet)";
    ];

  add_sub_command "build" "Build project"
    build_action [
    dup "-k";
    (* dup "-list-waiting-targets"; *)
    "", " \nYou can override the configuration also:";
    "", " ";
    "-njobs", "-njobs";
    "-v", "-verbosity";
    dup "-verbosity";
    dup "-asm";
    dup "-byte";
    dup "-scan";
    "-ocamlfind", "-use-ocamlfind";
    dup "-no-asm";
    dup "-no-byte";
    dup "-no-scan";
    "-no-ocamlfind", "-no-use-ocamlfind";
    dup "-time";
    dup "-print-conflicts";
    dup "-print-incomplete-meta";
  ] arg_anon
    [ "     ocp-build build [OPTIONS] [PACKAGES]";
      "";
      "Build this project";
    ];


  add_sub_command "install" "Install packages"
    install_action [
    "", " \nYou can override the configuration also:";
    "", " ";
    dup "-install-destdir";
    dup "-install-bin";
    dup "-install-lib";
    dup "-install-doc";
    dup "-install-data";
  ] arg_anon
    [ "     ocp-build install [OPTIONS] [PACKAGES]";
      "";
      "Install build packages. If no packages are specified,";
      "all packages are installed.";
    ];

  add_sub_command "test" "Build and run package tests"
    test_action [] arg_anon
    [ "     ocp-build test [OPTIONS] [PACKAGES]";
      "";
      "Build and run packages tests. If no PACKAGES are specified,";
      "tests from all packages are executed.";
    ];


  add_sub_command "clean" "Clean build artefacts"
    clean_action [] arg_anon_none
    [ "     ocp-build project [OPTIONS]";
      "";
      "Clean build artefacts in this project";
    ];


  add_sub_command "query" "Query environment information"
    query_action []
    (fun s -> query_include_dir := s :: !query_include_dir)
    [ "     ocp-build query [OPTIONS] [PACKAGES]";
      "";
      "Query information on the global OCaml environment";
    ];


  add_sub_command "uninstall" "Uninstall packages"
    uninstall_action [
      dup "-install-destdir";
    dup "-install-lib";
  ] arg_anon
    [ "     ocp-build uninstall [OPTIONS] [PACKAGES]";
      "";
      "Uninstall packages previously installed by ocp-build";
    ];

  add_sub_command "oasis" "Oasis compatibility"
    oasis_action []
    arg_anon
    [ "     ocp-build oasis [OPTIONS] [PACKAGES]";
      "";
      "Try to simulate oasis behavior for building packages";
    ];

  ()

let arg_usage =
  String.concat "\n"
    (
      [
        "ocp-build [OPTIONS] SUBCOMMAND [OPTIONS] [PACKAGES]";
        "";
        "The following subcommands are available:";
          "";
      ] @
        List.map (fun (name, help) ->
          Printf.sprintf "   ocp-build %-20s %s" name help
        ) (List.rev !subcommands_list)
          @
            [
              "";
              "Use 'ocp-build SUBCOMMAND -help' to get more information";
              "";
              "If you don't use a subcommand, the following raw options";
              "available:";
              "";
            ]
    )

exception SubCommandNotFound
let parse_args () =
  let (arg_list, arg_anon, arg_usage) =
    try
      if Array.length Sys.argv > 1 then
        try
          let arg1 = Sys.argv.(1) in
          let (action, subcommand) = StringMap.find arg1 !subcommands_map in
          action ();
          Sys.argv.(1) <- "ocp-build " ^ arg1;
          incr Arg.current;
          subcommand
        with Not_found -> raise SubCommandNotFound
      else raise SubCommandNotFound
    with SubCommandNotFound ->
      (arg_list
       @ [
       ], arg_anon, arg_usage)
  in
  let arg_list = arg_align arg_list in
  try
    Arg.parse arg_list (fun s -> targets_arg := s :: !targets_arg) arg_usage;
    List.rev !targets_arg
  with
  | PrintShortArgList ->
    Arg.usage short_arg_list arg_usage; exit 0
  | PrintLongArgList ->
    Arg.usage arg_list arg_usage; exit 0

*)
*)
