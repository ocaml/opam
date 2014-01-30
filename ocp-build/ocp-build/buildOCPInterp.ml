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
open BuildOCPVariable
open BuildOCPTree
open BuildOCPTypes


let continue_on_ocp_error = ref false

type prim = env list -> env -> plist

type config = {
  config_env : BuildOCPVariable.env;
  config_configs : set_option list StringMap.t;
  config_dirname : string;
  config_filename : string;
  config_filenames : (string * Digest.t option) list;
}

type state = {
  mutable packages : package IntMap.t;
  mutable npackages : int;
  mutable config_files : Digest.t StringMap.t;
}


let config_get config name =
  get [config.config_env] name

let meta_options = [
  "o",     [ "dep"; "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "oc",    [ "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "byte",  [  "bytecomp"; "bytelink"; ];
  "asm",   [  "asmcomp"; "asmlink"; ];
  "comp",  [  "bytecomp"; "asmcomp"; ];
  "link",  [  "bytelink"; "asmlink"; ];
  "debugflag", [ "debugflag"; "asmdebug"; "bytedebug"; ];
]


let initial_state () =
{ packages = IntMap.empty; npackages = 0; config_files = StringMap.empty; }

let copy_state s =
  { s with packages = s.packages }

let final_state state =
  if state.npackages = 0 then [||] else
    Array.init state.npackages (fun i ->
      IntMap.find i state.packages
    )

let new_package pj name dirname filename filenames kind options =
  let package_id = pj.npackages in
  pj.npackages <- pj.npackages + 1;
  let pk = {
    package_source_kind = "ocp";
    package_id = package_id;
    package_auto = None;
    package_version = "";
    package_loc = (-1);
    package_filename = filename;
    package_filenames = filenames;
    package_node = LinearToposort.new_node ();
    package_added = false;
    package_requires = [];
    package_requires_map = IntMap.empty;
    package_name = name;
    package_provides = name;
    package_type = kind;
    package_validated = false;
    package_dirname = dirname;
    package_deps_map = StringMap.empty;
    package_options = options;
  } in
  pj.packages <- IntMap.add pk.package_id pk pj.packages;
  pk

let empty_config = {
  config_env = empty_env;
  config_configs = StringMap.empty;
  config_dirname = "";
  config_filename = "";
  config_filenames = [];
}

let generated_config = {
  empty_config with
  config_env = BuildOCPVariable.set_bool empty_config.config_env "generated" true;
}

(*
let configs = Hashtbl.create 17
*)

let define_config config config_name options =
  { config with config_configs = StringMap.add config_name options config.config_configs }

let find_config config config_name =
  try
    StringMap.find config_name config.config_configs
  with Not_found ->
    failwith (Printf.sprintf "Error: configuration %S not found\n" config_name)

let new_package_dep pk s env =
    try
      StringMap.find s pk.package_deps_map
    with Not_found ->
      let dep = {
        dep_project = s;
        dep_link = false;
        dep_syntax = false;
        dep_optional = false;
        dep_options = env;
      }
      in
      pk.package_deps_map <- StringMap.add s dep pk.package_deps_map;
      dep

let add_project_dep pk s options =
  let dep = new_package_dep pk s options in
  dep.dep_link <- get_bool_with_default [options] "tolink" true;

  begin
    try
      dep.dep_optional <- get_bool [options] "optional"
    with Var_not_found _ -> ()
  end;
(*  Printf.eprintf "add_project_dep for %S = %S with link=%b\n%!"
    pk.package_name s dep.dep_link; *)
()

(* We want to check the existence of the dirname of a package as soon
 as possible, so that we can disable it and enable another one.
 Actually, this should only be done for .ocpi files, i.e. installed files,
 for which we should use another loading phase.
*)

let check_package pk =
  let options = pk.package_options in

    if get_bool_with_default [options] "enabled" true &&
       not ( BuildMisc.exists_as_directory pk.package_dirname ) then begin
      (* TODO: we should probably do much more than that, i.e. disable also a
         package when some files are missing. *)
      Printf.eprintf "Warning: directory %S for package does not exist:\n"
        pk.package_dirname;
      Printf.eprintf "  Package %S in %S disabled.\n%!"
        pk.package_name pk.package_filename;
      pk.package_options <- set_bool options "enabled" false;
    end else begin

      pk.package_version <- get_string_with_default [pk.package_options]
          "version"  "0.1-alpha";
      List.iter (fun (s, options) ->
        add_project_dep pk s options
      ) (try get [pk.package_options] "requires" with Var_not_found _ ->
        (*    Printf.eprintf "No 'requires' for package %S\n%!" name; *)
        []
      )
    end

let define_package pj name config kind =
  let dirname =
    try
      let list = get_strings [config.config_env] "dirname"  in
      BuildSubst.subst_global (String.concat Filename.dir_sep list)
    with Var_not_found _ ->
      config.config_dirname
  in
  let dirname = if dirname = "" then "." else dirname in
  new_package pj name
      dirname config.config_filename config.config_filenames kind config.config_env


let read_config_file (pj:state) filename =
  try
    let content = File.string_of_file filename in
    let digest = Digest.string content in
    begin
      try
        let digest2 = StringMap.find filename pj.config_files in
        if digest <> digest2 then begin
          Printf.eprintf "File %S modified during built. Exiting.\n%!" filename;
          exit 2
        end
      with Not_found ->
        pj.config_files <- StringMap.add filename digest pj.config_files
    end;
    Some (content, digest)
  with e ->
    Printf.eprintf "Error: file %S does not exist.\n%!" filename;
    None


let primitives = ref StringMap.empty
let add_primitive s help ( f : env list -> env -> plist) =
  let f envs env =
    try
      f (env :: envs) env
    with e ->
      Printf.eprintf "Warning: exception raised while running primitive %S\n%!" s;
      raise e
  in
  primitives := StringMap.add s (f, help) !primitives

let add_function s help f =
  let f _ env =
    try
      f [ env ] env
    with e ->
      Printf.eprintf "Warning: exception raised while running primitive %S\n%!" s;
      raise e
  in
  primitives := StringMap.add s (f, help) !primitives

let rec translate_toplevel_statements pj config list =
  match list with
    [] -> config
  | stmt :: list ->
    let config = translate_toplevel_statement pj config stmt in
    translate_toplevel_statements pj config list

and translate_toplevel_statement pj config stmt =
  match stmt with
  | StmtDefineConfig (config_name, options) ->
    let config_name = translate_string_expression config [config.config_env] config_name in
    define_config config config_name options
  (*  (fun old_options -> translate_options old_options options); *)
  | StmtDefinePackage (package_type, library_name, simple_statements) ->
    let library_name = translate_string_expression config [config.config_env] library_name in
    begin
      try
        let config = translate_statements pj config simple_statements in
        let (_ : package) = define_package pj library_name config package_type in
        ()
      with e ->
        Printf.eprintf "Error while interpreting package %S:\n%!" library_name;
        raise e
    end;
    config
  | StmtBlock statements ->
    ignore (translate_toplevel_statements pj config statements);
    config
  | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition config [config.config_env] cond then
        translate_toplevel_statements pj config ifthen
      else
        match ifelse with
          None -> config
        | Some ifelse ->
          translate_toplevel_statements pj config ifelse
    end
  | StmtInclude (filename, ifthen, ifelse) ->
    let filename = translate_string_expression config [config.config_env] filename in
    if Filename.check_suffix filename ".ocp" then begin
      Printf.eprintf "Warning, file %S, 'include %S', file argument should not\n"
        config.config_filename filename;
      Printf.eprintf "  have a .ocp extension, as it will be loaded independantly\n%!";
    end;
    let filename = BuildSubst.subst_global filename in
    let filename = if Filename.is_relative filename then
        Filename.concat config.config_dirname filename
      else filename
    in
    let (ast, digest) =
      match read_config_file pj filename with
      None -> None, None
      | Some (content, digest) ->
        Some (BuildOCPParse.read_ocamlconf filename content), Some digest
    in
    let old_filename = config.config_filename in
    let config = { config with
                   config_filenames = (filename, digest) :: config.config_filenames;
                 }
    in
    begin
      match ast, ifelse with
      | Some ast, _ ->
        let config = translate_toplevel_statements pj { config with config_filename = filename } ast in
        translate_toplevel_statements pj { config with config_filename = old_filename } ifthen
      | None, None -> config
      | None, Some ifelse ->
        translate_toplevel_statements pj config ifelse
    end

  | _ -> translate_simple_statement pj config stmt

and translate_statements pj config list =
  match list with
    [] -> config
  | stmt :: list ->
    let config = translate_statement pj config stmt in
    translate_statements pj config list

and translate_statement pj config stmt =
  match stmt with
  | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition config [config.config_env] cond then
        translate_statements pj config ifthen
      else
        match ifelse with
          None -> config
        | Some ifelse ->
          translate_statements pj config ifelse
    end
  | _ -> translate_simple_statement pj config stmt

and translate_simple_statement pj config stmt =
  match stmt with
  | StmtOption option ->
    { config with config_env =
                    translate_option config
                      [] config.config_env option }
  (*    | StmtSyntax (syntax_name, camlpN, extensions) -> config *)
  | StmtIfThenElse _
  | StmtBlock _
  | StmtInclude _
  | StmtDefinePackage _
  | StmtDefineConfig _ -> assert false


and translate_condition config envs cond =
  match cond with
  | IsEqual (exp1, exp2) ->
    let exp1 = translate_expression config envs exp1 in
    let exp2 = translate_expression config envs exp2 in
    exp1 = exp2

  | IsNonFalse exp ->
    let exp = try
      translate_expression config envs exp
    with _ -> []
    in
    exp <> []

  | NotCondition cond -> not (translate_condition config envs cond)
  | AndConditions (cond1, cond2) ->
    (translate_condition config envs cond1)
    && (translate_condition config envs cond2)
  | OrConditions (cond1, cond2) ->
    (translate_condition config envs cond1)
    || (translate_condition config envs cond2)

and translate_options config envs env list =
  match list with
    [] -> env
  | option :: list ->
    let env = translate_option config envs env option in
    translate_options config envs env list

and translate_option config envs env op =
  match op with
  | OptionConfigUse config_name ->
    let config_name = translate_string_expression config (env :: envs) config_name in
    translate_options config envs env (find_config config config_name)

  | OptionVariableSet (name, exp) ->

    (* TODO: global options *)
    let exp = translate_expression config (env :: envs) exp in
    let vars = try
      List.assoc name meta_options
    with Not_found -> [ name ]
    in
    List.fold_left (fun env name -> set env name exp) env vars

  | OptionVariableAppend (name, exp) ->
    (* TODO: global options *)

    let exp2 = translate_expression config (env :: envs) exp in

    let vars = try
      List.assoc name meta_options
    with Not_found -> [ name ]
    in
    List.fold_left (fun env name ->
      let exp1 = try get (env ::envs) name
      with Var_not_found _ ->
        failwith (Printf.sprintf "Variable %S is undefined (in +=)\n%!" name)
      in
      set env name (exp1 @ exp2)
    ) env vars

  | OptionIfThenElse (cond, ifthen, ifelse) ->
    begin
      if translate_condition config (env :: envs) cond then
        translate_option config envs env ifthen
      else
        match ifelse with
          None -> env
        | Some ifelse ->
          translate_option config envs env ifelse
    end
  | OptionBlock list -> translate_options config envs env list

and translate_string_expression config envs exp =
  match translate_expression config envs exp with
  [] | _ :: _ :: _ -> failwith "Single string expected"
  | [s , _] ->    s

and translate_expression config envs exp =
(*  Printf.eprintf "translate_expression\n%!"; *)
  match exp with

  | ExprString s -> [ s, empty_env ]

  | ExprPrimitive (s, args) ->
    let (f, _) = try StringMap.find s !primitives with
        Not_found ->
        failwith (Printf.sprintf "Could not find primitive %S\n%!" s)
    in
    f envs (translate_options config envs empty_env args)

  | ExprVariable name ->
    let exp = try get envs name
    with Var_not_found _ ->
      failwith (Printf.sprintf "Variable %S is undefined\n%!" name)
    in
    exp

  | ExprList list ->
    List.concat (List.map (translate_expression config envs) list)

  | ExprApply (exp, args) ->
    let exp = translate_expression config envs exp in
    List.map (fun (s, env) ->
      s, translate_options config envs env args
    ) exp

let read_ocamlconf pj config filename =
  let ast, digest =
    match read_config_file pj filename with
      None -> None, None
    | Some (content, digest) ->
      (try
        Some (BuildOCPParse.read_ocamlconf filename content)
      with BuildOCPParse.ParseError ->
        if not !continue_on_ocp_error then exit 2;
        None), Some digest
  in
  let config = { config with
                 config_dirname = Filename.dirname filename;
                 config_filename = filename;
                 config_filenames = (filename, digest) :: config.config_filenames;
               }
  in
  match ast with
  | None -> config
  | Some ast ->
    try
      translate_toplevel_statements pj config
        (StmtOption (OptionVariableSet("dirname", ExprString config.config_dirname)) ::  ast)
    with e ->
      Printf.eprintf "Error while interpreting file %S:\n%!" filename;
      Printf.eprintf "\t%s\n%!" (Printexc.to_string e);
      if not !continue_on_ocp_error then exit 2;
      config


open StringSubst


let rec eprint_plist indent list =
  match list with
    [] -> Printf.eprintf "%s[]\n" indent
  | list ->
    Printf.eprintf "%s[\n" indent;
    List.iter (fun (s, env) ->
      Printf.eprintf "%s  %S\n" indent s;
      if env <> empty_env then begin
        Printf.eprintf "%s  (\n" indent;
        eprint_env (indent ^ "  ") env;
        Printf.eprintf "%s  )\n" indent;
      end
    ) list;
    Printf.eprintf "%s]\n" indent;
    ()

and eprint_env indent env =
  iter (fun var v ->
    if v = true_value then
      Printf.eprintf "%s%s = true\n" indent var
    else
      match v with
      | [] ->
        Printf.eprintf "%s%s = []\n" indent var;
      | [s, env] when env = empty_env ->
        Printf.eprintf "%s%s = %S\n" indent var s;
      | _ ->
        Printf.eprintf "%s%s =\n" indent var;
        eprint_plist (indent ^ "  ") v
  ) env


let subst_basename filename =
  let basename = Filename.basename filename in
  try
    let pos = String.index basename '.' in
    String.sub basename 0 pos
  with Not_found -> basename

let filesubst = BuildSubst.create_substituter
    [
      "file", (fun (file, (env : env list) ) -> file);
      "basefile", (fun (file, env) -> Filename.basename file);
      "basename", (fun (file, env) -> subst_basename file);
      "dirname", (fun (file, env) -> Filename.dirname file);
      "extensions", (fun (file, env) ->
        try
          let pos = String.index file '.' in
          String.sub file pos (String.length file - pos)
        with Not_found -> "");
    ]

let _ =
  let subst_files envs to_file =

    let files = get_local envs "files" in
    let from_ext = get_strings_with_default envs "from_ext" [] in
    let keep = get_bool_with_default envs "keep_others" false in
    let files = List.fold_left (fun files (file, env) ->
        try
          let pos = String.index file '.' in
          if from_ext = [] || (
               let ext = String.sub file pos (String.length file - pos) in
               List.mem ext from_ext) then
            let file = BuildSubst.apply_substituter filesubst
                to_file (file,envs)
            in
            (* Printf.eprintf "subst to %S\n%!" file; *)
            (file, env) :: files
          else raise Not_found
        with Not_found ->
          if keep then
            (file,env) :: files
          else files
      ) [] files in
    List.rev files
  in

  let subst_file envs ( env : env) =
    let to_ext = get_strings_with_default envs "to_ext" [] in
    let to_file = match to_ext with
        [ to_ext ] -> "%{dirname}%/%{basename}%" ^ to_ext
      | _ ->
        try
          string_of_plist (get_local envs "to_file")
        with Var_not_found _ ->
          failwith "%subst_ext: to_ext must specify only one extension"
    in
    subst_files envs to_file
  in
  let subst_help =     [
      "Perform a substitution on a list of files";
      "ENV can contain:";
      "- files: the list of files";
      "- to_file: the destination, with substitutions";
      "- to_ext: an extension, if only the extension should be changed";
      "- from_ext: perform only on files ending with these extensions";
      "- keep_others: true if non-substituted files should be kept";
    ]  in
  add_primitive "subst_ext" subst_help subst_file;
  add_primitive "subst_file" subst_help subst_file;

  add_primitive "basefiles" [] (fun envs env ->
    subst_files envs "%{basefile}%"
  );

  add_primitive "path" []
    (fun envs env ->
      let path = get_strings envs "path" in
      let s =
        match path with
          [] -> ""
        | dirname :: other_files ->
          List.fold_left (fun path file ->
            Filename.concat path file
          ) dirname other_files
      in
      [ s, env ]
    );

  add_primitive "string" [
    "Returns the concatenation of a list of strings";
    "ENV must contain:";
    "- strings : the list of strings";
    "ENV can contain:";
    "- sep : a separator, to be added between strings";
  ]
    (fun envs env ->
      let path = get_strings envs "strings" in
      let sep = get_string_with_default envs "sep" "" in
      [ String.concat sep path, env ]
    );

  add_primitive "mem" [
    "Check if a string is included in a list of strings";
    "ENV must contain:";
    "- string : the string";
    "- strings : the list of strings";
  ]
    (fun envs env ->
      let string = get_string envs "string" in
      let strings = get_strings envs "strings" in
      let bool = List.mem string strings in
      plist_of_bool bool
    );


  add_function "disp" [
    "Display its environment ENV"
  ]
    (fun envs env ->
      Printf.printf "disp:\n%!";
      eprint_env "" env;
      []
    );

  add_function "exit" []
    (fun envs env ->
      let code = get_local_string_with_default envs "code" "0" in
      exit (int_of_string code)
    );

  add_function "pack" [] (fun envs env ->
    let to_module = get_local envs "to_module" in
    let files = get_local envs "files" in

    match to_module with
      [] | _ :: _ :: _ -> assert false (* TODO *)
    | [ packmodname, pack_env ] ->

      let modnames = ref [] in

      let files = List.map (fun (file, file_env) ->
          file,
          set_strings file_env "packed" (packmodname ::
              (try
                get_strings [ file_env ] "packed"
              with Var_not_found _ ->
                modnames := Filename.basename file :: !modnames;
                []))
        ) files in

      let pack_env = set_strings pack_env "pack" (List.rev !modnames) in

      files @
      [ packmodname ^ ".ml", pack_env ]


  );

  add_function "dstdir" [
    "Replaced by %{package_FULL_DST_DIR}%";
    "ENV must contain:";
    "- p : the package";
    "ENV can contain:";
    "- file : a filename that will be appended";
  ] (fun envs env ->
    let p = get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%" p in
    let s = try
      let file = get_local_string envs "file" in
      Filename.concat s file
    with Var_not_found _ -> s
    in
    [s, empty_env]
  );

  add_function "srcdir" [
    "Replaced by %{package_FULL_SRC_DIR}%";
    "ENV must contain:";
    "- p : the package";
    "ENV can contain:";
    "- file : a filename that will be appended";
] (fun envs env ->
    let p = get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_SRC_DIR}%%" p in
    let s =try
      let file = get_local_string envs "file" in
      Filename.concat s file
    with Var_not_found _ -> s
    in
    [s, empty_env]
  );

  add_function "byte_exe" [] (fun envs env ->
    let p = get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%/%s.byte" p p in
    [s, empty_env]
  );

  add_function "asm_exe" [] (fun envs env ->
    let p = get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%/%s.asm" p p in
    [s, empty_env]
  );

  add_function "split" [
    "Cut a string into a list of strings, at a given char,";
    "  empty strings are kept.";
    "ENV must contain:";
    "- s : the string to be cut";
    "ENV can contain:";
    "- sep : a string, whose first char will be the separator";
    "    (default to space)";
  ]
    (fun envs env ->
    let s = get_string envs "s" in
    let sep = get_string_with_default envs "sep" " " in
    let sep = if sep = "" then ' ' else sep.[0] in
    List.map (fun s -> (s, empty_env)) (OcpString.split s sep)
  );

  add_function "split_simplify" [
    "Cut a string into a list of strings, at a given char,";
    "  empty strings are removed.";
    "ENV must contain:";
    "- s : the string to be cut";
    "ENV can contain:";
    "- sep : a string, whose first char will be the separator";
    "    (default to space)";
  ] (fun envs env ->
    let s = get_string envs "s" in
    let sep = get_string_with_default envs "sep" " " in
    let sep = if sep = "" then ' ' else sep.[0] in
    List.map (fun s -> (s, empty_env)) (OcpString.split_simplify s sep)
  );

  let uniq_counter = ref 0 in
  add_function "uniq" [
    "Returns a uniq string, to be used as a uniq identifier";
  ] (fun _ _ ->
    incr uniq_counter; [ Printf.sprintf ".id_%d" !uniq_counter, empty_env]);
  ()


let primitives_help () = !primitives
