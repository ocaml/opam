(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2021 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type test = {
  repo_hash: string;
  tags : string list;
  commands: (string * string list) list;
}

type opam = {
  as_called : string;
  as_seen_in_opam : string;
}

let cmd_prompt = "### "
let no_opam_repo = "N0REP0"
let default_repo = "REPO"

let is_prefix pfx s =
  String.length s >= String.length pfx &&
  String.sub s 0 (String.length pfx) = pfx

let rem_prefix pfx s =
  if not (is_prefix pfx s) || s = pfx then invalid_arg "rem_prefix"
  else String.sub s (String.length pfx) (String.length s - String.length pfx)

(* Test file format: {v
   REPO_HASH
   ### opam command
   output line 1
   output...
   ### <filename>
   contents...
   ### opam command
   output...
   ### ENV_VAR=x opam command
   output...
v}*)

let load_test f =
  let ic = open_in f in
  let repo_hash, tags =
    match String.split_on_char ' ' (input_line ic) with
    | [] | exception End_of_file -> failwith "Malformed test file"
    | h::t -> h, t
  in
  let commands =
    let rec aux commands =
      match input_line ic, commands with
      | s, commands when is_prefix cmd_prompt s ->
        aux ((rem_prefix cmd_prompt s, []) :: commands)
      | s, ((cmd,out) :: commands) ->
        aux ((cmd, s::out) :: commands)
      | exception End_of_file ->
        List.rev_map (fun (cmd, out) -> cmd, List.rev out) commands
      | _ -> failwith "Malformed test file"
    in
    aux []
  in
  close_in ic;
  { repo_hash; tags; commands }

let base_env =
  let propagate v = try [v, Sys.getenv v] with Not_found -> [] in
  propagate "PATH" @
  propagate "HOME" @
  propagate "COMSPEC" @
  propagate "LIB" @
  propagate "SYSTEMROOT" @
  propagate "TMPDIR" @
  propagate "TMP" @
  propagate "TEMP" @
  (if Sys.win32 then ["SHELL", "/bin/sh"] else []) @
  [
    "OPAMKEEPBUILDDIR", "1";
    "OPAMCOLOR", "never";
    "OPAMUTF8", "never";
    "OPAMNOENVNOTICE", "1";
    "OPAMNODEPEXTS", "1";
    "OPAMDOWNLOADJOBS", "1";
  ]

(* See [opamprocess.safe_wait] *)
let rec waitpid pid =
  match Unix.waitpid [] pid with
  | exception Unix.Unix_error (Unix.EINTR,_,_) -> waitpid pid
  | exception Unix.Unix_error (Unix.ECHILD,_,_) -> 256
  | _, Unix.WSTOPPED _ -> waitpid pid
  | _, Unix.WEXITED n -> n
  | _, Unix.WSIGNALED _ -> failwith "signal"

exception Command_failure of int * string * string

type filt_sort =
  | Sed of string
  | Grep
  | GrepV

let escape_regexps s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (function
    | ('|' | '(' | ')' | '*' | '+' | '?'
    |  '[' | '.' | '^' | '$' | '{' | '\\') as c -> Buffer.add_char buf '\\'; Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let str_replace_path ?escape whichway filters s =
  let s =
    match escape with
    | Some `Unescape -> Re.(replace_string (compile @@ str "\\\\") ~by:"\\" s)
    | Some (`Backslashes | `Regexps) | None -> s
  in
  let escape_backslashes =
    match escape with
    | Some (`Backslashes | `Regexps) -> Re.(replace_string (compile @@ char '\\') ~by:"\\\\")
    | Some `Unescape | None -> fun s -> s
  in
  let escape_regexps =
    match escape with
    | Some `Regexps -> escape_regexps
    | Some `Backslashes -> escape_backslashes
    | Some `Unescape | None -> fun s -> s
  in
  List.fold_left (fun s (re, by) ->
      let re_path = Re.(
          seq [re; group (rep (diff any (alt [set ":;$\"'"; space])))]
        ) in
      match by with
      | Sed by ->
        Re.replace (Re.compile re_path) s ~f:(fun g ->
            escape_regexps by ^ escape_backslashes (whichway (Re.Group.(get g (nb_groups g - 1)))))
      | Grep | GrepV ->
        if (by = Grep) = Re.execp (Re.compile re) s then s else "\\c")
    s filters

let filters_of_var =
  List.map (fun (v, x) ->
      Re.(alt [seq [str "${"; str v; str "}"];
               seq [char '$'; str v; eow]];),
      Sed x)

let command
    ?(allowed_codes = [0]) ?(vars=[]) ?(silent=false) ?(filter=[]) ?(sort=false)
    cmd args =
  let env =
    Array.of_list @@
    List.map (fun (var, value) -> Printf.sprintf "%s=%s" var value) @@
    (List.filter (fun (v,_) ->
         List.find_opt (fun (v',_) -> String.equal v v') vars = None)
        base_env)
    @ vars
  in
  let input, stdout = Unix.pipe () in
  Unix.set_close_on_exec input;
  let ic = Unix.in_channel_of_descr input in
  set_binary_mode_in ic false;
  let cmd, orig_cmd =
    let maybe_resolved_cmd =
      if Sys.win32 then
        OpamStd.Option.default cmd @@ OpamSystem.resolve_command cmd
      else
        cmd
    in
      maybe_resolved_cmd, cmd
  in
  let args =
    if orig_cmd = "tar" || orig_cmd = "tar.exe" then
      List.map (Lazy.force (OpamSystem.get_cygpath_function ~command:cmd)) args
    else args
  in
  let pid =
    OpamProcess.create_process_env cmd (Array.of_list (cmd::args)) env
      Unix.stdin stdout stdout
  in
  Unix.close stdout;
  let rec filter_output out_buf ic =
    match input_line ic with
    | s ->
      let s =
        str_replace_path ~escape:`Unescape OpamSystem.back_to_forward filter s
      in
      if s = "\\c" then filter_output out_buf ic
      else
        (let out_buf = s::out_buf in
         if not silent && not sort then
           print_endline s;
         filter_output out_buf ic)
    | exception End_of_file -> out_buf
  in
  let out_buf = filter_output [] ic in
  let ret = waitpid pid in
  close_in ic;
  let out =
    if sort then List.sort String.compare out_buf
    else List.rev out_buf
  in
  if not silent && sort then
    List.iter print_endline out;
  let out = String.concat "\n" out in
  if not (List.mem ret allowed_codes) then
    raise (Command_failure (ret, String.concat " " (cmd :: args), out))
  else
    out

let finally f x k = match f x with
  | r -> k (); r
  | exception e ->
      (* When we're at least 4.05+
      let bt = Printexc.get_raw_backtrace () in*)
      (try k () with _ -> ());
      (*Printexc.raise_with_backtrace e bt*)
      raise e

(* Borrowed from ocamltest_stdlib.ml *)
let rec mkdir_p dir =
  if Sys.file_exists dir then ()
  else let () = mkdir_p (Filename.dirname dir) in
       if not (Sys.file_exists dir) then
         Unix.mkdir dir 0o777
       else ()

let erase_file path =
  try Sys.remove path
  with Sys_error _ when Sys.win32 ->
    (* Deal with read-only attribute on Windows. Ignore any error from chmod
       so that the message always come from Sys.remove *)
    let () = try Unix.chmod path 0o666 with Sys_error _ -> () in
    Sys.remove path

let rm_rf path =
  let rec erase path =
    if Sys.file_exists path && Sys.is_directory path then begin
      Array.iter (fun entry -> erase (Filename.concat path entry))
                 (Sys.readdir path);
      Unix.rmdir path
    end else erase_file path
  in
    try if Sys.file_exists path then erase path
    with Sys_error err ->
      raise (Sys_error (Printf.sprintf "Failed to remove %S (%s)" path err))

let rec with_temp_dir f =
  let s =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "opam-reftest-%06x" (Random.int 0xffffff))
  in
  if Sys.file_exists s then
    with_temp_dir f
  else
  (mkdir_p s;
   finally f s @@ fun () -> rm_rf s)

type filter = (Re.t * filt_sort) list

type command =
  | File_contents of string
  | Repo_pkg_file_contents of
      string * string (* name * version *)
      * [ `opam
        | `files of string (* file name *) ]
  | Pin_file_content of string
  | Opamfile of { files: string list;
             filter: filter; }
  | Json of { files: string list;
             filter: filter; }
  | Cache of { kind: [`installed | `repo];
               switch: string;
               nvs: string list;
               filter: filter; }
  | Run of { env: (string * string) list;
             cmd: string;
             args: string list; (* still escaped *)
             filter: filter;
             output: string option;
             unordered: bool;
             sort: bool;}
  | Export of (string * [`eq | `pluseq | `eqplus] * string) list
  | Comment of string

module Parse = struct

  open Re

  let re_str_atom =
    alt [
      seq [char '"';
           rep @@ alt
             [diff any (set "\"\\");
              seq [char '\\'; any]];
           char '"'];
      seq [char '\'';
           rep @@ diff any (char '\'');
           char '\''];
      seq [diff any (set "\"' ");
           rep @@ alt
             [diff any (set "\\ ");
              seq [char '\\'; any]]];
    ]

  let get_str s =
    let unescape =
      replace (compile @@ seq [char '\\'; group any])
        ~f:(fun g -> Group.get g 1)
    in
    let trim s = String.sub s 1 (String.length s - 2) in
    if s = "" then ""
    else match s.[0] with
      | '"' -> unescape (trim s)
      | '\'' -> trim s
      | _ -> unescape s

  let re_varbind =
    seq [
      group @@ seq [alpha; rep (alt [alnum; set "_-"])];
      group @@ alt [ str "+="; str "=+"; char '=' ];
      group @@ re_str_atom;
      rep space;
    ]

  let re_package =
    seq [
      str "<pkg:";
      group @@ seq [ alpha; rep @@ alt [ alnum; set "_-+" ]];
      char '.';
      group @@ rep1 @@ alt [ alnum; set "-_+.~" ];
      opt @@ seq [ char ':' ; group @@ rep1 @@ alt [ alnum; set "-_+.~" ] ];
      char '>'
    ]

  let command ?(vars=[]) str =
    if str.[0] = '<' && str.[String.length str - 1] = '>' then
      if String.length str > 4 && String.sub str 1 4 = "pin:" then
        Pin_file_content (String.sub str 5 (String.length str - 6))
      else
        try
          let grs = exec (compile re_package) str in
          let name = Group.get grs 1 in
          let version = Group.get grs 2 in
          Repo_pkg_file_contents
            (name, version,
             try
               let file = Group.get grs 3 in
               `files (file)
             with Not_found -> `opam)
        with Not_found ->
          File_contents (String.sub str 1 (String.length str - 2))
      else if str.[0] = ':' || str.[0] = '#' then
        Comment str
    else
      let varbinds, pos =
        let gr = exec (compile @@ rep re_varbind) str in
        List.map (fun gr ->
          Group.get gr 1,
          (match Group.get gr 2 with
           | "=" -> `eq
           | "+=" -> `pluseq
           | "=+" -> `eqplus
           | _ -> assert false),
          get_str (Group.get gr 3))
        (all (compile @@ re_varbind) (Group.get gr 0)),
      Group.stop gr 0
        in
    let cmd, pos =
      try
        let gr = exec ~pos (compile re_str_atom) str in
        Some (get_str (Group.get gr 0)),
        Group.stop gr 0
          with Not_found -> None, pos
        in
    let args =
      let grs = all ~pos (compile re_str_atom) str in
      List.map (fun gr -> Group.get gr 0) grs
      in
    let get_str ?escape s =
      str_replace_path ?escape OpamSystem.back_to_forward
        (filters_of_var vars)
        (get_str s)
    in
    let posix_re re =
      try Posix.re (get_str ~escape:`Regexps re)
      with Posix.Parse_error ->
        failwith (Printf.sprintf "Bad POSIX regexp: %s" re)
    in
    let rec get_args_rewr acc = function
      | [] -> List.rev acc, false, false, [], None
      | ("|"|">$") :: _ as rewr ->
          let rec get_rewr (unordered, sort, acc) = function
            | "|" :: re :: "->" :: str :: r ->
                get_rewr (unordered, sort, (posix_re re, Sed (get_str str)) :: acc) r
            | "|" :: "grep" :: "-v" :: re :: r ->
                get_rewr (unordered, sort, (posix_re re, GrepV) :: acc) r
            | "|" :: "grep" :: re :: r ->
                get_rewr (unordered, sort, (posix_re re, Grep) :: acc) r
            | "|" :: "sort" :: r ->
                if acc <> [] then
                  Printf.printf "Warning: sort should appear _before_ any filters\n%!";
            get_rewr (unordered, true, acc) r
            | "|" :: "unordered" :: r ->
                get_rewr (true, sort, acc) r
            | "|" :: "sed-cmd" :: cmd :: r ->
                let sandbox =
                  (* Sandbox prefix
                 >[...] /tmp/build_592d92_dune/opam-reftest-2b89f9/OPAM/opam-init/hooks/sandbox.sh "build" "cmd" <
                 >[...] ${BASEDIR}/opam-init/hooks/sandbox.sh "build" "cmd" <
                 -->
                   >[...] cmd <
              *)
                  seq [
                    alt [ char '/'; Re.str "${" ];
                non_greedy @@ rep1 any; Re.str "sandbox.sh";
                space;
                char '"';
                alt @@ List.map Re.str [ "build"; "install"; "remove" ];
                char '"';
                space;
                char '"'; Re.str cmd; char '"';
                space;
                ]
    in
            let with_quote_set s = set ("\"'"^s) in
            let opt_quoted r = [
              seq @@ [ char '"'] @  r @ [ char '"'; rep1 space ];
              seq @@ r @ [ rep1 space ];
              ] in
            let unix_prefix =
              (* Unix & Mac command prefix
                 >[...] /usr/bin/cmd <
                 >[...] /usr/bin/cmd <
                 -->
                   >[...] cmd <
              *)
              opt_quoted @@ [
                rep1 @@ seq [ char '/'; rep1 @@ diff any (with_quote_set "/ ") ];
                char '/';
                Re.str cmd;
                ]
            in
            let win_prefix =
              (* Windows command prefix
                 >[...] C:\Windows\system32\cmd.exe <
                 >[...] C:\Windows\system32\cmd <
                 >[...] C:\Windows\system 32\cmd <
                 -->
                   >[...] cmd <
              *)
              opt_quoted @@ [
                alpha; char ':';
                rep1 @@ seq [ char '\\'; opt @@ char '\\';
                              rep1 @@ diff any (with_quote_set "\\") ];
                char '\\'; opt @@ char '\\';
                Re.str cmd;
                opt @@ Re.str ".exe";
                ] in
            let re = alt @@ sandbox :: unix_prefix @ win_prefix in
            let str = Printf.sprintf "%s " cmd in
            get_rewr (unordered, sort, (re, Sed str) :: acc) r
            | ">$" :: output :: [] ->
                unordered, sort, List.rev acc, Some (get_str output)
            | [] ->
                unordered, sort, List.rev acc, None
            | r ->
                Printf.printf
              "Bad rewrite %S, expecting '| RE -> STR' or '>$ VAR'\n%!"
              (String.concat " " r);
            unordered, sort, List.rev acc, None
            in
        let unordered, sort, rewr, out = get_rewr (false, false, []) rewr in
        List.rev acc, unordered, sort, rewr, out
            | arg :: r -> get_args_rewr (arg :: acc) r
        in
    let args, unordered, sort, rewr, output = get_args_rewr [] args in
    match cmd with
    | Some "opam-cat" ->
        Opamfile { files = args; filter = rewr; }
    | Some "json-cat" ->
        Json { files = args; filter = rewr; }
    | Some "opam-cache" ->
      let kind, switch, nvs =
        match args with
        | "installed"::switch::nvs ->
          `installed, switch, nvs
        | "repo"::nvs ->
          `repo, "", nvs
        | _ ->
          failwith
            (Printf.sprintf
               "Bad usage of opam-cache %s.\n\
                expecting 'opam-cache <installed|repo> <switch?> [nvs]"
             (String.concat " " args))
      in
      Cache { kind; switch; nvs; filter = rewr; }
    | Some cmd ->
        let env, plus =
          List.fold_left (fun (env,plus) (v,op,value) ->
            match op with
            | `eq -> (v,value)::env, plus
            | `pluseq -> env, (v^"+="^value)::plus
            | `eqplus -> env, (v^"=+"^value)::plus)
          ([],[]) varbinds
    in
      (match plus with
       | [] -> ()
       | _ ->
           OpamConsole.error
           "variable bindings at the beginning of a command does not \
            support '+=' or '=+' operators: %s"
           (OpamStd.Format.pretty_list plus));
      Run {
        env;
        cmd;
        args;
        filter = rewr;
        output;
        unordered;
        sort;
        }
       | None ->
           Export varbinds
    end

let parse_command = Parse.command

let common_filters ?opam dir =
   let tmpdir = OpamSystem.real_path (Filename.get_temp_dir_name ()) in
   let open Re in
   let dir_to_regex dir =
     if Sys.win32 then
       [str dir; str (OpamSystem.back_to_forward dir); str (OpamSystem.apply_cygpath dir)]
     else
       [str dir] in
   [
     seq [ bol;
           alt [ str "#=== ERROR";
                 seq [ str "# "; alt @@ List.map str
                         [ "context";
                           "path";
                           "command";
                           "exit-code";
                           "env-file";
                           "output-file"]]]],
     GrepV;
     seq [bol; str cmd_prompt],
     Sed "##% ";
     alt (dir_to_regex dir),
     Sed "${BASEDIR}";
     seq [
          alt (dir_to_regex tmpdir);
          rep (set "/\\");
          str "opam-";
          rep1 (alt [xdigit; char '-'])],
     Sed "${OPAMTMP}";
   ] @
   (match opam with
    | None -> []
    | Some opam -> [ str opam.as_seen_in_opam, Sed "${OPAM}" ])

let run_cmd ~opam ~dir ?(vars=[]) ?(filter=[]) ?(silent=false) ?(sort=false) cmd args =
  let filter = filter @ common_filters ~opam dir in
  let var_filters = filters_of_var vars in
  let cmd = if cmd = "opam" then opam.as_called else cmd in
  let args =
    List.map (fun a ->
        let expanded =
          if a <> "" && a.[0] = '\'' then a
          else
            str_replace_path ~escape:`Backslashes OpamSystem.forward_to_back
              var_filters a
        in
        Parse.get_str expanded)
      args
  in
  try command ~vars ~filter ~silent ~sort cmd args, None
  with Command_failure (n,_, out) -> out, Some n

let write_file ~path ~contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let rec list_remove x = function
  | [] -> []
  | y :: r -> if x = y then r else y :: list_remove x r


let print_opamfile file =
  try
    let open OpamParserTypes.FullPos in
    let original = OpamParser.FullPos.file file in
    let rec mangle item =
      match item.pelem with
      | Section s ->
        {item with
         pelem =
           Section {s with
                    section_name =
                      OpamStd.Option.map (fun v ->
                          {v with pelem = mangle_string v.pelem})
                        s.section_name;
                    section_items =
                      {s.section_items with
                       pelem = List.map mangle s.section_items.pelem}}}
      | Variable(name, value) ->
        {item with pelem = Variable(name, mangle_value value)}
    and mangle_value item =
      match item.pelem with
      | String s ->
        {item with pelem = String(mangle_string s)}
      | Relop(op, l, r) ->
        {item with pelem = Relop(op, mangle_value l, mangle_value r)}
      | Prefix_relop(relop, v) ->
        {item with pelem = Prefix_relop(relop, mangle_value v)}
      | Logop(op, l, r) ->
        {item with pelem = Logop(op, mangle_value l, mangle_value r)}
      | Pfxop(op, v) ->
        {item with pelem = Pfxop(op, mangle_value v)}
      | List l ->
        {item with pelem = List{l with pelem = List.map mangle_value l.pelem}}
      | Group l ->
        {item with pelem = Group{l with pelem = List.map mangle_value l.pelem}}
      | Option(v, l) ->
        {item with pelem = Option(mangle_value v, {l with pelem = List.map mangle_value l.pelem})}
      | Env_binding(name, op, v) ->
        {item with pelem = Env_binding(name, op, mangle_value v)}
      | Bool _
      | Int _
      | Ident _ -> item
    and mangle_string = String.map (function '\\' -> '/' | c -> c)
    in
    let mangled =
      {original with file_contents = List.map mangle original.file_contents}
    in
    OpamPrinter.FullPos.Normalise.opamfile mangled
  with
  | Sys_error _ -> Printf.sprintf "# %s not found\n" file
  | e -> Printf.sprintf "# Error on file %s: %s\n" file (Printexc.to_string e)

let template_opamfile =
  OpamParser.FullPos.string {|
opam-version: "2.0"
synopsis: "A word"
description: "Two words."
authors: "the testing team"
homepage: "egapemoh"
maintainer: "maint@tain.er"
license: "MIT"
dev-repo: "hg+https://pkg@op.am"
bug-reports: "https://nobug"
|} "<nofile>"

let print_file ~filters ?(single_header=true) names_and_content =
  let files =
    let with_name () =
      List.map
        (fun (name, content) ->
           Printf.sprintf "=> %s <=\n%s" name content)
        names_and_content
    in
    if single_header then with_name () else
      match names_and_content with
      | [_, content] -> [content]
      | _ -> with_name ()
  in
  List.map (fun s -> OpamStd.String.split s '\n') files
  |> List.flatten
  |> List.iter (fun s ->
      let s =
        str_replace_path OpamSystem.back_to_forward
          filters s
      in
      if not (String.equal s "\\c") then print_string (s^"\n"))

let add_extra_files fpath opamfile file =
  let replace f ~default l =
    let found, nl =
      List.fold_left (fun (found, nl) item ->
          if found then found, item::nl else
            match f item with
            | Some y -> true, y::nl
            | None -> false, item::nl)
        (false, []) l
    in
    (if not found then default::nl else nl)
    |> List.rev
  in
  let open OpamParserTypes.FullPos in
  let pos = (* null pos *)
    { filename = opamfile.file_name; start = -1, -1; stop = -1, -1; }
  in
  let md5 = OpamHash.(to_string (compute ~kind:`MD5 fpath)) in
  let xf_pair =
    [{ pelem = String file; pos };
     { pelem = String md5; pos }]
  in
  let xf_pair_list =
    { pelem = List { pelem = xf_pair; pos}; pos}
  in
  let default =
    { pelem = Variable ({ pelem = "extra-files"; pos},
                        xf_pair_list); pos}
  in
  let with_field xfiles =
    Some { pelem = Variable
               ({ pelem = "extra-files"; pos},
                { pelem = List { pelem = xfiles ; pos}; pos}); pos}
  in
  let file_contents =
    replace ~default
      (function
        | { pelem = Variable
                ({ pelem = "extra-files"; _},
                 { pelem = List { pelem = xfiles ; _}; _}); _} ->
          (match xfiles with
           | [{ pelem = String xf; _};
              { pelem = String _checksum; _}] as xfield ->
             with_field @@
             if String.equal xf file then
               xf_pair
             else
               [{ pelem = List { pelem = xfield; pos}; pos};
                { pelem = List { pelem = xf_pair; pos}; pos}]
           | { pelem = List _; _}::_ ->
             (* There is only 2 construction
                * 1 file, list of 2 strings
                  * extra-files: [ "file" "checksum" ]
                * list of files
                  * extra-files: [[ "file" "checksum" ]]
                  * extra-files: [[ "file" "checksum" ] [ "file2" "checkums2"]]
             *)
             assert (List.for_all (function
                   { pelem = List
                         { pelem = [{ pelem = String _; _ };
                                    { pelem = String _; _}];
                           _}; _} -> true
                 | _ -> false) xfiles);
             with_field @@
             replace (function
                 | { pelem = List
                         { pelem =
                             [ { pelem = String xf; _ };
                               { pelem = String _; _}]; _}; _}
                   when String.equal xf file ->
                   Some xf_pair_list
                 | _ -> None)
               ~default:xf_pair_list xfiles
           (* cf comment above *)
           | _ -> assert false)
        | _ -> None)
      opamfile.file_contents
  in
  { opamfile with file_contents }

let run_test ?(vars=[]) ~opam t =
  let old_cwd = Sys.getcwd () in
  let opamroot0 = Filename.concat old_cwd ("root-"^t.repo_hash) in
  with_temp_dir @@ fun dir ->
  let dir = OpamSystem.real_path dir in
  Sys.chdir dir;
  let opamroot = Filename.concat dir "OPAM" in
  if Sys.win32 then
    ignore @@ command ~allowed_codes:[0; 1] ~silent:true
      "robocopy"
      ["/e"; "/copy:dat"; "/dcopy:dat"; "/sl"; opamroot0; opamroot]
  else
    ignore @@ command "cp" ["-PR"; opamroot0; opamroot];
  let vars = [
    "OPAM", opam.as_seen_in_opam;
    "OPAMROOT", opamroot;
    "BASEDIR", dir;
    "PATH", Sys.getenv "PATH";
  ] @ vars
  in
  if t.repo_hash = no_opam_repo then
    (mkdir_p (default_repo^"/packages");
     write_file ~path:(default_repo^"/repo") ~contents:{|opam-version: "2.0"|};
     ignore @@ command opam.as_called ~silent:true
       [ "repository"; "set-url"; "default"; "./"^default_repo;
         "--root"; opamroot]);
  ignore @@ command ~silent:true opam.as_called
    ["var"; "--quiet"; "--root"; opamroot; "--global"; "--cli=2.1";
     "sys-ocaml-version=4.08.0"];
  print_endline (String.concat " " (t.repo_hash::t.tags));
  let _vars =
    List.fold_left (fun vars (cmd, out) ->
        print_string cmd_prompt;
        print_endline cmd;
        match parse_command ~vars cmd with
        | Comment _ -> vars
        | File_contents path ->
          let contents = String.concat "\n" out ^ "\n" in
          write_file ~path ~contents;
          print_string contents;
          vars
        | Repo_pkg_file_contents (name, version, repo_file) ->
          let contents = String.concat "\n" out ^ "\n" in
          let opam_path =
            Printf.sprintf "%s/packages/%s/%s.%s/opam"
              default_repo name name version
          in
          let file_path file =
            Printf.sprintf "%s/packages/%s/%s.%s/files/%s"
              default_repo name name version file
          in
          (match repo_file with
           | `opam ->
             let contents =
               try
                 (* update with extra files if found *)
                 let files =
                   Filename.concat (Filename.dirname opam_path) "files"
                   |> Sys.readdir
                   |> Array.to_list
                   |> List.filter (fun f ->
                       not (Sys.is_directory (file_path f)))
                 in
                 match files with
                 | [] -> contents
                 | _ ->
                   let opamfile = OpamParser.FullPos.string contents opam_path in
                   List.fold_left (fun opamfile file ->
                       add_extra_files (file_path file) opamfile file)
                     opamfile files
                   |> OpamPrinter.FullPos.opamfile
               with Sys_error _ -> contents
             in
             (* and write opam file *)
             write_file ~path:opam_path ~contents;
           | `files file ->
             (* first update the given file *)
             let fpath = file_path file in
             write_file ~path:fpath ~contents;
             (* then update the opam file *)
             if Sys.file_exists opam_path then
               let opamfile = OpamParser.FullPos.file opam_path in
               let contents =
                 add_extra_files fpath opamfile file
                 |> OpamPrinter.FullPos.opamfile
               in
               write_file ~path:opam_path ~contents
          );
          print_string contents;
          ignore @@ run_cmd ~opam ~dir ~vars ~silent:true
            "opam" ["update"; "default"];
          vars
        | Pin_file_content path ->
          let open OpamParserTypes.FullPos in
          let raw_content = (String.concat "\n" out) in
          let opamfile = OpamParser.FullPos.string raw_content path in
          let nullify_pos p =
            {p with pos = { filename = path; start = -1, -1; stop = -1, -1; }}
          in
          let test_content, tpl_content =
            List.fold_left (fun (test_content, tpl_content) item ->
                match item with
                | { pelem = Variable (name, _); _} ->
                  let tpl_overwrite, test_content =
                    List.partition (function
                        | { pelem = Variable (n, _); _} -> n.pelem = name.pelem
                        | _ -> false)
                      test_content
                  in
                  let item =
                    match tpl_overwrite with
                    | [ item ] -> item
                    | _ -> item
                  in
                  test_content, item::tpl_content
                | { pelem = Section _ ; _} -> test_content, item::tpl_content
              )
              (opamfile.file_contents,[]) template_opamfile.file_contents
          in
          let file_contents =
            List.rev_map nullify_pos tpl_content
            @ List.map nullify_pos test_content
          in
          let contents =
            Printf.sprintf "%s\n"
              (OpamPrinter.FullPos.opamfile { opamfile with file_contents })
          in
          write_file ~path ~contents;
          print_string (raw_content ^ "\n");
          vars
        | Export bindings ->
          List.fold_left
            (fun vars -> fun (v, op, r) ->
               let r' =
                 str_replace_path ~escape:`Backslashes
                   OpamSystem.forward_to_back (filters_of_var vars) r
               in
               let value =
                 match op with
                 | `eq -> r'
                 | (`pluseq | `eqplus) as op ->
                   match List.find_opt (fun (v',_) -> String.equal v v') (base_env @ vars) with
                   | Some (_,c) ->
                     let sep = if Sys.win32 then ";" else ":" in
                     (match op with
                      | `pluseq -> r'^sep^c
                      | `eqplus -> c^sep^r')
                   | None -> r'
               in
               (v, value) :: List.filter (fun (w, _) -> not (String.equal v w)) vars)
            vars bindings
        | Opamfile { files; filter } ->
          let files =
            List.map (fun s ->
                let name =
                  Re.(replace_string (compile @@ str "$OPAMROOT")
                        ~by:opamroot s)
                in
                name, print_opamfile name)
              files
          in
          let filters = filter @ common_filters dir in
          print_file ~single_header:false ~filters files;
          vars
        | Json { files; filter } ->
          let files =
            List.map (fun s ->
                Re.(replace_string (compile @@ str "$OPAMROOT") ~by:opamroot s))
              files
          in
          let json_filters =
            let open Re in
            let hex_output ext =
              seq [ char '-'; rep1 digit;
                    char '-'; repn xdigit 6 (Some 6);
                    str ext ],
              Sed ext
            in
            [
              seq [ str {|"duration": |};
                    rep1 digit;
                    opt @@ seq [ char '.';
                                 rep1 @@ alt [digit ; char 'e'; char '-']];
                    eow ],
              Sed {|"duration": 6.2831853071|};
              hex_output ".env";
              hex_output ".out";
              str OpamVersion.(to_string (full ())), Sed "currentv";
              (* For opam Windows path *)
              str (String.escaped opam.as_seen_in_opam), Sed "${OPAM}";
            ]
          in
          let to_string f =
            let ic = open_in f in
            let content =
              let rec aux content =
                try aux (input_line ic :: content)
                with End_of_file -> content
              in
              aux []
              |> List.rev
              |> String.concat "\n"
            in
            close_in ic;
            match OpamJson.of_string content with
            | Some json ->
              OpamJson.to_string ~minify:false json ^ "\n"
            | None -> "# Return Error reading json\n"^content
          in
          let files =
            List.map (fun s ->
                let name =
                  Re.(replace_string (compile @@ str "$OPAMROOT")
                        ~by:opamroot s)
                in
                name, to_string name)
              files
          in
          let filters = filter @ common_filters ~opam dir @ json_filters in
          print_file ~single_header:false ~filters files;
          vars
        | Cache { switch; kind; nvs; filter } ->
          let nvs =
            List.fold_left (fun nvs s ->
                match OpamPackage.of_string_opt s with
                | Some nv ->
                  let n = OpamPackage.name nv in
                  let v = OpamPackage.version nv in
                  OpamPackage.Name.Map.update n
                    (OpamPackage.Version.Set.add v)
                    (OpamPackage.Version.Set.singleton v)
                    nvs
                | None ->
                  OpamPackage.Name.Map.add
                    (OpamPackage.Name.of_string s)
                    OpamPackage.Version.Set.empty
                    nvs)
              OpamPackage.Name.Map.empty nvs
          in
          (match kind with
           | `installed ->
             (let cache =
                OpamSwitchState.Installed_cache.load
                  (OpamPath.Switch.installed_opams_cache
                     (OpamFilename.Dir.of_string opamroot)
                     (OpamSwitch.of_string switch))
              in
              match cache with
              | None -> print_string "No cache\n"
              | Some cache when OpamPackage.Map.is_empty cache ->
                print_string "Empty cache\n"
              | Some cache ->
                let cache =
                  if OpamPackage.Name.Map.is_empty nvs then cache else
                    OpamPackage.Map.filter (fun nv _ ->
                        let n = OpamPackage.name nv in
                        match OpamPackage.Name.Map.find_opt n nvs with
                        | Some vs ->
                          OpamPackage.Version.Set.is_empty vs
                          || OpamPackage.Version.Set.mem
                            (OpamPackage.version nv) vs
                        | None -> false)
                      cache
                in
                let files =
                  OpamPackage.Map.fold (fun pkg opam files ->
                      let name = OpamPackage.to_string pkg in
                      let content = OpamFile.OPAM.write_to_string opam in
                      (name, content)::files)
                    cache []
                in
                print_file ~filters:(filter @ common_filters dir) files)
           | `repo ->
             (let cache =
                OpamRepositoryState.Cache.load
                  (OpamFilename.Dir.of_string opamroot)
              in
              match cache with
              | None -> print_string "No cache\n"
              | Some (_, cache) when OpamRepositoryName.Map.is_empty cache ->
                print_string "Empty cache\n"
              | Some (_, cache) ->
                let cache =
                  if OpamPackage.Name.Map.is_empty nvs then cache else
                    OpamRepositoryName.Map.map
                      (OpamPackage.Map.filter (fun nv _ ->
                           let n = OpamPackage.name nv in
                           match OpamPackage.Name.Map.find_opt n nvs with
                           | Some vs ->
                             OpamPackage.Version.Set.is_empty vs
                             || OpamPackage.Version.Set.mem
                               (OpamPackage.version nv) vs
                           | None -> false))
                      cache
                in
                let files =
                  OpamRepositoryName.Map.fold (fun reponame pkgmap files->
                      let pre = OpamRepositoryName.to_string reponame in
                      OpamPackage.Map.fold (fun pkg opam files ->
                          let name = pre ^ ":" ^ OpamPackage.to_string pkg in
                          let content = OpamFile.OPAM.write_to_string opam in
                          (name, content)::files)
                        pkgmap files)
                    cache []
                in
                print_file ~filters:(filter @ common_filters dir) files));
          vars
        | Run {env; cmd; args; filter; output; unordered; sort} ->
          let silent = output <> None || unordered in
          let r, errcode =
            run_cmd ~opam ~dir ~vars:(vars @ env) ~filter ~silent ~sort cmd args
          in
          (if unordered then
             (* print lines from Result, but respecting order from Expect *)
             let rec diffl acc r e =
               let expect_has rl =
                 let matching = List.filter (( = ) rl) in
                 List.length (matching e) > List.length (matching acc)
               in
               match r, e with
               | r, el::e when List.mem el acc ->
                 print_endline el; diffl (list_remove el acc) r e
               | rl::r, el::e ->
                 if rl = el then (print_endline el; diffl acc r e)
                 else if expect_has rl then diffl (rl::acc) r (el :: e)
                 else (print_endline rl; diffl acc r (el :: e))
               | [], _::el ->
                 diffl acc [] el
               | r, [] ->
                 assert (acc = []); List.iter print_endline r
             in
             diffl [] (String.split_on_char '\n' r) out);
          OpamStd.Option.iter (Printf.printf "# Return code %d #\n") errcode;
          match output with
          | None -> vars
          | Some v -> (v, r) :: List.filter (fun (w,_) -> v <> w) vars)
      vars
      t.commands
  in
  Sys.chdir old_cwd

let () =
  Random.self_init ();
  match Array.to_list Sys.argv with
  | _ :: opam :: input :: env ->
    let opam = OpamFilename.(to_string (of_string opam)) in
    let opam =
      (* NOTE: We need that to be able to have the same output from Sys.executable_name when calling the opam binary *)
      let opam_without_double_exe = Filename.chop_suffix opam ".exe" in
      if Sys.cygwin
      then {as_called = opam; as_seen_in_opam = opam_without_double_exe}
      else {as_called = opam_without_double_exe; as_seen_in_opam = opam_without_double_exe}
    in
    let vars =
      List.map (fun s -> match OpamStd.String.cut_at s '=' with
          | Some (var, value) -> var, value
          | None -> failwith "Bad 'var=value' argument")
        env
    in
    load_test input |> run_test ~opam ~vars
  | _ ->
    failwith "Expected arguments: opam.exe opam file.test [env-bindings]"
