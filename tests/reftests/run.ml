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

(* Simple CRAM-like test framework for opam tests.
   Features and format:
   - first line is
     * the git hash of the opam repository to use, an opamroot is already
       initialised with that repo as "default"
     * or N0REP0 for no dependency on opam repository, and an opamroot is
       already initialised with an empty `default` repository in `./REPO`
       directory, that you need to populate and not forget to run `opam update`
   - 'opam' is automatically redirected to the correct binary
   - the command prefix is `### `
   - use `### <FILENAME>`, then the contents below to create a file verbatim
   - use `### <pkg:NAME.VERSION>`, then the contents of an opam file below to
     add this package to `default` repository in `./REPO`
   - use `### <pkg:NAME.VERSION:FILENAME>`, then the contents below to add this
     file as a extra-file of the given package in the `default` repository
   - `### FOO=x BAR=y` to export variables for subsequent commands
   - shell-like command handling:
     * **NO pattern expansion, shell pipes, sequences or redirections**
     * `FOO=x BAR=y command`
     * Arguments can be quoted: eg `"foo\"bar"`, `'foo\bar'`, but not combined
       (`foo'bar'` is not translated to `foobar`)
     * Variable expansion in arguments (`$FOO` or `${FOO}`). Undefined variables
       are left as-is
     * rewrites: `| 'REGEXP' -> 'STR'` (can be repeated; set `STR` to `\c` to
       clear the line)
     * `| grep REGEXP`
     * `| unordered` compares lines without considering their ordering
     * variables from command outputs: `cmd args >$ VAR`
     * `### : comment`
     * opam-cat: prints a nromalised opam file
   - if you need more shell power, create a script using <FILENAME> then run it.
     Or just use `sh -c`... but beware for compatibility.

   The opam roots are generated using dynamically generated dune rules (see
   gen.ml and dune.inc), then the tests are run using this script.
*)

type test = {
  repo_hash: string;
  commands: (string * string list) list;
}

let cmd_prompt = "### "
let no_opam_repo = "N0REP0"
let default_repo = "REPO"

let is_prefix pfx s =
  String.length s >= String.length pfx &&
  String.sub s 0 (String.length pfx) = pfx

let rem_prefix pfx s =
  if not (is_prefix pfx s) then invalid_arg "rem_prefix"
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
  let repo_hash = try input_line ic with
    | End_of_file -> failwith "Malformed test file"
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
  { repo_hash; commands }

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

let str_replace_path ?(escape=false) whichway filters s =
  let escape =
    if escape then Re.(replace_string (compile @@ char '\\') ~by:"\\\\")
    else fun s -> s
  in
  List.fold_left (fun s (re, by) ->
      let re_path = Re.(
          seq [re; group (rep (diff any space))]
        ) in
      match by with
      | Some by ->
        Re.replace (Re.compile re_path) s
          ~f:(fun g ->
              escape (by ^ whichway (Re.Group.get g 1)))
      | None ->
        if Re.execp (Re.compile re) s then s else "\\c")
    s filters

let command
    ?(allowed_codes = [0]) ?(vars=[]) ?(silent=false) ?(filter=[])
    cmd args =
  let env =
    Array.of_list @@
    List.map (fun (var, value) -> Printf.sprintf "%s=%s" var value) @@
    (base_env @ vars)
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
  let out_buf = Buffer.create 273 in
  let rec filter_output ?(first=true) ic =
    match input_line ic with
    | s ->
      let s = str_replace_path OpamSystem.back_to_forward filter s in
      if s = "\\c" then filter_output ~first ic
      else
        (if not first then Buffer.add_char out_buf '\n';
         Buffer.add_string out_buf s;
         if not silent then print_endline s;
         filter_output ~first:false ic)
    | exception End_of_file -> ()
  in
  filter_output ic;
  let ret = waitpid pid in
  close_in ic;
  let out = Buffer.contents out_buf in
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

type command =
  | File_contents of string
  | Cat of string list
  | Run of { env: (string * string) list;
             cmd: string;
             args: string list; (* still escaped *)
             filter: (Re.t * string option) list;
             output: string option;
             unordered: bool; }
  | Export of (string * string) list
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
      char '=';
      group @@ re_str_atom;
      rep space;
    ]

  let re_package =
    seq [
      str "<pkg:";
      group @@ seq [ alpha; rep1 @@ alt [ alnum; set "_-+" ]];
      char '.';
      group @@ rep1 @@ alt [ alnum; set "-_+.~" ];
      opt @@ seq [ char ':' ; group @@ rep1 @@ alt [ alnum; set "-_+.~" ]];
      char '>'
    ]

  let command str =
    if str.[0] = '<' && str.[String.length str - 1] = '>' then
      let f =
        try
          let grs = exec (compile re_package) str in
          let name = Group.get grs 1 in
          let version = Group.get grs 2 in
          try
            let files = Group.get grs 3 in
            Printf.sprintf "%s/packages/%s/%s.%s/files/%s"
              default_repo name name version files
          with Not_found ->
            Printf.sprintf "%s/packages/%s/%s.%s/opam"
              default_repo name name version
        with Not_found ->
          String.sub str 1 (String.length str - 2)
      in
      File_contents f
    else if str.[0] = ':' || str.[0] = '#' then
      Comment str
    else
    match OpamStd.String.cut_at str ' ' with
    | Some ("opam-cat", files) ->
        Cat (OpamStd.String.split files ' ')
    | _ ->
    let varbinds, pos =
      let gr = exec (compile @@ rep re_varbind) str in
      List.map (fun gr -> Group.get gr 1, get_str (Group.get gr 2))
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
    let rec get_args_rewr acc = function
      | [] -> List.rev acc, false, [], None
      | "|" :: _ as rewr ->
        let rec get_rewr (unordered, acc) = function
          | "|" :: re :: "->" :: str :: r ->
            get_rewr (unordered, (Posix.re (get_str re), Some (get_str str)) :: acc) r
          | "|" :: "grep" :: re :: r ->
            get_rewr (unordered, (Posix.re (get_str re), None) :: acc) r
          | "|" :: "unordered" :: r ->
            get_rewr (true, acc) r
          | ">$" :: output :: [] ->
            unordered, List.rev acc, Some (get_str output)
          | [] ->
            unordered, List.rev acc, None
          | r ->
            Printf.printf
              "Bad rewrite %S, expecting '| RE -> STR' or '>$ VAR'\n%!"
              (String.concat " " r);
            unordered, List.rev acc, None
        in
        let unordered, rewr, out = get_rewr (false, []) rewr in
        List.rev acc, unordered, rewr, out
      | arg :: r -> get_args_rewr (arg :: acc) r
    in
    let args, unordered, rewr, output = get_args_rewr [] args in
    match cmd with
    | Some cmd ->
      Run {
        env = varbinds;
        cmd;
        args;
        filter = rewr;
        output;
        unordered;
      }
    | None ->
      Export varbinds
end

let parse_command = Parse.command

let common_filters dir =
   let tmpdir = Filename.get_temp_dir_name () in
    Re.[
      alt [str dir; str (OpamSystem.back_to_forward dir)],
      Some "${BASEDIR}";
      seq [opt (str "/private");
           alt [str tmpdir;
                str (OpamSystem.back_to_forward tmpdir)];
           rep (set "/\\");
           str "opam-";
           rep1 (alt [alnum; char '-'])],
      Some "${OPAMTMP}";
    ]

let run_cmd ~opam ~dir ?(vars=[]) ?(filter=[]) ?(silent=false) cmd args =
  let filter = common_filters dir @ filter in
  let opamroot = Filename.concat dir "OPAM" in
  let env_vars = [
    "OPAM", opam;
    "OPAMROOT", opamroot;
  ] @ vars
  in
  let var_filters =
    List.rev_map (fun (v, x) ->
        Re.(alt [seq [str "${"; str v; str "}"];
                 seq [char '$'; str v; eow]];),
        Some x)
      env_vars
  in
  let cmd = if cmd = "opam" then opam else cmd in
  let args =
    List.map (fun a ->
        let expanded =
          if a <> "" && a.[0] = '\'' then a
          else
            str_replace_path ~escape:true OpamSystem.forward_to_back
              var_filters a
        in
        Parse.get_str expanded)
      args
  in
  try command ~vars:env_vars ~filter ~silent cmd args, None
  with Command_failure (n,_, out) -> out, Some n

let write_file ~path ~contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let rec list_remove x = function
  | [] -> []
  | y :: r -> if x = y then r else y :: list_remove x r

let run_test ?(vars=[]) ~opam t =
  let opamroot0 = Filename.concat (Sys.getcwd ()) ("root-"^t.repo_hash) in
  with_temp_dir @@ fun dir ->
  let old_cwd = Sys.getcwd () in
  let opamroot = Filename.concat dir "OPAM" in
  if Sys.win32 then
    ignore @@ command ~allowed_codes:[0; 1] ~silent:true
      "robocopy"
      ["/e"; "/copy:dat"; "/dcopy:dat"; "/sl"; opamroot0; opamroot]
  else
    ignore @@ command "cp" ["-a"; opamroot0; opamroot];
  Sys.chdir dir;
  let dir = Sys.getcwd () in (* because it may need to be normalised on OSX *)
  if t.repo_hash = no_opam_repo then
    (mkdir_p (default_repo^"/packages");
     write_file ~path:(default_repo^"/repo") ~contents:{|opam-version: "2.0"|};
     ignore @@ command opam ~silent:true
       [ "repository"; "set-url"; "default"; "./"^default_repo;
         "--root"; opamroot]);
  ignore @@ command ~silent:true opam
    ["var"; "--quiet"; "--root"; opamroot; "--global"; "--cli=2.1";
     "sys-ocaml-version=4.08.0"];
  print_endline t.repo_hash;
  let _vars =
    List.fold_left (fun vars (cmd, out) ->
        print_string cmd_prompt;
        print_endline cmd;
        match parse_command cmd with
        | Comment _ -> vars
        | File_contents path ->
          let contents = String.concat "\n" out ^ "\n" in
          write_file ~path ~contents;
          print_string contents;
          vars
        | Export bindings ->
          List.fold_left
            (fun vars (v, r) -> (v, r) :: List.filter (fun (w, _) -> v <> w) vars)
            vars bindings
        | Cat files ->
          let print_opamfile header file =
            let content =
              let open OpamParserTypes.FullPos in
              let original = OpamParser.FullPos.file file in
              let rec mangle item =
                match item.pelem with
                | Section s ->
                  {item with pelem = Section {s with section_name = OpamStd.Option.map (fun v -> {v with pelem = mangle_string v.pelem}) s.section_name;
                                                     section_items = {s.section_items with pelem = List.map mangle s.section_items.pelem}}}
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
            in
            let str = if header then Printf.sprintf "=> %s <=\n" file else "" in
            let str = Printf.sprintf "%s%s" str content in
            let str =
              str_replace_path OpamSystem.back_to_forward
                (common_filters dir) str
            in
            print_string str
          in
          let files =
            List.map (fun s -> Re.(replace_string (compile @@ str "$OPAMROOT")
                                     ~by:opamroot s)) files
          in
          (match files with
           | file::[] -> print_opamfile false file
           | _::_  -> List.iter (print_opamfile true) files
           | [] -> ());
          vars
        | Run {env; cmd; args; filter; output; unordered} ->
          let silent = output <> None || unordered in
          let r, errcode =
            run_cmd ~opam ~dir ~vars:(vars @ env) ~filter ~silent cmd args
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
    let vars =
      List.map (fun s -> match OpamStd.String.cut_at s '=' with
          | Some (var, value) -> var, value
          | None -> failwith "Bad 'var=value' argument")
        env
    in
    load_test input |> run_test ~opam ~vars
  | _ ->
    failwith "Expected arguments: opam.exe opam file.test [env-bindings]"
