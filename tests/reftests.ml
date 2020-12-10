#!/usr/bin/env ocaml

#load "unix.cma"

let testdir =
  let d =
    Filename.concat
      (Filename.dirname Sys.argv.(0))
      "reftests"
  in
  if Filename.is_relative d then
    Filename.concat (Sys.getcwd ()) d
  else d

let test_files =
  match Array.to_list Sys.argv with
  | [] | [_] ->
    Array.fold_right (fun f acc ->
        if Filename.extension f = ".test"
        then Filename.concat testdir f :: acc
        else acc)
      (Sys.readdir testdir)
      []
  | _ :: args ->
    List.map (fun s ->
        let s =
          if Filename.check_suffix s ".test" then s
          else s ^ ".test"
        in
        if Filename.is_implicit s then
          Filename.concat testdir s
        else s)
      args

type test = {
  name: string;
  repo_hash: string;
  commands: (string * string list) list;
}

let cmd_prompt = "### "

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
  let name = Filename.remove_extension (Filename.basename f) in
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
  { name; repo_hash; commands }

let command ?(verbose=false) fmt =
  Printf.ksprintf (fun str ->
      if verbose then Printf.eprintf "+ %s\n%!" str;
      let ret = Sys.command str in
      if ret <> 0 then
        Printf.ksprintf failwith
          "Error code %d: %s"
          ret str)
    fmt

let finally f x k = match f x with
  | r -> k (); r
  | exception e -> (try k () with _ -> ()); raise e

let rec with_temp_dir f =
  let s =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "opam-reftest-%06x" (Random.int 0xffffff))
  in
  if Sys.file_exists s then
    with_temp_dir f
  else
  (command "mkdir -p %s" s;
   finally f s @@ fun () -> command "rm -rf %s" s)

let with_chdir d f =
  let oldcwd = Sys.getcwd () in
  finally (fun () -> Sys.chdir d; f ()) () (fun () -> Sys.chdir oldcwd)

let with_repo hash f =
  with_temp_dir (fun repo_dir ->
      let tgz = Filename.concat repo_dir "archive.tar.gz" in
      command "wget --quiet -O %s https://github.com/ocaml/opam-repository/archive/%s.tar.gz"
        tgz hash;
      command "cd %s && tar xzf %s --strip-components=1"
        repo_dir tgz;
      command "rm -f %s" tgz;
      f repo_dir)

let opam_cmd =
  let ( / ) = Filename.concat in
  testdir / ".." / ".." / "opam"

let run_cmd ?(verbose=true) opamroot logfile fmt =
  let complete_opam_cmd cmd args =
    Printf.sprintf  "%s %s --root=%s %s 2>&1 |sed 's#%s#${BASEDIR}#g' >>%s"
      opam_cmd cmd opamroot (String.concat " " args)
      (Filename.dirname opamroot) logfile
  in
  Printf.ksprintf (fun cmd ->
      try
        match String.split_on_char ' ' cmd with
        | "opam" :: cmd :: args ->
          command ~verbose "%s" (complete_opam_cmd cmd args)
        | lst ->
          let rec split var = function
            | v::r when Char.uppercase_ascii v.[0] = v.[0] ->
              split (v::var) r
            | "opam" :: cmd :: args ->
              Some (List.rev var, cmd, args)
            | _ -> None
          in
          match split [] lst with
          | Some (vars, cmd, args) ->
            command ~verbose "%s %s" (String.concat " " vars) (complete_opam_cmd cmd args)
          | None ->
            command ~verbose "%s >>%s 2>&1" cmd logfile
      with Failure _ -> ())
    fmt

let run_test t =
  with_temp_dir @@ fun tdir ->
    List.iter (fun (var, value) -> Unix.putenv var value) [
    "OPAMKEEPBUILDDIR", "1";
    "OPAMSWITCH", "";
    "OPAMCOLOR", "never";
    "OPAMJOBS", "1";
    "OPAMDOWNLOADJOBS", "1";
    "OPAMNOENVNOTICE", "1";
    "OPAMYES", "0";
    "OCAMLRUNPARAM", "";
  ];
  let opamroot = Filename.concat tdir ".opam" in
  with_repo t.repo_hash @@
  command "opam init --root=%s \
           --no-setup --bypass-checks --no-opamrc --bare \
           file://%s >/dev/null 2>&1"
    opamroot;
  command "opam var --global --root=%s sys-ocaml-version=4.08.0"
    opamroot;
  let out_dir = testdir ^ ".out" in
  command "mkdir -p %s" (Filename.quote out_dir);
  let logfile = Filename.concat out_dir t.name ^ ".test" in
  let oc = open_out logfile in
  output_string oc (t.repo_hash ^ "\n");
  close_out oc;
  with_chdir tdir @@ fun () ->
  List.iter (fun (cmd, out) ->
      run_cmd ~verbose:false opamroot logfile "echo '### '%s"
        (Filename.quote cmd);
      if cmd.[0] = '<' && cmd.[String.length cmd - 1] = '>' then
        let f = (String.sub cmd 1 (String.length cmd - 2)) in
        List.iter (fun line ->
            command ~verbose:false "echo '%s' >>%s" line (Filename.quote f);
            run_cmd ~verbose:false opamroot logfile "echo %s"
              (Filename.quote line))
          out
      else
        run_cmd opamroot logfile "%s" cmd)
    t.commands

let () =
  close_in stdin;
  List.iter (fun f ->
      Printf.eprintf "Testing %s...\n%!" f;
      let t = load_test f in
      run_test t)
    test_files
