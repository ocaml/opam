type test = {
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

let command fmt =
  Printf.ksprintf (fun str ->
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
      (Printf.sprintf "opam-reftest-%06x.opam" (Random.int 0xffffff))
  in
  if Sys.file_exists s then
    with_temp_dir f
  else
  (command "mkdir -p %s" s;
   finally f s @@ fun () -> command "rm -rf %s" s)

let run_cmd ~opam ~opamroot cmd =
  let complete_opam_cmd cmd args =
    Printf.sprintf "%s %s --color=never --root=%s %s 2>&1 |sed 's#%s#${BASEDIR}#g'"
      opam cmd opamroot (String.concat " " args)
      (Sys.getcwd ())
  in
  try
    match OpamStd.String.split cmd ' ' with
    | "opam" :: cmd :: args ->
      command "%s" (complete_opam_cmd cmd args)
    | lst ->
      let rec split var = function
        | v::r when OpamCompat.Char.uppercase_ascii v.[0] = v.[0] ->
          split (v::var) r
        | "opam" :: cmd :: args ->
          Some (List.rev var, cmd, args)
        | _ -> None
      in
      match split [] lst with
      | Some (vars, cmd, args) ->
        command "%s %s" (String.concat " " vars) (complete_opam_cmd cmd args)
      | None ->
        command "%s 2>&1" cmd
  with Failure _ -> ()

type command =
  | Run
  | File_contents of string

let parse_command cmd =
  if cmd.[0] = '<' && cmd.[String.length cmd - 1] = '>' then
    let f = String.sub cmd 1 (String.length cmd - 2) in
    File_contents f
  else
    Run

let write_file ~path ~contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let run_test t ~opam ~repo_dir =
  with_temp_dir @@ fun opamroot ->
  command "%s init --root=%s \
           --no-setup --bypass-checks --no-opamrc --bare \
           file://%s >/dev/null 2>&1"
    opam
    opamroot
    repo_dir;
  command "%s var --quiet --global --root=%s sys-ocaml-version=4.08.0 >/dev/null"
    opam opamroot;
  print_endline t.repo_hash;
  List.iter (fun (cmd, out) ->
      print_string cmd_prompt;
      print_endline cmd;
      match parse_command cmd with
      | File_contents path ->
        let contents = String.concat "\n" out in
        write_file ~path ~contents;
        print_endline contents
      | Run ->
        run_cmd ~opam ~opamroot cmd)
    t.commands

let () =
  let opam = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  let repo_dir = Sys.argv.(3) in
  load_test input |> run_test ~opam ~repo_dir
