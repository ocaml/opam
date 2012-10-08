(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

exception Process_error of OpamProcess.result
exception Internal_error of string

let _ =
  Printexc.register_printer (fun exn ->
    let open OpamProcess in
    match exn with
      Process_error r ->
        let b = Buffer.create 1000 in
        Printf.bprintf b "Exception OpamSystem.Process_error {\n";
        Printf.bprintf b "\tr_code = %d\n" r.r_code;
        Printf.bprintf b "\tr_duration = %.2f\n" r.r_duration;
        Printf.bprintf b "\tr_info = [ %s\t]\n"
          (String.concat "\n\t\t" r.r_info);
        Printf.bprintf b "\tr_stdout = [ %s\t]\n"
          (String.concat "\n\t\t" r.r_stdout);
        Printf.bprintf b "\tr_stderr = [ %s\t]\n"
          (String.concat "\n\t\t" r.r_stderr);
        Printf.bprintf b "\t\t}\n";
        Some (Buffer.contents b)
    | _ -> None)

let internal_error fmt =
  Printf.ksprintf (fun str -> raise (Internal_error str)) fmt

let process_error r =
  raise (Process_error r)

module Sys2 = struct
  open Unix

  (** behaves as [Sys.is_directory] except for symlinks, which returns always [false]. *)
  let is_directory file =
    (lstat file).st_kind = S_DIR
end

let log fmt = OpamGlobals.log "SYSTEM" fmt

let (/) = Filename.concat

let rec mk_temp_dir () =
  let s =
    Filename.temp_dir_name /
    Printf.sprintf "opam-%d-%d" (Unix.getpid ()) (Random.int 4096) in
  if Sys.file_exists s then
    mk_temp_dir ()
  else
    s

let lock_file () =
  !OpamGlobals.root_dir / "opam.lock"

let log_file () =
  Random.self_init ();
  let f = "command" ^ string_of_int (Random.int 2048) in
  !OpamGlobals.root_dir / "log" / f

let safe_mkdir dir =
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

let mkdir dir =
  let rec aux dir =
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      safe_mkdir dir;
    end in
  aux dir

let is_link filename =
  let open Unix in
  (lstat filename).st_kind = S_LNK

let remove_file file =
  try Unix.unlink file
  with Unix.Unix_error _ -> ()

let read file =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let write file contents =
  mkdir (Filename.dirname file);
  let oc = open_out_bin file in
  output_string oc contents;
  close_out oc

let chdir dir =
  if Sys.file_exists dir then (
    Unix.chdir dir
  ) else
    internal_error "%s does not exist!" dir

let in_dir dir fn =
  let reset_cwd =
    let cwd = Unix.getcwd () in
    fun () -> chdir cwd in
  chdir dir;
  try
    let r = fn () in
    reset_cwd ();
    r
  with e ->
    reset_cwd ();
    raise e

let list kind dir =
  if Sys.file_exists dir then
    in_dir dir (fun () ->
      let d = Sys.readdir (Unix.getcwd ()) in
      let d = Array.to_list d in
      let l = List.filter kind d in
      List.sort compare (List.map (Filename.concat dir) l))
  else
    []

let files_with_links =
  list (fun f -> try not (Sys.is_directory f) with _ -> true)

let files_all_not_dir =
  list (fun f -> try not (Sys2.is_directory f) with _ -> true)

let directories_strict =
  list (fun f -> try Sys2.is_directory f with _ -> false)

let directories_with_links =
  list (fun f -> try Sys.is_directory f with _ -> false)

let rec_files dir =
  let rec aux accu dir =
    let d = directories_with_links dir in
    let f = files_with_links dir in
    List.fold_left aux (f @ accu) d in
  aux [] dir

let rec remove_dir dir = (** WARNING it fails if [dir] is not a [S_DIR] or simlinks to a directory *)
  if Sys.file_exists dir then begin
    List.iter remove_file (files_all_not_dir dir);
    List.iter remove_dir (directories_strict dir);
    Unix.rmdir dir;
  end

let with_tmp_dir fn =
  let dir = mk_temp_dir () in
  try
    mkdir dir;
    let e = fn dir in
    remove_dir dir;
    e
  with e ->
    remove_dir dir;
    raise e

let remove file =
  if Sys.file_exists file && Sys2.is_directory file then
    remove_dir file
  else
    remove_file file

let getchdir s =
  let p = Unix.getcwd () in
  chdir s;
  p

let rec root path =
  let d = Filename.dirname path in
  if d = path || d = "" || d = "." then
    path
  else
    root d

(** Expand '..' and '.' *)
let normalize s =
  if Sys.file_exists s then
    getchdir (getchdir s)
  else
    s

let real_path p =
  if Sys.file_exists p && Sys.is_directory p then
    normalize p
  else (
    let dir = normalize (Filename.dirname p) in
    let dir =
      if Filename.is_relative dir then
        Sys.getcwd () / dir
      else
        dir in
    let base = Filename.basename p in
    if base = "." then
      dir
    else
      dir / base
  )

let replace_path bins =
  let path = ref "<not set>" in
  let env = Unix.environment () in
  for i = 0 to Array.length env - 1 do
    let k,v = match OpamMisc.cut_at env.(i) '=' with
      | Some (k,v) -> k,v
      | None       -> assert false in
    if k = "PATH" then (
      let bins = List.filter Sys.file_exists bins in
      let new_path = String.concat ":" bins in
      env.(i) <- "PATH=" ^ new_path;
      path := new_path;
    )
  done;
  env, !path

let get_current_path () =
  try OpamMisc.split (Sys.getenv "PATH") ':'
  with Not_found -> []

type command = string list

let run_process ?verbose ?path ?(add_to_env=[]) ?(add_to_path=[]) = function
  | []           -> invalid_arg "run_process"
  | cmd :: args ->
      let env, path =
        match path with
        | None   -> replace_path (add_to_path @ get_current_path ())
        | Some p -> replace_path p in
      let add_to_env = List.map (fun (k,v) -> k^"="^v) add_to_env in
      let env = Array.concat [ env; Array.of_list add_to_env ] in
      let name = log_file () in
      mkdir (Filename.dirname name);
      let str = String.concat " " (cmd :: args) in
      log "cwd=%s path=%s name=%s %s" (Unix.getcwd ()) path name str;
      if None <> try Some (String.index cmd ' ') with Not_found -> None then
        OpamGlobals.warning "Command %S contains 1 space" cmd;
      let verbose = match verbose with
        | None   -> !OpamGlobals.debug || !OpamGlobals.verbose
        | Some b -> b in
      let r = OpamProcess.run ~env ~name ~verbose cmd args in
      if not !OpamGlobals.debug then
        OpamProcess.clean_files r;
      r

let command ?verbose ?(add_to_env=[]) ?(add_to_path=[]) cmd =
  let r = run_process ?verbose ~add_to_env ~add_to_path cmd in
  if OpamProcess.is_success r then
    ()
  else
    process_error r

let commands ?verbose ?(add_to_env=[]) ?(add_to_path = []) commands =
  List.iter (command ?verbose ~add_to_env ~add_to_path) commands

let read_command_output ?path cmd =
  let r = run_process ?path cmd in
  if OpamProcess.is_success r then
    r.OpamProcess.r_stdout
  else
    process_error r

let copy src dst =
  if Sys.is_directory src then
    internal_error "%s is a directory!" src;
  if Sys.file_exists dst && Sys.is_directory dst then
    internal_error "%s is a directory!" dst;
  if  Sys.file_exists dst then
    remove_file dst;
  mkdir (Filename.dirname dst);
  if src <> dst then
    command ["cp"; src; dst ]

module Tar = struct

  let extensions =
    [ [ "tar.gz" ; "tgz" ], 'z'
    ; [ "tar.bz2" ; "tbz" ], 'j' ]

  let match_ext file ext =
    List.exists (Filename.check_suffix file) ext

  let assoc file =
    snd (List.find (function ext, _ -> match_ext file ext) extensions)

  let is_archive f =
    List.exists
      (fun suff -> Filename.check_suffix f suff)
      (List.concat (List.map fst extensions))

  let extract_function file =
    List.fold_left
      (function
        | Some s -> (fun _ -> Some s)
        | None   ->
            (fun (ext, c) ->
              if match_ext file ext then
                Some (fun dir -> command  [ "tar" ; Printf.sprintf "xf%c" c ; file; "-C" ; dir ])
              else
                None))
      None
      extensions
end

let is_tar_archive = Tar.is_archive

let extract file dst =
  with_tmp_dir (fun tmp_dir ->
    match Tar.extract_function file with
    | None   -> internal_error "%s is not a valid archive" file
    | Some f ->
        f tmp_dir;
        if Sys.file_exists dst then internal_error "Cannot overwrite %s" dst;
        match directories_strict tmp_dir with
        | [x] ->
            mkdir (Filename.dirname dst);
            command [ "mv"; x; dst]
        | _   -> internal_error "The archive contains mutliple root directories"
  )

let extract_in file dst =
  if not (Sys.file_exists dst) then
    OpamGlobals.error_and_exit "%s does not exist" file;
  match Tar.extract_function file with
  | None   -> internal_error "%s is not a valid archive" file
  | Some f -> f dst

let link src dst =
  mkdir (Filename.dirname dst);
  if Sys.file_exists dst then
    remove_file dst;
  Unix.link src dst

let flock () =
  let l = ref 0 in
  let file = lock_file () in
  let id = string_of_int (Unix.getpid ()) in
  let max_l = 5 in
  let rec loop () =
    if Sys.file_exists file && !l < max_l then begin
      let ic = open_in file in
      let pid = input_line ic in
      close_in ic;
      OpamGlobals.msg
        "An other process (%s) has already locked %S. Retrying in 1s (%d/%d)\n"
        pid file !l max_l;
      Unix.sleep 1;
      incr l;
      loop ()
    end else if Sys.file_exists file then begin
      OpamGlobals.msg "Too many attemps. Cancelling ...\n";
      exit 1
    end else begin
      let oc = open_out file in
      output_string oc id;
      flush oc;
      close_out oc;
      OpamGlobals.log id "locking %s" file;
    end in
  loop ()

let funlock () =
  let id = string_of_int (Unix.getpid ()) in
  let file = lock_file () in
  if Sys.file_exists file then begin
    let ic = open_in file in
    let s = input_line ic in
    close_in ic;
    if s = id then begin
      OpamGlobals.log id "unlocking %s" file;
      Unix.unlink file;
    end else
      OpamGlobals.error_and_exit "cannot unlock %s (%s)" file s
  end else
    OpamGlobals.error_and_exit "Cannot find %s" file

let with_flock f =
  try
    flock ();
    f ();
    funlock ();
  with e ->
    funlock ();
    raise e

let ocaml_version () =
  try
    match read_command_output [ "ocamlc" ; "-version" ] with
    | h::_ -> Some (OpamMisc.strip h)
    | []   -> internal_error "ocamlc -version"
  with _ ->
    None

let system_ocamlc_where () =
  try
    let path =
      try Sys.getenv "PATH"
      with Not_found -> "" in
    let path = OpamMisc.reset_env_value ~prefix:!OpamGlobals.root_dir path in
    match read_command_output ~path [ "ocamlc"; "-where" ] with
    | h::_ -> Some (OpamMisc.strip h)
    | []   -> internal_error "ocamlc -where"
  with _ ->
    None

let download_command = lazy (
  try
    command ~verbose:false ["which"; "curl"];
    (fun src -> [ "curl"; "--insecure" ; "-OL"; src ])
  with Process_error _ ->
    try
      command ~verbose:false ["which"; "wget"];
      (fun src -> [ "wget"; "--no-check-certificate" ; src ])
    with Process_error _ ->
      internal_error "Cannot find curl nor wget"
)

let download ~filename:src ~dirname:dst =
  let cmd = (Lazy.force download_command) src in
  let dst_file = dst / Filename.basename src in
  if dst_file = src then
    ()
  else if Sys.file_exists src then
    commands [
      [ "rm"; "-f"; dst_file ];
      [ "cp"; src; dst ]
    ]
  else (
    try in_dir dst (fun () -> command cmd)
    with _ ->
      OpamGlobals.error_and_exit "Cannot download %s, please check your connection settings." src
  );
  dst_file

let patch =
  let max_trying = 20 in
  fun p ->
    let patch opts n =
      command ("patch" :: ("-p" ^ string_of_int n) :: "-i" :: p :: opts) in
    let rec aux n =
      if n = max_trying then
        OpamGlobals.error_and_exit "Patching failed, can not determine the '-p' level to patch."
      else if None = try Some (patch ["--dry-run"] n) with _ -> None then
        aux (succ n)
      else
        patch [] n in
    aux 0
