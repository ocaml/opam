(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let log fmt = Globals.log "RUN" fmt

let (/) = Filename.concat

let rec mk_temp_dir str =
  let s =
    Filename.temp_dir_name /
    Printf.sprintf "opam-%s.%d-%d" str (Unix.getpid ()) (Random.int 4096) in
  if Sys.file_exists s then
    mk_temp_dir str
  else
    s

let tmp_dir =
  mk_temp_dir "run"

let lock_file () =
  !Globals.root_path / "opam.lock"

let log_file () =
  Random.self_init ();
  let f = "command" ^ string_of_int (Random.int 2048) in
  !Globals.root_path / "log" / f

let mkdir dir =
  log "mkdir %s" dir;
  let rec aux dir = 
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      Unix.mkdir dir 0o755;
    end in
  aux dir
  
let copy src dst =
  log "copying %s to %s" src dst;
  let n = 1024 in
  let b = String.create n in
  let read = ref min_int in
  let ic = open_in_bin src in
  let oc =
    if Sys.file_exists dst then
    open_out_bin dst
    else
    let perm = (Unix.stat src).Unix.st_perm in
    mkdir (Filename.dirname dst);
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] perm dst
  in
  while !read <>0 do
    read := input ic b 0 n;
    output oc b 0 !read;
  done;
  close_in ic;
  close_out oc;
  let st = Unix.lstat src in
  Unix.utimes dst (st.Unix.st_atime) (st.Unix.st_mtime)

let read file =
  log "read %s" file;
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let write file contents =
  mkdir (Filename.dirname file);
  log "write %s" file;
  let oc = open_out_bin file in
  output_string oc contents;
  close_out oc

let cwd = Unix.getcwd

let chdir dir =
  if Sys.file_exists dir then
    Unix.chdir dir
  else
    Globals.error_and_exit "%s does not exists!" dir

let in_dir dir fn =
  let cwd = Unix.getcwd () in
  chdir dir;
  try
    let r = fn () in
    chdir cwd;
    r
  with e ->
    chdir cwd;
    raise e
    
let list kind dir =
  in_dir dir (fun () ->
    let d = Sys.readdir (Unix.getcwd ()) in
    let d = Array.to_list d in
    let l = List.filter kind d in
    List.sort compare (List.map (Filename.concat dir) l)
  )

let files =
  list (fun f -> try not (Sys.is_directory f) with _ -> true)

let directories =
  list (fun f -> try Sys.is_directory f with _ -> false)

let rec_files dir =
  let rec aux accu dir =
    let d = directories dir in
    let f = files dir in
    List.fold_left aux (f @ accu) d in
  aux [] dir

let remove_file file =
  log "remove_file %s" file;
  try Unix.unlink file
  with Unix.Unix_error _ -> ()
    
let rec remove_dir dir =
  if Sys.file_exists dir then begin
    List.iter remove_file (files dir);
    List.iter remove_dir (directories dir);
    log "remove_dir %s" dir;
    Unix.rmdir dir;
  end

let remove file =
  if Sys.file_exists file && Sys.is_directory file then
    remove_dir file
  else
    remove_file file

let getchdir s =
  let p = Unix.getcwd () in
  let () = chdir s in
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

let add_path bins = 
  let path = ref "<not set>" in
  let env = Unix.environment () in
  for i = 0 to Array.length env - 1 do
    let k,v = match Utils.cut_at env.(i) '=' with
      | Some (k,v) -> k,v
      | None       -> assert false in
    if k = "PATH" then
    let new_path = match List.filter Sys.file_exists bins with
      | [] -> v
      | l  -> String.concat ":" l ^ ":" ^ v in
    env.(i) <- "PATH=" ^ new_path;
    path := new_path;
  done;
  env, !path

type command = string list

let run_process ?(add_to_env=[]) ?(add_to_path=[]) = function
  | []           -> invalid_arg "run_process"
  | cmd :: args ->
      let env, path = add_path add_to_path in
      let add_to_env = List.map (fun (k,v) -> k^"="^v) add_to_env in
      let env = Array.concat [ env; Array.of_list add_to_env ] in
      let name = log_file () in
      mkdir (Filename.dirname name);
      let str = String.concat " " (cmd :: args) in
      log "cwd=%s path=%s name=%s %s" (Unix.getcwd ()) path name str;
      let r = Process.run ~env ~name cmd args in
      if Process.is_failure r then (
        Globals.error "Command %S failed (see %s.{info,err,out})" str name;
        List.iter (Globals.msg "%s\n") r.Process.r_stdout;
        List.iter (Globals.msg "%s\n") r.Process.r_stderr;
        Globals.exit 1
      ) else if not !Globals.debug then
        Process.clean_files r;
      r

let command ?(add_to_env=[]) ?(add_to_path=[]) cmd =
  let r = run_process ~add_to_env ~add_to_path cmd in
  r.Process.r_code

let fold f =
  List.fold_left (fun err cmd ->
    match err, cmd with
    | _  , [] -> err
    | 0  , _  -> f cmd
    | err, _  -> err
  ) 0

let commands ?(add_to_env=[]) ?(add_to_path = []) = 
  fold (command ~add_to_env ~add_to_path)

let read_command_output ?(add_to_env=[]) ?(add_to_path=[]) cmd =
  let r = run_process ~add_to_env ~add_to_path cmd in
  r.Process.r_stdout

let is_archive file =
  List.fold_left
    (function
      | Some s -> fun _ -> Some s
      | None -> fun (ext, c) -> 
        if List.exists (Filename.check_suffix file) ext then
          Some (fun dir -> command  [ "tar" ; "xf"^c ; file; "-C" ; dir ])
        else
          None)
    None
    [ [ "tar.gz" ; "tgz" ], "z"
    ; [ "tar.bz2" ; "tbz" ], "j" ]

let extract file dst =
  log "untar %s" file;
  let files = read_command_output [ "tar" ; "tf" ; file ] in
  log "%s contains %d files: %s" file (List.length files) (String.concat ", " files);
  mkdir tmp_dir;
  let err =
    match is_archive file with
    | Some f_cmd -> f_cmd tmp_dir
    | None       -> Globals.error_and_exit "%s is not a valid archive" file in
  if err <> 0 then
    Globals.error_and_exit "Error while extracting %s" file
  else
    let aux accu name =
      if
        not 
          (let n = tmp_dir / name in
           try Sys.is_directory n with
             | Sys_error s when s = Printf.sprintf "%s: No such file or directory" n 
               (* for instance, when wrong symbolic link *) -> 
               Globals.warning "the file %s is skipped" n;
               true) 
      then
        let root = root name in
        let n = String.length root in
        let rest = String.sub name n (String.length name - n) in 
        (tmp_dir / name, dst ^  rest) :: accu
      else
        accu in
    let moves = List.fold_left aux [] files in
    List.iter (fun (src, dst) ->
      mkdir (Filename.dirname dst);
      copy src dst
    ) moves

let link src dst =
  log "linking %s to %s" src dst;
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
      Globals.msg
        "An other process (%s) has already locked %S. Retrying in 1s (%d/%d)\n"
        pid file !l max_l;
      Unix.sleep 1;
      incr l;
      loop ()
    end else if Sys.file_exists file then begin
      Globals.msg "Too many attemps. Cancelling ...\n";
      exit 1
    end else begin
      let oc = open_out file in
      output_string oc id;
      flush oc;
      close_out oc;
      Globals.log id "locking %s" file;
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
      Globals.log id "unlocking %s" file;
      Unix.unlink file;
    end else
      Globals.error_and_exit "cannot unlock %s (%s)" file s
  end else
    Globals.error_and_exit "Cannot find %s" file

let with_flock f x =
  try
    flock ();
    let r = f x in
    funlock ();
    r
  with e ->
    funlock ();
    raise e

let ocaml_version () =
  try
    let s = read_command_output [ "ocamlc" ; "-version" ] in
    Some (Utils.string_strip (List.hd s))
  with _ ->
    None

(* Only used by the compiler switch stuff *)
let download src dst =
  let cmd = match Globals.os with
    | Globals.Darwin -> [ "curl"; "-OL"; src ]
    | _              -> [ "wget"; src ] in
  mkdir tmp_dir;
  let e = in_dir tmp_dir (fun () -> command cmd) in
  let tmp_file = tmp_dir / Filename.basename src in
  if e = 0 then
    if Filename.check_suffix src "tar.gz"
    || Filename.check_suffix src "tar.bz2" then
      extract tmp_file dst
    else
      copy tmp_file (dst / Filename.basename src)

let patch p =
  let err = command ["patch"; "-p0"; "-i"; p] in
  if err <> 0 then
    Globals.error_and_exit "Cannot apply patch %s" p
