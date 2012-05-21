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

let tmp_dir = Filename.concat Filename.temp_dir_name "opam-archives"

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
  close_out oc

let read file =
  log "read %s" file;
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let write file contents =
  mkdir (Filename.dirname file);
  log "write %s" file;
  let oc = open_out file in
  output_string oc contents;
  close_out oc

let cwd = Unix.getcwd

let chdir dir =
(*   log "chdir %s" dir; *)
  Unix.chdir dir

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

(* XXX: the function might block for ever for some channels kinds *)
let read_lines ic =
  let lines = ref [] in
  begin
    try
      while true do
        let line = input_line ic in
        lines := line :: !lines;
      done
    with _ -> ()
  end;
  List.rev !lines

let read_command_output_ cmd =
  let ic = Unix.open_process_in cmd in
  let lines = read_lines ic in
  if Unix.close_process_in ic <> Unix.WEXITED 0 then
    None
  else
    Some lines

let read_command_output fmt =
  Printf.kprintf (fun cmd ->
    match read_command_output_ cmd with
    | None -> Globals.error_and_exit "command %s failed" cmd
    | Some lines -> lines
  ) fmt

(** Expand '..' and '.' *)
let normalize s =
  if Sys.file_exists s then
    getchdir (getchdir s)
  else
    s

let real_path p =
  let (/) = Filename.concat in
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
    let k,v = ExtString.String.split env.(i) "=" in
    if k = "PATH" then
    let new_path = match List.filter Sys.file_exists bins with
      | [] -> v
      | l  -> String.concat ":" l ^ ":" ^ v in
    env.(i) <- "PATH=" ^ new_path;
    path := new_path;
  done;
  env, !path
  
let command_with_path bins fmt =
  Printf.kprintf (fun cmd ->
    let env, path = add_path bins in 
    log "cwd=%s path=%s %s" (Unix.getcwd ()) path cmd;
    let (o,i,e as chans) = Unix.open_process_full cmd env in
    (* we MUST read the input_channels otherwise [close_process] will fail *)
    let err = read_lines e in
    let out = read_lines o in
    let str () =
      Printf.sprintf "out: %s\nerr: %s"
        (String.concat "\n" out)
        (String.concat "\n" err) in
    let msg () =
      Globals.msg "%s\n" (str ()) in
    match Unix.close_process_full chans with
      | Unix.WEXITED 0 -> 0
      | Unix.WEXITED i -> msg (); i
      | _              -> msg (); 1
  ) fmt
 
let command fmt =
  Printf.kprintf (fun str ->
    log "cwd=%s '%s'" (Unix.getcwd ()) str;
    Sys.command str;
  ) fmt

let fold f = List.fold_left (function 0 -> f | err -> fun _ -> err) 0

let commands = fold (command "%s")

let commands_with_path bins = fold (command_with_path bins "%s")

let is_archive file =
  List.fold_left
    (function
      | Some s -> fun _ -> Some s
      | None -> fun (ext, c) -> 
        if List.exists (Filename.check_suffix file) ext then
          Some (command "tar xf%c %s -C %s" c file)
        else
          None)
    None
    [ [ "tar.gz" ; "tgz" ], 'z'
    ; [ "tar.bz2" ; "tbz" ], 'j' ]

let extract file dst =
  log "untar %s" file;
  let files = read_command_output "tar tf %s" file in
  log "%s contains %d files: %s" file (List.length files) (String.concat ", " files);
  remove_dir tmp_dir;
  mkdir tmp_dir;
  let err =
    match is_archive file with
    | Some f_cmd -> f_cmd tmp_dir
    | None       -> Globals.error_and_exit "%s is not a valid archive" file in
  if err <> 0 then
    Globals.error_and_exit "Error while extracting %s" file
  else
    let aux accu name =
      if not (Sys.is_directory (Filename.concat tmp_dir name)) then
        let root = root name in
        let n = String.length root in
        let rest = String.sub name n (String.length name - n) in 
        (Filename.concat tmp_dir name, dst ^  rest) :: accu
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

let file () = Filename.concat !Globals.root_path "opam.lock"

let flock () =
  let l = ref 0 in
  let file = file () in
  let id = string_of_int (Unix.getpid ()) in
  let rec loop () =
    if Sys.file_exists file && !l < 5 then begin
      Globals.log id "Filesytem busy. Waiting 1s (%d)" !l;
      Unix.sleep 1;
      loop ()
    end else if Sys.file_exists file then begin
      Globals.log id "Too many attemps. Cancelling ...";
      Globals.error_and_exit "Too many attemps. Cancelling ...";
    end else begin
      let oc = open_out file in
      output_string oc id;
      flush oc;
      close_out oc;
      Globals.log id "locking %s" file;
    end in
  loop ()
    
let funlock () =
  let file = file () in
  let id = string_of_int (Unix.getpid ()) in
  if Sys.file_exists file then
    let ic = open_in file in
    let s = input_line ic in
    if s = id then begin
      Globals.log id "unlocking %s" file;
      Unix.unlink file;
    end

let with_flock f x =
  try
    flock ();
    let r = f x in
    funlock ();
    r
  with e ->
    funlock ();
    raise e
