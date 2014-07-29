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

exception Process_error of OpamProcess.result
exception Internal_error of string
exception Command_not_found of string

let log fmt = OpamGlobals.log "SYSTEM" fmt

let internal_error fmt =
  Printf.ksprintf (fun str ->
    log "error: %s" str;
    raise (Internal_error str)
  ) fmt

let process_error r =
  raise (Process_error r)

let command_not_found cmd =
  raise (Command_not_found cmd)

module Sys2 = struct
  (* same as [Sys.is_directory] except for symlinks, which returns always [false]. *)
  let is_directory file =
    try Unix.( (lstat file).st_kind = S_DIR )
    with Unix.Unix_error _ as e -> raise (Sys_error (Printexc.to_string e))
end

let (/) = Filename.concat

(* Separator for the PATH variables *)
let path_sep = match OpamGlobals.os () with
  | OpamGlobals.Win32 -> ';'
  | OpamGlobals.Cygwin | _ -> ':'

let temp_basename prefix =
  Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ()) (Random.int 0xFFFFFF)

let rec mk_temp_dir () =
  let s = Filename.temp_dir_name / temp_basename "opam" in
  if Sys.file_exists s then
    mk_temp_dir ()
  else
    s

let safe_mkdir dir =
  if not (Sys.file_exists dir) then
    try
      log "mkdir %s" dir;
      Unix.mkdir dir 0o755
    with
      Unix.Unix_error(Unix.EEXIST,_,_) -> ()

let mkdir dir =
  let rec aux dir =
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      safe_mkdir dir;
    end in
  aux dir

let temp_files = Hashtbl.create 1024

let rec temp_file ?dir prefix =
  let temp_dir = match dir with
    | None   -> !OpamGlobals.root_dir / "log"
    | Some d -> d in
  mkdir temp_dir;
  let file = temp_dir / temp_basename prefix in
  if Hashtbl.mem temp_files file then
    temp_file ?dir prefix
  else (
    Hashtbl.add temp_files file true;
    file
  )

let remove_file file =
  if Sys.file_exists file
  || (try let _ = Unix.lstat file in true with Unix.Unix_error _ -> false)
  then (
    try
      log "rm %s" file;
      Unix.unlink file
    with Unix.Unix_error _ as e ->
      internal_error "Cannot remove %s (%s)." file (Printexc.to_string e)
  )

let string_of_channel ic =
  let n = 32768 in
  let s = String.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_substring b s 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let read file =
  let ic = open_in_bin file in
  let s = string_of_channel ic in
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
    internal_error "%s does not exist." dir

let in_dir dir fn =
  let reset_cwd =
    let cwd =
      try Some (Sys.getcwd ())
      with Sys_error _ -> None in
    fun () ->
      match cwd with
      | None     -> ()
      | Some cwd -> try chdir cwd with Unix.Unix_error _ -> () in
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
      let d = Sys.readdir (Sys.getcwd ()) in
      let d = Array.to_list d in
      let l = List.filter kind d in
      List.sort compare (List.rev_map (Filename.concat dir) l)
    )
  else
    []

let files_with_links =
  list (fun f -> try not (Sys.is_directory f) with Sys_error _ -> true)

let files_all_not_dir =
  list (fun f -> try not (Sys2.is_directory f) with Sys_error _ -> true)

let directories_strict =
  list (fun f -> try Sys2.is_directory f with Sys_error _ -> false)

let directories_with_links =
  list (fun f -> try Sys.is_directory f with Sys_error _ -> false)

let rec_files dir =
  let rec aux accu dir =
    let d = directories_with_links dir in
    let f = files_with_links dir in
    List.fold_left aux (f @ accu) d in
  aux [] dir

let files dir =
  files_with_links dir

let rec_dirs dir =
  let rec aux accu dir =
    let d = directories_with_links dir in
    List.fold_left aux (d @ accu) d in
  aux [] dir

let dirs dir =
  directories_with_links dir

let dir_is_empty dir =
  Sys.file_exists dir &&
  in_dir dir (fun () -> Sys.readdir (Sys.getcwd ()) = [||])

(* XXX: won't work on windows *)
let remove_dir dir =
  log "rmdir %s" dir;
  if Sys.file_exists dir then (
    let err = Sys.command (Printf.sprintf "rm -rf %s" dir) in
      if err <> 0 then
        internal_error "Cannot remove %s (error %d)." dir err
  )

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
  let p = Sys.getcwd () in
  chdir s;
  p

let normalize s =
  getchdir (getchdir s)

let needs_normalization f =
  Sys.file_exists f && Filename.is_relative f

let real_path p =
  if not (needs_normalization p) then p
  else if Sys.is_directory p then
    normalize p
  else (
    let dir =
      let d = Filename.dirname p in
      if needs_normalization d then normalize d else d in
    let base = Filename.basename p in
    if base = "." then
      dir
    else
      dir / base
  )
type command = string list

let default_env =
  Unix.environment ()

let reset_env = lazy (
  let env = OpamMisc.env () in
  let env =
    let path_sep_str = String.make 1 path_sep in
    List.rev_map (fun (k,v as c) ->
      match k with
      | "PATH" ->
        k, String.concat path_sep_str
          (OpamMisc.reset_env_value path_sep ~prefix:!OpamGlobals.root_dir v)
      | _      -> c
    ) env in
  let env = List.rev_map (fun (k,v) -> k^"="^v) env in
  Array.of_list env
)

let command_exists ?(env=default_env) name =
  let cmd, args = "/bin/sh", ["-c"; Printf.sprintf "command -v %s" name] in
  let r = OpamProcess.run ~env ~name:(temp_file "command") ~verbose:false cmd args in
  OpamProcess.clean_files r;
  if OpamProcess.is_success r then
    let is_external_cmd s = String.contains s '/' in
    match r.OpamProcess.r_stdout with 
      cmdname::_ -> (* check that we have permission to execute the command *)
	if is_external_cmd cmdname then 
	  (try 
	    let open Unix in 
	    let uid = getuid() and groups = Array.to_list(getgroups()) in
	    let s = stat cmdname in
	    let cmd_uid = s.st_uid and cmd_gid = s.st_gid and cmd_perms = s.st_perm in
            let mask = 0o001
		lor (if uid = cmd_uid then 0o100 else 0)
		lor (if List.mem cmd_gid groups then 0o010 else 0) in
	    (cmd_perms land mask) <> 0
          with _ -> false)
	else true
    | _ -> false
  else false

let runs = ref []
let print_stats () =
  match !runs with
  | [] -> ()
  | l  ->
    OpamGlobals.msg "%d external processes called:\n  %s\n%!"
      (List.length l) (String.concat "\n  " (List.map (String.concat " ") l))

let log_file name = match name with
  | None   -> temp_file "log"
  | Some n -> temp_file ~dir:(Sys.getcwd ()) n

let log_cleanup r =
  if not !OpamGlobals.debug then OpamProcess.clean_files r

let run_process ?verbose ?(env=default_env) ~name ?metadata command =
  let chrono = OpamGlobals.timer () in
  runs := command :: !runs;
  match command with
  | []          -> invalid_arg "run_process"
  | cmd :: args ->

    (* Check that the command doesn't contain whitespaces *)
    if None <> try Some (String.index cmd ' ') with Not_found -> None then
      OpamGlobals.warning "Command %S contains 1 space" cmd;

    if command_exists ~env cmd then (

      let verbose = match verbose with
        | None   -> !OpamGlobals.debug || !OpamGlobals.verbose
        | Some b -> b in

      let r = OpamProcess.run ~env ~name ~verbose ?metadata cmd args in
      let str = String.concat " " (cmd :: args) in
      log "[%a] (in %.3fs) %s"
        (OpamGlobals.slog Filename.basename) name
        (chrono ()) str;
      r
    ) else
      (* Display a user-friendly message if the command does not exist *)
      command_not_found cmd

let command ?verbose ?env ?name ?metadata cmd =
  let name = log_file name in
  let r = run_process ?verbose ?env ~name ?metadata cmd in
  if OpamProcess.is_success r then log_cleanup r
  else process_error r

let commands ?verbose ?env ?name ?metadata ?(keep_going=false) commands =
  let name = log_file name in
  let run = run_process ?verbose ?env ~name ?metadata in
  let command r0 c =
    if keep_going || OpamProcess.is_success r0 then
      let r = run c in
      (if OpamProcess.is_failure r0 then r0 else r)
    else r0
  in
  match commands with
  | [] -> ()
  | c1::c ->
    let r = List.fold_left command (run c1) c in
    if OpamProcess.is_success r then log_cleanup r
    else process_error r

let read_command_output ?verbose ?env ?metadata cmd =
  let name = log_file None in
  let r = run_process ?verbose ?env ~name ?metadata cmd in
  if OpamProcess.is_success r then
    (log_cleanup r; r.OpamProcess.r_stdout)
  else
    process_error r

(* Return [None] if the command does not exist *)
let read_command_output_opt ?verbose ?env cmd =
  try Some (read_command_output ?verbose ?env cmd)
  with Command_not_found _ -> None

let copy src dst =
  if Sys.is_directory src then
    internal_error "Cannot copy %s: it is a directory." src;
  if Sys.file_exists dst && Sys.is_directory dst then
    internal_error "Cannot copy to %s: it is a directory." dst;
  if Sys.file_exists dst then
    remove_file dst;
  mkdir (Filename.dirname dst);
  command ["cp"; src; dst ]

let is_exec file =
  let stat = Unix.stat file in
  stat.Unix.st_kind = Unix.S_REG &&
  stat.Unix.st_perm land 0o111 <> 0

let install ?exec src dst =
  if Sys.is_directory src then
    internal_error "Cannot install %s: it is a directory." src;
  if Sys.file_exists dst && Sys.is_directory dst then
    internal_error "Cannot install to %s: it is a directory." dst;
  mkdir (Filename.dirname dst);
  let exec = match exec with
    | Some e -> e
    | None -> is_exec src in
  command
    ("install" :: "-m" :: (if exec then "0755" else "0644") ::
     [ src; dst ])

module Tar = struct

  let extensions =
    [ [ "tar.gz" ; "tgz" ], 'z'
    ; [ "tar.bz2" ; "tbz" ], 'j'
    ; [ "tar.xz" ; "txz" ], 'J'
    ; [ "tar.lzma" ; "tlz" ], 'Y'
    ]

  let guess_type f =
    let ic = open_in f in
    let c1 = input_char ic in
    let c2 = input_char ic in
    close_in ic;
    match c1, c2 with
    | '\031', '\139' -> Some 'z'
    | 'B'   , 'Z'    -> Some 'j'
    | '\xfd', '\x37' -> Some 'J'
    | '\x5d', '\x00' -> Some 'Y'
    | _              -> None

  let match_ext file ext =
    List.exists (Filename.check_suffix file) ext

  let is_archive f =
    List.exists
      (fun suff -> Filename.check_suffix f suff)
      (List.concat (List.rev_map fst extensions))

  let extract_function file =
    let command c dir =
      command [ "tar" ; Printf.sprintf "xf%c" c ; file; "-C" ; dir ] in

    let ext =
      List.fold_left
        (fun acc (ext, c) -> match acc with
          | Some f -> Some f
          | None   ->
            if match_ext file ext
            then Some (command c)
            else None)
        None
        extensions in
    match ext with
    | Some f -> Some f
    | None   ->
      match guess_type file with
      | None   -> None
      | Some c -> Some (command c)

end

module Zip = struct
  let is_archive f = Filename.check_suffix f "zip"

  let extract_function file =
    Some (fun dir -> command [ "unzip" ; file; "-d"; dir ])
end

let is_tar_archive = Tar.is_archive

let extract file dst =
  let kind, extract_function =
    if Zip.is_archive file then "zip", Zip.extract_function
    else "tar", Tar.extract_function
  in
  with_tmp_dir (fun tmp_dir ->
    match extract_function file with
    | None   -> internal_error "%s is not a valid %s archive." file kind
    | Some f ->
      f tmp_dir;
      if Sys.file_exists dst then
        internal_error "Extracting the archive will overwrite %s." dst;
      match directories_strict tmp_dir with
      | [x] ->
        mkdir (Filename.dirname dst);
        command [ "mv"; x; dst]
      | _   ->
        internal_error "The archive %S contains multiple root directories."
          file
  )

let extract_in file dst =
  if not (Sys.file_exists dst) then
    internal_error "%s does not exist." file;
  match Tar.extract_function file with
  | None   -> internal_error "%s is not a valid tar archive." file
  | Some f -> f dst

let link src dst =
  if Sys.file_exists src then (
    mkdir (Filename.dirname dst);
    if Sys.file_exists dst then
      remove_file dst;
    try
      log "ln -s %s %s" src dst;
      Unix.symlink src dst
    with Unix.Unix_error (Unix.EXDEV, _, _) ->
      (* Fall back to copy if hard links are not supported *)
      copy src dst
  ) else
    internal_error "link: %s does not exist." src

type lock = Unix.file_descr * string

let flock ?(read=false) file =
  let max_tries = 5 in
  let fd =
    Unix.openfile file [Unix.O_CREAT; Unix.O_RDWR] 0o600 in
  let lock_op = if read then Unix.F_TRLOCK else Unix.F_TLOCK in
  let rec loop attempt =
    try Unix.lockf fd lock_op 0
    with Unix.Unix_error (Unix.EAGAIN,_,_) ->
      if attempt > max_tries then
        OpamGlobals.error_and_exit
          "Timeout trying to acquire %s lock to %S, \
           is another opam process running ?"
          (if read then "read" else "write") file;
      log
        "Failed to %s-lock %S. (attempt %d/%d)"
        (if read then "read" else "write")
        file attempt max_tries;
      Unix.sleep 1;
      loop (attempt + 1)
  in
  log "locking %s" file;
  loop 1;
  fd, file

let funlock (fd,file) =
  (try (* Unlink file if write lock can be acquired *)
     Unix.lockf fd Unix.F_TLOCK 0;
     Unix.unlink file;
   with Unix.Unix_error _ -> ());
  Unix.close fd; (* implies Unix.lockf fd Unix.F_ULOCK 0 *)
  log "Lock released on %s" file

let ocaml_version = lazy (
  match read_command_output_opt ~verbose:false [ "ocamlc" ; "-version" ] with
  | Some (h::_) ->
    let version = OpamMisc.strip h in
    log "ocamlc version: %s" version;
    Some version
  | Some [] -> internal_error "`ocamlc -version` is empty."
  | None    -> None
)

let exists_alongside_ocamlc name =
  let path = try OpamMisc.getenv "PATH" with Not_found -> "" in
  let path = OpamMisc.split path path_sep in
  let ocamlc_dir =
    List.fold_left (function
        | None -> fun d ->
          if Sys.file_exists (d/"ocamlc") then Some d else None
        | s -> fun _ -> s)
      None path
  in
  match ocamlc_dir with
  | Some d -> Sys.file_exists (d / name)
  | None -> false

let ocaml_opt_available = lazy (exists_alongside_ocamlc "ocamlc.opt")
let ocaml_native_available = lazy (exists_alongside_ocamlc "ocamlopt")
let ocaml_natdynlink_available libdir =
  Sys.file_exists (libdir / "ocaml" / "dynlink.cmxa")

(* Reset the path to get the system compiler *)
let system command = lazy (
  let env = Lazy.force reset_env in
  match read_command_output_opt ~verbose:false ~env command with
  | None        -> None
  | Some (h::_) -> Some (OpamMisc.strip h)
  | Some ([])   -> internal_error "%S is empty." (String.concat " " command)
)

let system_ocamlc_where = system [ "ocamlc"; "-where" ]

let system_ocamlc_version = system [ "ocamlc"; "-version" ]

let download_command =
  let retry = string_of_int OpamGlobals.download_retry in
  let wget ~compress:_ src =
    let wget = [
      "wget";
      "--content-disposition"; "--no-check-certificate";
      "-t"; retry;
      src
    ] in
    command wget in
  let curl command ~compress src =
    let curl = [
      command;
      "--write-out"; "%{http_code}\\n"; "--insecure";
      "--retry"; retry; "--retry-delay"; "2";
    ] @ (if compress then ["--compressed"] else []) @ [
        "-OL"; src
    ] in
    match read_command_output curl with
    | [] -> internal_error "curl: empty response while downloading %s" src
    | l  ->
      let code = List.hd (List.rev l) in
      try if int_of_string code >= 400 then raise Exit
      with e ->
        OpamMisc.fatal e;
        internal_error "curl: code %s while downloading %s" code src in
  lazy (
    match OpamGlobals.curl_command with
    | Some cmd -> curl cmd
    | None ->
      if command_exists "curl" then
        curl "curl"
      else if command_exists "wget" then
        wget
      else
        internal_error "Cannot find curl nor wget."
  )

let really_download ~overwrite ?(compress=false) ~src ~dst =
  let download = (Lazy.force download_command) in
  let aux () =
    download ~compress src;
    match list (fun _ -> true) "." with
      ( [] | _::_::_ ) ->
      internal_error "Too many downloaded files."
    | [filename] ->
      if not overwrite && Sys.file_exists dst then
        internal_error "The downloaded file will overwrite %s." dst;
      commands [
        [ "rm"; "-f"; dst ];
        [ "mv"; filename; dst ];
      ];
      dst
  in
  try with_tmp_dir (fun tmp_dir -> in_dir tmp_dir aux)
  with
  | Internal_error s as e -> OpamGlobals.error "%s" s; raise e
  | e ->
    OpamMisc.fatal e;
    internal_error "Cannot download %s, please check your connection settings." src

let download ~overwrite ?compress ~filename:src ~dst:dst =
  if dst = src then
    dst
  else if Sys.file_exists src then (
    if not overwrite && Sys.file_exists dst then
      internal_error "The downloaded file will overwrite %s." dst;
    commands [
      [ "rm"; "-f"; dst ];
      [ "cp"; src; dst ]
    ];
    dst
  ) else
    really_download ~overwrite ?compress ~src ~dst

let patch p =
  let max_trying = 5 in
  if not (Sys.file_exists p) then
    internal_error "Patch file %S not found." p;
  let patch ~dryrun n =
    let opts = if dryrun then
        let open OpamGlobals in
        match OpamGlobals.os () with
        | FreeBSD | OpenBSD | NetBSD | DragonFly -> [ "-t"; "-C" ]
        | Unix | Linux | Darwin -> [ "--dry-run" ]
        | Win32 | Cygwin (* this is probably broken *)
        | Other _               -> [ "--dry-run" ]
      else [] in
    let verbose = if dryrun then Some false else None in
    command ?verbose ("patch" :: ("-p" ^ string_of_int n) :: "-i" :: p :: opts) in
  let rec aux n =
    if n = max_trying then
      internal_error "Patch %s does not apply." p
    else if None =
            try Some (patch ~dryrun:true n)
            with e -> OpamMisc.fatal e; None then
      aux (succ n)
    else
      patch ~dryrun:false n in
  aux 0

let () =
  let with_opam_info m =
    let git_version = match OpamVersion.git with
      | None   -> ""
      | Some v -> Printf.sprintf " (%s)" (OpamVersion.to_string v) in
    let opam_version =
      Printf.sprintf "%s%s" (OpamVersion.to_string OpamVersion.current) git_version in
    let os = OpamGlobals.os_string () in
    Printf.sprintf
      "# %-15s %s\n\
       # %-15s %s\n\
       %s"
      "opam-version" opam_version
      "os" os
      m in
  Printexc.register_printer (function
    | Process_error r     -> Some (OpamProcess.string_of_result r)
    | Internal_error m    -> Some (with_opam_info m)
    | Command_not_found c -> Some (Printf.sprintf "%S: command not found." c)
    | Sys.Break           -> Some "User interruption"
    | Unix.Unix_error (e, fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s"
          Sys.argv.(0) fn msg (Unix.error_message e) in
      Some (with_opam_info error)
    | _ -> None
  )
