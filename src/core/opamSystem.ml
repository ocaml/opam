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
    Unix.( (lstat file).st_kind = S_DIR )
end

let (/) = Filename.concat

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
  || (try let _ = Unix.lstat file in true with _ -> false)
  then (
    try
      log "rm %s" file;
      Unix.unlink file
    with e ->
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
      with _ -> None in
    fun () ->
      match cwd with
      | None     -> ()
      | Some cwd -> try chdir cwd with _ -> () in
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

let files dir =
  files_with_links dir

let rec_dirs dir =
  let rec aux accu dir =
    let d = directories_with_links dir in
    List.fold_left aux (d @ accu) d in
  aux [] dir

let dirs dir =
  directories_with_links dir

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
    List.rev_map (fun (k,v as c) ->
      match k with
      | "PATH" ->
        k, String.concat ":" (OpamMisc.reset_env_value ~prefix:!OpamGlobals.root_dir v)
      | _      -> c
    ) env in
  let env = List.rev_map (fun (k,v) -> k^"="^v) env in
  Array.of_list env
)

let command_exists ?(env=default_env) name =
  let open OpamGlobals in
  let cmd, args = match OpamGlobals.os () with
    | NetBSD
    | DragonFly -> "sh", ["-c"; Printf.sprintf "type %s" name]
    | _         -> "which", [name] in
  let r = OpamProcess.run ~env ~name:(temp_file "which") ~verbose:false cmd args in
  OpamProcess.clean_files r;
  OpamProcess.is_success r

let run_process ?verbose ?(env=default_env) ?name ?metadata = function
  | []           -> invalid_arg "run_process"
  | cmd :: args ->

    (* Set-up the log files *)
    let name = match name with
      | None   -> temp_file "log"
      | Some n -> temp_file ~dir:(Sys.getcwd ()) n in

    let str = String.concat " " (cmd :: args) in
    log "[%s] %s" (Filename.basename name) str;

    (* Check that the command doesn't contain whitespaces *)
    if None <> try Some (String.index cmd ' ') with Not_found -> None then
      OpamGlobals.warning "Command %S contains 1 space" cmd;

    if command_exists ~env cmd then (

      let verbose = match verbose with
        | None   -> !OpamGlobals.debug || !OpamGlobals.verbose
        | Some b -> b in

      let r = OpamProcess.run ~env ~name ~verbose ?metadata cmd args in
      if OpamProcess.is_success r && not !OpamGlobals.debug then
        OpamProcess.clean_files r;
      r
    ) else
      (* Display a user-friendly message if the command does not exist *)
      command_not_found cmd

let command ?verbose ?env ?name ?metadata cmd =
  let r = run_process ?verbose ?env ?name ?metadata cmd in
  if not (OpamProcess.is_success r) then
    process_error r

let commands ?verbose ?env ?name ?metadata ?(keep_going=false) commands =
  let err = ref None in
  let command =
    if keep_going then
      (fun c ->
        try command ?verbose ?env ?name ?metadata c with
        | Process_error r -> if !err = None then err := Some r)
    else
      fun c -> command ?verbose ?env ?name ?metadata c
  in
  List.iter command commands;
  match !err with
  | Some err -> raise (Process_error err)
  | None -> ()

let read_command_output ?verbose ?env ?metadata cmd =
  let r = run_process ?verbose ?env ?metadata cmd in
  if OpamProcess.is_success r then
    r.OpamProcess.r_stdout
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
  if src <> dst then
    command ["cp"; src; dst ]

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

let is_tar_archive = Tar.is_archive

let extract file dst =
  with_tmp_dir (fun tmp_dir ->
    match Tar.extract_function file with
    | None   -> internal_error "%s is not a valid tar archive." file
    | Some f ->
      f tmp_dir;
      if Sys.file_exists dst then
        internal_error "Extracting the archive will overwrite %s." dst;
      match directories_strict tmp_dir with
      | [x] ->
        mkdir (Filename.dirname dst);
        command [ "mv"; x; dst]
      | _   -> internal_error "The archive contains multiple root directories."
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

let flock file =
  let l = ref 0 in
  let id = string_of_int (Unix.getpid ()) in
  let max_l = 5 in
  let rec loop () =
    if Sys.file_exists file && !l < max_l then begin
      let ic = open_in file in
      let pid = input_line ic in
      close_in ic;
      OpamGlobals.msg
        "Another process (%s) has already locked %S. Retrying in 1s (%d/%d)\n"
        pid file !l max_l;
      Unix.sleep 1;
      incr l;
      loop ()
    end else if Sys.file_exists file then
      internal_error "Too many attempts. Cancelling."
    else begin
      let oc = open_out file in
      output_string oc id;
      flush oc;
      close_out oc;
      OpamGlobals.log id "locking %s" file;
    end in
  loop ()

let funlock file =
  let id = string_of_int (Unix.getpid ()) in
  if Sys.file_exists file then (
    let ic = open_in file in
    try
      let s = input_line ic in
      close_in ic;
      if s = id then (
        OpamGlobals.log id "unlocking %s" file;
        log "rm %s" file;
        Unix.unlink file;
      ) else
        internal_error "Cannot unlock %s (%s)." file s
    with _ ->
      OpamGlobals.error "%s is broken, removing it and continuing anyway." file;
      close_in ic;
      log "rm %s" file;
      Unix.unlink file;
  ) else
    log "Cannot find %s, but continuing anyway..." file

let ocaml_version = lazy (
  match read_command_output_opt ~verbose:false [ "ocamlc" ; "-version" ] with
  | Some (h::_) ->
    let version = OpamMisc.strip h in
    log "ocamlc version: %s" version;
    Some version
  | Some [] -> internal_error "`ocamlc -version` is empty."
  | None    -> None
)

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
  let wget src =
    let wget = [
      "wget";
      "--content-disposition"; "--no-check-certificate";
      "-t"; retry;
      src
    ] in
    command wget in
  let curl src =
    let curl = [
      "curl";
      "--write-out"; "%{http_code}\\n"; "--insecure";
      "--retry"; retry; "--retry-delay"; "2";
      "-OL"; src
    ] in
    match read_command_output curl with
    | [] -> internal_error "curl: empty response."
    | l  ->
      let code = List.hd (List.rev l) in
      try if int_of_string code >= 400 then internal_error "curl: error %s." code
      with _ -> internal_error "curl: %s is not a valid return code." code in
  lazy (
    if command_exists "curl" then
      curl
    else if command_exists "wget" then
      wget
    else
      internal_error "Cannot find curl nor wget."
  )

let really_download ~overwrite ~src ~dst =
  let download = (Lazy.force download_command) in
  let aux () =
    download src;
    match list (fun _ -> true) "." with
      ( [] | _::_::_ ) ->
      internal_error "Too many downloaded files."
    | [filename] ->
      let dst_file = dst / Filename.basename filename in
      if not overwrite && Sys.file_exists dst_file then
        internal_error "The downloaded file will overwrite %s." dst_file;
      commands [
        [ "rm"; "-f"; dst_file ];
        [ "mv"; filename; dst_file ];
      ];
      dst_file
  in
  try with_tmp_dir (fun tmp_dir -> in_dir tmp_dir aux)
  with
  | Internal_error s as e -> OpamGlobals.error "%s" s; raise e
  | _ -> internal_error "Cannot download %s, please check your connection settings." src

let download ~overwrite ~filename:src ~dirname:dst =
  let dst_file = dst / Filename.basename src in
  if dst_file = src then
    dst_file
  else if Sys.file_exists src then (
    if not overwrite && Sys.file_exists dst_file then
      internal_error "The downloaded file will overwrite %s." dst_file;
    commands [
      [ "rm"; "-f"; dst_file ];
      [ "cp"; src; dst ]
    ];
    dst_file
  ) else
    really_download ~overwrite ~src ~dst

let patch p =
  let max_trying = 20 in
  if not (Sys.file_exists p) then
    internal_error "Cannot find %s." p;
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
      internal_error "Application of %s failed: can not determine the correct patch level." p
    else if None = try Some (patch ~dryrun:true n) with _ -> None then
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
    | Unix.Unix_error (e, fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s"
          Sys.argv.(0) fn msg (Unix.error_message e) in
      Some (with_opam_info error)
    | _ -> None
  )
