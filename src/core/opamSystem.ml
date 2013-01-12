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
    Printf.sprintf "opam-%d-%06x" (Unix.getpid ()) (Random.int 100_000) in
  if Sys.file_exists s then
    mk_temp_dir ()
  else
    s

let safe_mkdir dir =
  let open Unix in
  if not (Sys.file_exists dir) then
    try mkdir dir 0o755
    with Unix_error(EEXIST,_,_) -> ()

let mkdir dir =
  let rec aux dir =
    if not (Sys.file_exists dir) then begin
      aux (Filename.dirname dir);
      safe_mkdir dir;
    end in
  aux dir


let temp_file ?dir prefix =
  let temp_dir = match dir with
    | None   -> !OpamGlobals.root_dir / "log"
    | Some d -> d in
  mkdir temp_dir;
  temp_dir / Printf.sprintf "%s-%06x" prefix (Random.int 0xFFFFFF)

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
    internal_error "%s does not exist." dir

let in_dir dir fn =
  let reset_cwd =
    let cwd = Sys.getcwd () in
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
      let d = Sys.readdir (Sys.getcwd ()) in
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

(* WARNING it fails if [dir] is not a [S_DIR] or simlinks to a directory *)
let rec remove_dir dir =
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
  let p = Sys.getcwd () in
  chdir s;
  p

(* Expand '..' and '.' *)
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
type command = string list

let default_env =
  Unix.environment ()

let reset_env = lazy (
  let env = OpamMisc.env () in
  let env =
    List.map (fun (k,v as c) ->
      match k with
      | "PATH" -> k, String.concat ":" (OpamMisc.reset_env_value ~prefix:!OpamGlobals.root_dir v)
      | _      -> c
    ) env in
  let env = List.map (fun (k,v) -> k^"="^v) env in
  Array.of_list env
)

let run_process ?verbose ?(env=default_env) ?name = function
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

    (* Display a user-friendly message if the command does not exists *)
    let cmd_exists =
      OpamProcess.run ~env ~name:(temp_file "which") ~verbose:false "which" [cmd] in
    OpamProcess.clean_files cmd_exists;

    if OpamProcess.is_success cmd_exists then (
      let verbose = match verbose with
        | None   -> !OpamGlobals.debug || !OpamGlobals.verbose
        | Some b -> b in
      let r = OpamProcess.run ~env ~name ~verbose cmd args in
      if not !OpamGlobals.debug then
        OpamProcess.clean_files r;
      r
    ) else
      internal_error "%S: command not found." cmd

let command ?verbose ?env ?name cmd =
  let r = run_process ?verbose ?env ?name cmd in
  if not (OpamProcess.is_success r) then
    process_error r

let commands ?verbose ?env ?name commands =
  List.iter (command ?verbose ?env ?name) commands

let read_command_output ?verbose ?env cmd =
  let r = run_process ?verbose ?env cmd in
  if OpamProcess.is_success r then
    r.OpamProcess.r_stdout
  else
    process_error r

let () =
  OpamGlobals.uname_s := function () ->
    match read_command_output ~verbose:false [ "uname"; "-s"] with
    | h::_ -> OpamMisc.strip h
    | []   -> failwith "uname -s"

let copy src dst =
  if Sys.is_directory src then
    internal_error "Cannot copy %s: it is a directory." src;
  if Sys.file_exists dst && Sys.is_directory dst then
    internal_error "Cannot copy to %s: it is a directory." dst;
  if  Sys.file_exists dst then
    remove_file dst;
  mkdir (Filename.dirname dst);
  if src <> dst then
    command ["cp"; src; dst ]

module Tar = struct

  let extensions =
    [ [ "tar.gz" ; "tgz" ], 'z'
    ; [ "tar.bz2" ; "tbz" ], 'j' ]

  let guess_type f =
    let ic = open_in f in
    let c1 = input_char ic in
    let c2 = input_char ic in
    close_in ic;
    match c1, c2 with
    | '\031', '\139' -> Some 'z'
    | 'B'   , 'Z'    -> Some 'j'
    | _              -> None

  let match_ext file ext =
    List.exists (Filename.check_suffix file) ext

  let is_archive f =
    List.exists
      (fun suff -> Filename.check_suffix f suff)
      (List.concat (List.map fst extensions))

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
        if Sys.file_exists dst then internal_error "Extracting the archive will overwrite %s." dst;
        match directories_strict tmp_dir with
        | [x] ->
            mkdir (Filename.dirname dst);
            command [ "mv"; x; dst]
        | _   -> internal_error "The archive contains mutliple root directories."
  )

let extract_in file dst =
  if not (Sys.file_exists dst) then
    internal_error "%s does not exist." file;
  match Tar.extract_function file with
  | None   -> internal_error "%s is not a valid tar archive." file
  | Some f -> f dst

let link src dst =
  mkdir (Filename.dirname dst);
  if Sys.file_exists dst then
    remove_file dst;
  Unix.link src dst

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
        Unix.unlink file;
      ) else
        internal_error "Cannot unlock %s (%s)." file s
    with _ ->
      OpamGlobals.error "%s is broken, removing it and continuing anyway." file;
        close_in ic;
      Unix.unlink file;
  ) else
    log "Cannot find %s, but continuing anyway..." file

let ocaml_version = lazy (
  try
    match read_command_output ~verbose:false [ "ocamlc" ; "-version" ] with
    | h::_ -> Some (OpamMisc.strip h)
    | []   -> internal_error "Cannot find ocamlc."
  with _ ->
    None
)

(* Reset the path to get the system compiler *)
let system command = lazy (
  try
    match read_command_output ~verbose:false ~env:(Lazy.force reset_env) command with
    | h::_ -> Some (OpamMisc.strip h)
    | []   -> internal_error "Cannot find %s." (try List.hd command with _ -> "<none>")
  with _ ->
    None
)

let system_ocamlc_where = system [ "ocamlc"; "-where" ]

let system_ocamlc_version = system [ "ocamlc"; "-version" ]

let download_command = lazy (
  try
    command ~verbose:false ["which"; "curl"];
    (fun src -> [ "curl"; "--insecure" ; "-OL"; src ])
  with Process_error _ ->
    try
      command ~verbose:false ["which"; "wget"];
      (fun src -> [ "wget"; "--content-disposition";
                  "--no-check-certificate"; src ])
    with Process_error _ ->
      internal_error "Cannot find curl nor wget."
)

let really_download ~overwrite ~src ~dst =
  let cmd = (Lazy.force download_command) src in
  let aux () =
    command cmd;
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

let patch =
  let max_trying = 20 in
  fun p ->
    if not (Sys.file_exists p) then
      internal_error "Cannot find %s." p;
    let patch ~dryrun n =
      let opts = if dryrun then ["--dry-run"] else [] in
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
  Printexc.register_printer (function
    | Process_error r  -> Some (OpamProcess.string_of_result r)
    | Internal_error m -> Some m
    | Unix.Unix_error (e,fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s" Sys.argv.(0) fn msg (Unix.error_message e) in
      Some error
    | _ -> None
  )
