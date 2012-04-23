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

open ExtString
open ExtList
open Namespace
open Uri

let log fmt = Globals.log "RUN" fmt

let mkdir f f_to = 
  let rec aux f_to = 
    if not (Sys.file_exists f_to) then begin
      aux (Filename.dirname f_to);
      Unix.mkdir f_to 0o755;
    end in
  aux (Filename.dirname f_to);
  f f_to

let copy src dst =
  log "Copying %s to %s" src dst;
  let n = 1024 in
  let b = String.create n in
  let read = ref min_int in
  let ic = open_in_bin src in
  let oc =
    if Sys.file_exists dst then
      open_out_bin dst
    else
      let perm = (Unix.stat src).Unix.st_perm in
      mkdir (open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] perm) dst
  in
  while !read <>0 do
    read := input ic b 0 n;
    output oc b 0 !read;
  done;
  close_in ic;
  close_out oc

let read file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

module type PROCESS = 
sig
  type ('a, 'b) t

  type 'a plist = 'a list (* order irrelevant *)

  type state = 
    | Pause
    | Not_yet_begun

  val cores : int (* above [cores + 1], the parallel running gain is not significant *)

  val init : int (* maximum number of parallel running task *) 
    -> ('a -> 'b) (* function to execute in parallel *)
    -> ('a -> string)
    -> ('a, 'b) t

  (** Run or continue the execution of the given list of processes. 
      The function returns as soon as one process terminates.
      NB The parallel running is performed on at most : [max 1 "the number of tasks indicated at [init] time"] . *)
  val filter_finished : ('a, 'b) t -> (state * 'a) plist (** By convention : list not empty *) -> ('a, 'b) t * (state * 'a) plist * ('a * 'b (* finished *))
end

module Process : PROCESS =
struct
  type ('a, 'b) t = { nb_proc : int ; f : 'a -> 'b ; to_str : 'a -> string }

  type 'a plist = 'a list (* order irrelevant *)

  type state = 
    | Pause
    | Not_yet_begun

  let cores = 1

  let init nb_proc f to_str = { nb_proc = max 1 nb_proc ; f ; to_str } 

  (* This function always execute in parallel. *)
  let filter_finished _ = failwith "To complete. Then, remove all the [filter_finished] defined after"

  (* Given a list of function to execute in parallel, this function always execute the first element. *)
  let filter_finished t = function
    | [] -> assert false (* by convention this is empty *)
    | (_, x) :: xs -> t, xs, (x, t.f x)

  (* Given a list of function to execute in parallel, this function always ask the user which to execute in case the list contains more than one element. *)
  let filter_finished t = function
    | [] -> assert false (* by convention this is empty *)
    | [_] as l -> filter_finished t l
    | l -> 
        Globals.msg " Choose which number to execute :\n%s\n(between 1 and %d) ? " (String.concat "\n" (List.mapi (fun i (_, x) -> Printf.sprintf "  [%d] %s" (succ i) (t.to_str x)) l)) (List.length l);

        match 
          try
            List.split_nth ((try int_of_string (read_line ()) with _ -> 1) - 1) l
          with
            | _ -> List.split_nth 1 l
        with 
          | l1, (_, x) :: l2 ->
              let xs = l1 @ l2 in
              t, xs, (x, t.f x)
          | _ -> assert false
end

(**************************)
(* from ocplib-system ... *)
(**************************)

let in_dir dir fn x =
  let cwd = Unix.getcwd () in
  Unix.chdir dir;
  try
    let r = fn x in
    Unix.chdir cwd;
    r
  with e ->
    Unix.chdir cwd;
    raise e

let directories () =
  let d = Sys.readdir (Unix.getcwd ()) in
  let d = Array.to_list d in
  List.filter (fun f -> try Sys.is_directory f with _ -> false) d

let files () =
  let d = Sys.readdir (Unix.getcwd ()) in
  let d = Array.to_list d in
  List.filter (fun f -> try not (Sys.is_directory f) with _ -> true) d

let safe_unlink file =
  try Unix.unlink file
  with Unix.Unix_error _ -> ()

let rec safe_rmdir dir =
  if Sys.file_exists dir then begin
    in_dir dir (fun () ->
      let dirs = directories () in
      let files = files () in
      List.iter safe_unlink files;
      List.iter safe_rmdir dirs;
    ) ();
    Unix.rmdir dir;
  end

let safe_rm file =
  if Sys.file_exists file && Sys.is_directory file then
    safe_rmdir file
  else
    safe_unlink file

let getchdir s = 
  let p = Unix.getcwd () in
  let () = Unix.chdir s in
  p

let rec root path =
  let d = Filename.dirname path in
  if d = path || d = "" || d = "." then
    path
  else
    root d

(* XXX: blocking function *)
let read_lines ic =
  let lines = ref [] in
  try while true do
    let line = input_line ic in
    if not (Filename.concat line "" = line) then
      lines := line :: !lines;
    done;
    !lines
  with _ ->
    !lines

let read_command_output_ cmd =
  let ic = Unix.open_process_in cmd in
  let lines = read_lines ic in
  if Unix.close_process_in ic <> Unix.WEXITED 0 then
    None
  else
    Some lines

let read_command_output cmd =
  match read_command_output_ cmd with
    | None -> Globals.error_and_exit "command %s failed" cmd
    | Some lines -> lines

let tmp_dir = Filename.concat Filename.temp_dir_name "opam-archives"

let normalize s =
  if Sys.file_exists s then
    getchdir (getchdir s)
  else
    s

let is_archive file =
  if List.exists (Filename.check_suffix file) [ "tar.gz" ; "tgz" ] then
    Some (fun tmp_dir -> Sys.command (Printf.sprintf "tar xvfz %s -C %s" file tmp_dir))
  else if List.exists (Filename.check_suffix file) [ "tar.bz2" ; "tbz" ] then
    Some (fun tmp_dir -> Sys.command (Printf.sprintf "tar xvfj %s -C %s" file tmp_dir))
  else
    None

let untar file nv =
  log "untar %s" file;
  let files = read_command_output ("tar tf " ^ file) in
  log "%d files found: %s" (List.length files) (String.concat ", " files);
  let dirname = Namespace.string_of_nv (fst nv) (snd nv) in
  let aux name =
    if String.starts_with name dirname then
      Filename.concat tmp_dir name, name
    else
      let root = root name in
      let n = String.length root in
      let rest = String.sub name n (String.length name - n) in 
      Filename.concat tmp_dir name, dirname ^  rest in
  let moves = List.map aux files in
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  let err =
    match is_archive file with
      | Some f_cmd -> f_cmd tmp_dir
      | None -> Globals.error_and_exit "%s is not a valid archive" file in
  List.iter (fun (src, dst) ->
    mkdir (copy src) dst
  ) moves;
  err

type command = 
  | Sh of string list
  | OCaml of string

let sys_command fmt =
  Printf.kprintf (fun str ->
    log "cwd=%s '%s'" (Unix.getcwd ()) str;
    Sys.command str;
  ) fmt

let fold_0 f = List.fold_left (function 0 -> f | err -> fun _ -> err) 0

let sys_commands = fold_0 (sys_command "%s")

let sys_command_with_bin bin fmt =
  Printf.kprintf (fun cmd ->
    if Sys.file_exists bin then begin
      let path = ref "<not set>" in
      let env = Unix.environment () in
      for i = 0 to Array.length env - 1 do
        let k,v = String.split env.(i) "=" in
        if k = "PATH" then
          let new_path = bin ^ ":" ^ v in
          env.(i) <- "PATH=" ^ new_path;
          path := new_path;
      done;
      log "cwd=%s path=%s %s" (Unix.getcwd ()) !path cmd;
      let (o,i,e as chans) = Unix.open_process_full cmd env in
      (* we MUST read the input_channels otherwise [close_process] will fail *)
      let err = read_lines e in
      let out = read_lines o in
      let str () = Printf.sprintf "out: %s\nerr: %s" (String.concat "\n" out) (String.concat "\n" err) in
      let msg () = Globals.msg "%s\n" (str ()) in
      match Unix.close_process_full chans with
      | Unix.WEXITED 0 -> 0
      | Unix.WEXITED i -> msg (); i
      | _              -> msg (); 1
    end else
      sys_command "%s" cmd
  ) fmt

let sys_commands_with_bin bin = fold_0 (sys_command_with_bin bin "%s")

let ocpget_config s_lib = 
  Printf.kprintf read_command_output_ "ocp-get --root %s config -I %s 2>/dev/null" !Globals.root_path s_lib

let sys_commands_general =
  let get_tmp_ml = 
    (* Return a temporary file. 
       The first call sets the name. Next calls will return that name. *)
    let ml = ref None in 
    fun () -> 
      match !ml with 
        | None -> 
          let tmp = Filename.temp_file "ocpget" ".ml" in
          let _ = ml := Some tmp in
          tmp
        | Some ml -> ml in

  let ocpget_lib = 
    (* library that is loaded by default with 'ocaml' *)
    [ "extlib", None
    ; "cudf", None
    ; "ocamlre", Some [ "re" ]
    ; "ocpgetboot", Some [ "pcre" ; "bat" ]
    ; "ocamlgraph", Some [ "graph" ]
    ; "dose", None
    ; "ocpget", Some [ "ocp-get-lib" ; "ocp-get" ] ] in

  fun bin ->
    fold_0 (function 
      | Sh l -> sys_command_with_bin bin "%s" (String.concat " " l)
      | OCaml s -> 

        (* construct the OCaml program *)
        let ml = get_tmp_ml () in
        let oc = open_out ml in
        let _ = Printf.fprintf oc "%s%!" s in
        let _ = close_out oc in

        (* compute the "-I ..." to give to 'ocaml' from the [ocpget_lib] *)
        (* NOTE We normally take from [ocpget_lib]. It would be interesting to also add all the cma that depends this library. *)
        let s_include, ocpget_lib = 
          List.fold_left (fun (acc, ocpget_lib) (s_lib, o_lib) -> 
            match ocpget_config s_lib with
              | None -> acc, ocpget_lib
              | Some l -> Printf.sprintf "%s%s " acc (String.concat " " l), (s_lib, o_lib) :: ocpget_lib
          ) ("", []) ocpget_lib in
        let ocpget_lib = List.rev ocpget_lib in

        (* execute the 'ocaml' command *)
        sys_command "ocaml %s %s %s" 
          s_include

          ((* cma *)
           List.fold_left
             (fun acc l_lib -> 
               List.fold_left
                 (fun acc s_lib -> 
                   Printf.sprintf "%s%s.cma " acc s_lib)
                 acc
                 (match l_lib with s_lib, None -> [ s_lib ] | _, Some l -> l))
             ""
             (("unix", None) :: ("str", None) :: ocpget_lib))

          ml)

(* Git wrappers *)
module Git = struct

  (* Init a git repository in [dirname] *)
  let init dirname =
    in_dir dirname (fun () ->
      let (_ : int) = sys_command "git init" in
      ()
    ) ()

  (* tentative to build a unique branch name from a repo name *)
  (*  ':' is forbidden and it cannot start by '/'  *)
  let remote_name url =
    let name = "R" ^ snd (uri_of_url url) in
    for i = 0 to String.length name - 1 do
      if name.[i] = ':' then
        name.[i] <- 'T';
    done;
    name

  (* Add a remote url in dirname *)
  let remote_add dirname url =
    in_dir dirname (fun () ->
      sys_command "git remote add %s %s" (remote_name url) url
    ) ()

  (* internal command *)
  let get_remotes dirname =
    in_dir dirname read_command_output "git remote" 

  let safe_remote_add dirname url =
    let name = remote_name url in
    log "name=%s" name;
    if List.mem name (get_remotes dirname) then
      (* Globals.error_and_exit "%s is already a remote branch in %s" name dirname; *)
      ()
    else if remote_add dirname url <> 0 then
      Globals.error_and_exit "cannot add remote branch %s in %s" name dirname

  let remote_rm dirname url =
    in_dir dirname (sys_command "git remote rm %s") (remote_name url)

  let safe_remote_rm dirname url =
    let name = remote_name url in
    if not (List.mem name (get_remotes dirname)) then
      (* Globals.error_and_exit "%s is not a remote branch in %s" name dirname; *)
      ()
    else if remote_rm dirname url <> 0 then
      Globals.error_and_exit "cannot remove remote branch %s in %s" name dirname
          
  (* Return the list of modified files of the git repository located
     at [dirname] *)
  let get_updates dirname =
    in_dir dirname (fun () ->
      let fetches = List.map (Printf.sprintf "git fetch %s") (get_remotes dirname) in
      let diff remote =
        read_command_output (Printf.sprintf "git diff remotes/%s/master --name-only" remote) in
      if sys_commands fetches = 0 then
        List.flatten (List.map diff (get_remotes dirname))
      else
        Globals.error_and_exit "Cannot fetch git repository %s" dirname
    ) ()

  (* Update the git repository located at [dirname] *)
  let update dirname =
    in_dir dirname (fun () ->
      let commands = List.map (Printf.sprintf "git pull %s master") (get_remotes dirname) in
      if sys_commands commands <> 0 then
        Globals.error_and_exit "Cannot update git repository %s" dirname
    ) ()

  (* Clone [repo] into the directory [dst] *)
  let clone repo dst =
    sys_command "git clone %s %s" repo dst
 
end

type download_result = 
  | From_http of string (* file *)
  | From_git
  | Url_error

let clone repo last_pwd nv =
  let b_name = Filename.chop_extension (Filename.basename repo) in
  let dst_git = Filename.concat tmp_dir b_name in
  log "cloning %s into %s" repo dst_git;
  if Sys.file_exists dst_git then
    safe_rm dst_git;
  let err = Git.clone repo b_name in
  if err = 0 then
    let s_from = Printf.sprintf "%s/%s" (Unix.getcwd ()) b_name in
    let s_to = Printf.sprintf "%s/%s" last_pwd (Namespace.string_of_nv (fst nv) (snd nv)) in
    if sys_command "mv -i %s %s" s_from s_to = 0 then
      From_git
    else
      Globals.error_and_exit "moving failed"
  else
    Globals.error_and_exit "cloning failed"

let exec_download = 
  let http s_wget url _ _ = 
    log "download %s" url;
    let dst = Filename.concat tmp_dir (Filename.basename url) in
    if Sys.file_exists dst then
      safe_rm dst;
    if sys_command "%s %s" s_wget url = 0 then
      From_http dst
    else
      Url_error in
  function
  | (Http|Https as uri), url ->
      (match Globals.os with
      | Globals.Darwin -> http "ftp"  (Uri.to_string (Some uri, url))
      | _              -> http "wget" (Uri.to_string (Some uri, url)))
  | Git, repo -> clone repo
  | Local, _  -> assert false

let download url nv =
  if not (Sys.file_exists tmp_dir) then
    Unix.mkdir tmp_dir 0o750;
  in_dir tmp_dir (fun s -> exec_download url s nv) (Unix.getcwd ())

let patch p nv =
  let dirname = Namespace.string_of_nv (fst nv) (snd nv) in
  log "patching %s using %s" dirname p;
  in_dir dirname (fun () ->
    sys_command "patch -p1 -f -i %s" p
  ) ()

let real_path p =
  let dir = Filename.dirname p in
  let dir = normalize dir in
  let base = Filename.basename p in
  let (/) = Filename.concat in
  if Filename.is_relative dir then
    (Sys.getcwd ()) / dir / base
  else
    dir / base
