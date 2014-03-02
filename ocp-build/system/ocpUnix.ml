(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open MinUnix
open OcpLang

let assoc_of_env env =
  let el = Array.to_list env in
  let el = List.map (fun s -> String.splitn 2 s '=') el in
  List.map (function [k] -> (k, "") | [k;v] -> (k,v) | _ -> assert false) el

let env_of_assoc el =
  let env = List.map (fun (k,v) -> k ^ "=" ^ v) el in
  Array.of_list env

let putenv_in env k v =
  let el = assoc_of_env env in
  let el = List.replace_assoc k v el in
  env_of_assoc el

let getenv_in env k =
  List.assoc k (assoc_of_env env)

let expand_in env s =
  let el = assoc_of_env env in
  let get k = try List.assoc k el with Not_found -> "" in
  let rec aux str =
    try
      let n = String.index str '$' in
      let prefix, rest = String.cut str n in
      let rec find i =
        if i < String.length rest && rest.[i] >= 'A' && rest.[i] <= 'Z' then
          find (i+1)
        else
          i in
      let i = find 0 in
      if i = n - 1 then
        [prefix ; get rest]
      else begin
        let k, rest = String.cut ~keep:true rest i in
        if i = 0 && String.length rest > 0 && rest.[0] = '$' then
          prefix :: get k :: "$" :: aux (String.after rest 0)
        else
          prefix :: get k :: aux rest
      end
    with Not_found ->
      [str] in
  String.concat "" (aux s)

let in_dir dir fn =
  let cwd = getcwd () in
  chdir dir;
  try
    let r = fn () in
    chdir cwd;
    r
  with e ->
    chdir cwd;
    raise e

(* Returns all the directories in the current dir ('.' and '..' are
   not in the list) *)
let directories () =
  let d = Sys.readdir (getcwd ()) in
  let d = Array.to_list d in
  List.filter (fun f -> try Sys.is_directory f with _ -> false) d

let files () =
  let d = Sys.readdir (getcwd ()) in
  let d = Array.to_list d in
  List.filter (fun f -> try not (Sys.is_directory f) with _ -> true) d

(* Returns all the sub-files *)
let tree () =
  let rec aux prefix =
    in_dir prefix (fun () ->
      let files = files () in
      let directories = directories () in
      List.fold_left
        (fun accu d -> aux (Filename.concat prefix d) @ accu)
        (List.map (fun f -> Filename.concat prefix f) files)
        directories
    ) in
  aux (getcwd ())

let safe_unlink file =
  try unlink file
  with Unix_error _ -> ()

let safe_mkdir dir perm =
  try mkdir dir perm
  with Unix_error (EEXIST, _, _) -> ()

let rec safe_rec_mkdir dir perm =
  let dirs = String.split dir Filename.dir_sep.[0] in
  let (_ : string) = List.fold_left
    (fun p d ->
      let path = Filename.concat p d in
      safe_mkdir path perm;
      path)
    (if Filename.is_relative dir then "" else "/")
    dirs in
  ()

let rec safe_rec_rmdir dir =
  if Sys.file_exists dir then begin
    in_dir dir (fun () ->
      let dirs = directories () in
      let files = files () in
      List.iter safe_unlink files;
      List.iter safe_rec_rmdir dirs;
    );
    rmdir dir;
  end

(*
(* !! Must call this before spawning any threads !! *)
let daemonize () =
  match fork () with
    | 0 ->
      if setsid () == -1 then
        failwith "Unix.setsid failed";
      begin match fork () with
        | 0 ->
          let nullfd = openfile "/dev/null" [ O_WRONLY ] 0 in
          begin try
                  close stdin;
                  dup2 nullfd stdout;
                  dup2 nullfd stderr;
            with exn -> close nullfd; raise exn
          end;
          close nullfd
        | _ -> exit 0
      end
    | _ -> exit 0
*)

let string_of_stdout fn =
  let tmp = Filename.temp_file "stdout2str" "txt" in
  let oc = openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  let saved_stdout = dup stdout in
  flush Pervasives.stdout;
  dup2 oc stdout;
  close oc;
  let reset () =
    flush Pervasives.stdout;
    dup2 saved_stdout stdout;
    close saved_stdout;
    let s = File.string_of_file tmp in
    safe_unlink tmp;
    s in
  begin
    try fn ()
    with e ->
      let (_ : string) = reset () in
      raise e
  end;
  reset ()

let modtime f =
  (MinUnix.stat f).MinUnix.st_mtime
