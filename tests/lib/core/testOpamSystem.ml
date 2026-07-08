(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)


let (/) = Filename.concat

(* this is a copy from run.ml & opamSystem *)
let finally f x k = match f x with
  | r -> k (); r
  | exception e ->
    (* When we're at least 4.05+
       let bt = Printexc.get_raw_backtrace () in*)
    (try k () with _ -> ());
    (*Printexc.raise_with_backtrace e bt*)
    raise e

let rec mkdir_p dir =
  if Sys.file_exists dir then ()
  else let () = mkdir_p (Filename.dirname dir) in
    if not (Sys.file_exists dir) then
      Unix.mkdir dir 0o777
    else ()

let erase_file path =
  try Sys.remove path
  with Sys_error _ when Sys.win32 ->
    (* Deal with read-only attribute on Windows. Ignore any error from chmod
       so that the message always come from Sys.remove *)
    let () = try Unix.chmod path 0o666 with Sys_error _ -> () in
    Sys.remove path

let is_reg_dir dir =
  match Unix.lstat dir with
  | {Unix.st_kind = Unix.S_DIR; _} -> true
  | {Unix.st_kind =
       Unix.(S_REG | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK); _} ->
    false
  | exception Unix.(Unix_error (ENOENT, _, _)) when Sys.win32 ->
    (* This is usually caused by invalid symlinks extracted by a
       Cygwin/MSYS2 tar. *)
    false

let rm_rf path =
  let rec erase path =
    if is_reg_dir path then begin
      Array.iter (fun entry -> erase (Filename.concat path entry))
        (Sys.readdir path);
      Unix.rmdir path
    end else erase_file path
  in
  try if Sys.file_exists path then erase path
  with Sys_error err ->
    raise (Sys_error (Printf.sprintf "Failed to remove %S (%s)" path err))

let tmp = Unix.realpath (Filename.get_temp_dir_name ())

let rec with_temp_dir f =
  let s =
    tmp / Printf.sprintf "opam-reftest-%06x" (Random.int 0xffffff)
  in
  if Sys.file_exists s then
    with_temp_dir f
  else
    (mkdir_p s;
     finally f s @@ fun () ->
     rm_rf s)

let touch file =
  let oc = open_out_bin file in
  output_string oc "";
  close_out oc
(* -- end copy *)

let test_real_path ~ctxt () =
  let fun_name = "OpamSystem.real_path" in
  let test ~on ~setup ~unresolved ~expected =
    let test_name = Printf.sprintf "(%s)" unresolved in
    if List.exists (String.equal Sys.os_type) on then
      (Option.iter (fun f -> f ()) setup;
       let got =
         try Ok (OpamSystem.real_path unresolved)
         with Sys_error msg -> Error msg
            | e -> Error (Printexc.to_string e)
       in
       if Result.equal ~ok:String.equal ~error:String.equal expected got then
         OpamUnit.success ~ctxt ~fun_name ~test_name ()
       else
         let expected =
           match expected with
           | Ok x -> x
           | Error msg -> "Error:"^msg
         in
         let got = match got with Ok x -> x | Error msg -> "Error:"^msg in
         OpamUnit.failure ~ctxt ~fun_name ~test_name ~expected ~got ())
    else
      let reason = Printf.sprintf "Test does not run on %s" Sys.os_type in
      OpamUnit.skip ~ctxt ~fun_name ~test_name ~reason ()
  in
  with_temp_dir @@ fun dir ->
  let d1 = dir / "d1" in
  let d2 = d1 / "d2" in
  let d3 = d2 / "d3" in
  mkdir_p d3;
  let f2 = d2 / "f2" in
  touch f2;
  let orig_cwd = Sys.getcwd () in
  Sys.chdir dir;
  let (!) p = dir / p in
  let testcases =
    let win32 = ["Win32"] in
    let unix = ["Unix"] in
    let cygwin = ["Cygwin"] in
    let all = unix @ cygwin @ win32 in
    let absolute_paths =
      let existent = [
        all, None, ! "d1"/".", Ok (! "d1");
        all, None, ! "d1"/"."/"."/"."/"", Ok (! "d1");
        all, None, ! "d1", Ok (! "d1");
        all, None, ! "d1"/"..", Ok dir;
        all, None, ! "d1"/".."/"", Ok dir;
        all, None, ! "d1"/"d2", Ok (! "d1"/"d2");
        all, None, ! "d1"/".."/"d1", Ok (! "d1");
        all, None, ! "d1"/"d2"/"d3"/".."/"f2", Ok (! "d1"/"d2"/"f2");
      ] in
      let inexistent = [
        all, None, ! "d1"/"dx", Ok (! "d1"/"dx");
        all, None, ! "d1"/".."/"dx", Ok (! "dx");
        all, None, ! "d1"/"dx"/"d3"/".."/"fx", Ok (! "d1"/"dx"/"fx");
      ]
      in
      existent @ inexistent
    in
    let relative_paths =
      let existent = [
        all, None, ".", Ok dir;
        all, None, "."/"."/"."/"", Ok dir;
        all, None, "..", Ok tmp;
        all, None, "d1"/".", Ok (! "d1");
        all, None, "d1"/"."/"."/"."/"", Ok (! "d1");
        all, None, "d1", Ok (! "d1");
        all, None, "d1"/"..", Ok dir;
        all, None, "d1"/".."/"", Ok dir;
        all, None, "d1"/"d2", Ok (! "d1"/"d2");
        all, None, "d1"/".."/"d1", Ok (! "d1");
        all, None, "d1"/"d2"/"d3"/".."/"f2", Ok (! "d1"/"d2"/"f2");
        win32, None, "C:Windows", Ok (! "Windows");
      ] in
      let inexistent = [
        all, None, "d1"/"dx", Ok (! "d1"/"dx");
        all, None, "d1"/".."/"dx", Ok (! "dx");
        all, None, "d1"/"dx"/"d3"/".."/"fx", Ok (! "d1"/"dx"/"fx");
        win32, None, "C:/d1/d2", Ok {|C:\d1\d2|};
        win32, None, {|C:\d1\d2\..|}, Ok {|C:\d1|};
      ]
      in
      existent @ inexistent
    in
    let always_root = [
      unix @ cygwin, None, ".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/"..", Ok "/";
      unix @ cygwin, None, "/..", Ok "/";
      win32, None, ".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/".."/"..", Ok {|C:\|};
      win32, None, "/..", Ok {|C:\|};
      win32, None, "C:/../../../", Ok {|C:\|};
    ] in
    (* keep last because of chdirs *)
    let remove_dir =
      let create_go_rm d =
        Some (fun () -> mkdir_p d; Sys.chdir d; rm_rf d)
      in
      let d4 = d1/"d4" in
      [
        unix, create_go_rm d4 , ".", Error "No such file or directory";
        unix, create_go_rm d4 , d4, Ok d4;
        unix, create_go_rm d4 , ".."/"d1", Error "No such file or directory";
        unix, create_go_rm d4 , "d4"/".."/"d1", Error "No such file or directory";
      ]
    in
    absolute_paths
    @ relative_paths
    @ always_root
    @ remove_dir
  in
  List.iter
    (fun (on, setup, unresolved, expected) ->
       test ~on ~setup ~unresolved ~expected)
    testcases;
  Sys.chdir orig_cwd

let test ~ctxt () =
  test_real_path ~ctxt ()
