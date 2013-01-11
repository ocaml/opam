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

module Base = OpamMisc.Base

let log fmt = OpamGlobals.log "FILENAME" fmt

module Dir = struct

  include OpamMisc.Base

  let of_string dirname =
    if not (Filename.is_relative dirname) then
      dirname
    else
      OpamSystem.real_path dirname

  let to_string dirname =
    if dirname.[String.length dirname - 1] = Filename.dir_sep.[0] then
      Filename.concat (Filename.dirname dirname) (Filename.basename dirname)
    else
      dirname

end

let raw_dir s = s

let with_tmp_dir fn =
  OpamSystem.with_tmp_dir (fun dir -> fn (Dir.of_string dir))

let rmdir dirname =
  log "rmdir %s" (Dir.to_string dirname);
  OpamSystem.remove (Dir.to_string dirname)

let cwd () =
  Dir.of_string (Unix.getcwd ())

let mkdir dirname =
  OpamSystem.mkdir (Dir.to_string dirname)

let list_dirs d =
  let fs = OpamSystem.directories_with_links (Dir.to_string d) in
  List.map Dir.of_string fs

let in_dir dirname fn =
  if Sys.file_exists dirname then
    OpamSystem.in_dir dirname fn
  else
    OpamGlobals.error_and_exit "%s does not exist!" dirname

let exec dirname ?env ?name cmds =
  let env = match env with
    | None   -> None
    | Some l -> Some (Array.of_list (List.map (fun (k,v) -> k^"="^v) l)) in
  in_dir dirname
    (fun () -> OpamSystem.commands ?env ?name cmds)

let move_dir src dst =
  OpamSystem.command [ "mv"; Dir.to_string src; Dir.to_string dst ]

let copy_dir src dst =
  with_tmp_dir (fun tmp ->
    OpamSystem.command [ "rsync"; "-a"; Filename.concat (Dir.to_string src) "/"; Dir.to_string tmp ];
    match list_dirs tmp with
    | [f] ->
      rmdir dst;
      move_dir f dst
    | _ -> OpamGlobals.error_and_exit "Error while copying %s to %s" (Dir.to_string src) (Dir.to_string dst)
  )

let link_dir src dst =
  rmdir dst;
  let tmp_dst = Filename.concat (Filename.basename src) (Filename.basename dst) in
  let base = Filename.dirname dst in
  mkdir base;
  if dst = tmp_dst then
    in_dir base (fun () -> OpamSystem.command [ "ln"; "-s"; src])
  else
    in_dir base (fun () ->
      OpamSystem.commands [
        ["ln"; "-s"; src];
        ["mv"; (Filename.basename src); (Filename.basename dst) ];
      ])

let basename_dir dirname =
  Base.of_string (Filename.basename (Dir.to_string dirname))

let dirname_dir dirname =
  Dir.to_string (Filename.dirname (Dir.of_string dirname))

let exists_dir dirname =
  Sys.file_exists (Dir.to_string dirname)

let (/) d1 s2 =
  let s1 = Dir.to_string d1 in
  raw_dir (Filename.concat s1 s2)

type t = {
  dirname:  Dir.t;
  basename: Base.t;
}

let create dirname basename =
  let b1 = Filename.dirname (Base.to_string basename) in
  let b2 = Base.of_string (Filename.basename (Base.to_string basename)) in
  if basename = b2 then
    { dirname; basename }
  else
    { dirname = dirname / b1; basename = b2 }

let of_basename basename =
  let dirname = Dir.of_string "." in
  { dirname; basename }

let raw_file str =
  let dirname = raw_dir (Filename.dirname str) in
  let basename = Base.of_string (Filename.basename str) in
  create dirname basename

let to_string t =
  Filename.concat (Dir.to_string t.dirname) (Base.to_string t.basename)

let digest t =
  Digest.to_hex (Digest.file (to_string t))

let touch t =
  OpamSystem.write (to_string t) ""

let chmod t p =
  Unix.chmod (to_string t) p

let of_string s =
  let dirname = Filename.dirname s in
  let basename = Filename.basename s in
  {
    dirname  = Dir.of_string dirname;
    basename = Base.of_string basename;
  }

let dirname t = t.dirname

let basename t = t.basename

let read filename =
  OpamSystem.read (to_string filename)

let write filename raw =
  OpamSystem.write (to_string filename) raw

let remove filename =
  OpamSystem.remove_file (to_string filename)

let exists filename =
  Sys.file_exists (to_string filename)

let with_contents fn filename =
  fn (read filename)

let check_suffix filename s =
  Filename.check_suffix (to_string filename) s

let add_extension filename suffix =
  of_string ((to_string filename) ^ "." ^ suffix)

let chop_extension filename =
  of_string (Filename.chop_extension (to_string filename))

let list_files d =
  let fs = OpamSystem.rec_files (Dir.to_string d) in
  List.map of_string fs

let copy src dst =
  OpamSystem.copy (to_string src) (to_string dst)

let move src dst =
  OpamSystem.command [ "mv"; to_string src; to_string dst ]

let link src dst =
(*  if Lazy.force OpamGlobals.os = OpamGlobals.Win32 then
    copy src dst
  else *)
    OpamSystem.link (to_string src) (to_string dst)

let process_in fn src dst =
  let src_s = to_string src in
  let dst = Filename.concat (Dir.to_string dst) (Filename.basename src_s) in
  fn src (of_string dst)

let copy_in = process_in copy

let link_in = process_in link

let extract filename dirname =
  OpamSystem.extract (to_string filename) (Dir.to_string dirname)

let extract_in filename dirname =
  OpamSystem.extract_in (to_string filename) (Dir.to_string dirname)

let starts_with dirname filename =
  OpamMisc.starts_with ~prefix:(Dir.to_string dirname) (to_string filename)

let remove_prefix ~prefix filename =
  let prefix =
    let str = Dir.to_string prefix in
    if str = "" then "" else Filename.concat str "" in
  let dirname = to_string filename in
  match OpamMisc.remove_prefix ~prefix dirname with
  | None   -> OpamGlobals.error_and_exit "%s is not a prefix of %s" prefix dirname
  | Some s -> s

let download ~overwrite filename dirname =
  mkdir dirname;
  let file = OpamSystem.download ~overwrite ~filename:(to_string filename) ~dirname:(Dir.to_string dirname) in
  of_string file

let download_iter ~overwrite filenames dirname =
  let rec aux = function
    | []   ->
      OpamSystem.internal_error "Cannot download %s" (String.concat ", " (List.map to_string filenames))
    | h::t ->
      try download ~overwrite h dirname
      with _ -> aux t in
  aux filenames

let patch filename dirname =
  in_dir dirname (fun () -> OpamSystem.patch (to_string filename))

let address_of_string address =
  if Sys.file_exists address
  then Dir.of_string address
  else raw_dir address

let with_flock file f x =
  try
    OpamSystem.flock (to_string file);
    let r = f x in
    OpamSystem.funlock (to_string file);
    r
  with e ->
    OpamSystem.funlock (to_string file);
    raise e

module O = struct
  type tmp = t
  type t = tmp
  let compare x y = compare (to_string x) (to_string y)
  let to_string = to_string
end

module Map = OpamMisc.Map.Make(O)
module Set = OpamMisc.Set.Make(O)

module OP = struct

  let (/) = (/)

  let (//) d1 s2 =
    let d = Filename.dirname s2 in
    let b = Filename.basename s2 in
    if d <> "." then
      create (d1 / d) (Base.of_string b)
  else
      create d1 (Base.of_string s2)

end

module Attribute = struct

  type t = {
    base: Base.t;
    md5 : string;
    perm: int option;
  }

  let base t = t.base

  let md5 t = t.md5

  let perm t = t.perm

  let create base md5 perm =
    { base; md5; perm=Some perm }

  let to_string t =
    let perm = match t.perm with
      | None   -> ""
      | Some p -> Printf.sprintf " 0o%o" p in
    Printf.sprintf "%s %s%s" (Base.to_string t.base) t.md5 perm

  let of_string s =
    match OpamMisc.split s ' ' with
    | [base; md5]      -> { base=Base.of_string base; md5; perm=None }
    | [base;md5; perm] -> { base=Base.of_string base; md5; perm=Some (int_of_string perm) }
    | k                -> OpamGlobals.error_and_exit "Remote_file: %s" (String.concat " " k)

  module O = struct
    type tmp = t
    type t = tmp
    let to_string = to_string
    let compare = compare
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end
