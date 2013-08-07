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

module Base = OpamMisc.Base

let log fmt = OpamGlobals.log "FILENAME" fmt

module Dir = struct

  include OpamMisc.Base

  let of_string dirname =
    if (String.length dirname >= 1 && dirname.[0] = '~') then
      let home = OpamMisc.getenv "HOME" in
      match dirname with
      | "~" -> home
      | _   ->
        let prefix = Filename.concat "~" "" in
        let suffix = OpamMisc.remove_prefix ~prefix dirname in
        Filename.concat home suffix
    else if Filename.is_relative dirname then
      OpamSystem.real_path dirname
    else
      dirname

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

let cleandir dirname =
  log "cleandir %s" (Dir.to_string dirname);
  OpamSystem.remove (Dir.to_string dirname);
  mkdir dirname

let rec_dirs d =
  let fs = OpamSystem.rec_dirs (Dir.to_string d) in
  List.rev (List.rev_map Dir.of_string fs)

let dirs d =
  let fs = OpamSystem.dirs (Dir.to_string d) in
  List.rev (List.rev_map Dir.of_string fs)

let in_dir dirname fn =
  if Sys.file_exists dirname then
    OpamSystem.in_dir dirname fn
  else
    OpamSystem.internal_error "Cannot CD to %s: the directory does not exist!" dirname

let exec dirname ?env ?name ?metadata ?keep_going cmds =
  let env = match env with
    | None   -> None
    | Some l -> Some (Array.of_list (List.rev_map (fun (k,v) -> k^"="^v) l)) in
  in_dir dirname
    (fun () -> OpamSystem.commands ?env ?name ?metadata ?keep_going cmds)

let move_dir ~src ~dst =
  OpamSystem.command [ "mv"; Dir.to_string src; Dir.to_string dst ]

let exists_dir dirname =
  Sys.file_exists (Dir.to_string dirname)

let copy_dir ~src ~dst =
  if exists_dir dst then
    OpamSystem.internal_error
      "Cannot create %s as the directory already exists." (Dir.to_string dst);
  OpamSystem.command [ "cp"; "-pPR"; Dir.to_string src; Dir.to_string dst ]

let link_dir ~src ~dst =
  if exists_dir dst then
    OpamSystem.internal_error "Cannot link: %s already exists." (Dir.to_string dst)
  else (
    mkdir (Filename.dirname dst);
    OpamSystem.link (Dir.to_string src) (Dir.to_string dst)
  )

let basename_dir dirname =
  Base.of_string (Filename.basename (Dir.to_string dirname))

let dirname_dir dirname =
  Dir.to_string (Filename.dirname (Dir.of_string dirname))

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

let raw str =
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

let open_in filename =
  open_in (to_string filename)

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

let rec_files d =
  let fs = OpamSystem.rec_files (Dir.to_string d) in
  List.rev_map of_string fs

let files d =
  let fs = OpamSystem.files (Dir.to_string d) in
  List.rev_map of_string fs

let copy ~src ~dst =
  if src <> dst then OpamSystem.copy (to_string src) (to_string dst)

let move ~src ~dst =
  if src <> dst then OpamSystem.command [ "mv"; to_string src; to_string dst ]

let link ~src ~dst =
  if src <> dst then OpamSystem.link (to_string src) (to_string dst)

let readlink src =
  if exists src then
    try of_string (Unix.readlink (to_string src))
    with _ -> src
  else
    OpamSystem.internal_error "%s does not exist." (to_string src)

let is_symlink src =
  try
    let s = Unix.lstat (to_string src) in
    s.Unix.st_kind = Unix.S_LNK
  with _ ->
    OpamSystem.internal_error "%s does not exist." (to_string src)

let starts_with dirname filename =
  OpamMisc.starts_with ~prefix:(Dir.to_string dirname) (to_string filename)

let remove_prefix prefix filename =
  let prefix =
    let str = Dir.to_string prefix in
    if str = "" then "" else Filename.concat str "" in
  let filename = to_string filename in
  OpamMisc.remove_prefix ~prefix filename

let process_in ?root fn src dst =
  let basename = match root with
    | None   -> basename src
    | Some r ->
      if starts_with r src then remove_prefix r src
      else OpamSystem.internal_error "%s is not a prefix of %s"
          (Dir.to_string r) (to_string src) in
  let dst = Filename.concat (Dir.to_string dst) basename in
  fn ~src ~dst:(of_string dst)

let copy_in ?root = process_in ?root copy

let link_in = process_in link

let extract filename dirname =
  OpamSystem.extract (to_string filename) (Dir.to_string dirname)

let extract_in filename dirname =
  OpamSystem.extract_in (to_string filename) (Dir.to_string dirname)

type generic_file =
  | D of Dir.t
  | F of t

let extract_generic_file filename dirname =
  match filename with
  | F f ->
    log "extracting %s to %s"
      (to_string f)
      (Dir.to_string dirname);
    extract f dirname
  | D d ->
    if d <> dirname then (
      log "copying %s to %s"
        (Dir.to_string d)
        (Dir.to_string dirname);
      copy_dir ~src:d ~dst:dirname
    )

let ends_with suffix filename =
  OpamMisc.ends_with ~suffix (to_string filename)

let remove_suffix suffix filename =
  let suffix = Base.to_string suffix in
  let filename = to_string filename in
  OpamMisc.remove_suffix ~suffix filename

let download ~overwrite filename dirname =
  mkdir dirname;
  let file = OpamSystem.download ~overwrite
      ~filename:(to_string filename) ~dirname:(Dir.to_string dirname) in
  of_string file

let download_iter ~overwrite filenames dirname =
  let rec aux = function
    | []   ->
      let filenames = List.map to_string filenames in
      OpamSystem.internal_error "Cannot download %s." (OpamMisc.pretty_list filenames)
    | h::t ->
      try download ~overwrite h dirname
      with _ -> aux t in
  aux filenames

let patch filename dirname =
  in_dir dirname (fun () -> OpamSystem.patch (to_string filename))

let with_flock file f x =
  OpamSystem.flock (to_string file);
  try
    let r = f x in
    OpamSystem.funlock (to_string file);
    r
  with e ->
    OpamSystem.funlock (to_string file);
    raise e

let checksum f =
  if exists f then
    [digest f]
  else
    []

let checksum_dir d =
  if exists_dir d then
    List.map digest (rec_files d)
  else
    []

let prettify_dir d =
  OpamMisc.prettify_path (Dir.to_string d)

let prettify s =
  OpamMisc.prettify_path (to_string s)

let to_json x = `String (to_string x)

module O = struct
  type tmp = t
  type t = tmp
  let compare x y = compare (to_string x) (to_string y)
  let to_string = to_string
  let to_json = to_json
end

module Map = OpamMisc.Map.Make(O)
module Set = OpamMisc.Set.Make(O)

let copy_files ~src ~dst =
  let files = rec_files src in
  List.iter (fun file ->
      if not !OpamGlobals.do_not_copy_files then
        let base = remove_prefix src file in
        let dst_file = create dst (Base.of_string base) in
        if exists dst_file then
          OpamGlobals.warning
            "%s is replaced by the packager's overlay files. \
             Set OPAMDONOTCOPYFILES to a non-empty value to no \
             copy the overlay files."
            (to_string dst_file);
        OpamGlobals.msg "Copying %s to %s/\n" (to_string file) (Dir.to_string dst);
        copy ~src:file ~dst:dst_file
    ) files

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
    | [base;md5; perm] -> { base=Base.of_string base; md5;
                            perm=Some (int_of_string perm) }
    | k                -> OpamSystem.internal_error
                            "remote_file: '%s' is not a valid line."
                            (String.concat " " k)

  let to_json x =
    `O ([ ("base" , Base.to_json x.base);
          ("md5"  , `String x.md5)]
        @ match x. perm with
          | None   -> []
          | Some p -> ["perm", `String (string_of_int p)])

  module O = struct
    type tmp = t
    type t = tmp
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

let to_attribute root file =
  let basename = Base.of_string (remove_prefix root file) in
  let perm =
    let s = Unix.stat (to_string file) in
    s.Unix.st_perm in
  let digest = digest file in
  Attribute.create basename digest perm
