(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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

module Base = OpamStd.AbstractString

let log fmt = OpamConsole.log "FILENAME" fmt
let slog = OpamConsole.slog

module Dir = struct

  include OpamStd.AbstractString

  let of_string dirname =
    let dirname =
      if dirname = "~" then OpamStd.Sys.home ()
      else if
        OpamStd.String.starts_with ~prefix:("~"^Filename.dir_sep) dirname
      then
        Filename.concat (OpamStd.Sys.home ())
          (OpamStd.String.remove_prefix ~prefix:("~"^Filename.dir_sep) dirname)
      else dirname
    in
    OpamSystem.real_path dirname

  let to_string dirname = dirname

end

let raw_dir s = s

let with_tmp_dir fn =
  OpamSystem.with_tmp_dir (fun dir -> fn (Dir.of_string dir))

let with_tmp_dir_job fjob =
  OpamSystem.with_tmp_dir_job (fun dir -> fjob (Dir.of_string dir))

let rmdir dirname =
  OpamSystem.remove_dir (Dir.to_string dirname)

let cwd () =
  Dir.of_string (Unix.getcwd ())

let mkdir dirname =
  OpamSystem.mkdir (Dir.to_string dirname)

let cleandir dirname =
  log "cleandir %a" (slog Dir.to_string) dirname;
  OpamSystem.remove (Dir.to_string dirname);
  mkdir dirname

let rec_dirs d =
  let fs = OpamSystem.rec_dirs (Dir.to_string d) in
  List.rev (List.rev_map Dir.of_string fs)

let dirs d =
  let fs = OpamSystem.dirs (Dir.to_string d) in
  List.rev (List.rev_map Dir.of_string fs)

let dir_is_empty d =
  OpamSystem.dir_is_empty (Dir.to_string d)

let in_dir dirname fn = OpamSystem.in_dir dirname fn

let env_of_list l = Array.of_list (List.rev_map (fun (k,v) -> k^"="^v) l)

let exec dirname ?env ?name ?metadata ?keep_going cmds =
  let env = match env with
    | None   -> None
    | Some l -> Some (env_of_list l) in
  in_dir dirname
    (fun () -> OpamSystem.commands ?env ?name ?metadata ?keep_going cmds)

let move_dir ~src ~dst =
  OpamSystem.command ~verbose:(OpamSystem.verbose_for_base_commands ())
    [ "mv"; Dir.to_string src; Dir.to_string dst ]

let exists_dir dirname =
  try (Unix.stat (Dir.to_string dirname)).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false

let copy_dir ~src ~dst =
  if exists_dir dst then
    OpamSystem.internal_error
      "Cannot create %s as the directory already exists." (Dir.to_string dst);
  OpamSystem.command ~verbose:(OpamSystem.verbose_for_base_commands ())
    [ "cp"; "-PR"; Dir.to_string src; Dir.to_string dst ]

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

let to_list_dir dir =
  let base d = Dir.of_string (Filename.basename (Dir.to_string d)) in
  let rec aux acc dir =
    let d = dirname_dir dir in
    if d <> dir then aux (base dir :: acc) d
    else base dir :: acc in
  aux [] dir

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

let digest_regex = Re.(compile (repn xdigit 32 (Some 32)))

let valid_digest s =
  OpamStd.String.exact_match digest_regex s

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
  try open_in (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let open_out filename =
  try open_out (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let write filename raw =
  OpamSystem.write (to_string filename) raw

let remove filename =
  OpamSystem.remove_file (to_string filename)

let exists filename =
  try (Unix.stat (to_string filename)).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

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

let install ?exec ~src ~dst () =
  if src <> dst then OpamSystem.install ?exec (to_string src) (to_string dst)

let move ~src ~dst =
  if src <> dst then
    OpamSystem.command ~verbose:(OpamSystem.verbose_for_base_commands ())
      [ "mv"; to_string src; to_string dst ]

let link ~src ~dst =
  if src <> dst then OpamSystem.link (to_string src) (to_string dst)

let readlink src =
  if exists src then
    try of_string (Unix.readlink (to_string src))
    with Unix.Unix_error _ -> src
  else
    OpamSystem.internal_error "%s does not exist." (to_string src)

let is_symlink src =
  try
    let s = Unix.lstat (to_string src) in
    s.Unix.st_kind = Unix.S_LNK
  with Unix.Unix_error _ ->
    OpamSystem.internal_error "%s does not exist." (to_string src)

let is_exec file =
  try OpamSystem.is_exec (to_string file)
  with Unix.Unix_error _ ->
    OpamSystem.internal_error "%s does not exist." (to_string file)

let starts_with dirname filename =
  OpamStd.String.starts_with ~prefix:(Dir.to_string dirname) (to_string filename)

let remove_prefix prefix filename =
  let prefix =
    let str = Dir.to_string prefix in
    if str = "" then "" else Filename.concat str "" in
  let filename = to_string filename in
  OpamStd.String.remove_prefix ~prefix filename

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
    log "extracting %a to %a"
      (slog to_string) f
      (slog Dir.to_string) dirname;
    extract f dirname
  | D d ->
    if d <> dirname then (
      log "copying %a to %a"
        (slog Dir.to_string) d
        (slog Dir.to_string) dirname;
      copy_dir ~src:d ~dst:dirname
    )

let ends_with suffix filename =
  OpamStd.String.ends_with ~suffix (to_string filename)

let remove_suffix suffix filename =
  let suffix = Base.to_string suffix in
  let filename = to_string filename in
  OpamStd.String.remove_suffix ~suffix filename

let patch filename dirname =
  in_dir dirname (fun () -> OpamSystem.patch (to_string filename))

let with_flock ?read file f x =
  let lock = OpamSystem.flock ?read (to_string file) in
  try
    let r = f x in
    OpamSystem.funlock lock;
    r
  with e ->
    OpamStd.Exn.register_backtrace e;
    OpamSystem.funlock lock;
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

let prettify_path s =
  let aux ~short ~prefix =
    let prefix = Filename.concat prefix "" in
    if OpamStd.String.starts_with ~prefix s then
      let suffix = OpamStd.String.remove_prefix ~prefix s in
      Some (Filename.concat short suffix)
    else
      None in
  try
    match aux ~short:"~" ~prefix:(OpamStd.Sys.home ()) with
    | Some p -> p
    | None   -> s
  with Not_found -> s

let prettify_dir d =
  prettify_path (Dir.to_string d)

let prettify s =
  prettify_path (to_string s)

let to_json x = `String (to_string x)

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
end

module Map = OpamStd.Map.Make(O)
module Set = OpamStd.Set.Make(O)

let copy_files ~src ~dst =
  let files = rec_files src in
  List.iter (fun file ->
      let base = remove_prefix src file in
      let dst_file = create dst (Base.of_string base) in
      if OpamCoreConfig.(!r.verbose_level >= 2) then
        OpamConsole.msg "Copying %s %s %s/\n"
          (prettify file)
          (if exists dst_file then "over" else "to")
          (prettify_dir dst);
      copy ~src:file ~dst:dst_file
    ) files

module Op = struct

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
    { base; md5; perm=perm }

  let to_string_list t =
    let perm = match t.perm with
      | None   -> []
      | Some p -> [Printf.sprintf "0o%o" p] in
    Base.to_string t.base :: t.md5 :: perm

  let of_string_list = function
    | [base; md5]      -> { base=Base.of_string base; md5; perm=None }
    | [base;md5; perm] -> { base=Base.of_string base; md5;
                            perm=Some (int_of_string perm) }
    | k                -> OpamSystem.internal_error
                            "remote_file: '%s' is not a valid line."
                            (String.concat " " k)

  let to_string t = String.concat " " (to_string_list t)
  let of_string s = of_string_list (OpamStd.String.split s ' ')

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

  module Set = OpamStd.Set.Make(O)

  module Map = OpamStd.Map.Make(O)

end

let to_attribute root file =
  let basename = Base.of_string (remove_prefix root file) in
  let perm =
    let s = Unix.stat (to_string file) in
    s.Unix.st_perm in
  let digest = digest file in
  Attribute.create basename digest (Some perm)
