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

let log fmt = Globals.log "TYPES" fmt

module type Abstract = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
  module Set: Set.S with type elt = t
  module Map: Map.S with type key = t
end

(* The basic implementation of abstract types is just abstracted
   [string] *)
module Base = struct
  type t = string
  let of_string x = x
  let to_string x = x
  module O = struct type t = string let compare = compare end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end

(** {2 Filenames} *)

(* Absolute directory names *)
module Dirname = struct

  include Base

  let of_string dirname =
    Run.real_path dirname

  let rmdir dirname =
    Run.remove (to_string dirname)

  let mkdir dirname =
    Run.mkdir (to_string dirname)

  let exec dirname cmds =
    Run.in_dir (to_string dirname) (fun () -> Run.commands cmds)
end
    
type dirname = Dirname.t

(** Basenames *)
module Basename = Base
type basename = Basename.t

(** Raw file contents *)
module Raw = Base
type raw = Raw.t

(* Keep a link to [Filename] for the standard library *)
module F = Filename

module Stdlib_filename = F

module Filename = struct

  type t = {
    dirname:  Dirname.t;
    basename: Basename.t;
  }

  let create dirname basename = { dirname; basename }
    
  let to_string t =
    F.concat (Dirname.to_string t.dirname) (Basename.to_string t.basename)

  let of_string s =
    let dirname = F.dirname s in
    let basename = F.basename s in
    {
      dirname  = Dirname.of_string dirname;
      basename = Basename.of_string basename;
    }

  let read filename =
    let str = Run.read (to_string filename) in
    Raw.of_string str

  let write filename raw =
    Run.write (to_string filename) (Raw.to_string raw)

  let remove filename =
    Run.remove_file (to_string filename)

  let exists filename =
    Sys.file_exists (to_string filename)

  let with_raw fn filename =
    let raw = read filename in
    fn raw

  let check_suffix filename s =
    F.check_suffix (to_string filename) s

  let list d =
    let fs = Run.files (Dirname.to_string d) in
    List.map of_string fs

  let copy src dst =
    Run.copy (to_string src) (to_string dst)

  let link src dst =
    if Globals.os = Globals.Win32 then
      copy src dst
    else
      Run.link (to_string src) (to_string dst)

  let copy_in src dst =
    let src_s = to_string src in
    let dst = F.concat dst (F.basename src_s) in
    copy src (of_string dst)

  let link_in src dst =
    let src_s = to_string src in
    let dst = F.concat dst (F.basename src_s) in
    link src (of_string dst)

  let extract filename dirname =
    Run.extract (to_string filename) (Dirname.to_string dirname)

  module O = struct type tmp = t type t = tmp let compare = compare end
  module Map = Map.Make(O)
  module Set = Set.Make(O)
end
type filename = Filename.t

let (/) d1 s2 =
  let s1 = Dirname.to_string d1 in
  Dirname.of_string (F.concat s1 s2)

let (//) d1 s2 =
  Filename.create d1 (Basename.of_string s2)

(** {2 Package name and versions} *)

(** Versions *)
module V = Base

(** Names *)
module N = Base

module NV = struct

  type t = {
    name   : N.t;
    version: V.t;
  }

  let create name version = { name; version }

  let name t = t.name

  let version t = t.version

  let sep = '.'

  let check s =
      try
        let i = String.index s sep in
        let name = String.sub s 0 i in
        let version = String.sub s (i+1) (String.length s - i - 1) in
        Some {name; version}
      with _ ->
        None

  let of_string s = match check s with
    | Some x -> x
    | None   -> Globals.error_and_exit "%s is not a valid versioned package name" s

  let of_filename f =
    let f = Filename.to_string f in
    let b = F.basename f in
    if F.check_suffix b ".opam" then
      check (F.chop_suffix b ".opam")
    else if F.check_suffix b ".tar.gz" then
      check (F.chop_suffix b ".tar.gz")
    else
      None

  let of_dpkg d =
    { name    = N.of_string d.Debian.Packages.name;
      version = V.of_string d.Debian.Packages.version }

  let of_cudf table pkg =
    let real_version =
      Debian.Debcudf.get_real_version
        table
        (pkg.Cudf.package, pkg.Cudf.version) in
    { name    = N.of_string pkg.Cudf.package;
      version = V.of_string real_version; }

  let to_string t =
    Printf.sprintf "%s%c%s" (N.to_string t.name) sep (V.to_string t.version)

  module O = struct type tmp = t type t = tmp let compare = compare end
  module Set = Set.Make (O)
  module Map = Map.Make (O)

  let to_map nv =
    Set.fold (fun nv map ->
      let name = name nv in
      let version = version nv in
      let versions =
        if N.Map.mem name map then
          N.Map.find name map
        else
          V.Set.empty in
      N.Map.add name (V.Set.add version versions) (N.Map.remove name map)
    ) nv N.Map.empty

  let string_of_set s =
    let l = Set.fold (fun nv l -> to_string nv :: l) s [] in
    Printf.sprintf "{ %s }" (String.concat ", " l)

end

(** OCaml version *)
module OCaml_V = Base

(** OPAM version *)
module OPAM_V = Base

(** {2 Repositories} *)

(** OPAM repositories *)
module Repository = struct

  type t = {
    name: string;
    kind: string;
    address: string;
  }

  let create ~name ~kind ~address =
    { name; kind; address }

  let of_string _ =
    failwith "Use Repository.create instead"

  let name t = t.name

  let kind t = t.kind

  let address t = t.address

  let default = {
    name   = Globals.default_repository_name;
    kind    = Globals.default_repository_kind;
    address = Globals.default_repository_address;
  }

  let to_string r =
    Printf.sprintf "%s(%s %s)" r.name r.address r.kind

  module O = struct type tmp = t type t = tmp let compare = compare end
  module Set = Set.Make(O)
  module Map = Map.Make(O)

end
type repository = Repository.t
      
(** {2 Variable names} *)

(** Variable names are used in .config files *)
module Variable = Base
type variable = Variable.t

(** {2 Command line arguments} *)

(** Upload arguments *)
type upload = {
  opam   : filename;
  descr  : filename;
  archive: filename;
}

let string_of_upload u =
  Printf.sprintf "opam=%s descr=%s archive=%s"
    (Filename.to_string u.opam)
    (Filename.to_string u.descr)
    (Filename.to_string u.archive)

(** Remote arguments *)
type remote =
  | List
  | Add of string
  | Rm of string

let string_of_remote = function
  | List -> "list"
  | Add s -> Printf.sprintf "add %s" s
  | Rm  s -> Printf.sprintf "rm %s" s

type config_option =
  | Includes of N.t list
  | Bytecomp of (N.t * string) list
  | Asmcomp  of (N.t * string) list
  | Bytelink of (N.t * string) list
  | Asmlink  of (N.t * string) list

type rec_config_option = {
  recursive: bool;
  options  : config_option;
}

type config =
  | Compil   of rec_config_option
  | Variable of (N.t * Variable.t) list
  | Subst    of Filename.t list

let p msg l =
  Printf.sprintf "%s %s"
    msg
    (String.concat ","
       (List.map (fun (n,l) -> Printf.sprintf "%s.%s" (N.to_string n) l) l))

let string_of_config_option = function
  | Includes l ->
      Printf.sprintf "include %s" (String.concat "," (List.map N.to_string l))
  | Bytecomp l -> p "bytecomp" l
  | Asmcomp l  -> p "asmcomp" l
  | Bytelink l -> p "bytelink" l
  | Asmlink l  -> p "asmlink" l

let string_of_rec_config_option o =
  Printf.sprintf "%b %s" o.recursive (string_of_config_option o.options)

let p l =
  String.concat ","
    (List.map
       (fun (n,v) -> Printf.sprintf "%s.%s" (N.to_string n) (Variable.to_string v)) l)

let string_of_config = function
  | Compil c   -> string_of_rec_config_option c
  | Variable l -> p l
  | Subst l    -> String.concat "," (List.map Filename.to_string l)
