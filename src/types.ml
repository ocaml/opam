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

(** Define the basic types on which OPAM operates *)

(** {2 Abstract types} *)

(** All abstract types should implement this signature *)
module type Abstract = sig

  (** Abstract type *)
  type t

  (** Create an abstract value from a string *)
  val of_string: string -> t

  (** Convert an abstract value to a string *)
  val to_string: t -> string

  (** Collection of abstract values *)
  module Set: Set.S with type elt = t

  (** Dictionaries of abstract values *)
  module Map: Map.S with type key = t
end

(** The basic implementation of abstract types is just abstracted [string] *)
module Base = struct
  type t = string
  let of_string x = x
  let to_string x = x
  module O = struct type t = string let compare = compare end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end

(** {2 Filenames} *)

(** Absolute directory names *)
module Dirname : sig

  include Abstract

  (** Remove a directory *)
  val remove: t -> unit

end = struct

  include Base

  let of_string dirname =
    Run.real_path dirname

  let remove dirname =
    Run.remove (to_string dirname)

end
    
type dirname = Dirname.t
let d str = Dirname.of_string str

(** Basenames *)
module Basename : Abstract = Base
type basename = Basename.t
let b str = Basename.of_string str

(** Raw file contents *)
module Raw : Abstract = Base

(* Keep a link to [Filename] for the standard library *)
module F = Filename

(** non-directory filenames *)
module Filename : sig

  include Abstract

  (** Create a filename from a dirname and a basename *)
  val create: dirname -> basename -> t

  (** Retrieves the contents from the hard disk. *)
  val read: t -> Raw.t

  (** Removes everything in [filename] if existed. *)
  val remove: t -> unit

  (** Removes everything in [filename] if existed, then write [contents] instead. *)
  val write: t -> Raw.t -> unit

  (** see [Sys.file_exists] *)
  val exists: t -> bool

  (** Check whether a file has a given suffix *)
  val check_suffix: t -> string -> bool

  (** List all the filenames (ie. which are not directories) in a directory *)
  val list: dirname -> t list

  (** Apply a function on the contents of a file *)
  val with_raw: (Raw.t -> 'a) -> t -> 'a

  (** Copy a file in a directory *)
  val copy_in: t -> dirname -> unit

  (** Symlink a file in a directory *)
  val link_in: t -> dirname -> unit

  (** Copy a file *)
  val copy: t -> t -> unit

  (** Symlink a file. If symlink is not possible on the system, use copy instead. *)
  val link: t -> t -> unit

  (** Extract an archive in a given directory (it rewrites the root to
      match [dirname] dir if needed) *)
  val extract: t -> dirname -> unit

end = struct

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
    let dst = F.concat src_s (F.basename src_s) in
    copy src (of_string dst)

  let link_in src dst =
    let src_s = to_string src in
    let dst = F.concat src_s (F.basename src_s) in
    link src (of_string dst)

  let extract filename dirname =
    Run.extract (to_string filename) (Dirname.to_string dirname)

  module O = struct type tmp = t type t = tmp let compare = compare end
  module Map = Map.Make(O)
  module Set = Set.Make(O)
end
type filename = Filename.t

let (/) d1 d2 =
  let s1 = Dirname.to_string d1 in
  let s2 = Dirname.to_string d2 in
  Dirname.of_string (F.concat s1 s2)

let (//) = Filename.create

(** {2 Package name and versions} *)

(** Versions *)
module V : Abstract = Base

(** Names *)
module N : Abstract = Base

(** Package (name x version) pairs *)
module NV : sig
  include Abstract

  (** Return the package name *)
  val name : t -> N.t

  (** Return the version name *)
  val version: t -> V.t

  (** Create a new pair (name x version) *)
  val create: N.t -> V.t -> t

  (** Create a new pair from a filename. This function extracts [$name]
      and [$version] from [/path/to/$name.$version.XXX *)
  val of_file: filename -> t

  (** Create a new pair from a debian package *)
  val of_dpkg: Debian.Packages.package -> t

  (** Create a new pair from a cudf package *)
  val of_cudf: Debian.Debcudf.tables -> Cudf.package -> t

  (** Convert a set of pairs to a map [name -> versions] *)
  val to_map: Set.t -> V.Set.t N.Map.t

end = struct

  type t = {
    name   : N.t;
    version: V.t;
  }

  let create name version = { name; version }

  let name t = t.name

  let version t = t.version

  let sep = '.'

  let of_string s =
    let n, version =
      try
        let i = String.rindex s sep in
        String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1)
      with _ ->
        Globals.error_and_exit "%s is not a valid versioned package name" s in
    { name    = N.of_string n;
      version = V.of_string n  }

  let of_file f =
    let f = Filename.to_string f in
    let b = F.basename f in
    of_string (F.chop_extension b)

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

end

(** OCaml version *)
module OCaml_V : Abstract = Base

(** OPAM version *)
module OPAM_V : Abstract = Base

(** {2 Repositories} *)

(** OPAM repositories *)
module Repository : sig

  include Abstract

  (** Create a repository *)
  val create: name:string -> kind:string -> t
  
  (** Get the repository name *)
  val name: t -> string
  
  (** Get the repository kind *)
  val kind: t -> string

end = struct

  type t = {
    name: string;
    kind: string;
  }

  let create ~name ~kind = { name; kind }

  let of_string _ =
    failwith "Use Repository.create instead"

  let name t = t.name

  let kind t = t.kind

  let to_string r =
    Printf.sprintf "%s(%s)" r.name r.kind

  module O = struct type tmp = t type t = tmp let compare = compare end
  module Set = Set.Make(O)
  module Map = Map.Make(O)

end
type repository = Repository.t
      
(** {2 Variable names} *)

(** Variable names are used in .config files *)
module Variable : Abstract = Base
