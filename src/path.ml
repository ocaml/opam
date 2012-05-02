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

open ExtList
open ExtString
open Types

let log fmt = Globals.log "PATH" fmt

module Dirname : Abstract = Base
type dirname = Dirname.t
let d str = Dirname.of_string str

module Basename : Abstract = Base
type basename = Basename.t
let b str = Basename.of_string str

(* Raw file contents *)
module Raw : Abstract = Base

(* Keep a link to [Filename] for the standard library *)
module F = Filename

(* non-directory files *)
module Filename : sig

  include Abstract

  val create: dirname -> basename -> t

  (** Retrieves the contents from the hard disk. *)
  val read : t -> Raw.t

  (** Removes everything in [filename] if existed. *)
  val remove : t -> unit

  (** Removes everything in [filename] if existed, then write [contents] instead. *)
  val write : t -> Raw.t -> unit

  (** see [Sys.file_exists] *)
  val exists : t -> bool

  (** Apply a function on the contents of a file *)
  val with_raw: (Raw.t -> 'a) -> t -> 'a

end = struct

  type t = {
    dirname:  Dirname.t;
    basename: Basename.t;
  }

  let create dirname basename = { dirname; basename }
    
  let to_string t =
    F.concat (Dirname.to_string t.dirname) (Basename.to_string t.basename)

  let of_string s =
    let dirname = Filename.dirname s in
    let basename = Filename.basename s in
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
    Run.safe_rm (to_string filename)

  let exists filename =
    Sys.file_exists (to_string filename)

  let with_raw fn filename =
    let raw = read filename in
    fn raw

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

(** Global state *)
module type GLOBAL = sig

  (** Contains [$opam] *)
  type t

  val create: dirname -> t

  (** Root dir: [$opam/] *)
  val root: t -> dirname

  (** Main configuration file: [$opam/config] *)
  val config: t -> filename

  (** OPAM files: [$opam/opam/$NAME.$VERSION.opam] *)
  val opam: t -> NV.t -> filename
    
  (** Description file: [$opam/descr/$NAME.$VERSION] *)
  val descr: t -> NV.t -> filename

  (** Archives files: [$opam/archives/$NAME.$VERSION.tar.gz] *)
  val archive: t -> NV.t -> filename

  (** OPAM files folder: [$opam/opam/] *)
  val opam_dir: t -> dirname

  (** Description files folder: [$opam/descr/] *)
  val descr_dir: t -> dirname

  (** Archives files folder: [$opam/archives/] *)
  val archive_dir: t -> dirname
end

module Global : GLOBAL = struct
  type t = dirname

  let create opam = opam

  let root opam = opam

  let dirname_of_nv nv = Dirname.of_string (NV.to_string nv)

  let config t = t // b "config"

  let opam_dir t = t / d "opam"

  let opam t nv = opam_dir t // b (NV.to_string nv)

  let descr_dir t = t / d "descr"

  let descr t nv = descr_dir t // b (NV.to_string nv)

  let archive_dir t = t / d "archive"

  let archive t nv = archive_dir t // b (NV.to_string nv ^ ".tar.gz")
end

(** Compiler-version related state *)
module type COMPILER = sig

  (** Contains [$opam] and [$OVERSION] *)
  type t
    
  val create: dirname -> V.t -> t

  (** Installed libraries for the package: [$opam/$OVERSION/lib/NAME] *)
  val lib: t -> N.t -> dirname

  (** Installed binaries: [$opam/$OVERSION/bin] *)
  val bin: t -> dirname

  (** List of installed packages with their version: [$opam/$OVERSION/installed] *)
  val installed: t -> filename

  (** Tempory folders used to decompress the corresponding archives:
      [$opam/$OVERSION/build/$NAME-$VERSION] *)
  val build: t -> NV.t -> dirname

  (** Tempory folder: [$opam/$OVERSION/build] *)
  val build_dir: t -> dirname

  (** Installed files for a given package: [$opam/$OVERSION/install/$NAME.install] *)
  val install: t -> N.t -> filename

  (** Installed files: [$opam/$OVERSION/install/] *)
  val install_dir: t -> dirname
    
  (** Packages to reinstall on next upgrade: [$opam/$OVERSION/reinstall]  *)
  val reinstall: t -> filename

  (** Compile and link flags for a given package:
      [$opam/$OVERSION/config/$NAME.config] *)
  val config: t -> N.t -> filename

  (** Configuration folder: [$opam/$OVERSION/config] *)
  val config_dir: t -> dirname
end

module Compiler : COMPILER = struct

  type t = dirname

  let create opam oversion =
    Dirname.of_string (V.to_string oversion)

    let lib t n = t / d "lib" / d (N.to_string n)

    let bin t = t / d "bin"

    let installed t = t // b "installed"

    let build_dir t = t / d "build"

    let build t nv = build_dir t / d (NV.to_string nv)

    let install_dir t = t / d "install"

    let install t n = install_dir t // b (N.to_string n ^ ".install")

    let reinstall t = t // b "reinstall"

    let config_dir t = t / d "config"

    let config t n = config_dir t // b (N.to_string n ^ ".config")
end

module type REPOSITORY = sig
  type t

  val create: dirname -> repository -> t

  (** Return the repository folder: [$opam/repo/$repo] *)
  val root: t -> dirname

  (** Return the repository index: [$opam/repo/index] *)
  val index: t -> dirname

  (** Return the repository kind: [$opam/repo/$repo/kind] *)
  val kind: t -> filename

  (** Return the repository address: [$opam/repo/$repo/address] *)
  val address: t -> filename

  (** Return the OPAM file for a given package: [$opam/repo/$repo/opam/$NAME.$VERSION.opam] *)
  val opam: t -> NV.t -> filename

  (** Return the OPAM folder: [$opam/repo/$repo/opam/] *)
  val opam_dir: t -> dirname

  (** Return the description file for a given package: [$opam/repo/$repo/descr/$NAME.VERSION] *)
  val descr: t -> NV.t -> filename

  (** Return the description folder *)
  val descr_dir: t -> dirname

  (** Return the archive for a giben package: [$opam/repo/$repo/archives/$NAME.$VERSION.tar.gz *)
  val archive: t -> NV.t -> filename

  (** Return the archive folder: [$opam/repo/$repo/archives/] *)
  val archive_dir: t -> dirname

  (** Return the list of updated packages: [$opam/repo/$repo/updated] *)
  val updated: t -> filename

  (** Return the upload folder for a given version: [$opam/repo/$repo/upload/$NAME.$VERSION/] *)
  val upload: t -> NV.t -> dirname

  (** Return the upload folder: [$opam/repo/$repo/upload] *)
  val upload_dir: t -> dirname

end

module Repository : REPOSITORY = struct
  type t = {
    root: dirname; (* [$opam/] *)
    repo: dirname; (* [$opam/repo/$repo] *)
  }

  let create root r = {
    root;
    repo = root / d "repo" / d r.repo_name;
  }

  let root t = t.repo

  let index t = t.root / d "repo" / d "index"

  let kind t = t.repo // b "kind"

  let address t = t.repo // b "address"

  let opam_dir t = t.repo / d "opam"

  let opam t nv = opam_dir t // b (NV.to_string nv ^ ".opam")

  let descr_dir t = t.repo / d "descr"

  let descr t nv = descr_dir t // b (NV.to_string nv)

  let archive_dir t = t.repo / d "archives"

  let archive t nv = archive_dir t // b (NV.to_string nv ^ ".tar.gz")

  let updated t = t.repo // b "updated"

  let upload_dir t = t.repo / d "upload"

  let upload t nv = upload_dir t / d (NV.to_string nv)

end

















