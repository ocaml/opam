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

  type t

  val to_string: t -> string

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



















