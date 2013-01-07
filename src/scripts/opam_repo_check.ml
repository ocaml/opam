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

(* Script to check that a given repository is well-typed (or well-parsed) *)
open OpamTypes
open OpamFilename.OP

let normalize = ref false

let () =
  let usage = Printf.sprintf "Usage: %s" Sys.argv.(0) in
  let specs = [
    ("--version", Arg.Unit OpamVersion.message, " Display version information");
    ("--normalize", Arg.Set normalize         , " Normalize all files in the repository");
  ] in
  let ano x =
    Printf.eprintf "%s: invalid argument" x in
  Arg.parse specs ano usage

let write f_write fic st = if !normalize then f_write fic st

let () =
  let t = OpamPath.Repository.raw (OpamFilename.cwd ()) in
  OpamPackage.Set.iter (fun nv ->
    OpamGlobals.msg "Processing (package) %s\n" (OpamPackage.to_string nv);

    (** Descr *)
    let descr = OpamPath.Repository.descr t nv in
    write OpamFile.Descr.write descr (OpamFile.Descr.read descr);

    (** OPAM *)
    let opam = OpamPath.Repository.opam t nv in
    write OpamFile.OPAM.write opam (OpamFile.OPAM.read opam);

    (** URL *)
    let url = OpamPath.Repository.url t nv in
    if OpamFilename.exists url then (
      write OpamFile.URL.write url (OpamFile.URL.read url);
    );

    (** Dot_install *)
    let dot_install = OpamPath.Repository.files t nv // (OpamPackage.Name.to_string (OpamPackage.name nv) ^ ".install") in
    if OpamFilename.exists dot_install then (
      write OpamFile.Dot_install.Raw.write dot_install (OpamFile.Dot_install.Raw.read dot_install);
    );
  ) (OpamRepository.packages t);

  (** Comp *)
  let comps = OpamFilename.list_files (OpamPath.Repository.compilers_dir t) in
  List.iter (fun comp ->
    let comp_ = OpamFile.Comp.read comp in
    OpamGlobals.msg "Processing (compiler) %s\n" (OpamCompiler.to_string (OpamFile.Comp.name comp_));
    write OpamFile.Comp.write comp comp_;
  ) comps
