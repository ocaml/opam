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

  (** packages *)
  OpamPackage.Set.iter (fun package ->
    OpamGlobals.msg "Processing (package) %s\n" (OpamPackage.to_string package);

    (** Descr *)
    let descr = OpamPath.Repository.descr t package in
    write OpamFile.Descr.write descr (OpamFile.Descr.read descr);

    (** OPAM *)
    let opam = OpamPath.Repository.opam t package in
    write OpamFile.OPAM.write opam (OpamFile.OPAM.read opam);

    (** URL *)
    let url = OpamPath.Repository.url t package in
    if OpamFilename.exists url then (
      write OpamFile.URL.write url (OpamFile.URL.read url);
    );

    (** Dot_install *)
    let dot_install = OpamPath.Repository.files t package // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install") in
    if OpamFilename.exists dot_install then (
      write OpamFile.Dot_install.Raw.write dot_install (OpamFile.Dot_install.Raw.read dot_install);
    );
  ) (OpamRepository.packages t);

  (** compilers *)
  OpamCompiler.Set.iter (fun compiler ->
    OpamGlobals.msg "Processing (compiler) %s\n" (OpamCompiler.to_string compiler);

    (** Comp *)
    let comp = OpamPath.Repository.compiler t compiler in
    write OpamFile.Comp.write comp (OpamFile.Comp.read comp);

    (** Comp_descr *)
    let comp_descr = OpamPath.Repository.compiler_descr t compiler in
    write OpamFile.Comp_descr.write comp_descr (OpamFile.Comp_descr.read comp_descr);

  ) (OpamRepository.compilers t);
