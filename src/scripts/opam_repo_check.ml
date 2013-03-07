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

let opt_normalize = ref false
let opt_repair = ref false

let () =
  let usage = Printf.sprintf "Usage: %s" Sys.argv.(0) in
  let specs = [
    ("--version"  , Arg.Unit OpamVersion.message, " Display version information");
    ("--normalize", Arg.Set opt_normalize         , " Normalize all files in the repository");
    ("--repair"   , Arg.Set opt_repair            , " Try to repair most of warnings");
  ] in
  let ano x =
    Printf.eprintf "%s: invalid argument" x in
  Arg.parse specs ano usage

let write f_write fic st = if !opt_normalize then f_write fic st

module Check = struct
(*   let descr x = x *)
   let opam t =
     List.fold_left
       (fun t (s, f_cons, f_make) ->
         f_make t (List.map
                     (function
                       | (CString "make", f0) :: l, f1 as x ->
                           if !opt_repair then
                             (CIdent "make", f0) :: l, f1
                           else
                             let _ = OpamGlobals.warning "unescaped 'make' in %s" s in
                             x
                       | x -> x)
                     (f_cons t)))
       t
       [ "build", OpamFile.OPAM.build, OpamFile.OPAM.with_build
       ; "remove", OpamFile.OPAM.remove, OpamFile.OPAM.with_remove ]
(*   let url x = x *)
(*   let dot_install x = x *)
(*   let comp x = x *)
(*   let comp_descr x = x *)
end

let () =
  let t = OpamFilename.cwd () in
  let prefix, packages = OpamRepository.packages t in

  (** packages *)
  OpamPackage.Set.iter (fun package ->
    OpamGlobals.msg "Processing (package) %s\n" (OpamPackage.to_string package);

    let prefix = OpamRepository.find_prefix prefix package in

    (** Descr *)
    let descr = OpamPath.Repository.descr t prefix package in
    write OpamFile.Descr.write descr (OpamFile.Descr.read descr);

    (** OPAM *)
    let opam = OpamPath.Repository.opam t prefix package in
    write OpamFile.OPAM.write opam (Check.opam (OpamFile.OPAM.read opam));

    (** URL *)
    let url = OpamPath.Repository.url t prefix package in
    if OpamFilename.exists url then (
      write OpamFile.URL.write url (OpamFile.URL.read url);
    );

    (** Dot_install *)
    let dot_install =
      OpamPath.Repository.files t prefix package
      // (OpamPackage.Name.to_string
          (OpamPackage.name package) ^ ".install") in
    if OpamFilename.exists dot_install then
      write OpamFile.Dot_install.write dot_install (OpamFile.Dot_install.read dot_install);

  ) packages;

  (** compilers *)
  OpamCompiler.Map.iter (fun compiler (comp, descr) ->
    OpamGlobals.msg "Processing (compiler) %s\n" (OpamCompiler.to_string compiler);
    write OpamFile.Comp.write comp (OpamFile.Comp.read comp);
    match descr with
    | None   -> ()
    | Some d -> write OpamFile.Comp_descr.write d (OpamFile.Comp_descr.read d);

  ) (OpamRepository.compilers t);
