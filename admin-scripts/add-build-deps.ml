#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
open Opam_admin_top;;

(* Add the "build" dependency flag to all ocamlfind depends *)

let to_build = List.map OpamPackage.Name.of_string ["ocamlfind"]

let addbuild (pkg, (flags, cstr) as atom) =
  if List.mem pkg to_build && not (List.mem OpamTypes.Depflag_Build flags) then
    OpamFormula.Atom (pkg, (OpamTypes.Depflag_Build::flags, cstr))
  else
    OpamFormula.Atom atom
;;

iter_packages ~opam:(fun _ opam0 ->
    let open OpamFile.OPAM in
    let opam = opam0 in
    let opam = with_depends opam @@ OpamFormula.map addbuild @@ depends opam in
    let opam = with_depopts opam @@ OpamFormula.map addbuild @@ depopts opam in
    let opam = if opam <> opam0
      then with_opam_version opam @@ OpamVersion.of_string "1.2"
      else opam
    in
    opam)
  ()
