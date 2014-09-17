#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;

(* Converts OPAM 1.2 packages for compat with OPAM 1.1
   * merge 'install' with 'build'
   * remove the new fields: install, flags and dev-repo
   * remove dependency flags ('build', 'test', 'doc')
   * set file version
*)

let to_1_1 _ opam =
  let module OF = OpamFile.OPAM in
  if
    OpamVersion.compare (OF.opam_version opam) (OpamVersion.of_string "1.2")
    < 0
  then opam
  else
  let opam = OF.with_build opam (OF.build opam @ OF.install opam) in
  let opam = OF.with_install opam [] in
  let opam = OF.with_flags opam [] in
  let opam = OF.with_dev_repo opam None in
  let opam = OF.with_opam_version opam (OpamVersion.of_string "1.1") in
  let remove_ext =
    OpamFormula.map (fun (n, (_,formula)) ->
        OpamFormula.Atom (n, ([], formula)))
  in
  let opam = OF.with_depends opam (remove_ext (OF.depends opam)) in
  let opam = OF.with_depopts opam (remove_ext (OF.depopts opam)) in
  opam
;;

Opam_admin_top.iter_packages ~opam:to_1_1 ()
