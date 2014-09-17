#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;

(* Converts OPAM 1.2 packages for compat with OPAM 1.1
   * merge 'install' with 'build'
   * remove the new fields: install, flags and dev-repo
   * remove dependency flags ('build', 'test', 'doc')
   * set file version
   * replace inequality constraints with '> & <'
*)

let rewrite_constraint ~conj = (* Rewrites '!=' *)
  OpamFormula.map OpamFormula.(function
      | (`Neq,v) ->
        prerr_endline "XX";
        if conj then And (Atom (`Lt,v), Atom (`Gt,v))
        else Or (Atom (`Lt,v), Atom (`Gt,v))
      | atom -> Atom atom)
;;

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
    OpamFormula.map (fun (n, (_,cstr)) ->
        OpamFormula.Atom (n, ([], rewrite_constraint ~conj:false cstr)))
  in
  let opam = OF.with_depends opam (remove_ext (OF.depends opam)) in
  let opam = OF.with_depopts opam (remove_ext (OF.depopts opam)) in
  let opam =
    OF.with_conflicts opam
      (OpamFormula.map (fun (n, cstr) ->
           OpamFormula.Atom (n, rewrite_constraint ~conj:true cstr))
          (OF.conflicts opam))
  in
  opam
;;

Opam_admin_top.iter_packages ~opam:to_1_1 ()
