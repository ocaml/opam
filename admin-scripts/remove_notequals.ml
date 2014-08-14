#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
open Opam_admin_top;;

(* Remove '!=' from the formulas, for backwards compatibility with 1.1 *)

let rewrite_formula ~conj =
  let open OpamFormula in
  map @@ fun (pkg, cstr) ->
  Atom (pkg,
        cstr |>
        map @@ function
        | (`Neq,v) ->
          if conj then And (Atom (`Lt,v), Atom (`Gt,v))
          else Or (Atom (`Lt,v), Atom (`Gt,v))
        | atom -> Atom atom)
;;

map_packages ~opam:(fun opam ->
    let open OpamFile.OPAM in
    let opam = with_conflicts opam @@ rewrite_formula ~conj:true @@ conflicts opam in
    let opam = with_depends opam @@ rewrite_formula ~conj:false @@ depends opam in
    opam)
  ()
