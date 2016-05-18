#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
#directory "+../re";;
open Opam_admin_top;;
open OpamTypes;;

iter_packages ~opam:(fun _ opam ->
    let module O = OpamFile.OPAM in
    if O.install opam <> [] then opam else
      let rec rev_split_while acc cond = function
        | [] -> acc, []
        | x::r when cond x -> rev_split_while (x::acc) cond r
        | l -> acc, List.rev l
      in
      let condition = function
        | (CString "install",_)::_, _ -> true
        | (CString "cp",_)::r, _ ->
          (try
             let dest =
               match List.filter (function
                   | CString s, _ -> not (OpamStd.String.starts_with ~prefix:"-" s)
                   | CIdent _, _ -> true)
                   (List.rev r)
               with
               | (d, _)::_ -> d
               | _ -> raise Not_found
             in
             let dests = ["prefix";"bin";"sbin";"lib";"man";"doc";"share";"etc";
                          "toplevel";"stublibs";"doc"] in
             match dest with
             | CIdent i -> List.mem i dests
             | CString s ->
               Re.(execp (compile (seq [alt (List.map str dests); str "}%"])) s)
           with Not_found -> false)
        | l, _ ->
          List.exists (function
              | (CString arg, _) -> OpamStd.String.contains ~sub:"install" arg
              | _ -> false)
            l
      in
      let install, build =
        rev_split_while [] condition (List.rev (O.build opam))
      in
      opam |>
      OpamFile.OPAM.with_build build |>
      OpamFile.OPAM.with_install install)
  ()
;;
