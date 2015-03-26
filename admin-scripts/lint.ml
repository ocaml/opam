#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
open Opam_admin_top;;

iter_packages ~opam:(fun nv opam ->
    match OpamFile.OPAM.validate opam with
    | [] -> opam
    | w ->
      OpamGlobals.msg "\r\027[KIn %s:\n%s\n"
        (OpamPackage.to_string nv)
        (OpamFile.OPAM.warns_to_string w);
      opam
  ) ()
