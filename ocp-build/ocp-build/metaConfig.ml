(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)


(* open BuildBase *)
(* open Stdlib2 *)

(*
  peerocaml:~/.opam/4.00.1/lib/ocaml%  ocamlfind printconf
  Effective configuration:
  Configuration file:
  /home/lefessan/.opam/4.00.1/lib/findlib.conf
  Search path:
  /home/lefessan/.opam/4.00.1/lib
  Packages will be installed in/removed from:
  /home/lefessan/.opam/4.00.1/lib
  META files will be installed in/removed from:
  the corresponding package directories
  The standard library is assumed to reside in:
  /home/lefessan/.opam/4.00.1/lib/ocaml
  The ld.conf file can be found here:
  /home/lefessan/.opam/4.00.1/lib/ocaml/ld.conf
*)

(* TODO: We could also try to find "findlib/findlib.conf", in case
   ocamlfind is not installed. We could even generate it ! *)

let load_config () =
  try
    let (status, lines) =
      BuildMisc.get_stdout_lines
        [ "ocamlfind" ] [ "printconf" ]
    in
    let search_path = ref [] in
    let rec iter lines =
      match lines with
        "Search path:" :: lines ->
          iter_path lines
      | [] -> ()
      | _ :: lines -> iter lines

    and iter_path lines =
      match lines with
      | path :: lines when OcpString.starts_with path "    " ->
        search_path := String.sub path 4 (String.length path - 4) :: !search_path;
        iter_path lines
      | _ -> iter lines

    in
    iter lines;
    List.rev !search_path
  with e ->
    Printf.eprintf "MetaConfig: exception %S\n%!" (Printexc.to_string e);
    []
