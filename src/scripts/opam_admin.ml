(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner

let default_cmd =
  let doc = "Administration tool for local repositories." in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "opam-admin" ~version:OpamVersion.(to_string current) ~doc

let make_repo_cmd =
  let doc = "Initialize a repo for serving files." in
  Term.(Opam_mk_repo.(pure process $ args)),
  Term.info "make" ~doc

let check_repo_cmd =
  let doc = "Check a local repo for errors." in
  Term.(Opam_repo_check.(pure process $ args)),
  Term.info "check" ~doc

let () =
  try
    match
      Term.eval_choice ~catch:false
        default_cmd [make_repo_cmd; check_repo_cmd]
    with
    | `Error _ -> exit 2
    | _ -> exit 0
  with
  | OpamGlobals.Exit i -> exit i
