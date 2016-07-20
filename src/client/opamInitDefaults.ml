(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let repository_url = {
  OpamUrl.
  transport = "https";
  path = "opam.ocaml.org";
  hash = None;
  backend = `http;
}

let eval_variables = [
  OpamVariable.of_string "sys-ocaml-version", ["ocamlc"; "-vnum"],
  "OCaml version present on your system indenpendently of opam, if any";
]

let init_config () =
  OpamFile.Config.create
    [] None [OpamRepositoryName.of_string (OpamUrl.to_string repository_url)]
    OpamStateConfig.(Lazy.force default.jobs)
    OpamStateConfig.(default.dl_jobs)
  |> OpamFile.Config.with_eval_variables eval_variables
