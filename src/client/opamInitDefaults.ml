(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let repository_url = {
  OpamUrl.
  transport = "https";
  path = "opam.ocaml.org";
  hash = None;
  backend = `http;
}

let default_compiler =
  OpamFormula.ors [
    OpamFormula.Atom (OpamPackage.Name.of_string "ocaml-system",
                      OpamFormula.Atom
                        (`Geq, OpamPackage.Version.of_string "4.02.3"));
    OpamFormula.Atom (OpamPackage.Name.of_string "ocaml-base-compiler",
                      OpamFormula.Empty);
  ]

let eval_variables = [
  OpamVariable.of_string "sys-ocaml-version", ["ocamlc"; "-vnum"],
  "OCaml version present on your system independently of opam, if any";
]

let wrappers =
  let cmd t = [
    CString "%{root}%/opam-init/hooks/sandbox.sh", None;
    CString t, None;
  ] in
  let w = OpamFile.Wrappers.empty in
  if OpamStd.Sys.(os () = Linux) then (* Sandboxing scripts only available there *)
    { w with
      OpamFile.Wrappers.
      wrap_build = [cmd "build", None];
      wrap_install = [cmd "install", None];
      wrap_remove = [cmd "remove", None];
    }
  else w

module I = OpamFile.InitConfig

let init_config =
  I.empty |>
  I.with_repositories
    [OpamRepositoryName.of_string "default", (repository_url, None)] |>
  I.with_default_compiler default_compiler |>
  I.with_eval_variables eval_variables |>
  I.with_wrappers wrappers
