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
    CString "%{hooks}%/sandbox.sh", None;
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

let bwrap_cmd = "bwrap"
let bwrap_filter =
  Some (FOp (FIdent ([], OpamVariable.of_string "os", None), `Eq, FString "linux"))
let bwrap_string =
  Printf.sprintf "Sandboxing tool %s was not found. You should install \
                  'bubblewrap', or manually disable package build sandboxing \
                  and remove bwrap from required dependencies(at your own risk). \
                  You can display the built-in configuration with %s to set \
                  up an %s file. See \
                  https://github.com/projectatomic/bubblewrap for details."
    (OpamConsole.colorise `bold bwrap_cmd)
    (OpamConsole.colorise `bold "opam init --show-default-opamrc")
    (OpamConsole.colorise `underline "opamrc")

let recommended_tools =
  let make = OpamStateConfig.(Lazy.force !r.makecmd) in
  [
    (([make], ""), None);
    ((["m4"], ""), None);
    ((["cc"], ""), None);
  ]

let required_tools =
  let fetch_cmd_user =
    let open OpamStd.Option.Op in
    match
      OpamStd.Env.getopt "OPAMCURL",
      OpamStd.Env.getopt "OPAMFETCH" >>| fun s ->
      OpamStd.String.split s ' '
    with
    | Some cmd, _ | _, Some (cmd::_) -> [cmd]
    | _ -> []
  in
  [
    ((["curl"; "wget"] @ fetch_cmd_user, ""), None);
    ((["diff"], ""), None);
    ((["patch"], ""), None);
    ((["tar"], ""), None);
    ((["unzip"], ""), None);
    (([bwrap_cmd], bwrap_string), bwrap_filter);
  ]


let init_scripts =
  [ (("sandbox.sh", OpamScript.bwrap), bwrap_filter);
  ]

module I = OpamFile.InitConfig

let init_config =
  I.empty |>
  I.with_repositories
    [OpamRepositoryName.of_string "default", (repository_url, None)] |>
  I.with_default_compiler default_compiler |>
  I.with_eval_variables eval_variables |>
  I.with_wrappers wrappers |>
  I.with_recommended_tools recommended_tools |>
  I.with_required_tools required_tools |>
  I.with_init_scripts init_scripts
