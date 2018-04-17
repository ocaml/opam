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

let linux_filter =
  Some (FOp (FIdent ([], OpamVariable.of_string "os", None), `Eq, FString "linux"))

let wrappers () =
  let cmd t = [
    CString "%{hooks}%/sandbox.sh", None;
    CString t, None;
  ] in
  let w = OpamFile.Wrappers.empty in
  { w with
    OpamFile.Wrappers.
    wrap_build = [cmd "build", linux_filter];
    wrap_install = [cmd "install", linux_filter];
    wrap_remove = [cmd "remove", linux_filter];
  }

let bwrap_cmd = "bwrap"
let bwrap_filter = linux_filter
let bwrap_string () =
  Printf.sprintf "Sandboxing tool %s was not found. You should install \
                  'bubblewrap', or manually disable package build sandboxing \
                  and remove bwrap from required dependencies(at your own risk). \
                  You can display the built-in configuration with %s to set \
                  up an %s file. See \
                  https://github.com/projectatomic/bubblewrap for details."
    (OpamConsole.colorise `bold bwrap_cmd)
    (OpamConsole.colorise `bold "opam init --show-default-opamrc")
    (OpamConsole.colorise `underline "opamrc")

let fetch_cmd_user () =
  let open OpamStd.Option.Op in
  match
    OpamStd.Env.getopt "OPAMCURL",
    OpamStd.Env.getopt "OPAMFETCH" >>| fun s ->
    OpamStd.String.split s ' '
  with
  | Some cmd, _ | _, Some (cmd::_) -> Some cmd
  | _ -> None

let dl_tools () =
  match fetch_cmd_user () with
  | None -> ["curl"; "wget"]
  | Some cmd -> [cmd]

let dl_tool () =
  match fetch_cmd_user () with
  | None ->  None
  | Some cmd -> Some [(CString cmd), None]

let recommended_tools () =
  let make = OpamStateConfig.(Lazy.force !r.makecmd) in
  [
    [make], None, None;
    ["m4"], None, None;
    ["cc"], None, None;
  ]

let required_tools () =
  [
    dl_tools(),
    Some "A download tool is required, check env variables OPAMCURL or OPAMFETCH",
    None;
    ["diff"], None, None;
    ["patch"], None, None;
    ["tar"], None, None;
    ["unzip"], None, None;
    [bwrap_cmd], Some (bwrap_string()), bwrap_filter;
  ]

let init_scripts () =
  [ (("sandbox.sh", OpamScript.bwrap), bwrap_filter);
  ]

module I = OpamFile.InitConfig

let (@|) g f = OpamStd.Op.(g @* f) ()

let init_config () =
  I.empty |>
  I.with_repositories
    [OpamRepositoryName.of_string "default", (repository_url, None)] |>
  I.with_default_compiler default_compiler |>
  I.with_eval_variables eval_variables |>
  I.with_wrappers @| wrappers |>
  I.with_recommended_tools @| recommended_tools |>
  I.with_required_tools @| required_tools |>
  I.with_init_scripts @| init_scripts |>
  I.with_dl_tool @| dl_tool
