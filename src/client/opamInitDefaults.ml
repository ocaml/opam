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

let os_filter os =
  FOp (FIdent ([], OpamVariable.of_string "os", None), `Eq, FString os)

let linux_filter = os_filter "linux"
let macos_filter = os_filter "macos"
let openbsd_filter = os_filter "openbsd"
let freebsd_filter = os_filter "freebsd"
let sandbox_filter = FOr (linux_filter, macos_filter)

let gpatch_filter = FOr (openbsd_filter, freebsd_filter)
let patch_filter = FNot gpatch_filter

let gtar_filter = openbsd_filter
let tar_filter = FNot gtar_filter

let wrappers ~sandboxing () =
  let cmd t = [
    CString "%{hooks}%/sandbox.sh", None;
    CString t, None;
  ] in
  let w = OpamFile.Wrappers.empty in
  if sandboxing then
    { w with
      OpamFile.Wrappers.
      wrap_build = [cmd "build", Some sandbox_filter];
      wrap_install = [cmd "install", Some sandbox_filter];
      wrap_remove = [cmd "remove", Some sandbox_filter];
    }
  else w

let bwrap_cmd = "bwrap"
let bwrap_filter = linux_filter
let bwrap_string () = Printf.sprintf
    "Sandboxing tool %s was not found. You should install 'bubblewrap'. \
     See http://opam.ocaml.org/doc/2.0/FAQ.html#Why-opam-asks-me-to-install-bwrap."
    bwrap_cmd

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

let required_tools ~sandboxing () =
  [
    dl_tools(),
    Some "A download tool is required, check env variables OPAMCURL or OPAMFETCH",
    None;
    ["diff"], None, None;
    ["patch"], None, Some patch_filter;
    ["gpatch"], None, Some gpatch_filter;
    ["tar"], None, Some tar_filter;
    ["gtar"], None, Some gtar_filter;
    ["unzip"], None, None;
  ] @
  if sandboxing then [
    [bwrap_cmd], Some (bwrap_string()), Some bwrap_filter;
    ["sandbox-exec"], None, Some macos_filter;
  ] else []

let init_scripts () = [
  ("sandbox.sh", OpamScript.bwrap), Some bwrap_filter;
  ("sandbox.sh", OpamScript.sandbox_exec), Some macos_filter;
]

module I = OpamFile.InitConfig

let (@|) g f = OpamStd.Op.(g @* f) ()

let init_config ?(sandboxing=true) () =
  I.empty |>
  I.with_repositories
    [OpamRepositoryName.of_string "default", (repository_url, None)] |>
  I.with_default_compiler default_compiler |>
  I.with_eval_variables eval_variables |>
  I.with_wrappers @| wrappers ~sandboxing |>
  I.with_recommended_tools @| recommended_tools |>
  I.with_required_tools @| required_tools ~sandboxing |>
  I.with_init_scripts @| init_scripts |>
  I.with_dl_tool @| dl_tool
