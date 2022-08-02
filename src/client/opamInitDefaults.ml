(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2019 OCamlPro                                        *)
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
                      OpamFormula.Empty);
    OpamFormula.Atom (OpamPackage.Name.of_string "ocaml-base-compiler",
                      OpamFormula.Empty);
  ]

let default_invariant =
  OpamFormula.Atom
    (OpamPackage.Name.of_string "ocaml",
     OpamFormula.Atom
       (`Geq, OpamPackage.Version.of_string "4.05.0"))

let eval_variables = [
  OpamVariable.of_string "sys-ocaml-version", ["ocamlc"; "-vnum"],
  "OCaml version present on your system independently of opam, if any";
  OpamVariable.of_string "sys-ocaml-arch", ["sh"; "-c"; "ocamlc -config 2>/dev/null | tr -d '\\r' | grep '^architecture: ' | sed -e 's/.*: //' -e 's/i386/i686/' -e 's/amd64/x86_64/'"],
  "Target architecture of the OCaml compiler present on your system";
  OpamVariable.of_string "sys-ocaml-cc", ["sh"; "-c"; "ocamlc -config 2>/dev/null | tr -d '\\r' | grep '^ccomp_type: ' | sed -e 's/.*: //'"],
  "Host C Compiler type of the OCaml compiler present on your system";
  OpamVariable.of_string "sys-ocaml-libc", ["sh"; "-c"; "ocamlc -config 2>/dev/null | tr -d '\\r' | grep '^os_type: ' | sed -e 's/.*: //' -e 's/Win32/msvc/' -e '/^msvc$/!s/.*/libc/'"],
  "Host C Runtime Library type of the OCaml compiler present on your system";
]

let os_filter os =
  FOp (FIdent ([], OpamVariable.of_string "os", None), `Eq, FString os)

let linux_filter = os_filter "linux"
let macos_filter = os_filter "macos"
let openbsd_filter = os_filter "openbsd"
let freebsd_filter = os_filter "freebsd"
let not_open_free_bsd_filter =
  FNot (FOr (openbsd_filter,  freebsd_filter))
let win32_filter = os_filter "win32"
let not_win32_filter =
  FOp (FIdent ([], OpamVariable.of_string "os", None), `Neq, FString "win32")
let sandbox_filter = FOr (linux_filter, macos_filter)

let gpatch_filter = FOr (openbsd_filter, freebsd_filter)
let patch_filter = FNot gpatch_filter

let gtar_filter = openbsd_filter
let tar_filter = FNot gtar_filter

let getconf_filter = FNot (FOr (win32_filter, freebsd_filter))

let sandbox_wrappers =
  let cmd t = [
    CString "%{hooks}%/sandbox.sh", None;
    CString t, None;
  ] in
  [ `build  [cmd "build", Some sandbox_filter];
    `install  [cmd "install", Some sandbox_filter];
    `remove  [cmd "remove", Some sandbox_filter];
  ]

let wrappers ~sandboxing () =
  let w = OpamFile.Wrappers.empty in
  if sandboxing then
    List.fold_left OpamFile.Wrappers.(fun w -> function
        | `build wrap_build -> { w with wrap_build }
        | `install wrap_install -> { w with wrap_install }
        | `remove wrap_remove -> { w with wrap_remove }
      ) OpamFile.Wrappers.empty sandbox_wrappers
  else w

let bwrap_cmd = "bwrap"
let bwrap_filter = linux_filter
let bwrap_string () = Printf.sprintf
    "Sandboxing tool %s was not found. You should install 'bubblewrap'. \
     See https://opam.ocaml.org/doc/FAQ.html#Why-does-opam-require-bwrap."
    bwrap_cmd

let req_dl_tools () =
  let msg =
    Some "A download tool is required, check env variables OPAMCURL or OPAMFETCH"
  in
  let default =
    [
      ["curl"; "wget"], msg, Some not_open_free_bsd_filter;
      ["fetch"], msg, Some freebsd_filter;
      ["ftp"], msg, Some openbsd_filter
    ]
  in
  let open OpamStd.Option.Op in
  let cmd =
    (OpamRepositoryConfig.E.fetch_t ()
     >>= fun s ->
     match OpamStd.String.split s ' ' with
     | c::_ -> Some c
     | _ -> None)
    >>+ fun () -> OpamRepositoryConfig.E.curl_t ()
  in
  match cmd with
  | Some cmd ->
    [[cmd], Some "Custom download tool, check OPAMCURL or OPAMFETCH", None]
  | None -> default

let dl_tool () =
  let open OpamStd.Option.Op in
  (OpamRepositoryConfig.E.fetch_t ()
   >>+ fun () -> OpamRepositoryConfig.E.curl_t ())
  >>| fun cmd -> [(CString cmd), None]

let recommended_tools () =
  let make = OpamStateConfig.(Lazy.force !r.makecmd) in
  [
    [make], None, None;
    ["cc"], None, Some not_win32_filter;
  ]

let required_tools ~sandboxing () =
  req_dl_tools () @
  [
    ["diff"], None, None;
    ["patch"], None, Some patch_filter;
    ["gpatch"], None, Some gpatch_filter;
    ["tar"], None, Some tar_filter;
    ["gtar"], None, Some gtar_filter;
    ["unzip"], None, None;
    ["getconf"], None, Some getconf_filter;
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
  I.with_default_invariant default_invariant |>
  I.with_eval_variables eval_variables |>
  I.with_wrappers @| wrappers ~sandboxing |>
  I.with_recommended_tools @| recommended_tools |>
  I.with_required_tools @| required_tools ~sandboxing |>
  I.with_init_scripts @| init_scripts |>
  I.with_dl_tool @| dl_tool
