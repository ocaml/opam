(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
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

open OpamTypesBase

let log fmt = OpamConsole.log "CLIENTGLOBALS" fmt

(* Detect OCaml specifics (all done lazily) *)

let reset_env = lazy (
  let env = OpamStd.Env.list () in
  let env =
    let path_sep = OpamStd.Sys.path_sep () in
    let path_sep_str = String.make 1 path_sep in
    List.rev_map (fun (k,v as c) ->
      match k with
      | "PATH" ->
        k, String.concat path_sep_str
          (OpamStd.Env.reset_value path_sep
             ~prefix:OpamClientConfig.(OpamFilename.Dir.to_string !r.root_dir)
             v)
      | _      -> c
    ) env in
  let env = List.rev_map (fun (k,v) -> k^"="^v) env in
  Array.of_list env
)

let ocaml_cmd ~system cmd =
  let env = if system then Some (Lazy.force reset_env) else None in
  try
    match
      OpamSystem.read_command_output ?env ~verbose:false [ "ocamlc" ; cmd ]
    with
    | h::_ -> Some (OpamStd.String.strip h)
    | [] ->
      log "ERROR: ocamlc found but `ocamlc %s` is empty." cmd;
      None
  with OpamSystem.Command_not_found _ ->
    log "%s ocamlc not found" (if system then "system" else "opam");
    None

let exists_alongside_ocamlc name =
  let path = try OpamStd.Env.get "PATH" with Not_found -> "" in
  let path = OpamStd.String.split path (OpamStd.Sys.path_sep ()) in
  let ocamlc_dir =
    List.fold_left (function
        | None -> fun d ->
          if Sys.file_exists (Filename.concat d "ocamlc") then Some d else None
        | s -> fun _ -> s)
      None path
  in
  match ocamlc_dir with
  | Some d -> Sys.file_exists (Filename.concat d name)
  | None -> false

let ocaml_version = lazy (ocaml_cmd ~system:false "-version")
let ocaml_where = lazy (ocaml_cmd ~system:false "-where")
let ocaml_opt_available = lazy (exists_alongside_ocamlc "ocamlc.opt")
let ocaml_native_available = lazy (exists_alongside_ocamlc "ocamlopt")
let ocaml_natdynlink_available = lazy OpamStd.Option.Op.(
    (Lazy.force ocaml_where >>| fun d ->
     Sys.file_exists (Filename.concat d "dynlink.cmxa"))
    +! false
)

let system_ocamlc_version = lazy (ocaml_cmd ~system:true "-version")
let system_ocamlc_where = lazy (ocaml_cmd ~system:true "-where")
let system_compiler = lazy (
  OpamStd.Option.Op.(Lazy.force system_ocamlc_version >>|
                      OpamCompiler.of_string)
)

let search_files = ["findlib"]
