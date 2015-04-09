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

let self_upgrade_bootstrapping_value = "bootstrapping"

let init_config () =
  let open OpamGlobals.Config in
  let open OpamMisc.Option.Op in
  let self_upgrade =
    if env_string "NOSELFUPGRADE" = Some self_upgrade_bootstrapping_value
    then Some `Running
    else env_bool "NOSELFUPGRADE" >>| function true -> `Disable | false -> `None
  in
  let editor = OpamMisc.Option.Op.(
      env_string "EDITOR" ++ OpamMisc.Env.(getopt "VISUAL" ++ getopt "EDITOR")
    ) in
  let current_switch, switch_from =
    match env_string "SWITCH" with
    | Some s -> Some (OpamSwitch.of_string s), Some `Env
    | None -> None, None
  in
  OpamClientConfig.(
    setk
      (fun conf -> setk (fun c -> r := c) (fun () -> conf))
      (fun () -> !r)
  )
    ?root_dir:(env_string "ROOT" >>| OpamFilename.Dir.of_string)
    ?current_switch
    ?switch_from
    ?jobs:(env_int "JOBS")
    ?dl_jobs:(env_int "DOWNLOADJOBS")
    ?keep_build_dir:(env_bool "KEEPBUILDDIR")
    ?no_base_packages:(env_bool "NOBASEPACKAGES")
    ?build_test:(env_bool "BUILDTEST")
    ?build_doc:(env_bool "BUILDDOC")
    ?show:(env_bool "SHOW")
    ?dryrun:(env_bool "DRYRUN")
    ?fake:(env_bool "FAKE")
    ?print_stats:(env_bool "STATS")
    ?sync_archives:(env_bool "SYNCARCHIVES")
    ?self_upgrade
    ?pin_kind_auto:(env_bool "PINKINDAUTO")
    ?autoremove:(env_bool "AUTOREMOVE")
    ?editor
    ?makecmd:(env_string "MAKECMD" >>| fun s -> lazy s)
    ()


(* Detect OCaml specifics (all done lazily) *)

let reset_env = lazy (
  let env = OpamMisc.Env.list () in
  let env =
    let path_sep = OpamMisc.Sys.path_sep () in
    let path_sep_str = String.make 1 path_sep in
    List.rev_map (fun (k,v as c) ->
      match k with
      | "PATH" ->
        k, String.concat path_sep_str
          (OpamMisc.Env.reset_value path_sep
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
    | h::_ -> Some (OpamMisc.String.strip h)
    | [] ->
      log "ERROR: ocamlc found but `ocamlc %s` is empty." cmd;
      None
  with OpamSystem.Command_not_found _ ->
    log "%s ocamlc not found" (if system then "system" else "opam");
    None

let exists_alongside_ocamlc name =
  let path = try OpamMisc.Env.get "PATH" with Not_found -> "" in
  let path = OpamMisc.String.split path (OpamMisc.Sys.path_sep ()) in
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
let ocaml_natdynlink_available = lazy OpamMisc.Option.Op.(
    (Lazy.force ocaml_where >>| fun d ->
     Sys.file_exists (Filename.concat d "dynlink.cmxa"))
    +! false
)

let system_ocamlc_version = lazy (ocaml_cmd ~system:true "-version")
let system_ocamlc_where = lazy (ocaml_cmd ~system:true "-where")
let system_compiler = lazy (
  OpamMisc.Option.Op.(Lazy.force system_ocamlc_version >>|
                      OpamCompiler.of_string)
)

let filter_deps =
  OpamTypesBase.filter_deps
    ~build:true
    ~test:(OpamClientConfig.(!r.build_test))
    ~doc:(OpamClientConfig.(!r.build_doc))

let search_files = ["findlib"]

let load_conf_file opamroot =
  let f = OpamPath.config opamroot in
  if OpamFilename.exists f then
    OpamFilename.with_flock ~read:true f
      (fun f -> Some (OpamFile.Config.read f)) f
  else None

let write_conf_file opamroot conf =
  let f = OpamPath.config opamroot in
  OpamFilename.with_flock ~read:false f
    (OpamFile.Config.write f) conf
