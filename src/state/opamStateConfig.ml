(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

type t = {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t option;
  switch_from: [ `Env | `Command_line | `Default ];
  jobs: int Lazy.t;
  dl_jobs: int;
  build_test: bool;
  build_doc: bool;
  dryrun: bool;
  makecmd: string Lazy.t;
  ignore_constraints_on: name_set;
}

let default = {
  root_dir = OpamFilename.Op.(
      OpamFilename.Dir.of_string (OpamStd.Sys.home ()) / ".opam"
    );
  current_switch = None;
  switch_from = `Default;
  jobs = lazy (max 1 (OpamSystem.cpu_count () - 1));
  dl_jobs = 3;
  build_test = false;
  build_doc = false;
  dryrun = false;
  makecmd = lazy OpamStd.Sys.(
      match os () with
      | FreeBSD | OpenBSD | NetBSD | DragonFly -> "gmake"
      | _ -> "make"
    );
  ignore_constraints_on = OpamPackage.Name.Set.empty;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:[ `Env | `Command_line | `Default ] ->
  ?jobs:(int Lazy.t) ->
  ?dl_jobs:int ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?dryrun:bool ->
  ?makecmd:string Lazy.t ->
  ?ignore_constraints_on:name_set ->
  'a

let setk k t
    ?root_dir
    ?current_switch
    ?switch_from
    ?jobs
    ?dl_jobs
    ?build_test
    ?build_doc
    ?dryrun
    ?makecmd
    ?ignore_constraints_on
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    root_dir = t.root_dir + root_dir;
    current_switch =
      (match current_switch with None -> t.current_switch | s -> s);
    switch_from = t.switch_from + switch_from;
    jobs = t.jobs + jobs;
    dl_jobs = t.dl_jobs + dl_jobs;
    build_test = t.build_test + build_test;
    build_doc = t.build_doc + build_doc;
    dryrun = t.dryrun + dryrun;
    makecmd = t.makecmd + makecmd;
    ignore_constraints_on = t.ignore_constraints_on + ignore_constraints_on;
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  let current_switch, switch_from =
    match env_string "SWITCH" with
    | Some "" | None -> None, None
    | Some s -> Some (OpamSwitch.of_string s), Some `Env
  in
  setk (setk (fun c -> r := c; k)) !r
    ?root_dir:(env_string "ROOT" >>| OpamFilename.Dir.of_string)
    ?current_switch
    ?switch_from
    ?jobs:(env_int "JOBS" >>| fun s -> lazy s)
    ?dl_jobs:(env_int "DOWNLOADJOBS")
    ?build_test:(env_bool "WITHTEST" ++ env_bool "BUILDTEST")
    ?build_doc:(env_bool "WITHDOC" ++ env_bool "BUILDDOC")
    ?dryrun:(env_bool "DRYRUN")
    ?makecmd:(env_string "MAKECMD" >>| fun s -> lazy s)
    ?ignore_constraints_on:
      (env_string "IGNORECONSTRAINTS" >>| fun s ->
       OpamStd.String.split s ',' |>
       List.map OpamPackage.Name.of_string |>
       OpamPackage.Name.Set.of_list)

let init ?noop:_ = initk (fun () -> ())

let opamroot ?root_dir () =
  let open OpamStd.Option.Op in
  (root_dir >>+ fun () ->
   OpamStd.Env.getopt "OPAMROOT" >>| OpamFilename.Dir.of_string)
  +! default.root_dir

let load opamroot =
  OpamFile.Config.read_opt (OpamPath.config opamroot)

let get_current_switch_from_cwd root =
  let open OpamStd.Option.Op in
  OpamFilename.find_in_parents
    (fun d ->
       OpamSwitch.of_string (OpamFilename.Dir.to_string d) |>
       OpamPath.Switch.switch_config root |>
       OpamFile.Switch_config.read_opt |> function
       | None -> false
       | Some conf -> conf.OpamFile.Switch_config.opam_root = Some root)
    (OpamFilename.cwd ())
  >>| OpamSwitch.of_dirname

let load_defaults root_dir =
  let current_switch =
    match OpamStd.Config.env_string "SWITCH" with
    | Some "" | None -> get_current_switch_from_cwd root_dir
    | _ -> (* OPAMSWITCH is set, no need to lookup *) None
  in
  match load root_dir with
  | None ->
    update ?current_switch ();
    None
  | Some conf ->
    let open OpamStd.Option.Op in
    OpamRepositoryConfig.update
      ?download_tool:(OpamFile.Config.dl_tool conf >>| function
        | (CString c,None)::_ as t
          when OpamStd.String.ends_with ~suffix:"curl" c -> lazy (t, `Curl)
        | t -> lazy (t, `Default))
      ~validation_hook:(OpamFile.Config.validation_hook conf)
      ();
    update
      ?current_switch:(OpamFile.Config.switch conf)
      ~switch_from:`Default
      ~jobs:(lazy (OpamFile.Config.jobs conf))
      ~dl_jobs:(OpamFile.Config.dl_jobs conf)
      ();
    update ?current_switch ();
    Some conf

let get_switch () =
  match !r.current_switch with
  | Some s -> s
  | None ->
    OpamConsole.error_and_exit
      "No switch is currently set. Please use 'opam switch' to set or install \
       a switch"
