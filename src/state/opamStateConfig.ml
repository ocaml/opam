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

open OpamTypes

type t = {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t option;
  switch_from: [ `Env | `Command_line | `Default ];
  jobs: int Lazy.t;
  dl_jobs: int;
  external_tags: string list;
  keep_build_dir: bool;
  no_base_packages: bool;
  build_test: bool;
  build_doc: bool;
  show: bool;
  dryrun: bool;
  fake: bool;
  makecmd: string Lazy.t;
  json_out: string option;
}

let default = {
  root_dir = OpamFilename.Op.(
      OpamFilename.Dir.of_string (OpamStd.Sys.home ()) / ".opam"
    );
  current_switch = None;
  switch_from = `Default;
  jobs = lazy (max 1 (OpamSystem.cpu_count () - 1));
  dl_jobs = 3;
  external_tags = [];
  keep_build_dir = false;
  no_base_packages = false;
  build_test = false;
  build_doc = false;
  show = false;
  dryrun = false;
  fake = false;
  makecmd = lazy OpamStd.Sys.(
      match os () with
      | FreeBSD | OpenBSD | NetBSD | DragonFly -> "gmake"
      | _ -> "make"
    );
  json_out = None;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:[ `Env | `Command_line | `Default ] ->
  ?jobs:(int Lazy.t) ->
  ?dl_jobs:int ->
  ?external_tags:string list ->
  ?keep_build_dir:bool ->
  ?no_base_packages:bool ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?show:bool ->
  ?dryrun:bool ->
  ?fake:bool ->
  ?makecmd:string Lazy.t ->
  ?json_out:string option ->
  'a

let setk k t
    ?root_dir
    ?current_switch
    ?switch_from
    ?jobs
    ?dl_jobs
    ?external_tags
    ?keep_build_dir
    ?no_base_packages
    ?build_test
    ?build_doc
    ?show
    ?dryrun
    ?fake
    ?makecmd
    ?json_out
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    root_dir = t.root_dir + root_dir;
    current_switch =
      (match current_switch with None -> t.current_switch | s -> s);
    switch_from = t.switch_from + switch_from;
    jobs = t.jobs + jobs;
    dl_jobs = t.dl_jobs + dl_jobs;
    external_tags = t.external_tags + external_tags;
    keep_build_dir = t.keep_build_dir + keep_build_dir;
    no_base_packages = t.no_base_packages + no_base_packages;
    build_test = t.build_test + build_test;
    build_doc = t.build_doc + build_doc;
    show = t.show + show;
    dryrun = t.dryrun + dryrun;
    fake = t.fake + fake;
    makecmd = t.makecmd + makecmd;
    json_out = t.json_out + json_out;
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
    ?external_tags:None
    ?keep_build_dir:(env_bool "KEEPBUILDDIR")
    ?no_base_packages:(env_bool "NOBASEPACKAGES")
    ?build_test:(env_bool "BUILDTEST")
    ?build_doc:(env_bool "BUILDDOC")
    ?show:(env_bool "SHOW")
    ?dryrun:(env_bool "DRYRUN")
    ?fake:(env_bool "FAKE")
    ?makecmd:(env_string "MAKECMD" >>| fun s -> lazy s)
    ?json_out:(env_string "JSON" >>| function "" -> None | s -> Some s)

let init ?noop:_ = initk (fun () -> ())

let opamroot ?root_dir () =
  let open OpamStd.Option.Op in
  (root_dir >>+ fun () ->
   OpamStd.Env.getopt "OPAMROOT" >>| OpamFilename.Dir.of_string)
  +! default.root_dir

let load opamroot =
  OpamFile.Config.read_opt (OpamPath.config opamroot)

let write opamroot conf =
  OpamFile.Config.write (OpamPath.config opamroot) conf

let filter_deps ?(dev=true) f =
  OpamTypesBase.filter_deps
    ~build:true
    ~test:(!r.build_test)
    ~doc:(!r.build_doc)
    ~dev
    f

let load_defaults root_dir =
  match load root_dir with
  | None -> None
  | Some conf ->
    let open OpamStd.Option.Op in
    OpamRepositoryConfig.update
      ?download_tool:(OpamFile.Config.dl_tool conf >>| function
        | (CString c,None)::_ as t
          when OpamStd.String.ends_with ~suffix:"curl" c -> lazy (t, `Curl)
        | t -> lazy (t, `Default))
      ();
    let criteria kind =
      let c = OpamFile.Config.criteria conf in
      try Some (List.assoc kind c) with Not_found -> None
    in
    OpamSolverConfig.update
      ?external_solver:(OpamFile.Config.solver conf >>| fun s -> lazy(Some s))
      ?solver_preferences_default:(criteria `Default >>| fun s-> Some(lazy s))
      ?solver_preferences_upgrade:(criteria `Upgrade >>| fun s-> Some(lazy s))
      ?solver_preferences_fixup:(criteria `Fixup >>| fun s -> Some(lazy s))
      ();
    update
      ?current_switch:(OpamFile.Config.switch conf)
      ~switch_from:`Default
      ~jobs:(lazy (OpamFile.Config.jobs conf))
      ~dl_jobs:(OpamFile.Config.dl_jobs conf)
      ();
    Some conf

let get_switch () =
  match !r.current_switch with
  | Some s -> s
  | None ->
    OpamConsole.error_and_exit
      "No switch is currently set. Please use 'opam switch' to set or install \
       a switch"
