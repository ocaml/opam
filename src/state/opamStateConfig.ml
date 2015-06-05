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

type t = {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t;
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
}

let default = {
  root_dir = OpamFilename.Op.(
      OpamFilename.Dir.of_string (OpamStd.Sys.home ()) / ".opam"
    );
  current_switch = OpamSwitch.system;
  switch_from = `Default;
  jobs = lazy (OpamSystem.cpu_count ());
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
  unit -> 'a

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
   ()
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    root_dir = t.root_dir + root_dir;
    current_switch = t.current_switch + current_switch;
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
  }

let set t = setk (fun x -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg -> r := cfg) !r

let init ?noop:_ =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  let current_switch, switch_from =
    match env_string "SWITCH" with
    | Some s -> Some (OpamSwitch.of_string s), Some `Env
    | None -> None, None
  in
  setk (setk (fun c -> r := c)) !r
    ?root_dir:(env_string "ROOT" >>| OpamFilename.Dir.of_string)
    ?current_switch
    ?switch_from
    ?jobs:(env_int "JOBS" >>| fun s -> lazy s)
    ?dl_jobs:(env_int "DOWNLOADJOBS")
    ?keep_build_dir:(env_bool "KEEPBUILDDIR")
    ?no_base_packages:(env_bool "NOBASEPACKAGES")
    ?build_test:(env_bool "BUILDTEST")
    ?build_doc:(env_bool "BUILDDOC")
    ?show:(env_bool "SHOW")
    ?dryrun:(env_bool "DRYRUN")
    ?fake:(env_bool "FAKE")
    ?makecmd:(env_string "MAKECMD" >>| fun s -> lazy s)
    ()

let opamroot ?root_dir () =
  let open OpamStd.Option.Op in
  (root_dir >>+ fun () ->
   OpamStd.Env.getopt "OPAMROOT" >>| OpamFilename.Dir.of_string)
  +! default.root_dir

let load opamroot =
  let f = OpamPath.config opamroot in
  if OpamFilename.exists f then
    OpamFilename.with_flock ~read:true
      (OpamFilename.add_extension f "lock")
      (fun f -> Some (OpamFile.Config.read f)) f
  else None

let write opamroot conf =
  let f = OpamPath.config opamroot in
  OpamFilename.with_flock ~read:false
    (OpamFilename.add_extension f "lock")
    (OpamFile.Config.write f) conf

let filter_deps f =
  OpamTypesBase.filter_deps
    ~build:true
    ~test:(!r.build_test)
    ~doc:(!r.build_doc)
    f
