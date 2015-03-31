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
  switch_set: [ `Env of string | `Command_line of string | `Not_set ];
  jobs: int;
  dl_jobs: int;
  external_tags: string list;
  keep_build_dir: bool;
  no_base_packages: bool;
  build_test: bool;
  build_doc: bool;
  show: bool;
  dryrun: bool;
  fake: bool;
  print_stats: bool;
  sync_archives: bool;
  self_upgrade: [ `Disable | `Running | `None ];
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
  makecmd: string Lazy.t;
}

let default = {
  root_dir = OpamFilename.OP.(
      OpamFilename.Dir.of_string (OpamMisc.Sys.home ()) / ".opam"
    );
  switch_set = `Not_set;
  jobs = 1;
  dl_jobs = 3;
  external_tags = [];
  keep_build_dir = false;
  no_base_packages = false;
  build_test = false;
  build_doc = false;
  show = false;
  dryrun = false;
  fake = false;
  print_stats = false;
  sync_archives = false;
  self_upgrade = `None;
  pin_kind_auto = false;
  autoremove = false;
  editor = "nano";
  makecmd = lazy OpamMisc.Sys.(
      match os () with
      | FreeBSD | OpenBSD | NetBSD | DragonFly -> "gmake"
      | _ -> "make"
    );
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?switch_set:[ `Env of string | `Command_line of string | `Not_set ] ->
  ?jobs: int ->
  ?dl_jobs: int ->
  ?external_tags:string list ->
  ?keep_build_dir:bool ->
  ?no_base_packages:bool ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?show:bool ->
  ?dryrun:bool ->
  ?fake:bool ->
  ?print_stats:bool ->
  ?sync_archives:bool ->
  ?self_upgrade:[ `Disable | `Running | `None ] ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  ?makecmd:string Lazy.t ->
  unit -> 'a

let setk k t
    ?root_dir
    ?switch_set
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
    ?print_stats
    ?sync_archives
    ?self_upgrade
    ?pin_kind_auto
    ?autoremove
    ?editor
    ?makecmd
   ()
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    root_dir = t.root_dir + root_dir;
    switch_set = t.switch_set + switch_set;
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
    print_stats = t.print_stats + print_stats;
    sync_archives = t.sync_archives + sync_archives;
    self_upgrade = t.self_upgrade + self_upgrade;
    pin_kind_auto = t.pin_kind_auto + pin_kind_auto;
    autoremove = t.autoremove + autoremove;
    editor = t.editor + editor;
    makecmd = t.makecmd + makecmd;
  }

let set = setk (fun x -> x)

let r = ref default

let update = setk (fun cfg -> r := cfg) !r
