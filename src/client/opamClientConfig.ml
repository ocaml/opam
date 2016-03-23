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
  print_stats: bool;
  sync_archives: bool;
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
}

let default = {
  print_stats = false;
  sync_archives = false;
  pin_kind_auto = true;
  autoremove = false;
  editor = "nano";
}

type 'a options_fun =
  ?print_stats:bool ->
  ?sync_archives:bool ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  'a

let setk k t
    ?print_stats
    ?sync_archives
    ?pin_kind_auto
    ?autoremove
    ?editor
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    print_stats = t.print_stats + print_stats;
    sync_archives = t.sync_archives + sync_archives;
    pin_kind_auto = t.pin_kind_auto + pin_kind_auto;
    autoremove = t.autoremove + autoremove;
    editor = t.editor + editor;
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  let editor =
    env_string "EDITOR" ++ OpamStd.Env.(getopt "VISUAL" ++ getopt "EDITOR")
  in
  setk (setk (fun c -> r := c; k)) !r
    ?print_stats:(env_bool "STATS")
    ?sync_archives:(env_bool "SYNCARCHIVES")
    ?pin_kind_auto:(env_bool "PINKINDAUTO")
    ?autoremove:(env_bool "AUTOREMOVE")
    ?editor

let init ?noop:_ = initk (fun () -> ())

let search_files = ["findlib"]

open OpamStd.Op

let opam_init ?root_dir ?strict =
  (* (i) get root dir *)
  let root = OpamStateConfig.opamroot ?root_dir () in

  (* (ii) load conf file and set defaults *)
  (* the init for OpamFormat is done in advance since (a) it has an effect on
     loading the global config (b) the global config has no effect on it *)
  OpamFormatConfig.initk ?strict @@ fun ?log_dir ->
  let initialised = OpamStateConfig.load_defaults root <> None in
  (* !X fixme: don't drop the loaded config file to reload it afterwards (when
     loading the global_state) like that... *)

  (* (iii) load from env and options using OpamXxxConfig.init *)
  let log_dir =
    if log_dir = None && initialised
    then Some OpamFilename.(Dir.to_string Op.(root / "log"))
    else None
  in
  (fun () -> ()) |>
  OpamStd.Config.initk ?log_dir |>
  OpamRepositoryConfig.initk |>
  OpamSolverConfig.initk |>
  OpamStateConfig.initk ~root_dir:root |>
  initk
