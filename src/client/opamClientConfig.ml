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
  self_upgrade: [ `Disable | `Running | `None ];
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
}

let default = {
  print_stats = false;
  sync_archives = false;
  self_upgrade = `None;
  pin_kind_auto = false;
  autoremove = false;
  editor = "nano";
}

type 'a options_fun =
  ?print_stats:bool ->
  ?sync_archives:bool ->
  ?self_upgrade:[ `Disable | `Running | `None ] ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  unit -> 'a

let setk k t
    ?print_stats
    ?sync_archives
    ?self_upgrade
    ?pin_kind_auto
    ?autoremove
    ?editor
   ()
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    print_stats = t.print_stats + print_stats;
    sync_archives = t.sync_archives + sync_archives;
    self_upgrade = t.self_upgrade + self_upgrade;
    pin_kind_auto = t.pin_kind_auto + pin_kind_auto;
    autoremove = t.autoremove + autoremove;
    editor = t.editor + editor;
  }

let set t = setk (fun x -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg -> r := cfg) !r

let self_upgrade_bootstrapping_value = "bootstrapping"

let init ?noop:_ =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  let self_upgrade =
    if env_string "NOSELFUPGRADE" = Some self_upgrade_bootstrapping_value
    then Some `Running
    else env_bool "NOSELFUPGRADE" >>| function true -> `Disable | false -> `None
  in
  let editor =
    env_string "EDITOR" ++ OpamStd.Env.(getopt "VISUAL" ++ getopt "EDITOR")
  in
  setk (setk (fun c -> r := c)) !r
    ?print_stats:(env_bool "STATS")
    ?sync_archives:(env_bool "SYNCARCHIVES")
    ?self_upgrade
    ?pin_kind_auto:(env_bool "PINKINDAUTO")
    ?autoremove:(env_bool "AUTOREMOVE")
    ?editor
    ()

let search_files = ["findlib"]
