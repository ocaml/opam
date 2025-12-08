(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This modules handles the conversion from older repository and package
    versions to the current one *)

open OpamTypes
open OpamStateTypes

(** Raised when the opam root has been updated to a newer format, and further
    action (opam init/update) is needed.
    [Upgrade_done conf reinit] specifies the new config file and a reinit
    function to call instead of default (see {!OpamCliMain.main_catch_all}). *)
exception Upgrade_done of OpamFile.Config.t * (OpamFile.Config.t -> unit) option

(** The latest version of the opam root format, that normal operation of this
    instance of opam requires *)
val latest_version: OpamVersion.t

(** [as_necessary requested_lock global_lock root config]
    Runs the upgrade from its current format to the latest compatible version
    for the opam root at [root] directory. Performs an on-the-fly upgrade
    (loaded state, not written) if possible: no hard upgrade needed, and no
    write lock required ([requested_lock]). If upgrade need to be written (hard
    upgrade), a write lock on the global state ([global_lock]) is taken and
    when it's done raises [Upgrade_done updated_config]. Otherwise, it returns
    the upgraded or unchanged config file and a status of remaining upgrades.*)
val as_necessary:
  ?reinit:(OpamFile.Config.t -> unit) -> 'a lock -> OpamSystem.lock -> dirname ->
  OpamFile.Config.t ->
  OpamFile.Config.t * gt_changes

(* [as_necessary_repo lock gt] does the upgrades at repo level. [lock] is the
   required lock for repo state and gt the on-the-fly upgraded global lock. If
   [lock] is none or read, it will perform an on-the-fly upgrade of repos-config
   and return it. if [lock] is a write lock, it locks opam root and perform and
   apply all needed upgrades (write global, repo & state config if needed).
   At global state loading, [as_necessary] is called, so this function will do
   the upgrades only if there is a known on-the-fly upgrade done by
   [as_necessary]. Otherwise, it is an no-op and returns None. *)
val as_necessary_repo:
  'a lock -> 'b global_state -> OpamFile.Repos_config.t option

(* As [as_necessay_repo] but acts on a given [switch]. In case of write [lock]
   and upgrade that need to be done, it applies modification to all switches
   (as well as global and repo config). *)
val as_necessary_switch:
  'a lock -> switch -> 'b global_state -> OpamFile.Switch_config.t option

(* Try to launch a hard upgrade from 2;1 alpha's & beta's root
   to 2.1~rc one. Raises [Upgrade_done] (catched by main
   function) if an upgrade is done, otherwise do nothing.
   It is intend to be called after a config file that error with
   {!OpamPp.Bad_version} *)
val hard_upgrade_from_2_1_intermediates:
  ?reinit:(OpamFile.Config.t -> unit) -> ?global_lock: OpamSystem.lock ->
  dirname -> unit

(** Converts the opam file format, including rewriting availability conditions
    based on OCaml-related variables into dependencies. The filename is used to
    report errors *)
val opam_file_from_1_2_to_2_0:
  ?filename:OpamFile.OPAM.t OpamFile.t -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** Runs the opam file format from the file's format to current. Supplying
    [filename] enables additional notification messages *)
val opam_file:
  ?quiet:bool -> ?filename:OpamFile.OPAM.t OpamFile.t ->
  OpamFile.OPAM.t -> OpamFile.OPAM.t

(** Convert the comp file to an opam one, using {!OpamFile.Comp.to_package} and
    applying filter rewriting *)
val comp_file:
  ?package:package -> ?descr:OpamFile.Descr_legacy.t -> OpamFile.Comp.t ->
  OpamFile.OPAM.t

(** Runs the opam file format from the file's format to current, and adds data
    from 'url' and 'descr' files found in the specified dir or the opam file's
    metadata dir, if not already present in the opam file. If [files] is [true],
    also adds the names and hashes of files found below 'files/'. Supplying
    [filename] enables additional notification messages *)
val opam_file_with_aux:
  ?quiet:bool -> ?dir:dirname -> files:bool -> ?filename:OpamFile.OPAM.t OpamFile.t ->
  OpamFile.OPAM.t -> OpamFile.OPAM.t
