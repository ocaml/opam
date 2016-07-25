(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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

(** The latest version of the opam root format, that normal operation of this
    instance of opam requires *)
val latest_version: OpamVersion.t

(** Runs the upgrade from its current format to the latest version for the opam
    root at the given directory. A global write lock must be supplied, and the
    updated global config is returned. *)
val as_necessary: OpamSystem.lock -> dirname -> OpamFile.Config.t -> OpamFile.Config.t
