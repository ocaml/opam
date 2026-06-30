(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2016 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Git repository backend (based on {!OpamVCS}) *)

(** Returns [OpamProcess.default_env] + some git specific environment variables
    used to make git calls more reproducible.
    Note however that it cannot be used if you need values from the global git
    configs (e.g. /etc/gitconfig or ~/.gitconfig) *)
val env : unit -> string array

module VCS: OpamVCS.VCS

module B: OpamRepositoryBackend.S
