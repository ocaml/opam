(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Names of a repository elements layout} *)

val opam_f : string
val repo_f : string
val repo_d : string
val packages_d : string
val download_cache_d : string
val files_d : string

(** {1 Deprecated files: still valid for opam 2.0 but deprecated} *)

val legacy_url_f : string
val legacy_descr_f : string
