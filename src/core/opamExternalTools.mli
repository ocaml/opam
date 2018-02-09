(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t

val unpack : t -> (string * string list)

(** NOTE: Please try to use this function as little as possible *)
val custom : (string * string list) -> t

val has_space : t -> bool
val cmd_to_string : t -> string

(** For printing only *)
val to_string : t -> string

module Unzip : sig
  val extract : file:string -> dir:string -> t
end

module Tar : sig
  val extract_gzip : file:string -> dir:string -> t
  val extract_bzip2 : file:string -> dir:string -> t
  val extract_xz : file:string -> dir:string -> t
  val extract_lzma : file:string -> dir:string -> t
  val create_gzip : output:string -> inputs:string list -> t
end

module Patch : sig
  val apply : file:string -> t
end

module Diff : sig
  val dirs : dir1:string -> dir2:string -> t
end

module Rm : sig
  val recursive : dir:string -> t
end

module Cmd : sig
  val rm_recursive : dir:string -> t
  val get_os_version : t
end

module Sw_vers : sig
  val get_os_version : t
end

module Getprop : sig
  val get_os_version : t
end

module Sleep : sig
  val one_second : t
end

module Cp : sig
  val cp : src:string -> dst:string -> t
  val recursive : srcs:string list -> dst:string -> t
end

module Mv : sig
  val mv : src:string -> dst:string -> t
end

module Install : sig
  val exec : src:string -> dst:string -> t
  val file : src:string -> dst:string -> t
end

module Sysctl : sig
  val get_hw_ncpu : t
end

module Getconf : sig
  val get_nproc : t
end

module Lsb_release : sig
  val get_id : t
  val get_release : t
end

module Tput : sig
  val cols : t
end

module Stty : sig
  val size : t
end

module Uname : sig
  val kern_name : t
  val kern_version : t
  val arch : t
  val freebsd_version : t
end

module Openssl : sig
  val sha256 : file:string -> t
  val sha512 : file:string -> t
end

module Git : sig
  val get_user_name : t
  val get_user_email : t
end

module OCaml : sig
  val vnum : t
end

module Command : sig
  val lookup : cmd:string -> t
end
