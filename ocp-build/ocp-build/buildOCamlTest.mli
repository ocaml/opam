(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

type stats = {
  mutable tests_nsuccesses : int;
  mutable tests_nfailures : int;
  mutable tests_failures : (string * string) list;
  mutable tests_timings : (string * float) list;
}

val init : unit -> stats
val test_package :
  BuildEngineTypes.build_context ->
  stats -> BuildTypes.package_info ->
  bool -> (* benchmarks only ? *)
  unit
val finish : stats -> int -> unit
