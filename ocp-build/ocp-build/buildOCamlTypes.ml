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

type force_kind =
  Force_IMPL
| Force_INTF
| Force_not


type pp = {
  mutable pp_option : string list;
  mutable pp_flags : BuildEngineTypes.command_argument list;
  mutable pp_requires : BuildEngineTypes.build_file list;
}
