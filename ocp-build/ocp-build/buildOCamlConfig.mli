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

open BuildOCPTypes
open BuildOCPVariable

(* These values are modified by reading the configuration *)
val ocamlc_cmd : string list source_option
val ocamldoc_cmd : string list source_option
val ocamlcc_cmd : string list source_option
val ocamllex_cmd : string list source_option
val ocamlyacc_cmd : string list source_option
val ocamlmklib_cmd : string list source_option
val ocamldep_cmd : string list source_option
val ocamlopt_cmd : string list source_option
val native_support : bool source_option
val byte_support : bool source_option

(*
val mklib_cmd : BuildTypes.mklib_kind ref
val ar_cmd : string ref
val ranlib_cmd : string ref
val libdirs : (string * string) list ref
*)

(* These values are global, but could be set per project, as we can
  change the compiler depending on that.

   TODO: Maybe we could even attach
  these values to a particular compiler, and cache them so that we
   can load them each time that compiler is used.. *)
val ocaml_config_version : string list source_option
val ocaml_config_system : string list source_option
val ocaml_config_architecture :  string list source_option
val ocaml_config_ext_obj :  string source_option
val ocaml_config_ext_lib :  string source_option
val ocaml_config_ext_dll :  string source_option


module TYPES : sig
  type ocaml_config = {
    ocaml_version : string;
    ocaml_version_major : string;
    ocaml_version_minor : string;
    ocaml_version_point : string;
    ocaml_ocamllib : string;
    ocaml_system : string;
    ocaml_architecture : string;
    ocaml_ext_obj : string;
    ocaml_ext_lib : string;
    ocaml_ext_dll : string;
    ocaml_os_type : string;
    ocaml_ocamlbin : string;
  }

  type config_output = {
    mutable cout_ocaml : ocaml_config option;
    mutable cout_ocamlc : string list option;
    mutable cout_ocamldoc : string list option;
    mutable cout_ocamlcc : string list option;
    mutable cout_ocamlopt : string list option;
    mutable cout_ocamldep : string list option;
    mutable cout_ocamlyacc : string list option;
    mutable cout_ocamlmklib : string list option;
    mutable cout_ocamllex : string list option;
    mutable cout_meta_dirnames : string list;
    mutable cout_native_support : bool option;
    mutable cout_byte_support : bool option;
    mutable cout_ocamllib : string option;
    mutable cout_ocamlbin : string option;
  }

end

open TYPES

(*
val arg_list : unit -> (string * Arg.spec * string) list
val load_global_config : File.t -> unit
*)

val check_config : BuildOptions.config_input -> config_output
val set_global_config : config_output -> unit
