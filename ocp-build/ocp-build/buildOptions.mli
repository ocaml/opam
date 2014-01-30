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


(* open BuildBase *)
open SimpleConfig

val shortcut_arg :
  string -> string ->
  (string * Arg.spec * string) list ->
  (string * Arg.spec * string)


type config_input = {
  mutable cin_ocamlc_variants : string list;
  mutable cin_ocamlopt_variants : string list;
  mutable cin_ocamldep_variants : string list;
  mutable cin_ocamllex_variants : string list;
  mutable cin_ocamlyacc_variants : string list;
  mutable cin_ocamlmklib_variants : string list;
  mutable cin_bytecode : bool;
  mutable cin_native : bool;
  mutable cin_ocamlbin : string option;
  mutable cin_ocamllib : string option;
  mutable cin_use_ocamlfind : bool;
  mutable cin_ocps_in_ocamllib : bool;
  mutable cin_meta_dirnames : string list;
  mutable cin_ocps_dirnames : string list;

  mutable cin_color : bool;
  mutable cin_autoscan : bool;
  mutable cin_digest : bool;
  mutable cin_verbosity : int;
  mutable cin_njobs : int;

  mutable cin_install_destdir : string option;
  mutable cin_install_bin : string option;
  mutable cin_install_lib : string option;
  mutable cin_install_doc : string option;
  mutable cin_install_data : string option;
}

val arg_list : unit -> (string * Arg.spec * string) list
val arg_list1 : (string * Arg.spec * string) list

val load : File.t  -> config_input
val maybe_save : unit -> unit

val must_save_project : unit -> unit

module ProjectOptions : sig
  val config_file : SimpleConfig.config_file

  val njobs_option : int option SimpleConfig.config_option
  val verbosity_option : int option SimpleConfig.config_option
  val autoscan_option : bool option SimpleConfig.config_option
  val digest_option : bool option SimpleConfig.config_option
  val bytecode_option : bool option SimpleConfig.config_option
  val native_option : bool option SimpleConfig.config_option

  val install_destdir_option : string option SimpleConfig.config_option
  val install_bindir_option : string option SimpleConfig.config_option
  val install_libdir_option : string option SimpleConfig.config_option
  val install_datadir_option :
    string option SimpleConfig.config_option
  val install_docdir_option : string option SimpleConfig.config_option
  val use_ocamlfind_option : bool SimpleConfig.config_option
  val autoscan_option : bool option SimpleConfig.config_option
  val ocamllib_option : string option SimpleConfig.config_option
  val project_ocpbuild_version : string SimpleConfig.config_option
  val project_external_dirs_option :
    string list SimpleConfig.config_option
  val root_files : File.t list SimpleConfig.config_option

end

module UserOptions : sig
  val default_filename : string
  val config_file : SimpleConfig.config_file
  val njobs_option : int SimpleConfig.config_option
  val verbosity_option : int SimpleConfig.config_option
  val autoscan_option : bool SimpleConfig.config_option
  val color_option : bool SimpleConfig.config_option
  val digest_option : bool SimpleConfig.config_option
  val bytecode_option : bool SimpleConfig.config_option
  val native_option : bool SimpleConfig.config_option
end

val load_config : SimpleConfig.config_file -> File.t -> unit
val apply_arguments : unit -> unit
val save_config : SimpleConfig.config_file -> unit

val arg_set_int :
  int SimpleConfig.config_option -> string * Arg.spec * string
val arg_set_int_option :
  int option SimpleConfig.config_option -> string * Arg.spec * string

val arg_set_string :
  string SimpleConfig.config_option -> string * Arg.spec * string
val arg_set_string_option :
  string option SimpleConfig.config_option -> string * Arg.spec * string

val arg_set_true :
  bool SimpleConfig.config_option -> string * Arg.spec * string
val arg_set_false :
  bool SimpleConfig.config_option -> string * Arg.spec * string

val arg_set_true_option :
  bool option SimpleConfig.config_option -> string * Arg.spec * string
val arg_set_false_option :
  bool option SimpleConfig.config_option -> string * Arg.spec * string

val find_project_root : unit -> File.t
val project_build_dirname : string
val project_config_basename : string

val merge :
   (string * Arg.spec * string) list list ->
   (string * Arg.spec * string) list
