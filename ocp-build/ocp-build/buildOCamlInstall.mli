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

type install_where = {
  install_destdir : string option;

  install_libdirs : string list;
  install_bindir : string;
  install_datadir : string option;

  install_ocamlfind : string list;
  install_ocamllib : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}

type uninstall_state

val install :
  install_where ->
  install_what ->
  BuildTypes.package_info -> string -> unit

val find_installdir :
  install_where ->
  install_what ->
  string -> string option

val uninstall_init : install_where -> uninstall_state
val uninstall_finish : uninstall_state -> unit
val uninstall_by_name : uninstall_state ->  string -> unit
val uninstall : uninstall_state -> BuildTypes.package_info -> unit

type package_uninstaller = {
  mutable un_nfiles : int;
  mutable un_ndirs : int;
  mutable un_version : string;
  mutable un_name : string;
  mutable un_descr : string;
  mutable un_warning : string option;
  mutable un_directory : string;
  mutable un_type : string;
  mutable un_packages : string list;
}

val is_installed : install_where -> string -> bool
val list_installed : install_where -> package_uninstaller list
