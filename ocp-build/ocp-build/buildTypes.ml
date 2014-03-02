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

(* open OcpLang *)

(* open BuildBase *)
(* open Stdlib2 *)
open BuildEngineTypes
open BuildOCPTree
open BuildOCPTypes

module StringsMap = Map.Make(struct
  type t = string list
  let compare = compare
end)

type mklib_kind =
    MKLIB_Unix
  | MKLIB_Msvc

type target_kind =
| CMI
| CMO
| CMX
| CMXS
| CMA
| CMXA
| CMXA_A
| C_A
| RUN_BYTE
| RUN_ASM

type module_origin =
    ML | MLI | MLandMLI

and package_info = {
  lib_context : BuildEngineTypes.build_context;
  lib_id : int;
  lib_name : string;

  mutable lib_version : string;
  mutable lib_dirname : File.t;
  mutable lib_provides : string;
  mutable lib_type : package_type;
  mutable lib_tag : string;
  mutable lib_meta : bool;
  (* true means that it should be ignored about objects *)
  lib_filename : string;
  lib_source_kind : string;

  lib_node : LinearToposort.node;

  mutable lib_requires : package_info package_dependency list;
  mutable lib_added : bool;
  mutable lib_options : BuildOCPVariable.env;

  lib_loc : string * int * string;
  lib_src_dir : build_directory;
  lib_dst_dir : build_directory;
  lib_mut_dir : build_directory;
  lib_modules : (module_origin * string) StringMap.t ref;
  mutable lib_internal_modules :
    (build_directory *
    ((module_origin * string) StringMap.t ref)) StringsMap.t;
  mutable lib_byte_targets : (build_file * target_kind) list;
  lib_build_targets : build_file list ref;
  lib_doc_targets : build_file list ref;
  lib_test_targets : build_file list ref;
  mutable lib_cmo_objects : build_file list;
  mutable lib_bytecomp_deps : build_file list;
  mutable lib_bytelink_deps : build_file list;
  mutable lib_asm_targets : (build_file * target_kind) list;
  mutable lib_asm_cmx_objects : build_file list; (* .cmx *)
  mutable lib_asm_cmxo_objects : build_file list; (* .o *)
  mutable lib_asmcomp_deps : build_file list;
  mutable lib_asmlink_deps : build_file list;
  mutable lib_clink_deps : build_file list;
  mutable lib_dep_deps : build_file IntMap.t;
  mutable lib_includes : string list option;
  mutable lib_sources : BuildOCPVariable.plist;
  mutable lib_tests : BuildOCPVariable.plist;
  mutable lib_archive : string;
  mutable lib_stubarchive : string;

  mutable lib_ready : build_file list;
  mutable lib_installed : bool;
  mutable lib_install : bool;

  (* [lib_bundles] specifies the other packages that should be
     uninstalled when this package is uninstalled.

     The field is initialized when installing packages, from the
      "bundle" variables of packages being installed.
  *)
  mutable lib_bundles : package_info list;
  mutable lib_builder_context : builder_context;
}

and builder_context = {
  build_context : build_context;
  mutable packages_by_name : package_info StringMap.t;
  all_projects : (int, package_info) Hashtbl.t;
  config_filename_validated_table : (string, build_file) Hashtbl.t;
  uniq_rules : (string, build_rule) Hashtbl.t;
}
