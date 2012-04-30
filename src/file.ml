(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Types
open Path
open File_format

exception Parsing of string

module type IO_FILE = sig

  (** kind of IO file, for debugging purposes *)
  val kind : string

  (** File contents *)
  type t

  (** An empty file *)
  val empty : t

  (** Parse a string or raise the exception [Parsing _]. *)
  val of_string : Raw.t -> t

  (** Return the raw content of a file as a string *)
  val to_string : t -> Raw.t

end

module Lines : IO_FILE with type t = string list list = struct

  let kind = "lines"

  (* Lines of space separated words *)
  type t = string list list

  let empty = []

  let of_string raw =
    let lexbuf = Lexing.from_string (Raw.to_string raw) in
    Linelexer.main lexbuf

  let to_string lines =
    let buf = Buffer.create 1024 in
    List.iter (fun l ->
      Buffer.add_string buf (String.concat " " l);
      Buffer.add_string buf "\n"
    ) lines;
  Raw.of_string (Buffer.contents buf)

end

module Installed : sig
  include IO_FILE with type t = V.t N.Map.t
end = struct

  let kind = "installed"

  type t = V.t N.Map.t

  let empty = N.Map.empty
    
  let of_string s =
    let lines = Lines.of_string s in
    let map = ref empty in
    let add n v = map := N.Map.add n v !map in
    List.iter (function
      | [name; version] -> add (N.of_string name) (V.of_string version)
      | _               -> Globals.error_and_exit "installed"
    ) lines;
    !map

  let to_string t =
    let buf = Buffer.create 1024 in
    N.Map.iter
      (fun n v -> Printf.bprintf buf "%s %s\n" (N.to_string n) (V.to_string v))
      t;
    Raw.of_string (Buffer.contents buf)

end

module Cudf = struct
  include Cudf
  let find_field x key = 
    try Some (List.assoc key x.Cudf.pkg_extra) with Not_found -> None
end

module Syntax : sig
  include IO_FILE

  val check: t -> string list -> unit

end = struct

  type t = File_format.file

  let to_string str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

  let of_string =
    File_format.string_of_file

  let check f fields =
    if not (File_format.is_valid f fields) then
      Globals.error_and_exit "The following file contains fields not in {%s}:\n%s"
        (String.concat "," fields)
        (Filename.to_string f)
end

module Config : sig

  include IO_FILE

  type source = {
    path: string;
    kind: string;
  }

  (** destruct *)
  val opam_version : t -> V.t
  val sources : t -> source list
  val ocaml_version : t -> V.t

  (** construct *)
  val create : V.t  (* opam *) -> source list -> V.t (* ocaml *) -> t

end = struct

    let kind = "config"

    type repository = {
      path: string;
      kind: string;
    }

    type t =
        { opam_version : V.t (* opam version *)
        ; repositories : repository list
        ; ocaml_version : V.t }

    let opam_version t = t.opam_version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version

    let create opam_version sources ocaml_version =
      { opam_version ; sources ; ocaml_version }

    let empty = {
      version = Globals.api_version;
      sources = [ { 
        path = Globals.default_hostname;
        kind = Globals.default_repository_kind;
      } ];
      ocaml_version = Version "empty version" 
    }

    open File_format

    let parse_repository = parse_option parse_string (parse_singleton parse_string)

    let s_opam_version = "opam-version"
    let s_repositories = "repositories"
    let s_ocaml_version = "ocaml_version"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_ocaml_version;
    ]

    let of_string f =
      let s = Syntax.of_string f in
      Syntax.check s valid_fields;
      let opam_version = assoc s s_opam_version parse_string in
      let repositories = assoc s s_repositories parse_repository in
      let ocaml_version = assoc s_ocaml_version parse_string in
      { opam_version; repositories; ocaml_version }

   let to_string t =
     let s = [ 
       Variable (s_opam_version, t.opam_version);
       Variable (s_repositories, t.repositories);
       Variable (s_ocaml_version, t.ocaml_version);
     ] in
     Syntax.to_string s
end

module OPAM : sig 

  include IO_FILE

  (** destruct *)
  val name        : t -> N.t
  val version     : t -> V.t
  val maintainer  : t -> string
  val substs      : t -> filename list
  val build       : t -> string list list
  val depends     : t -> Debian.Format822.vpkgformula
  val conflicts   : t -> Debian.Format822.vpkglist
  val libraries   : t -> string list
  val syntax      : t -> string list

  (** Convert to Debian packages to feed the solver *)
  val to_package : t -> bool (* true : installed *) -> Debian.Packages.package

end = struct

  let kind = "opam"

  type t = {
    name       : N.t;
    version    : V.t;
    maintainer : string;
    substs     : filename list;
    build      : string list list;
    depends    : Debian.Format822.vpkgformula;
    conflicts  : Debian.Format822.vpkglist;
    libraries  : string list;
    syntax     : string list;
  }

  let empty = {
    name       = "<none>";
    version    = "<none>";
    maintainer = "<none>";
    substs     = [];
    build      = [];
    depends    = [];
    conflicts  = [];
    libraries  = [];
    syntax     = [];
  }

  let s_version     = "version"
  let s_maintainer  = "maintainer"
  let s_substs      = "substs"
  let s_build       = "build"
  let s_depends     = "depends"
  let s_conflicts   = "conflicts"
  let s_libraries   = "libraries"
  let s_syntax      = "syntax"
    
  (* to convert to cudf *)
  (* see [Debcudf.add_inst] for more details about the format *)
  let s_status = "status" 
    
  (* see [Debcudf.add_inst] for more details about the format *)
  let s_installed   = "  installed" 

  let valid_fields = [
    s_version;
    s_maintainer;
    s_substs;
    s_build;
    s_depends;
    s_conflicts;
    s_libraries;
    s_syntax;
  ]

  let name t = t.name
  let version t = t.version
  let substs t = t.substs
  let build t = t.build
  let depends t = t.depends
  let conflicts t = t.conflicts
  let libraries t = t.libraries
  let syntax t = real_path t.patches
    
  module D = Debian.Package

  let default_package t =
    { D.default_package with 
      D.name      = t.name ;
      D.version   = t.version ;
      D.depends   = t.depends ;
      D.conflicts = t.conflicts }

  let to_package t installed =
    let p = default_package t in
    if installed then 
      { p with D.extras = (s_status, s_installed) :: p.D.extras }
    else
      p

  let to_string t =
    let s = [
      Variable ("opam-version", Globals.opam_version);
      Section {
        kind = "package";
        name = N.to_string t.name;
        items = [
          Variable (s_version, String (V.to_string t.version));
          Variable (s_maintainer, t.maintainer);
          Variable (s_substs, make_list (make_string |> Filename.to_string) t.substs);
          Variable (s_build, make_list (make_list make_string) t.build);
          Variable (s_depends, make_or_formula t.depends);
          Variable (s_conflicts, make_and_formula t.conflicts);
          Variable (s_libraries, make_list make_string t.libraries);
          Variable (s_syntax, make_list make_string t.syntax);
        ]
      }
    ] in
    Syntax.to_string s

  let of_string str =
    let s = Syntax.of_line str in
    Syntax.check s valid_fields;
    let opam_version = assoc s s_opam_version parse_string in
    if opam_version <> Globals.opam_version then
      Globals.error_and_exit "%s is not a supported OPAM version" opam_version;
    match assoc_section s "package" with
    | [ name, s ] ->
        let s = s.items in
        let version    = assoc s s_version (parse_string |> V.of_string) in
        let maintainer = assoc s s_maintainer parse_string in
        let substs     = assoc_list s s_substs (parse_string |> Filename.of_string) in
        let build      = assoc_list s s_build (parse_list parse_string) in
        let depends    = assoc_list s s_depends parse_or_formula in
        let conflicts  = assoc_list s s_conflicts parse_and_formula in
        let libraries  = assoc_list s s_libraries parse_string in
        let syntax     = assoc s s s_libraries parse_string in
        { name; version; maintainer; substs; build;
          depends; conflicts; libraries; syntax }
end

module Install : sig

  include IO_FILE

  type move = {
    src: filename;
    dst: [`absolute|`relative] * filename;
  }

  (** destruct *)
  val lib:  t -> filename list
  val bin:  t -> move list
  val misc: t -> move list

  val string_of_move : move -> string

end = struct

  let kind = "to_install"
    
  type move = {
    src: filename;
    dst: [`absolute|`relative] * filename;
  }

  type t =  {
    lib : path list ;
    bin : move list ;
    misc: move list ;
  }

  let lib t = t.lib
  let bin t = t.bin
  let misc t = t.misc

  let string_of_move m =
    let src = Filename.to_string m.src in
    let dst = match src.dst with
      | `absolute, dst -> dst
      | `relative, dst -> Printf.sprintf "R:%s" dst in
    Printf.sprintf "%s => %s" src dst

  let empty = {
    lib  = [] ;
    bin  = [] ;
    misc = [] ;
  }

  let s_lib = "lib"
  let s_bin = "bin"
  let s_misc = "misc"

  let valid_fields = [
    s_lib;
    s_bin;
    s_misc;
  ]

  let to_string t =
    let make_move m =
      if m.src = snd m.dst then
        String m.src
      else
        Option (String m.src, [String m.dst]) in
    let s = [
      Variable (s_lib, make_list make_string t.lib);
      Variable (s_bin, make_list make_move t.bin);
      Variable (s_misc, make_list make_move t.misc);
    ] in
    Filename.to_string s

  let of_string str =
    let s = Syntax.of_string str in
    Syntax.check s valid_fields;
    let parse_move kind = function
      | String s -> { src = s; dst = (`relative, s) }
      | Option (src, [String dst]) -> { src; dst = (kind, dst) } in
    let lib = assoc s s_lib (parse_list parse_string) in
    let bin = assoc s s_bin (parse_list (parse_move `relative)) in
    let misc = assoc s s_bin (parse_list (parse_move `absolute)) in
    { lib; bin; misc }

end

(* Package config X.config *)
module PConfig : sig
  
  include IO_FILE

  module Variable : Abstract

  type value =
    | B of bool
    | S of string

  module type Section = sig
    type section
    val asmcomp : t -> section -> string list
    val bytecomp: t -> section -> string list
    val asmlink : t -> section -> string list
    val bytelink: t -> section -> string list
    val variable: t -> section -> Variable.t  -> value
    include Abstract with type t = section
  end

  module Library : Section
  module Syntax  : Section

  val variable: t -> Variable.t  -> value

end = struct

  let kind = ".config"

  type value =
    | B of bool
    | S of string

  type section = { 
    name     : string ;
    bytecomp : string list ;
    asmcomp  : string list ;
    bytelink : string list ;
    asmlink  : string list ; 
    variables: (string * value) list;
  }

  type item = {
    libraries: section list;
    syntax   : section list;
    variables: (sting * value) list;
  }

  let empty = {
    libraries = [];
    syntax    = [];
    variables = [];
  }

  module Section (M : sig val get : t -> section list end) = struct
    include Base

    let find t s =
      List.assoc s (M.get t)

    let bytecomp t s = (find t s).bytecomp
    let asmcomp  t s = (find t s).asmcomp
    let bytelink t s = (find t s).bytelink
    let asmlink  t s = (find t s).asmlink

    let variable t s = List.assoc s t.variables
  end

  module Library = Section (struct let get t = t.libraries end)
  module Syntax  = Section (struct let get t = t.syntax end)

  let s_bytecomp = "bytecomp"
  let s_asmcomp  = "asmcomp"
  let s_bytelink = "bytelink"
  let s_asmlink  = "asmlink"

  let valid_fields = [
    s_bytecomp;
    s_asmcomp;
    s_bytelink;
    s_asmlink;
  ]

  let of_string str =
    let s = Syntax.of_string str in
    let parse_value = function
      | String s -> S s
      | Bool   b -> B b in
    let parse_section s =
      let name = s.name in
      let bytecomp = assoc_string_list s.items s_bytecomp in
      let asmcomp  = assoc_string_list s.items s_asmcomp  in
      let bytelink = assoc_string_list s.items s_bytecomp in
      let asmlink  = assoc_string_list s.items s_asmlink  in
      let variables =
        List.filter (fun x -> not (List.mem x valid_fields)) (variables s) in
      { name; bytecomp; asmcomp; bytelink; asmlink; variables } in
    let libraries = List.map parse_section (sections s "library") in
    let syntax    = List.map parse_section (sections s "syntax") in
    let variables = variables s parse_value in
    { libraries; syntax; variables }

  let rec to_string t =
    let of_value = function
      | B b -> Bool b
      | S s -> String s in
    let of_variables l =
      List.map (fun (k,v) -> Variable (k, of_value v)) l in
    let of_section kind s =
      Section
        { kind;
          name = s.name;
          items = [
            Variable (s_bytecomp, make_list make_string s.bytecomp);
            Variable (s_asmcomp , make_list make_string s.asmcomp);
            Variable (s_bytelink, make_list make_string s.bytelink);
            Variable (s_asmlink , make_list make_string s.asmlink);
          ] @ of_variables s.variables 
        } in
    let of_library l = of_section "library" l in
    let of_syntax s = of_section "syntax" l in
    of_variables t.variables
    @ List.map of_library t.libraries
    @ List.map of_syntax t.syntax

end

module Make (F : IO_FILE) = struct
  let log = Globals.log ("FILE." ^ F.kind)

  (** Write some contents to a file *)
  let write f v =
    log "write %s" (Filename.to_string f);
    Filename.write f (Raw.of_string (F.to_string v))

  (** Read file contents *)
  let read f =
    let filename = Filename.to_string f in
    log "read_opt %s" filename;
    if Filename.exists f then
      F.of_string (Raw.of_string (Filename.read f))
    else
      Globals.error_and_exit "File %s does not exit" (Filename.to_string f)
end


module File = struct

  module Config = struct include Config include Make (Config) end
  module OPAM = struct include Spec include Make (OPAM) end
  module Install = struct include To_install include Make (Install) end
  module PConfig = struct include PConfig include Make (PConfig) end

  module Installed =
  struct
    module M_installed = Make (Installed)
      
    module Map = struct
      let find f = N_map.of_list (M_installed.Exceptionless.find f)
      let add k v = M_installed.add k (N_map.bindings v)
      let modify_def f f_map = add f (f_map (find f))
    end 

    include Installed
    include M_installed
  end
end

