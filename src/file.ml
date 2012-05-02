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
  val of_string : Filename.t -> Raw.t -> t

  (** Return the raw content of a file as a string *)
  val to_string : Filename.t -> t -> Raw.t

end

module Lines : IO_FILE with type t = string list list = struct

  let kind = "lines"

  (* Lines of space separated words *)
  type t = string list list

  let empty = []

  let of_string _ raw =
    let lexbuf = Lexing.from_string (Raw.to_string raw) in
    Linelexer.main lexbuf

  let to_string _ lines =
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
    
  let of_string f s =
    let lines = Lines.of_string f s in
    let map = ref empty in
    let add n v = map := N.Map.add n v !map in
    List.iter (function
      | [name; version] -> add (N.of_string name) (V.of_string version)
      | _               -> Globals.error_and_exit "installed"
    ) lines;
    !map

  let to_string _ t =
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

  include IO_FILE with type t = File_format.file

  val check: t -> string list -> unit

end = struct

  let kind = "syntax"

  type t = File_format.file

  let empty = File_format.empty

  let of_string f str =
    let lexbuf = Lexing.from_string (Raw.to_string str) in
    Parser.main Lexer.token lexbuf (Filename.to_string f)

  let to_string _ t =
    Raw.of_string (File_format.string_of_file t)

  let check f fields =
    if not (File_format.is_valid f.contents fields) then
      Globals.error_and_exit "The following file contains fields not in {%s}:\n%s"
        (String.concat "," fields)
        f.filename
end

module Config : sig

  include IO_FILE

  type repository = {
    path: string;
    kind: string;
  }

  (** destruct *)
  val opam_version : t -> V.t
  val repositories : t -> repository list
  val ocaml_version : t -> V.t

  (** construct *)
  val create : V.t  (* opam *) -> repository list -> V.t (* ocaml *) -> t

end = struct

    let kind = "config"

    type repository = {
      path: string;
      kind: string;
    }

    let to_repo (path, kind) =
      let kind = match kind with
        | None   -> Globals.default_repository_kind
        | Some k -> k in
      { path; kind }

    let of_repo r =
      Option (String r.path, [ String r.kind ])

    type t = {
      opam_version  : V.t ; (* opam version *)
      repositories  : repository list ;
      ocaml_version : V.t ;
    }

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let ocaml_version t = t.ocaml_version

    let create opam_version repositories ocaml_version =
      { opam_version ; repositories ; ocaml_version }

    let empty = {
      opam_version = V.of_string Globals.opam_version;
      repositories = [ { 
        path = Globals.default_repository;
        kind = Globals.default_repository_kind;
      } ];
      ocaml_version = V.of_string Sys.ocaml_version;
    }

    open File_format

    let s_opam_version = "opam-version"
    let s_repositories = "repositories"
    let s_ocaml_version = "ocaml_version"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_ocaml_version;
    ]

    let of_string filename f =
      let s = Syntax.of_string filename f in
      Syntax.check s valid_fields;
      let opam_version =
        assoc s.contents s_opam_version (parse_string |> V.of_string) in
      let repositories =
        assoc s.contents s_repositories (parse_list (parse_string_option |> to_repo)) in
      let ocaml_version =
        assoc s.contents s_ocaml_version (parse_string |> V.of_string) in
      { opam_version; repositories; ocaml_version }

   let to_string filename t =
     let s = {
       filename = Filename.to_string filename;
       contents = [ 
         Variable (s_opam_version , String (V.to_string t.opam_version));
         Variable (s_repositories , make_list of_repo t.repositories);
         Variable (s_ocaml_version, String (V.to_string t.ocaml_version));
       ] 
     } in
     Syntax.to_string filename s
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
    name       = N.of_string "<none>";
    version    = V.of_string "<none>";
    maintainer = "<none>";
    substs     = [];
    build      = [];
    depends    = [];
    conflicts  = [];
    libraries  = [];
    syntax     = [];
  }

  let s_opam_version = "opam-version"

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
  let maintainer t = t.maintainer
  let version t = t.version
  let substs t = t.substs
  let build t = t.build
  let depends t = t.depends
  let conflicts t = t.conflicts
  let libraries t = t.libraries
  let syntax t = t.syntax
    
  module D = Debian.Packages

  let default_package t =
    { D.default_package with 
      D.name      = N.to_string t.name ;
      D.version   = V.to_string t.version ;
      D.depends   = t.depends ;
      D.conflicts = t.conflicts }

  let to_package t installed =
    let p = default_package t in
    if installed then 
      { p with D.extras = (s_status, s_installed) :: p.D.extras }
    else
      p

  let to_string filename t =
    let s = {
      filename = Filename.to_string filename;
      contents = [
        Variable ("opam-version", String Globals.opam_version);
        Section {
          File_format.kind = "package";
          name = N.to_string t.name;
          items = [
            Variable (s_version, String (V.to_string t.version));
            Variable (s_maintainer, String t.maintainer);
            Variable (s_substs, make_list (Filename.to_string |> make_string) t.substs);
            Variable (s_build, make_list (make_list make_string) t.build);
            Variable (s_depends, make_or_formula t.depends);
            Variable (s_conflicts, make_and_formula t.conflicts);
            Variable (s_libraries, make_list make_string t.libraries);
            Variable (s_syntax, make_list make_string t.syntax);
          ]
        }
      ] 
    } in
    Syntax.to_string filename s

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let opam_version = assoc s.contents s_opam_version parse_string in
    if opam_version <> Globals.opam_version then
      Globals.error_and_exit "%s is not a supported OPAM version" opam_version;
    let package = get_section_by_kind s.contents "package" in
    let name = N.of_string package.File_format.name in
    let s = package.items in
    let version    = assoc s s_version (parse_string |> V.of_string) in
    let maintainer = assoc s s_maintainer parse_string in
    let substs     = 
      assoc_list s s_substs (parse_list (parse_string |> Filename.of_string)) in
    let build      = assoc_list s s_build (parse_list (parse_list parse_string)) in
    let depends    = assoc_list s s_depends parse_or_formula in
    let conflicts  = assoc_list s s_conflicts parse_and_formula in
    let libraries  = assoc_list s s_libraries parse_string_list in
    let syntax     = assoc_list s s_syntax parse_string_list in
    { name; version; maintainer; substs; build;
      depends; conflicts; libraries; syntax }
end

module Install : sig

  include IO_FILE

  type move = {
    src: filename;
    dst: filename;
    dst_kind: [`absolute|`relative];
  }

  (** destruct *)
  val lib:  t -> filename list
  val bin:  t -> move list
  val misc: t -> move list

  val string_of_move : move -> string

end = struct

  let kind = "install"
    
  type move = {
    src: filename;
    dst: filename;
    dst_kind: [`absolute|`relative];
  }

  type t =  {
    lib : Filename.t list ;
    bin : move list ;
    misc: move list ;
  }

  let lib t = t.lib
  let bin t = t.bin
  let misc t = t.misc

  let string_of_move m =
    let src = Filename.to_string m.src in
    let dst = match m.dst_kind with
      | `absolute -> Filename.to_string m.dst
      | `relative -> Printf.sprintf "R:%s" (Filename.to_string m.dst) in
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

  let to_string filename t =
    let string f = String (Filename.to_string f) in
    let make_move m =
      if m.src = m.dst then
        string m.src
      else
        Option (string m.src, [string m.dst]) in
    let s = {
      filename = Filename.to_string filename;
      contents = [
        Variable (s_lib , make_list (Filename.to_string |> make_string) t.lib);
        Variable (s_bin , make_list make_move t.bin);
        Variable (s_misc, make_list make_move t.misc);
      ]
    } in
    Syntax.to_string filename s

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let parse_move dst_kind v =
      match parse_string_option v with
      | s, None ->
          let f = Filename.of_string s in
          { src = f; dst = f; dst_kind = `relative }
      | src, Some dst ->
          { src = Filename.of_string src;
            dst = Filename.of_string dst;
            dst_kind 
          } in
    let lib =
      assoc_list s.contents s_lib (parse_list (parse_string |> Filename.of_string)) in
    let bin =
      assoc_list s.contents s_bin (parse_list (parse_move `relative)) in
    let misc =
      assoc_list s.contents s_misc (parse_list (parse_move `absolute)) in
    { lib; bin; misc }

end

(* Package config X.config *)
module PConfig : sig
  
  include IO_FILE

  type value =
    | B of bool
    | S of string

  module Variable : Abstract

  module Section  : Abstract

  module type SECTION = sig
    val asmcomp : t -> Section.t -> string list
    val bytecomp: t -> Section.t -> string list
    val asmlink : t -> Section.t -> string list
    val bytelink: t -> Section.t -> string list
    val variable: t -> Section.t -> Variable.t  -> value
  end

  module Library : SECTION

  module Syntax  : SECTION

  val variables: t -> Variable.t  -> value

end = struct

  let kind = ".config"

  type value =
    | B of bool
    | S of string

  let s str = S str
  let b bool = B bool

  module Variable : Abstract = Base

  module Section  : Abstract = Base

  type section = { 
    name      : Section.t ;
    bytecomp  : string list ;
    asmcomp   : string list ;
    bytelink  : string list ;
    asmlink   : string list ; 
    lvariables: (Variable.t * value) list;
  }

  type t = {
    libraries: section list;
    syntax   : section list;
    variables: (Variable.t * value) list;
  }

  let empty = {
    libraries = [];
    syntax    = [];
    variables = [];
  }

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

  let of_string filename str =
    let file = Syntax.of_string filename str in
    let parse_value = parse_or [
      (parse_string |> s);
      (parse_bool   |> b);
    ] in
    let parse_section s =
      let name = Section.of_string s.File_format.name in
      let bytecomp = assoc_string_list s.items s_bytecomp in
      let asmcomp  = assoc_string_list s.items s_asmcomp  in
      let bytelink = assoc_string_list s.items s_bytecomp in
      let asmlink  = assoc_string_list s.items s_asmlink  in
      let lvariables =
        List.filter (fun (x,_) -> not (List.mem x valid_fields)) (variables s.items) in
      let lvariables =
        List.map (fun (k,v) -> Variable.of_string k, parse_value v) lvariables in
      { name; bytecomp; asmcomp; bytelink; asmlink; lvariables } in
    let libraries = assoc_sections file.contents "library" parse_section in
    let syntax    = assoc_sections file.contents "syntax" parse_section in
    let variables =
      List.map
        (fun (k,v) -> Variable.of_string k, parse_value v)
        (variables file.contents) in
    { libraries; syntax; variables }

  let rec to_string filename t =
    let of_value = function
      | B b -> Bool b
      | S s -> String s in
    let of_variables l =
      List.map (fun (k,v) -> Variable (Variable.to_string k, of_value v)) l in
    let of_section kind s =
      Section
        { File_format.kind;
          name = Section.to_string s.name;
          items = [
            Variable (s_bytecomp, make_list make_string s.bytecomp);
            Variable (s_asmcomp , make_list make_string s.asmcomp);
            Variable (s_bytelink, make_list make_string s.bytelink);
            Variable (s_asmlink , make_list make_string s.asmlink);
          ] @ of_variables s.lvariables 
        } in
    let of_library l = of_section "library" l in
    let of_syntax s = of_section "syntax" s in
    Syntax.to_string filename {
      filename = Filename.to_string filename;
      contents =
        of_variables t.variables
        @ List.map of_library t.libraries
        @ List.map of_syntax t.syntax
    }

  let variables t s = List.assoc s t.variables

  module type SECTION = sig
    val asmcomp : t -> Section.t -> string list
    val bytecomp: t -> Section.t -> string list
    val asmlink : t -> Section.t -> string list
    val bytelink: t -> Section.t -> string list
    val variable: t -> Section.t -> Variable.t  -> value
  end

  module Make (M : sig val get : t -> section list end) : SECTION = struct

    let find t name =
      List.find (fun s -> s.name = name) (M.get t)

    let bytecomp t s = (find t s).bytecomp
    let asmcomp  t s = (find t s).asmcomp
    let bytelink t s = (find t s).bytelink
    let asmlink  t s = (find t s).asmlink
    let variable t n s = List.assoc s (find t n).lvariables
  end

  module Library = Make (struct let get t = t.libraries end)
  module Syntax  = Make (struct let get t = t.syntax    end)
end

module Make (F : IO_FILE) = struct
  let log = Globals.log ("FILE." ^ F.kind)

  (** Write some contents to a file *)
  let write f v =
    log "write %s" (Filename.to_string f);
    Filename.write f (F.to_string f v)

  (** Read file contents *)
  let read f =
    let filename = Filename.to_string f in
    log "read_opt %s" filename;
    if Filename.exists f then
      F.of_string f (Filename.read f)
    else
      Globals.error_and_exit "File %s does not exit" (Filename.to_string f)
end


module File = struct

  module Config = struct include Config include Make (Config) end
  module OPAM = struct include OPAM include Make (OPAM) end
  module Install = struct include Install include Make (Install) end
  module PConfig = struct include PConfig include Make (PConfig) end
  module Installed = struct include Installed include Make (Installed) end
end
