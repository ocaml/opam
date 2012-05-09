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
open Utils
open File_format

exception Parsing of string

module Lines = struct

  let internal = "lines"

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


module Syntax = struct

  let internal = "syntax"

  type t = File_format.file

  let empty = File_format.empty

  let of_string f str =
    let lexbuf = Lexing.from_string (Raw.to_string str) in
    Parser.main Lexer.token lexbuf (Filename.to_string f)

  let to_string _ t =
    Raw.of_string (File_format.string_of_file t)

  let check f fields =
    if not (File_format.is_valid f.contents fields) then
      Globals.error_and_exit "{ %s } are invalid field names in %s. Valid fields are { %s }"
        (String.concat ", " (invalid_fields f.contents fields))
        f.filename
        (String.concat ", " fields)
end

module X = struct

module Installed = struct

  let internal = "installed"

  type t = NV.Set.t

  let empty = NV.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let map = ref empty in
    let add n v = map := NV.Set.add (NV.create n v) !map in
    List.iter (function
      | [name; version] -> add (N.of_string name) (V.of_string version)
      | _               -> Globals.error_and_exit "installed"
    ) lines;
    !map

  let to_string _ t =
    let buf = Buffer.create 1024 in
    NV.Set.iter
      (fun nv -> Printf.bprintf buf "%s %s\n" (N.to_string (NV.name nv)) (V.to_string (NV.version nv)))
      t;
    Raw.of_string (Buffer.contents buf)

end

module Reinstall = struct

  include Installed

  let internal = "reinstall"

end

module Updated = struct

  include Installed

  let internal = "updated"

end

module Repo_index = struct
    
  let internal = "repo-index"

  type t = string N.Map.t

  let empty = N.Map.empty

  let of_string filename str =
    let lines = Lines.of_string filename str in
    List.fold_left (fun map -> function
      | [name_s; repo_s] ->
          let name = N.of_string name_s in
          if N.Map.mem name map then
            Globals.error_and_exit "multiple lines for package %s" name_s
          else
            N.Map.add name repo_s map
      | x ->
          Globals.error_and_exit "'%s' is not a valid repository index line" (String.concat " " x)
    ) N.Map.empty lines

  let to_string filename map =
    let lines = N.Map.fold (fun name repo lines ->
      [ N.to_string name; repo] :: lines
    ) map [] in
    Lines.to_string filename (List.rev lines)

end

module Repo_config = struct

  let internal = "repo-config"

  type t = repository

  let empty =
    Repository.create ~name:"<none>" ~address:"<none>" ~kind:"<none>"

  let s_name = "name"
  let s_kind = "kind"
  let s_address = "address"

  let of_string filename str =
    let s = Syntax.of_string filename str in
    let name = assoc s.contents s_name parse_string in
    let address = assoc s.contents s_address parse_string in
    let kind = assoc s.contents s_kind parse_string in
    Repository.create ~name ~address ~kind

  let to_string filename t =
    let s = {
      filename = Filename.to_string filename;
      contents = [
        Variable (s_name   , String (Repository.name t));
        Variable (s_address, String (Repository.address t));
        Variable (s_kind   , String (Repository.kind t));
      ] } in
    Syntax.to_string filename s

end

module Descr = struct

  let internal = "descr"

  type t = Lines.t

  let empty = []

  let synopsis = function
    | []   -> ""
    | h::_ -> String.concat " " h

  let full l =
    let one l = String.concat " " l in
    String.concat "\n" (List.map one l)

  let of_string = Lines.of_string

  let to_string = Lines.to_string

end

module Config = struct

    let internal = "config"

    let to_repo (name, option) =
      let address, kind = match option with
        | Some (address, kind) -> address, kind
        | None                 -> 
            Globals.default_repository_kind,
            Globals.default_repository_address in
      Repository.create ~name ~address ~kind

    let of_repo r =
      Option (String (Repository.name r),
              [ String (Repository.address r);
                String (Repository.kind r) ])

    type t = {
      opam_version  : OPAM_V.t ;
      repositories  : repository list ;
      ocaml_version : OCaml_V.t ;
    }

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let ocaml_version t = t.ocaml_version

    let create opam_version repositories ocaml_version =
      { opam_version ; repositories ; ocaml_version }

    let empty = {
      opam_version = OPAM_V.of_string Globals.opam_version;
      repositories = [];
      ocaml_version = OCaml_V.of_string Sys.ocaml_version;
    }

    open File_format

    let s_opam_version = "opam-version"
    let s_repositories = "repositories"
    let s_ocaml_version = "ocaml-version"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_ocaml_version;
    ]

    let of_string filename f =
      let s = Syntax.of_string filename f in
      Syntax.check s valid_fields;
      let opam_version =
        assoc s.contents s_opam_version (parse_string |> OPAM_V.of_string) in
      let repositories =
        assoc s.contents s_repositories
          (parse_list (parse_string_option parse_string_pair |> to_repo)) in
      let ocaml_version =
        assoc s.contents s_ocaml_version (parse_string |> OCaml_V.of_string) in
      { opam_version; repositories; ocaml_version }

   let to_string filename t =
     let s = {
       filename = Filename.to_string filename;
       contents = [ 
         Variable (s_opam_version , String (OPAM_V.to_string t.opam_version));
         Variable (s_repositories , make_list of_repo t.repositories);
         Variable (s_ocaml_version, String (OCaml_V.to_string t.ocaml_version));
       ] 
     } in
     Syntax.to_string filename s
end

module OPAM = struct

  let internal = "opam"

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
    s_opam_version;
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

  let to_package t ~installed =
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
    let build      =
      assoc_default Globals.default_build_command
        s s_build (parse_list (parse_list parse_string)) in
    let depends    = assoc_list s s_depends parse_or_formula in
    let conflicts  = assoc_list s s_conflicts parse_and_formula in
    let libraries  = assoc_list s s_libraries parse_string_list in
    let syntax     = assoc_list s s_syntax parse_string_list in
    { name; version; maintainer; substs; build;
      depends; conflicts; libraries; syntax }
end

module Dot_install = struct

  let internal = "install"
    
  type t =  {
    lib : filename list ;
    bin : (filename * filename) list ;
    misc: (filename * filename) list ;
  }

  let string_of_move (src, dst) =
    let src = Filename.to_string src in
    let dst = Filename.to_string dst in
    Printf.sprintf "%s => %s" src dst

  let lib t = t.lib
  let bin t = t.bin
  let misc t = t.misc

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
    let make_move (src, dst) =
      if src = dst then
        string src
      else
        Option (string src, [string dst]) in
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
    let parse_move v =
      match parse_string_option parse_single_string v with
      | s  , None     -> let f = Filename.of_string s in (f, f)
      | src, Some dst -> (Filename.of_string src, Filename.of_string dst) in
    let lib = assoc_list s.contents s_lib (parse_list (parse_string |> Filename.of_string)) in
    let bin = assoc_list s.contents s_bin (parse_list parse_move) in
    let misc = assoc_list s.contents s_misc (parse_list parse_move) in
    { lib; bin; misc }

end

module Dot_config = struct

  let internal = ".config"

  type value =
    | B of bool
    | S of string

  let s str = S str
  let b bool = B bool

  type section = { 
    name      : string ;
    includes  : string list ;
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

  let s_includes = "includes"
  let s_bytecomp = "bytecomp"
  let s_asmcomp  = "asmcomp"
  let s_bytelink = "bytelink"
  let s_asmlink  = "asmlink"

  let valid_fields = [
    s_includes;
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
      let name = s.File_format.name in
      let includes = assoc_string_list s.items s_includes in
      let bytecomp = assoc_string_list s.items s_bytecomp in
      let asmcomp  = assoc_string_list s.items s_asmcomp  in
      let bytelink = assoc_string_list s.items s_bytecomp in
      let asmlink  = assoc_string_list s.items s_asmlink  in
      let lvariables =
        List.filter (fun (x,_) -> not (List.mem x valid_fields)) (variables s.items) in
      let lvariables =
        List.map (fun (k,v) -> Variable.of_string k, parse_value v) lvariables in
      { name; includes; bytecomp; asmcomp; bytelink; asmlink; lvariables } in
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
          name = s.name;
          items = [
            Variable (s_includes, make_list make_string s.includes);
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
    val includes: t -> string -> string list
    val asmcomp : t -> string -> string list
    val bytecomp: t -> string -> string list
    val asmlink : t -> string -> string list
    val bytelink: t -> string -> string list
    val variable: t -> string -> Variable.t  -> value
  end

  module Section (M : sig val get : t -> section list end) : SECTION = struct

    let find t name =
      List.find (fun s -> s.name = name) (M.get t)

    let includes t s = (find t s).includes
    let bytecomp t s = (find t s).bytecomp
    let asmcomp  t s = (find t s).asmcomp
    let bytelink t s = (find t s).bytelink
    let asmlink  t s = (find t s).asmlink
    let variable t n s = List.assoc s (find t n).lvariables
  end

  module Library = Section (struct let get t = t.libraries end)
  module Syntax  = Section (struct let get t = t.syntax    end)
end

end

module type F = sig
  val internal : string
  type t
  val empty : t
  val of_string : Filename.t -> Raw.t -> t
  val to_string : Filename.t -> t -> Raw.t
end

module Make (F : F) = struct

  let log = Globals.log ("FILE." ^ F.internal)

  let write f v =
    log "write %s" (Filename.to_string f);
    Filename.write f (F.to_string f v)

  let read f =
    let filename = Filename.to_string f in
    log "read %s" filename;
    if Filename.exists f then
      F.of_string f (Filename.read f)
    else
      Globals.error_and_exit "File %s does not exit" (Filename.to_string f)

  let safe_read f =
    let filename = Filename.to_string f in
    log "read %s" filename;
    if Filename.exists f then
      F.of_string f (Filename.read f)
    else
      F.empty

end

open X

module type IO_FILE = sig
  type t
  val empty: t
  val write: filename -> t -> unit
  val read : filename -> t
  val safe_read: filename -> t
end

module Config = struct
  include Config
  include Make (Config)
end

module Repo_index = struct
  include Repo_index
  include Make (Repo_index)
end

module Repo_config = struct
  include Repo_config
  include Make (Repo_config)
end

module Descr = struct
  include Descr
  include Make (Descr)
end

module Reinstall = struct
  include Reinstall
  include Make (Reinstall)
end

module OPAM = struct
  include OPAM
  include Make (OPAM)
end

module Dot_install = struct
  include Dot_install
  include Make (Dot_install)
end

module Dot_config = struct
  include Dot_config
  include Make (Dot_config)
end

module Installed = struct
  include Installed
  include Make (Installed)
end

module Updated = struct
  include Updated
  include Make (Updated)
end
