(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
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
    try
      let lexbuf = Lexing.from_string (Raw.to_string str) in
      let filename = Filename.to_string f in
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
      Parser.main Lexer.token lexbuf filename
    with e ->
      Globals.error "Parsing error while reading %s" (Filename.to_string f);
      raise e

  let to_string ?(indent_variable = fun _ -> false) _ t =
    Raw.of_string (File_format.string_of_file ~indent_variable t)

  let check f fields =
    if not (File_format.is_valid f.contents fields) then
      Globals.error_and_exit "{ %s } are invalid field names in %s. Valid fields are { %s }"
        (String.concat ", " (invalid_fields f.contents fields))
        f.filename
        (String.concat ", " fields)
end

module X = struct

module Filenames = struct

  let internal = "filenames"

  type t = Filename.Set.t

  let empty = Filename.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let lines = Utils.filter_map (function
      | []  -> None
      | [f] -> Some (Filename.of_string f)
      | s   ->
          Globals.error_and_exit "%S is not a valid filename" (String.concat " " s)
    ) lines in
    Filename.Set.of_list lines

  let to_string f s =
    let lines =
      List.map (fun f -> [Filename.to_string f]) (Filename.Set.elements s) in
    Lines.to_string f lines

end

module Urls_txt = struct

  let internal = "urls-txt"

  type t = Remote_file.Set.t

  let empty = Remote_file.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let rs = Utils.filter_map (function
      | [] -> None
      | l  -> Some (Remote_file.of_string (String.concat " " l))
    ) lines in
    Remote_file.Set.of_list rs

  let to_string f t =
    let lines =
      List.map (fun r -> [Remote_file.to_string r]) (Remote_file.Set.elements t) in
    Lines.to_string f lines

end

module URL = struct

  let internal = "url"

  type t = {
    url     : string;
    kind    : string option;
    checksum: string option;
  }

  let empty = {
    url     = "<none>";
    kind    = None;
    checksum= None;
  }

  let s_archive = "archive"
  let s_checksum = "checksum"
  let s_git = "git"

  let valid_fields = [
    s_archive;
    s_checksum;
    s_git;
  ]

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let archive = assoc_option s.contents s_archive parse_string in
    let git = assoc_option s.contents s_git parse_string in
    let checksum = assoc_option s.contents s_checksum parse_string in
    let url, kind = match archive, git with
      | None  , None   -> Globals.error_and_exit "Missing URL"
      | Some x, None   -> x, None
      | None  , Some x -> x, Some "git"
      | _ -> Globals.error_and_exit "Too many URLS" in
    { url; kind; checksum }

  let to_string filename t =
    let url_name = match t.kind with
      | Some "git"   -> "git"
      | Some "curl"
      | Some "rsync"
      | None         -> "archive"
      | Some x -> Globals.error_and_exit "%s is an unknown backend" x in
    let s = {
      filename = Filename.to_string filename;
      contents = [
        Variable (url_name , String t.url);
      ] @ match t.checksum with
        | None   -> []
        | Some c -> [Variable (s_checksum, String c)]
    } in
    Syntax.to_string filename s

  let url t = t.url
  let kind t = t.kind
  let checksum t = t.checksum

  let with_checksum t checksum = { t with checksum = Some checksum }

  let create ?checksum url =
    { url; checksum; kind = None }

end

module Updated = struct

  let internal = "updated"

  type t = NV.Set.t

  let empty = NV.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let map = ref empty in
    let add n v = map := NV.Set.add (NV.create n v) !map in
    List.iter (function
      | []              -> ()
      | [name; version] -> add (N.of_string name) (V.of_string version)
      | _               -> Globals.error_and_exit "[file.ml/module Installed]"
    ) lines;
    !map

  let to_string _ t =
    let buf = Buffer.create 1024 in
    NV.Set.iter
      (fun nv -> Printf.bprintf buf "%s %s\n" (N.to_string (NV.name nv)) (V.to_string (NV.version nv)))
      t;
    Raw.of_string (Buffer.contents buf)

end

module Installed = struct

  include Updated

  let internal = "installed"

  let check t =
    let map = NV.to_map t in
    N.Map.iter (fun n vs ->
      if V.Set.cardinal vs <> 1 then
        Globals.error_and_exit "Multiple versions installed for package %s: %s"
          (N.to_string n) (V.Set.to_string vs)
    ) map

  let to_string f t =
    check t;
    Updated.to_string f t

end

module Reinstall = struct

  include Installed

  let internal = "reinstall"

end

module Repo_index = struct

  let internal = "repo-index"

  type t = string list N.Map.t

  let empty = N.Map.empty

  let of_string filename str =
    let lines = Lines.of_string filename str in
    List.fold_left (fun map -> function
      | name_s :: repo_s ->
          let name = N.of_string name_s in
          if N.Map.mem name map then
            Globals.error_and_exit "multiple lines for package %s" name_s
          else
            N.Map.add name repo_s map
      | [] -> map
    ) N.Map.empty lines

  let to_string filename map =
    let lines = N.Map.fold (fun name repo_s lines ->
      (N.to_string name :: repo_s) :: lines
    ) map [] in
    Lines.to_string filename (List.rev lines)

end

module Pinned = struct

  let internal = "pinned"

  type t = pin_option N.Map.t

  let empty = N.Map.empty

  let of_string filename str =
    let m = Repo_index.of_string filename str in
    N.Map.map (function
      | [x] -> pin_option_of_string x
      | _   -> Globals.error_and_exit "too many pinning options"
    ) m

  let to_string filename map =
    let aux x = [ string_of_pin_option x ] in
    Repo_index.to_string filename (N.Map.map aux map)

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
        Variable (s_address, String (Dirname.to_string (Repository.address t)));
        Variable (s_kind   , String (Repository.kind t));
      ] } in
    Syntax.to_string filename s

end

module Descr = struct

  let internal = "descr"

  type t = Lines.t

  let empty = []

  let create str =
    [[str]]

  let synopsis = function
    | []   -> ""
    | h::_ -> String.concat " " h

  let full l =
    let one l = String.concat " " l in
    String.concat "\n" (List.map one l)

  let of_string = Lines.of_string

  let to_string = Lines.to_string

end

let s_opam_version = "opam-version"

module Aliases = struct

  let internal = "aliases"

  type t = (Alias.t * OCaml_V.t) list

  let empty = []

  let to_string filename t =
    let l =
      List.map
        (fun (alias,oversion) -> [Alias.to_string alias; OCaml_V.to_string oversion])
        t in
    Lines.to_string filename l

  let of_string filename s =
    let l = Lines.of_string filename s in
    List.fold_left (fun accu -> function
      | []                -> accu
      | [alias; oversion] -> (Alias.of_string alias, OCaml_V.of_string oversion) :: accu
      | _                 -> failwith "switches"
    ) [] l

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
              [ String (Dirname.to_string (Repository.address r));
                String (Repository.kind r) ])

    type t = {
      opam_version  : OPAM_V.t ;
      repositories  : repository list ;
      ocaml_version : Alias.t option ;
      system_ocaml_version: OCaml_V.t option ;
      cores         : int;
    }

    let with_repositories t repositories = { t with repositories }
    let with_ocaml_version t v = { t with ocaml_version = Some v }
    let with_system_ocaml_version t v = { t with system_ocaml_version = Some v}

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let ocaml_version t = match t.ocaml_version with None -> Alias.of_string "<none>" | Some v -> v
    let system_ocaml_version t = t.system_ocaml_version
    let cores t = t.cores

    let create opam_version repositories cores =
      { opam_version ; repositories ; ocaml_version = None ; system_ocaml_version = None ; cores }

    let empty = {
      opam_version = OPAM_V.of_string Globals.opam_version;
      repositories = [];
      ocaml_version = None;
      system_ocaml_version = None;
      cores = Globals.default_cores;
    }

    open File_format

    let s_repositories = "repositories"
    let s_ocaml_version = "ocaml-version"
    let s_system_ocaml_version = "system-ocaml-version"
    let s_system_ocaml_version2 = "system_ocaml-version"
    let s_cores = "cores"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_ocaml_version;
      s_system_ocaml_version;
      s_system_ocaml_version2;
      s_cores;
    ]

    let of_string filename f =
      let s = Syntax.of_string filename f in
      Syntax.check s valid_fields;
      let opam_version =
        assoc s.contents s_opam_version (parse_string |> OPAM_V.of_string) in
      let repositories =
        assoc_default [] s.contents s_repositories
          (parse_list (parse_string_option parse_string_pair_of_list |> to_repo)) in
      let ocaml_version =
        assoc_option s.contents s_ocaml_version (parse_string |> Alias.of_string) in
      let system_ocaml_version =
        assoc_option s.contents s_system_ocaml_version (parse_string |> OCaml_V.of_string) in
      let system_ocaml_version2 =
        assoc_option s.contents s_system_ocaml_version2 (parse_string |> OCaml_V.of_string) in
      let system_ocaml_version =
        match system_ocaml_version, system_ocaml_version2 with
        | Some v, _
        | _     , Some v -> Some v
        | None  , None   -> None in
      let cores = assoc s.contents s_cores parse_int in
      { opam_version; repositories; ocaml_version; system_ocaml_version; cores }

   let to_string filename t =
     let s = {
       filename = Filename.to_string filename;
       contents = [
         Variable (s_opam_version , make_string (OPAM_V.to_string t.opam_version));
         Variable (s_repositories , make_list of_repo t.repositories);
         Variable (s_cores        , make_int t.cores);
       ]
       @ (
         match t.ocaml_version with
           | None   -> []
           | Some v -> [ Variable (s_ocaml_version, make_string (Alias.to_string v)) ]
       )
       @ (
         match t.system_ocaml_version with
           | None   -> []
           | Some v -> [ Variable (s_system_ocaml_version, make_string (OCaml_V.to_string v)) ]
       )
     } in
     Syntax.to_string filename s
end

module OPAM = struct

  let internal = "opam"

  type section = Types.section

  type t = {
    name       : N.t;
    version    : V.t;
    maintainer : string;
    substs     : basename list;
    build_env  : (string * string * string) list;
    build      : command list;
    remove     : command list;
    depends    : cnf_formula;
    depopts    : cnf_formula;
    conflicts  : and_formula;
    libraries  : section list;
    syntax     : section list;
    others     : (string * value) list;
    ocaml_version: ocaml_constraint option;
  }

  let empty = {
    name       = N.of_string "<none>";
    version    = V.of_string "<none>";
    maintainer = "<none>";
    substs     = [];
    build_env  = [];
    build      = [];
    remove     = [];
    depends    = [];
    depopts    = [];
    conflicts  = [];
    libraries  = [];
    syntax     = [];
    others     = [];
    ocaml_version = None;
  }

  let make
      ~name ~version ~maintainer ~substs ~build_env ~build ~remove
      ~depends ~depopts ~conflicts ~libraries ~syntax ~others
      ~ocaml_version =
    { name; version; maintainer;
      substs; build_env; build;
      remove; depends; depopts; conflicts;
      libraries; syntax; others;
      ocaml_version }

  let create nv =
    let name = NV.name nv in
    let version = NV.version nv in
    { empty with name; version }

  let s_version     = "version"
  let s_name        = "name"
  let s_maintainer  = "maintainer"
  let s_substs      = "substs"
  let s_build       = "build"
  let s_build_env   = "build-env"
  let s_remove      = "remove"
  let s_depends     = "depends"
  let s_depopts     = "depopts"
  let s_conflicts   = "conflicts"
  let s_libraries   = "libraries"
  let s_syntax      = "syntax"
  let s_license     = "license"
  let s_authors     = "authors"
  let s_homepage    = "homepage"
  let s_ocaml_version = "ocaml-version"

  (* to convert to cudf *)
  (* see [Debcudf.add_inst] for more details about the format *)
  let s_status = "status"

  (* see [Debcudf.add_inst] for more details about the format *)
  let s_installed   = "  installed"

  let useful_fields = [
    s_opam_version;
    s_maintainer;
    s_substs;
    s_build;
    s_remove;
    s_depends;
    s_depopts;
    s_conflicts;
    s_libraries;
    s_syntax;
    s_ocaml_version;
    s_build_env;
  ]

  let valid_fields =
    useful_fields @ [
      s_license;
      s_authors;
      s_homepage;
      s_version;
      s_name;
    ]

  let name t = t.name
  let maintainer t = t.maintainer
  let version t = t.version
  let substs t = t.substs
  let build t = t.build
  let remove t = t.remove
  let depends t = t.depends
  let depopts t = t.depopts
  let conflicts t = t.conflicts
  let libraries t = t.libraries
  let syntax t = t.syntax
  let ocaml_version t = t.ocaml_version
  let build_env t = t.build_env

  let with_depends t depends = { t with depends }
  let with_depopts t depopts = { t with depopts }
  let with_build t build = { t with build }
  let with_remove t remove = { t with remove }
  let with_libraries t libraries = { t with libraries }
  let with_substs t substs = { t with substs }
  let with_ocaml_version t ocaml_version = { t with ocaml_version }
  let with_maintainer t maintainer = { t with maintainer }

  module D = Debian.Packages

  (* XXX: Pre-encode the depends and conflict fields to avoid
     headaches when interfacing with the solver *)
  let lencode = List.map (fun ((n,a),c) -> (Common.CudfAdd.encode n,a), c)
  let llencode = List.map lencode

  let default_package t =
    let depopts =
      string_of_value (File_format.make_cnf_formula (llencode t.depopts)) in
    { D.default_package with
      D.name      = N.to_string t.name ;
      D.version   = V.to_string t.version ;
      D.depends   = llencode t.depends ;
      D.conflicts = lencode t.conflicts ;
      D.extras    = (s_depopts, depopts) :: D.default_package.D.extras }

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
        Variable (s_opam_version, String Globals.opam_version);
(*        Variable (s_name, String (N.to_string t.name));
          Variable (s_version, String (V.to_string t.version)); *)
        Variable (s_maintainer, String t.maintainer);
        Variable (s_substs, make_list (Basename.to_string |> make_string) t.substs);
        Variable (s_build_env, make_list make_env_variable t.build_env);
        Variable (s_build, make_list make_command t.build);
        Variable (s_remove, make_list make_command t.remove);
        Variable (s_depends, make_cnf_formula t.depends);
        Variable (s_depopts, make_cnf_formula t.depopts);
        Variable (s_conflicts, make_and_formula t.conflicts);
        Variable (s_libraries, make_list (Section.to_string |> make_string) t.libraries);
        Variable (s_syntax, make_list (Section.to_string |> make_string) t.syntax);
      ] @ (
        match t.ocaml_version with
        | None   -> []
        | Some v -> [ Variable (s_ocaml_version, make_constraint v) ]
      ) @
        List.map (fun (s, v) -> Variable (s, v)) t.others;
    } in
    Syntax.to_string
      ~indent_variable:(fun s -> List.mem s [s_build ; s_remove ; s_depends ; s_depopts])
      filename s

  let of_string filename str =
    let nv = NV.of_filename filename in
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let s = s.contents in
    let opam_version = assoc s s_opam_version parse_string in
    if opam_version <> Globals.opam_version then
      Globals.error_and_exit "%s is not a supported OPAM version" opam_version;
    let name_f = assoc_option  s s_name (parse_string |> N.of_string) in
    let name = match name_f, nv with
      | None  , None    ->
          Globals.error_and_exit "%s is an invalid OPAM filename" (Filename.to_string filename);
      | Some n, None    -> n
      | None  , Some nv -> NV.name nv
      | Some n, Some nv ->
          if NV.name nv <> n then
            Globals.error_and_exit
              "Inconsistant naming scheme in %s"
              (Filename.to_string filename)
          else
            n in
    let version_f = assoc_option s s_version (parse_string |> V.of_string) in
    let version = match version_f, nv with
      | None  , None    ->
          Globals.error_and_exit "%s is an invalid OPAM filename" (Filename.to_string filename);
      | Some v, None    -> v
      | None  , Some nv -> NV.version nv
      | Some v, Some nv ->
          if NV.version nv <> v then
            Globals.error_and_exit
              "Inconsistant versioning scheme in %s"
              (Filename.to_string filename)
          else
            v in
    let maintainer = assoc s s_maintainer parse_string in
    let substs     =
      assoc_list s s_substs (parse_list (parse_string |> Basename.of_string)) in
    let build_env = assoc_list s s_build_env (parse_list parse_env_variable) in
    let build      = assoc_default [] s s_build parse_commands in
    let remove     = assoc_list s s_remove parse_commands in
    let depends    = assoc_list s s_depends parse_cnf_formula in
    let depopts    = assoc_list s s_depopts parse_cnf_formula in
    let conflicts  = assoc_list s s_conflicts parse_and_formula in
    let libraries  = assoc_list s s_libraries (parse_list (parse_string |> Section.of_string)) in
    let syntax     = assoc_list s s_syntax (parse_list (parse_string |> Section.of_string)) in
    let ocaml_version = assoc_option s s_ocaml_version parse_constraint in
    let others     =
      Utils.filter_map (function
        | Variable (x,v) -> if List.mem x useful_fields then None else Some (x,v)
        | _              -> None
      ) s in
    { name; version; maintainer; substs; build; remove;
      depends; depopts; conflicts; libraries; syntax; others;
      ocaml_version; build_env }
end

module Dot_install_raw = struct

  let internal = ".install(*raw*)"

  type t =  {
    lib : string optional list;
    bin : (string optional * string option) list;
    toplevel: string optional list;
    misc: (string optional * string option) list;
  }

  let lib t = t.lib
  let bin t = t.bin
  let toplevel t = t.toplevel
  let misc t = t.misc

  let with_bin t bin = { t with bin }
  let with_lib t lib = { t with lib }
  let with_toplevel t toplevel = { t with toplevel }

  let empty = {
    lib  = [] ;
    bin  = [] ;
    toplevel = [] ;
    misc = [] ;
  }

  let s_lib = "lib"
  let s_bin = "bin"
  let s_misc = "misc"
  let s_toplevel = "toplevel"

  let valid_fields = [
    s_opam_version;
    s_lib;
    s_bin;
    s_toplevel;
    s_misc;
  ]

  (* Filenames starting by ? are not always present. *)
  let optional_of_string str =
    if String.length str > 0 && str.[0] = '?' then
      { optional=true; c=String.sub str 1 (String.length str - 1) }
    else
      { optional=false; c=str }

  let string_of_optional t =
    let o = if t.optional then "?" else "" in
    Printf.sprintf "%s%s" o t.c

  let to_string filename t =
    let make_option (src, option) =
      let src = String (string_of_optional src) in
      match option with
      | None     -> src
      | Some dst -> Option (src, [String dst]) in
    let s = {
      filename = Filename.to_string filename;
      contents = [
        Variable (s_lib , make_list (string_of_optional |> make_string) t.lib);
        Variable (s_bin , make_list make_option t.bin);
        Variable (s_toplevel, make_list (string_of_optional |> make_string) t.toplevel);
        Variable (s_misc, make_list make_option t.misc);
      ]
    } in
    Syntax.to_string ~indent_variable:(fun _ -> true) filename s

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let parse_option = parse_or [
      ("string", fun v -> optional_of_string (parse_string v), None);
      ("option", function
        | Option (String src, [String dst]) -> optional_of_string src, Some dst
        | _ -> failwith "option");
    ] in
    let lib = assoc_list s.contents s_lib (parse_list (parse_string |> optional_of_string)) in
    let bin = assoc_list s.contents s_bin (parse_list parse_option) in
    let toplevel = assoc_list s.contents s_toplevel (parse_list (parse_string |> optional_of_string)) in
    let misc = assoc_list s.contents s_misc (parse_list parse_option) in
    { lib; bin; misc; toplevel }

end

module Dot_install = struct

  let internal = ".install"

  type t =  {
    lib : filename optional list ;
    bin : (filename optional * basename) list ;
    toplevel : filename optional list;
    misc: (filename optional * filename) list ;
  }

  let string_of_move (src, dst) =
    let src = Filename.to_string src in
    let dst = Filename.to_string dst in
    Printf.sprintf "%s => %s" src dst

  let lib t = t.lib
  let bin t = t.bin
  let misc t = t.misc
  let toplevel t = t.toplevel

  module R = Dot_install_raw

  let empty = {
    lib  = [] ;
    bin  = [] ;
    toplevel = [];
    misc = [] ;
  }

  let map_o fn x =
    { optional = x.optional; c = fn x.c }

  let to_string filename t =
    let to_bin (src, dst) =
      map_o Filename.to_string src,
      if Filename.basename src.c = dst then None else Some (Basename.to_string dst) in
    let to_misc (src, dst) =
      map_o Filename.to_string src,
      Some (Filename.to_string dst) in
    R.to_string filename
      { lib = List.map (map_o Filename.to_string) t.lib
      ; bin = List.map to_bin t.bin
      ; toplevel = List.map (map_o Filename.to_string) t.toplevel
      ; R.misc = List.map to_misc t.misc }

  let of_string filename str =
    let t = R.of_string filename str in
    let of_bin = function
      | s  , None     -> let f = map_o Filename.of_string s in (f, Filename.basename f.c)
      | src, Some dst -> (map_o Filename.of_string src, Basename.of_string dst) in
    let of_misc = function
      | s  , None     -> let f = map_o Filename.of_string s in (f, f.c)
      | src, Some dst -> (map_o Filename.of_string src, Filename.of_string dst) in
    { lib = List.map (map_o Filename.of_string) t.R.lib
    ; bin = List.map of_bin t.R.bin
    ; toplevel = List.map (map_o Filename.of_string) t.R.toplevel
    ; misc = List.map of_misc t.R.misc }

end

module Dot_config = struct

  let internal = ".config"

  let s str = S str
  let b bool = B bool
  type section = Types.section

  type s = {
    name      : section;
    kind      : string ;
    bytecomp  : string list ;
    asmcomp   : string list ;
    bytelink  : string list ;
    asmlink   : string list ;
    requires  : section list;
    lvariables: (variable * variable_contents) list;
  }

  type t = {
    sections : s list;
    variables: (variable * variable_contents) list;
  }

  let create variables =
    { variables; sections = [] }

  let empty = {
    sections  = [];
    variables = [];
  }

  let s_bytecomp = "bytecomp"
  let s_asmcomp  = "asmcomp"
  let s_bytelink = "bytelink"
  let s_asmlink  = "asmlink"
  let s_requires = "requires"

  let valid_fields = [
    s_opam_version;
    s_bytecomp;
    s_asmcomp;
    s_bytelink;
    s_asmlink;
    s_requires;
  ]

  let of_string filename str =
    let file = Syntax.of_string filename str in
    let parse_value = parse_or [
      "string", (parse_string |> s);
      "bool"  , (parse_bool   |> b);
    ] in
    let parse_variables items =
      let l = List.filter (fun (x,_) -> not (List.mem x valid_fields)) (variables items) in
      List.map (fun (k,v) -> Variable.of_string k, parse_value v) l in
    let parse_requires = parse_list (parse_string |> Section.of_string) in
    let parse_section kind s =
      let name =  Section.of_string s.File_format.name in
      let bytecomp = assoc_string_list s.items s_bytecomp in
      let asmcomp  = assoc_string_list s.items s_asmcomp  in
      let bytelink = assoc_string_list s.items s_bytecomp in
      let asmlink  = assoc_string_list s.items s_asmlink  in
      let requires = assoc_list s.items s_requires parse_requires in
      let lvariables = parse_variables s.items in
      { name; kind; bytecomp; asmcomp; bytelink; asmlink; lvariables; requires } in
    let libraries = assoc_sections file.contents "library" (parse_section "library") in
    let syntax    = assoc_sections file.contents "syntax" (parse_section "syntax") in
    let sections  = libraries @ syntax in
    let variables = parse_variables file.contents in
    { sections; variables }

  let rec to_string filename t =
    let of_value = function
      | B b -> Bool b
      | S s -> String s in
    let of_variables l =
      List.map (fun (k,v) -> Variable (Variable.to_string k, of_value v)) l in
    let make_require = Section.to_string |> make_string in
    let of_section s =
      Section
        { File_format.name = Section.to_string s.name;
          kind  = s.kind;
          items = [
            Variable (s_bytecomp, make_list make_string s.bytecomp);
            Variable (s_asmcomp , make_list make_string s.asmcomp);
            Variable (s_bytelink, make_list make_string s.bytelink);
            Variable (s_asmlink , make_list make_string s.asmlink);
            Variable (s_requires, make_list make_require s.requires);
          ] @ of_variables s.lvariables
        } in
    Syntax.to_string filename {
      filename = Filename.to_string filename;
      contents =
        of_variables t.variables
        @ List.map of_section t.sections
    }

  let variables t = List.map fst t.variables

  let variable t s = List.assoc s t.variables

  module type SECTION = sig
    val available: t -> section list
    val kind     : t -> section -> string
    val asmcomp  : t -> section -> string list
    val bytecomp : t -> section -> string list
    val asmlink  : t -> section -> string list
    val bytelink : t -> section -> string list
    val requires : t -> section -> section list
    val variable : t -> section -> variable -> variable_contents
    val variables: t -> section -> variable list
  end

  module MK (M : sig val get : t -> s list end) : SECTION = struct

    let find t name =
      List.find (fun s -> s.name = name) (M.get t)

    let available t = List.map (fun s -> s.name) (M.get t)
    let kind t s = (find t s).kind
    let bytecomp t s = (find t s).bytecomp
    let asmcomp  t s = (find t s).asmcomp
    let bytelink t s = (find t s).bytelink
    let asmlink  t s = (find t s).asmlink
    let requires t s = (find t s).requires
    let variable t n s = List.assoc s (find t n).lvariables
    let variables t n = List.map fst (find t n).lvariables
  end

  let filter t n = List.filter (fun s -> s.kind = n) t.sections
  module Library  = MK (struct let get t = filter t "library" end)
  module Syntax   = MK (struct let get t = filter t "syntax"  end)
  module Section  = MK (struct let get t = t.sections end)
end

module Env = struct

  let internal = "env"

  type t = (string * string) list

  let empty = []

  let of_string filename s =
    let l = Lines.of_string filename s in
    List.fold_left (fun accu -> function
      | []  -> accu
      | [s] ->
          (match Utils.cut_at s '=' with
          | None      -> failwith (s ^ ": invalid env variable")
          | Some(k,v) -> (k, v) :: accu)
      | x   -> failwith (String.concat " " x ^ ": invalid env variable")
    ) [] l

  let to_string filename t =
    let l = List.map (fun (k,v) -> [ k^"="^v ]) t in
    Lines.to_string filename l

end


module Comp = struct

  let internal = "comp"

  type section = Types.section

  type t = {
    opam_version : OPAM_V.t ;
    name         : OCaml_V.t ;
    preinstalled : bool;
    src          : filename option ;
    patches      : filename list ;
    configure    : string list ;
    make         : string list ;
    build        : string list list ;
    bytecomp     : string list ;
    asmcomp      : string list ;
    bytelink     : string list ;
    asmlink      : string list ;
    packages     : and_formula ;
    requires     : section list;
    pp           : ppflag option;
    env          : (string * string * string) list;
  }

  let empty = {
    opam_version = OPAM_V.of_string Globals.opam_version;
    name         = OCaml_V.of_string "<none>";
    src          = None;
    preinstalled = false;
    patches   = [];
    configure = [];
    make      = [];
    build     = [];
    bytecomp  = [];
    asmcomp   = [];
    bytelink  = [];
    asmlink   = [];
    packages  = [];
    requires  = [];
    pp        = None;
    env       = [];
  }

  let create_preinstalled name packages env =
    { empty with name; preinstalled = true; packages; env }

  let s_name      = "name"
  let s_src       = "src"
  let s_patches   = "patches"
  let s_configure = "configure"
  let s_make      = "make"
  let s_build     = "build"
  let s_bytecomp  = "bytecomp"
  let s_asmcomp   = "asmcomp"
  let s_bytelink  = "bytelink"
  let s_asmlink   = "asmlink"
  let s_packages  = "packages"
  let s_requires  = "requires"
  let s_pp        = "pp"
  let s_env       = "env"
  let s_preinstalled = "preinstalled"

  let valid_fields = [
    s_opam_version;
    s_name;
    s_src;
    s_patches;
    s_configure;
    s_make;
    s_build;
    s_bytecomp;
    s_asmcomp;
    s_bytelink;
    s_asmlink;
    s_packages;
    s_requires;
    s_pp;
    s_env;
    s_preinstalled;
  ]

  let name t = t.name
  let patches t = t.patches
  let configure t = t.configure
  let make t = t.make
  let build t = t.build
  let src t = t.src
  let packages t = t.packages
  let asmlink t = t.asmlink
  let asmcomp t = t.asmcomp
  let bytelink t = t.bytelink
  let bytecomp t = t.bytecomp
  let requires t = t.requires
  let pp t = t.pp
  let preinstalled t = t.preinstalled
  let env t = t.env

  let of_string filename str =
    let file = Syntax.of_string filename str in
    Syntax.check file valid_fields;
    let s = file.contents in
    let parse_camlp4 = function
      | List ( Ident "CAMLP4" :: l ) ->
          Some (Camlp4 (parse_string_list (List l)))
      | _ -> raise (Bad_format "camlp4") in
    let parse_ppflags = parse_or [
      ("camlp4"     , parse_camlp4);
      ("string-list", parse_string_list |> fun x -> Some (Cmd x));
    ] in
    let opam_version =
      assoc s s_opam_version (parse_string |> OPAM_V.of_string) in
    let name      = assoc s s_name (parse_string |> OCaml_V.of_string) in
    let src       = assoc_option s s_src (parse_string |> Filename.raw) in
    let patches   = assoc_list s s_patches (parse_list (parse_string |> Filename.raw)) in
    let configure = assoc_string_list s s_configure in
    let make      = assoc_string_list s s_make      in
    let build     = assoc_list s s_build (parse_list parse_string_list) in
    let env       = assoc_list s s_env (parse_list parse_env_variable) in
    let bytecomp  = assoc_string_list s s_bytecomp  in
    let asmcomp   = assoc_string_list s s_asmcomp   in
    let bytelink  = assoc_string_list s s_bytecomp  in
    let asmlink   = assoc_string_list s s_asmlink   in
    let packages  = assoc_list s s_packages parse_and_formula in
    let requires  =
      assoc_list s s_requires (parse_list (parse_string |> Section.of_string)) in
    let pp = assoc_default None s s_pp parse_ppflags in
    let preinstalled = assoc_default false  s s_preinstalled parse_bool in

    if build <> [] && (configure @ make) <> [] then
      Globals.error_and_exit "You cannot use 'build' and 'make'/'configure' \
                              fields at the same time.";
    if not preinstalled && src = None then
      Globals.error_and_exit "You should either specify an url (with 'sources')  \
                              or use 'preinstalled: true' to pick the already installed \
                              compiler version.";
    { opam_version; name; src;
      patches; configure; make; build;
      bytecomp; asmcomp; bytelink; asmlink; packages;
      requires; pp;
      preinstalled; env;
    }

  let to_string filename s =
    let make_ppflag = function
      | Cmd l    -> make_list make_string l
      | Camlp4 l -> List (Symbol "CAMLP4" :: List.map make_string l) in
    Syntax.to_string filename {
      filename = Filename.to_string filename;
      contents = [
        Variable (s_name        , make_string (OCaml_V.to_string s.name));
      ] @ (match s.src with
          | None   -> []
          | Some s -> [Variable (s_src, make_string (Filename.to_string s))]
      ) @ [
        Variable (s_opam_version, make_string (OPAM_V.to_string s.opam_version));
        Variable (s_preinstalled, make_bool s.preinstalled);
        Variable (s_patches     , make_list (Filename.to_string |> make_string) s.patches);
        Variable (s_configure   , make_list make_string s.configure);
        Variable (s_make        , make_list make_string s.make);
        Variable (s_build       , make_list (make_list make_string) s.build);
        Variable (s_bytecomp    , make_list make_string s.bytecomp);
        Variable (s_asmcomp     , make_list make_string s.asmcomp);
        Variable (s_bytelink    , make_list make_string s.bytelink);
        Variable (s_asmlink     , make_list make_string s.asmlink);
        Variable (s_packages    , make_and_formula s.packages);
        Variable (s_requires    , make_list (Section.to_string |> make_string) s.requires);
        Variable (s_env         , make_list make_env_variable s.env);
      ] @ match s.pp with
         | None    -> []
         | Some pp -> [ Variable (s_pp, make_ppflag pp) ]
    }

end

module Subst = struct

  let internal = "subst"

  type t = Raw.t

  let empty = Raw.of_string ""

  let of_string filename str = str

  let to_string filename t = t

  let replace t f =
    let subst str =
      let str = String.sub str 2 (String.length str - 4) in
      let v = Full_variable.of_string str in
      string_of_variable_contents (f v) in
    let rex = Re_perl.compile_pat "%\\{[^%]+\\}%" in
    let str = Pcre.substitute ~rex ~subst (Raw.to_string t) in
    Raw.of_string str

  let replace_string s f =
    Raw.to_string (replace (Raw.of_string s) f)

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

  let log = Globals.log (Printf.sprintf "FILE(%s)" F.internal)

  let write f v =
    log "write %s" (Filename.to_string f);
    Filename.write f (F.to_string f v)

  let read f =
    let filename = Filename.to_string f in
    log "read %s" filename;
    if Filename.exists f then
      try F.of_string f (Filename.read f)
      with Bad_format msg ->
        Globals.error_and_exit "File %s: %s" (Filename.to_string f) msg
    else
      Globals.error_and_exit "File %s does not exist" (Filename.to_string f)

  let safe_read f =
    let filename = Filename.to_string f in
    log "safe_read %s" filename;
    if Filename.exists f then
      read f
    else (
      log "Cannot find %s" (Filename.to_string f);
      F.empty
    )

  let filename = Filename.of_string "/dummy/"

  let of_raw raw =
    try F.of_string filename raw
    with Bad_format msg ->
      Globals.error_and_exit "%s:\n%s" msg (Raw.to_string raw)

  let to_raw t =
    F.to_string filename t

end

open X

module type IO_FILE = sig
  type t
  val empty: t
  val write: filename -> t -> unit
  val read : filename -> t
  val safe_read: filename -> t
  val to_raw: t -> raw
  val of_raw: raw -> t
end

module Config = struct
  include Config
  include Make (Config)
end

module Repo_index = struct
  include Repo_index
  include Make (Repo_index)
end

module Pinned = struct
  include Pinned
  include Make (Pinned)
end

module Repo_config = struct
  include Repo_config
  include Make (Repo_config)
end

module Descr = struct
  include Descr
  include Make (Descr)
end

module Aliases = struct
  include Aliases
  include Make (Aliases)
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
  module Raw = struct
    include Dot_install_raw
    include Make (Dot_install_raw)
  end
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

module Subst = struct
  include Subst
  include Make (Subst)
end

module Comp = struct
  include Comp
  include Make (Comp)
end

module Env = struct
  include Env
  include Make (Env)
end

module URL = struct
  include URL
  include Make (URL)
end

module Urls_txt = struct
  include Urls_txt
  include Make(Urls_txt)
end

module Filenames = struct
  include Filenames
  include Make(Filenames)
end
