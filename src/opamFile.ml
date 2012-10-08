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

exception Parsing of string

open OpamTypes
open OpamMisc.OP

module Lines = struct

  let internal = "lines"

  (* Lines of space separated words *)
  type t = string list list

  let empty = []

  let of_string _ raw =
    OpamLineLexer.main (Lexing.from_string raw)

  let to_string _ lines =
    let buf = Buffer.create 1024 in
    List.iter (fun l ->
      Buffer.add_string buf (String.concat " " l);
      Buffer.add_string buf "\n"
    ) lines;
    Buffer.contents buf

end

module Syntax = struct

  let internal = "syntax"

  type t = file

  let empty = OpamFormat.empty

  let of_string f str =
    try
      let lexbuf = Lexing.from_string str in
      let filename = OpamFilename.to_string f in
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
      OpamParser.main OpamLexer.token lexbuf filename
    with e ->
      OpamGlobals.error "Parsing error while reading %s" (OpamFilename.to_string f);
      raise e

  let to_string ?(indent_variable = fun _ -> false) _ t =
    OpamFormat.string_of_file ~indent_variable t

  let check f fields =
    if not (OpamFormat.is_valid f.file_contents fields) then
      OpamGlobals.error_and_exit "{ %s } are invalid field names in %s. Valid fields are { %s }"
        (String.concat ", " (OpamFormat.invalid_fields f.file_contents fields))
        f.file_name
        (String.concat ", " fields)
end

module X = struct

module Filenames = struct

  let internal = "filenames"

  type t = filename_set

  let empty = OpamFilename.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let lines = OpamMisc.filter_map (function
      | []  -> None
      | [f] -> Some (OpamFilename.of_string f)
      | s   ->
          OpamGlobals.error_and_exit "%S is not a valid filename" (String.concat " " s)
    ) lines in
    OpamFilename.Set.of_list lines

  let to_string f s =
    let lines =
      List.map (fun f -> [OpamFilename.to_string f]) (OpamFilename.Set.elements s) in
    Lines.to_string f lines

end

module Urls_txt = struct

  let internal = "urls-txt"

  type t = file_attribute_set

  let empty = OpamFilename.Attribute.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let rs = OpamMisc.filter_map (function
      | [] -> None
      | l  -> Some (OpamFilename.Attribute.of_string (String.concat " " l))
    ) lines in
    OpamFilename.Attribute.Set.of_list rs

  let to_string f t =
    let lines =
      List.map (fun r -> [OpamFilename.Attribute.to_string r]) (OpamFilename.Attribute.Set.elements t) in
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
    let archive = OpamFormat.assoc_option s.file_contents s_archive OpamFormat.parse_string in
    let git = OpamFormat.assoc_option s.file_contents s_git OpamFormat.parse_string in
    let checksum = OpamFormat.assoc_option s.file_contents s_checksum OpamFormat.parse_string in
    let url, kind = match archive, git with
      | None  , None   -> OpamGlobals.error_and_exit "Missing URL"
      | Some x, None   -> x, None
      | None  , Some x -> x, Some "git"
      | _ -> OpamGlobals.error_and_exit "Too many URLS" in
    { url; kind; checksum }

  let to_string filename t =
    let url_name = match t.kind with
      | Some "git"   -> "git"
      | Some "curl"
      | Some "rsync"
      | None         -> "archive"
      | Some x -> OpamGlobals.error_and_exit "%s is an unknown backend" x in
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (url_name , OpamFormat.make_string t.url);
      ] @ match t.checksum with
        | None   -> []
        | Some c -> [Variable (s_checksum, OpamFormat.make_string c)]
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

  type t = package_set

  let empty = OpamPackage.Set.empty

  let of_string f s =
    let lines = Lines.of_string f s in
    let map = ref empty in
    let add n v = map := OpamPackage.Set.add (OpamPackage.create n v) !map in
    List.iter (function
      | []              -> ()
      | [name; version] -> add (OpamPackage.Name.of_string name) (OpamPackage.Version.of_string version)
      | _               -> OpamGlobals.error_and_exit "[file.ml/module Installed]"
    ) lines;
    !map

  let to_string _ t =
    let buf = Buffer.create 1024 in
    OpamPackage.Set.iter
      (fun nv ->
        Printf.bprintf buf "%s %s\n"
          (OpamPackage.Name.to_string (OpamPackage.name nv))
          (OpamPackage.Version.to_string (OpamPackage.version nv)))
      t;
    Buffer.contents buf

end

module Installed = struct

  include Updated

  let internal = "installed"

  let check t =
    let map = OpamPackage.to_map t in
    OpamPackage.Name.Map.iter (fun n vs ->
      if OpamPackage.Version.Set.cardinal vs <> 1 then
        OpamGlobals.error_and_exit "Multiple versions installed for package %s: %s"
          (OpamPackage.Name.to_string n) (OpamPackage.Version.Set.to_string vs)
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

  type t = string list OpamPackage.Name.Map.t

  let empty = OpamPackage.Name.Map.empty

  let of_string filename str =
    let lines = Lines.of_string filename str in
    List.fold_left (fun map -> function
      | name_s :: repo_s ->
          let name = OpamPackage.Name.of_string name_s in
          if OpamPackage.Name.Map.mem name map then
            OpamGlobals.error_and_exit "multiple lines for package %s" name_s
          else
            OpamPackage.Name.Map.add name repo_s map
      | [] -> map
    ) OpamPackage.Name.Map.empty lines

  let to_string filename map =
    let lines = OpamPackage.Name.Map.fold (fun name repo_s lines ->
      (OpamPackage.Name.to_string name :: repo_s) :: lines
    ) map [] in
    Lines.to_string filename (List.rev lines)

end

module Pinned = struct

  let internal = "pinned"

  type t = pin_option OpamPackage.Name.Map.t

  let empty = OpamPackage.Name.Map.empty

  let of_string filename str =
    let m = Repo_index.of_string filename str in
    OpamPackage.Name.Map.map (function
      | [x]   -> pin_option_of_string x
      | [k;x] -> pin_option_of_string ?kind:(Some k) x
      | _     -> OpamGlobals.error_and_exit "too many pinning options"
    ) m

  let to_string filename map =
    let aux x = [ kind_of_pin_option x; path_of_pin_option x ] in
    Repo_index.to_string filename (OpamPackage.Name.Map.map aux map)

end

module Repo_config = struct

  let internal = "repo-config"

  type t = repository

  let empty = create_repository ~name:"<none>" ~address:"<none>" ~kind:"<none>"

  let s_name = "name"
  let s_kind = "kind"
  let s_address = "address"

  let of_string filename str =
    let s = Syntax.of_string filename str in
    let name = OpamFormat.assoc s.file_contents s_name OpamFormat.parse_string in
    let address = OpamFormat.assoc s.file_contents s_address OpamFormat.parse_string in
    let kind = OpamFormat.assoc s.file_contents s_kind OpamFormat.parse_string in
    create_repository ~name ~address ~kind

  let to_string filename t =
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_name   , OpamFormat.make_string t.repo_name);
        Variable (s_address, OpamFormat.make_string (OpamFilename.Dir.to_string t.repo_address));
        Variable (s_kind   , OpamFormat.make_string t.repo_kind);
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

  type t = (alias * compiler_version) list

  let empty = []

  let to_string filename t =
    let l =
      List.map
        (fun (alias,oversion) -> [OpamAlias.to_string alias;
                                  OpamVersion.Compiler.to_string oversion])
        t in
    Lines.to_string filename l

  let of_string filename s =
    let l = Lines.of_string filename s in
    List.fold_left (fun accu -> function
      | []                -> accu
      | [alias; oversion] -> (OpamAlias.of_string alias,
                              OpamVersion.Compiler.of_string oversion) :: accu
      | _                 -> failwith "switches"
    ) [] l

end

module Config = struct

    let internal = "config"

    let to_repo (name, option) =
      let address, kind = match option with
        | Some (address, kind) -> address, kind
        | None                 ->
            OpamGlobals.default_repository_kind,
            OpamGlobals.default_repository_address in
      create_repository ~name ~address ~kind

    let of_repo r =
      Option (String r.repo_name,
              [ String (OpamFilename.Dir.to_string r.repo_address);
                String r.repo_kind ])

    type t = {
      opam_version  : opam_version ;
      repositories  : repository list ;
      alias         : alias option ;
      system_version: compiler_version option ;
      cores         : int;
    }

    let with_repositories t repositories = { t with repositories }
    let with_alias t v = { t with alias = Some v }
    let with_system_version t v = { t with system_version = Some v}

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let alias t = match t.alias with
      | None   -> OpamAlias.of_string "<none>"
      | Some v -> v
    let system_version t = t.system_version
    let cores t = t.cores

    let create opam_version repositories cores =
      { opam_version ; repositories ; alias = None ; system_version = None ; cores }

    let empty = {
      opam_version = OpamVersion.OPAM.of_string OpamGlobals.opam_version;
      repositories = [];
      alias = None;
      system_version = None;
      cores = OpamGlobals.default_cores;
    }

    let s_repositories = "repositories"
    let s_ocaml_version = "ocaml-version"
    let s_alias = "alias"
    let s_system_version = "system-ocaml-version"
    let s_system_version2 = "system_ocaml-version"
    let s_cores = "cores"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_alias;
      s_ocaml_version;
      s_system_version;
      s_system_version2;
      s_cores;
    ]

    let of_string filename f =
      let s = Syntax.of_string filename f in
      Syntax.check s valid_fields;
      let opam_version =
        OpamFormat.assoc s.file_contents s_opam_version (OpamFormat.parse_string |> OpamVersion.OPAM.of_string) in
      let repositories =
        OpamFormat.assoc_list s.file_contents s_repositories
          (OpamFormat.parse_list
             (OpamFormat.parse_string_option OpamFormat.parse_string_pair_of_list |> to_repo)) in
      let alias =
        OpamFormat.assoc_option s.file_contents s_ocaml_version (OpamFormat.parse_string |> OpamAlias.of_string) in
      let alias2 =
        OpamFormat.assoc_option s.file_contents s_alias (OpamFormat.parse_string |> OpamAlias.of_string) in
      let system_version =
        OpamFormat.assoc_option s.file_contents s_system_version (OpamFormat.parse_string |> OpamVersion.Compiler.of_string) in
      let system_version2 =
        OpamFormat.assoc_option s.file_contents s_system_version2 (OpamFormat.parse_string |> OpamVersion.Compiler.of_string) in
      let system_version =
        match system_version, system_version2 with
        | Some v, _
        | _     , Some v -> Some v
        | None  , None   -> None in
      let alias =
        match alias, alias2 with
        | Some v, _
        | _     , Some v -> Some v
        | None  , None   -> None in
      let cores = OpamFormat.assoc s.file_contents s_cores OpamFormat.parse_int in
      { opam_version; repositories; alias; system_version; cores }

   let to_string filename t =
     let s = {
       file_name     = OpamFilename.to_string filename;
       file_contents = [
         Variable (s_opam_version , OpamFormat.make_string (OpamVersion.OPAM.to_string t.opam_version));
         Variable (s_repositories , OpamFormat.make_list of_repo t.repositories);
         Variable (s_cores        , OpamFormat.make_int t.cores);
       ]
       @ (
         match t.alias with
           | None   -> []
           | Some v -> [ Variable (s_alias, OpamFormat.make_string (OpamAlias.to_string v)) ]
       )
       @ (
         match t.system_version with
           | None   -> []
           | Some v -> [ Variable (s_system_version, OpamFormat.make_string (OpamVersion.Compiler.to_string v)) ]
       )
     } in
     Syntax.to_string filename s
end

module OPAM = struct

  let internal = "opam"

  type t = {
    name       : OpamPackage.Name.t;
    version    : OpamPackage.Version.t;
    maintainer : string;
    substs     : basename list;
    build_env  : (string * string * string) list;
    build      : command list;
    remove     : command list;
    depends    : formula;
    depopts    : formula;
    conflicts  : formula;
    libraries  : section list;
    syntax     : section list;
    patches    : (basename * filter option) list;
    files      : (basename * filter option) list;
    others     : (string * value) list;
    ocaml_version: compiler_constraint option;
  }

  let empty = {
    name       = OpamPackage.Name.of_string "<none>";
    version    = OpamPackage.Version.of_string "<none>";
    maintainer = "<none>";
    substs     = [];
    build_env  = [];
    build      = [];
    remove     = [];
    depends    = OpamFormula.Empty;
    depopts    = OpamFormula.Empty;
    conflicts  = OpamFormula.Empty;
    libraries  = [];
    syntax     = [];
    files      = [];
    patches    = [];
    others     = [];
    ocaml_version = None;
  }

  let create nv =
    let name = OpamPackage.name nv in
    let version = OpamPackage.version nv in
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
  let s_patches     = "patches"
  let s_files       = "files"
  let s_configure_style = "configure-style"

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
    s_patches;
    s_files;
  ]

  let valid_fields =
    useful_fields @ [
      s_license;
      s_authors;
      s_homepage;
      s_version;
      s_name;
      s_configure_style;
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
  let patches t = t.patches
  let files t = t.files

  let with_depends t depends = { t with depends }
  let with_depopts t depopts = { t with depopts }
  let with_build t build = { t with build }
  let with_remove t remove = { t with remove }
  let with_libraries t libraries = { t with libraries }
  let with_substs t substs = { t with substs }
  let with_ocaml_version t ocaml_version = { t with ocaml_version }
  let with_maintainer t maintainer = { t with maintainer }
  let with_files t files = { t with files }
  let with_patches t patches = { t with patches }

  module D = Debian.Packages

  (* XXX: Pre-encode the depends and conflict fields to avoid
     headaches when interfacing with the solver *)
  let encode = OpamFormula.map (fun (n,c) -> OpamPackage.Name.of_string (Common.CudfAdd.encode (OpamPackage.Name.to_string n)), c)

  let default_package t =
    let depopts = OpamFormat.string_of_value (OpamFormat.make_opt_formula t.depopts) in
    { D.default_package with
      D.name      = OpamPackage.Name.to_string t.name ;
      D.version   = OpamPackage.Version.to_string t.version ;
      D.depends   = OpamFormula.to_cnf (encode t.depends);
      D.conflicts = OpamFormula.to_conjunction (encode t.conflicts);
      D.extras    = (s_depopts, depopts) :: D.default_package.D.extras }

  let to_package t ~installed =
    let p = default_package t in
    if installed then
      { p with D.extras = (s_status, s_installed) :: p.D.extras }
    else
      p

  let to_string filename t =
    let make_file = OpamFormat.make_option (OpamFilename.Base.to_string |> OpamFormat.make_string) OpamFormat.make_filter in
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_opam_version, OpamFormat.make_string OpamGlobals.opam_version);
        Variable (s_maintainer  , OpamFormat.make_string t.maintainer);
        Variable (s_substs      , OpamFormat.make_list (OpamFilename.Base.to_string |> OpamFormat.make_string) t.substs);
        Variable (s_build_env   , OpamFormat.make_list OpamFormat.make_env_variable t.build_env);
        Variable (s_build       , OpamFormat.make_list OpamFormat.make_command t.build);
        Variable (s_remove      , OpamFormat.make_list OpamFormat.make_command t.remove);
        Variable (s_depends     , OpamFormat.make_formula t.depends);
        Variable (s_depopts     , OpamFormat.make_opt_formula t.depopts);
        Variable (s_conflicts   , OpamFormat.make_formula t.conflicts);
        Variable (s_libraries   , OpamFormat.make_list (OpamVariable.Section.to_string |> OpamFormat.make_string) t.libraries);
        Variable (s_syntax      , OpamFormat.make_list (OpamVariable.Section.to_string |> OpamFormat.make_string) t.syntax);
        Variable (s_files       , OpamFormat.make_list make_file t.files);
        Variable (s_patches     , OpamFormat.make_list make_file t.patches);
      ] @ (
        match t.ocaml_version with
        | None   -> []
        | Some v -> [ Variable (s_ocaml_version, OpamFormat.make_constraint v) ]
      ) @
        List.map (fun (s, v) -> Variable (s, v)) t.others;
    } in
    Syntax.to_string
      ~indent_variable:(fun s -> List.mem s [s_build ; s_remove ; s_depends ; s_depopts])
      filename s

  let of_string filename str =
    let nv = OpamPackage.of_filename filename in
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let s = s.file_contents in
    let opam_version = OpamFormat.assoc s s_opam_version OpamFormat.parse_string in
    if opam_version <> OpamGlobals.opam_version then
      OpamGlobals.error_and_exit "%s is not a supported OPAM version" opam_version;
    let name_f = OpamFormat.assoc_option s s_name (OpamFormat.parse_string |> OpamPackage.Name.of_string) in
    let name = match name_f, nv with
      | None  , None    ->
          OpamGlobals.error_and_exit "%s is an invalid OPAM filename" (OpamFilename.to_string filename);
      | Some n, None    -> n
      | None  , Some nv -> OpamPackage.name nv
      | Some n, Some nv ->
          if OpamPackage.name nv <> n then
            OpamGlobals.error_and_exit
              "Inconsistant naming scheme in %s"
              (OpamFilename.to_string filename)
          else
            n in
    let version_f = OpamFormat.assoc_option s s_version (OpamFormat.parse_string |> OpamPackage.Version.of_string) in
    let version = match version_f, nv with
      | None  , None    ->
          OpamGlobals.error_and_exit "%s is an invalid OPAM filename" (OpamFilename.to_string filename);
      | Some v, None    -> v
      | None  , Some nv -> OpamPackage.version nv
      | Some v, Some nv ->
          if OpamPackage.version nv <> v then
            OpamGlobals.error_and_exit
              "Inconsistant versioning scheme in %s"
              (OpamFilename.to_string filename)
          else
            v in
    let maintainer = OpamFormat.assoc s s_maintainer OpamFormat.parse_string in
    let substs =
      OpamFormat.assoc_list s s_substs
        (OpamFormat.parse_list (OpamFormat.parse_string |> OpamFilename.Base.of_string)) in
    let build_env =
      OpamFormat.assoc_list s s_build_env
        (OpamFormat.parse_list OpamFormat.parse_env_variable) in
    let build = OpamFormat.assoc_list s s_build OpamFormat.parse_commands in
    let remove = OpamFormat.assoc_list s s_remove OpamFormat.parse_commands in
    let depends = OpamFormat.assoc_default OpamFormula.Empty s s_depends OpamFormat.parse_formula in
    let depopts = OpamFormat.assoc_default OpamFormula.Empty s s_depopts OpamFormat.parse_opt_formula in
    let conflicts = OpamFormat.assoc_default OpamFormula.Empty s s_conflicts OpamFormat.parse_formula in
    let libraries = OpamFormat.assoc_list s s_libraries (OpamFormat.parse_list (OpamFormat.parse_string |> OpamVariable.Section.of_string)) in
    let syntax = OpamFormat.assoc_list s s_syntax (OpamFormat.parse_list (OpamFormat.parse_string |> OpamVariable.Section.of_string)) in
    let ocaml_version = OpamFormat.assoc_option s s_ocaml_version OpamFormat.parse_constraint in
    let parse_file = OpamFormat.parse_option (OpamFormat.parse_string |> OpamFilename.Base.of_string) OpamFormat.parse_filter in
    let patches = OpamFormat.assoc_list s s_patches (OpamFormat.parse_list parse_file) in
    let files = OpamFormat.assoc_list s s_files (OpamFormat.parse_list parse_file) in
    let others     =
      OpamMisc.filter_map (function
        | Variable (x,v) -> if List.mem x useful_fields then None else Some (x,v)
        | _              -> None
      ) s in
    { name; version; maintainer; substs; build; remove;
      depends; depopts; conflicts; libraries; syntax; others;
      files; patches; ocaml_version; build_env }
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
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_lib     , OpamFormat.make_list (string_of_optional |> OpamFormat.make_string) t.lib);
        Variable (s_bin     , OpamFormat.make_list make_option t.bin);
        Variable (s_toplevel, OpamFormat.make_list (string_of_optional |> OpamFormat.make_string) t.toplevel);
        Variable (s_misc    , OpamFormat.make_list make_option t.misc);
      ]
    } in
    Syntax.to_string ~indent_variable:(fun _ -> true) filename s

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let parse_option = OpamFormat.parse_or [
      ("string", fun v -> optional_of_string (OpamFormat.parse_string v), None);
      ("option", function
        | Option (String src, [String dst]) -> optional_of_string src, Some dst
        | _ -> failwith "option");
    ] in
    let lib =
      OpamFormat.assoc_list s.file_contents s_lib
        (OpamFormat.parse_list (OpamFormat.parse_string |> optional_of_string)) in
    let bin = OpamFormat.assoc_list s.file_contents s_bin (OpamFormat.parse_list parse_option) in
    let toplevel =
      OpamFormat.assoc_list s.file_contents s_toplevel
        (OpamFormat.parse_list (OpamFormat.parse_string |> optional_of_string)) in
    let misc = OpamFormat.assoc_list s.file_contents s_misc (OpamFormat.parse_list parse_option) in
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
    let src = OpamFilename.to_string src in
    let dst = OpamFilename.to_string dst in
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
      map_o OpamFilename.to_string src,
      if OpamFilename.basename src.c = dst then None else Some (OpamFilename.Base.to_string dst) in
    let to_misc (src, dst) =
      map_o OpamFilename.to_string src,
      Some (OpamFilename.to_string dst) in
    R.to_string filename
      { lib = List.map (map_o OpamFilename.to_string) t.lib
      ; bin = List.map to_bin t.bin
      ; toplevel = List.map (map_o OpamFilename.to_string) t.toplevel
      ; R.misc = List.map to_misc t.misc }

  let of_string filename str =
    let t = R.of_string filename str in
    let of_bin = function
      | s  , None     -> let f = map_o OpamFilename.of_string s in (f, OpamFilename.basename f.c)
      | src, Some dst -> (map_o OpamFilename.of_string src, OpamFilename.Base.of_string dst) in
    let of_misc = function
      | s  , None     -> let f = map_o OpamFilename.of_string s in (f, f.c)
      | src, Some dst -> (map_o OpamFilename.of_string src, OpamFilename.of_string dst) in
    { lib = List.map (map_o OpamFilename.of_string) t.R.lib
    ; bin = List.map of_bin t.R.bin
    ; toplevel = List.map (map_o OpamFilename.of_string) t.R.toplevel
    ; misc = List.map of_misc t.R.misc }

end

module Dot_config = struct

  let internal = ".config"

  let s str = S str
  let b bool = B bool

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
    let parse_value = OpamFormat.parse_or [
      "string", (OpamFormat.parse_string |> s);
      "bool"  , (OpamFormat.parse_bool   |> b);
    ] in
    let parse_variables items =
      let l = List.filter (fun (x,_) -> not (List.mem x valid_fields)) (OpamFormat.variables items) in
      List.map (fun (k,v) -> OpamVariable.of_string k, parse_value v) l in
    let parse_requires = OpamFormat.parse_list (OpamFormat.parse_string |> OpamVariable.Section.of_string) in
    let parse_section kind s =
      let name =  OpamVariable.Section.of_string s.section_name in
      let bytecomp = OpamFormat.assoc_string_list s.section_items s_bytecomp in
      let asmcomp  = OpamFormat.assoc_string_list s.section_items s_asmcomp  in
      let bytelink = OpamFormat.assoc_string_list s.section_items s_bytecomp in
      let asmlink  = OpamFormat.assoc_string_list s.section_items s_asmlink  in
      let requires = OpamFormat.assoc_list s.section_items s_requires parse_requires in
      let lvariables = parse_variables s.section_items in
      { name; kind; bytecomp; asmcomp; bytelink; asmlink; lvariables; requires } in
    let libraries = OpamFormat.assoc_sections file.file_contents "library" (parse_section "library") in
    let syntax    = OpamFormat.assoc_sections file.file_contents "syntax" (parse_section "syntax") in
    let sections  = libraries @ syntax in
    let variables = parse_variables file.file_contents in
    { sections; variables }

  let rec to_string filename t =
    let of_value = function
      | B b -> Bool b
      | S s -> String s in
    let of_variables l =
      List.map (fun (k,v) -> Variable (OpamVariable.to_string k, of_value v)) l in
    let make_require = OpamVariable.Section.to_string |> OpamFormat.make_string in
    let of_section s =
      Section
        { section_name  = OpamVariable.Section.to_string s.name;
          section_kind  = s.kind;
          section_items = [
            Variable (s_bytecomp, OpamFormat.make_list OpamFormat.make_string s.bytecomp);
            Variable (s_asmcomp , OpamFormat.make_list OpamFormat.make_string s.asmcomp);
            Variable (s_bytelink, OpamFormat.make_list OpamFormat.make_string s.bytelink);
            Variable (s_asmlink , OpamFormat.make_list OpamFormat.make_string s.asmlink);
            Variable (s_requires, OpamFormat.make_list make_require s.requires);
          ] @ of_variables s.lvariables
        } in
    Syntax.to_string filename {
      file_name     = OpamFilename.to_string filename;
      file_contents =
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
          (match OpamMisc.cut_at s '=' with
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

  type t = {
    opam_version : opam_version ;
    name         : compiler_version ;
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
    packages     : formula ;
    requires     : section list;
    pp           : ppflag option;
    env          : (string * string * string) list;
  }

  let empty = {
    opam_version = OpamVersion.OPAM.of_string OpamGlobals.opam_version;
    name         = OpamVersion.Compiler.of_string "<none>";
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
    packages  = OpamFormula.Empty;
    requires  = [];
    pp        = None;
    env       = [];
  }

  let create_preinstalled name packages env =
    let mk n = Atom (n, Empty) in
    let rec aux accu t = match accu, t with
      | Empty, x  -> mk x
      | _    , x  -> And(accu, mk x) in
    let packages = List.fold_left aux OpamFormula.Empty packages in
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
    let s = file.file_contents in
    let parse_camlp4 = function
      | List ( Ident "CAMLP4" :: l ) ->
          Some (Camlp4 (OpamFormat.parse_string_list (List l)))
      | _ -> raise (OpamFormat.Bad_format "camlp4") in
    let parse_ppflags = OpamFormat.parse_or [
      ("camlp4"     , parse_camlp4);
      ("string-list", OpamFormat.parse_string_list |> fun x -> Some (Cmd x));
    ] in
    let opam_version =
      OpamFormat.assoc s s_opam_version (OpamFormat.parse_string |> OpamVersion.OPAM.of_string) in
    let name = OpamFormat.assoc s s_name (OpamFormat.parse_string |> OpamVersion.Compiler.of_string) in
    let src = OpamFormat.assoc_option s s_src (OpamFormat.parse_string |> OpamFilename.raw_file) in
    let patches =
      OpamFormat.assoc_list s s_patches
        (OpamFormat.parse_list (OpamFormat.parse_string |> OpamFilename.raw_file)) in
    let configure = OpamFormat.assoc_string_list s s_configure in
    let make = OpamFormat.assoc_string_list s s_make      in
    let build = OpamFormat.assoc_list s s_build (OpamFormat.parse_list OpamFormat.parse_string_list) in
    let env = OpamFormat.assoc_list s s_env (OpamFormat.parse_list OpamFormat.parse_env_variable) in
    let bytecomp = OpamFormat.assoc_string_list s s_bytecomp  in
    let asmcomp = OpamFormat.assoc_string_list s s_asmcomp   in
    let bytelink = OpamFormat.assoc_string_list s s_bytecomp  in
    let asmlink = OpamFormat.assoc_string_list s s_asmlink   in
    let packages = OpamFormat.assoc_default OpamFormula.Empty s s_packages OpamFormat.parse_formula in
    let requires =
      OpamFormat.assoc_list s s_requires
        (OpamFormat.parse_list (OpamFormat.parse_string |> OpamVariable.Section.of_string)) in
    let pp = OpamFormat.assoc_default None s s_pp parse_ppflags in
    let preinstalled = OpamFormat.assoc_default false  s s_preinstalled OpamFormat.parse_bool in

    if build <> [] && (configure @ make) <> [] then
      OpamGlobals.error_and_exit "You cannot use 'build' and 'make'/'configure' \
                              fields at the same time.";
    if not preinstalled && src = None then
      OpamGlobals.error_and_exit "You should either specify an url (with 'sources')  \
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
      | Cmd l    -> OpamFormat.make_list OpamFormat.make_string l
      | Camlp4 l -> List (Symbol "CAMLP4" :: List.map OpamFormat.make_string l) in
    Syntax.to_string filename {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_opam_version, OpamFormat.make_string (OpamVersion.OPAM.to_string s.opam_version));
        Variable (s_name, OpamFormat.make_string (OpamVersion.Compiler.to_string s.name));
      ] @ (match s.src with
          | None   -> []
          | Some s -> [Variable (s_src, OpamFormat.make_string (OpamFilename.to_string s))]
      ) @ [
        Variable (s_patches     , OpamFormat.make_list (OpamFilename.to_string |> OpamFormat.make_string) s.patches);
        Variable (s_configure   , OpamFormat.make_list OpamFormat.make_string s.configure);
        Variable (s_make        , OpamFormat.make_list OpamFormat.make_string s.make);
        Variable (s_build       , OpamFormat.make_list (OpamFormat.make_list OpamFormat.make_string) s.build);
        Variable (s_bytecomp    , OpamFormat.make_list OpamFormat.make_string s.bytecomp);
        Variable (s_asmcomp     , OpamFormat.make_list OpamFormat.make_string s.asmcomp);
        Variable (s_bytelink    , OpamFormat.make_list OpamFormat.make_string s.bytelink);
        Variable (s_asmlink     , OpamFormat.make_list OpamFormat.make_string s.asmlink);
        Variable (s_packages    , OpamFormat.make_formula s.packages);
        Variable (s_requires    , OpamFormat.make_list (OpamVariable.Section.to_string |> OpamFormat.make_string) s.requires);
        Variable (s_env         , OpamFormat.make_list OpamFormat.make_env_variable s.env);
      ] @ (match s.pp with
        | None    -> []
        | Some pp -> [ Variable (s_pp, make_ppflag pp) ]
      ) @ [
        Variable (s_preinstalled, OpamFormat.make_bool s.preinstalled);
      ]
    }

end

module Subst = struct

  let internal = "subst"

  type t = string

  let empty = ""

  let of_string filename str = str

  let to_string filename t = t

  let replace t f =
    let subst str =
      let str = String.sub str 2 (String.length str - 4) in
      let v = OpamVariable.Full.of_string str in
      OpamVariable.string_of_variable_contents (f v) in
    let rex = Re_perl.compile_pat "%\\{[^%]+\\}%" in
    Pcre.substitute ~rex ~subst t

  let replace_string = replace

end

end

module type F = sig
  val internal : string
  type t
  val empty : t
  val of_string : filename -> string -> t
  val to_string : filename -> t -> string
end

module Make (F : F) = struct

  let log = OpamGlobals.log (Printf.sprintf "FILE(%s)" F.internal)

  let write f v =
    log "write %s" (OpamFilename.to_string f);
    OpamFilename.write f (F.to_string f v)

  let read f =
    let filename = OpamFilename.to_string f in
    log "read %s" filename;
    if OpamFilename.exists f then
      try F.of_string f (OpamFilename.read f)
      with OpamFormat.Bad_format msg ->
        OpamGlobals.error_and_exit "File %s: %s" (OpamFilename.to_string f) msg
    else
      OpamGlobals.error_and_exit "File %s does not exist" (OpamFilename.to_string f)

  let safe_read f =
    if OpamFilename.exists f then
      read f
    else (
      log "Cannot find %s" (OpamFilename.to_string f);
      F.empty
    )

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
