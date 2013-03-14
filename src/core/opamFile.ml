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

open OpamTypes
open OpamMisc.OP

module Lines = struct

  (* Lines of space separated words *)
  type t = string list list

  let of_string str =
    OpamLineLexer.main (Lexing.from_string str)

  let to_string (lines: t) =
    let buf = Buffer.create 1024 in
    List.iter (fun l ->
      Buffer.add_string buf (String.concat " " l);
      Buffer.add_string buf "\n"
    ) lines;
    Buffer.contents buf

end

module Syntax = struct

  type t = file

  let of_string filename str =
    let filename = OpamFilename.to_string filename in
    let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
    OpamParser.main OpamLexer.token lexbuf filename

  let to_string ?(indent_variable = fun _ -> false) (t: t) =
    OpamFormat.string_of_file ~indent_variable t

  let check f fields =
    if not (OpamFormat.is_valid f.file_contents fields) then (
      let invalids = OpamFormat.invalid_fields f.file_contents fields in
      let too_many, invalids = List.partition (fun x -> List.mem x fields) invalids in
      if too_many <> [] then
        OpamGlobals.error
          "%s appears too many times in %s"
          f.file_name
          (OpamMisc.string_of_list (fun x -> x) too_many);
      if invalids <> [] then
        OpamGlobals.error "%s are invalid field names in %s. Valid fields are %s"
          (OpamMisc.string_of_list (fun x -> x) invalids)
          f.file_name
          (OpamMisc.string_of_list (fun x -> x) fields);
      OpamGlobals.exit 5;
    )
end

module X = struct

module Prefix = struct

  let internal = "prefix"

  type t = string name_map

  let empty = OpamPackage.Name.Map.empty

  let of_string _ s =
    let lines = Lines.of_string s in
    List.fold_left (fun map -> function
      | []          -> map
      | [nv;prefix] -> OpamPackage.Name.Map.add (OpamPackage.Name.of_string nv) prefix map
      | s ->
        OpamGlobals.error_and_exit
          "%S is not a valid prefix line"
          (String.concat " " s)
    ) OpamPackage.Name.Map.empty lines

  let to_string _ s =
    let lines =
      OpamPackage.Name.Map.fold (fun nv prefix l ->
        [OpamPackage.Name.to_string nv; prefix] :: l
      ) s [] in
    Lines.to_string lines

end

module Filenames = struct

  let internal = "filenames"

  type t = filename_set

  let empty = OpamFilename.Set.empty

  let of_string _ s =
    let lines = Lines.of_string s in
    let lines = OpamMisc.filter_map (function
      | []  -> None
      | [f] -> Some (OpamFilename.of_string f)
      | s   ->
          OpamGlobals.error_and_exit "%S is not a valid filename" (String.concat " " s)
    ) lines in
    OpamFilename.Set.of_list lines

  let to_string _ s =
    let lines =
      List.rev_map
        (fun f -> [OpamFilename.to_string f])
        (OpamFilename.Set.elements s) in
    Lines.to_string lines

end

module Urls_txt = struct

  let internal = "urls-txt"

  type t = file_attribute_set

  let empty = OpamFilename.Attribute.Set.empty

  let of_string _ s =
    let lines = Lines.of_string s in
    let rs = OpamMisc.filter_map (function
      | [] -> None
      | l  -> Some (OpamFilename.Attribute.of_string (String.concat " " l))
    ) lines in
    OpamFilename.Attribute.Set.of_list rs

  let to_string _ t =
    let lines =
      List.rev_map
        (fun r -> [OpamFilename.Attribute.to_string r])
        (OpamFilename.Attribute.Set.elements t) in
    Lines.to_string lines

end

module URL = struct

  let internal = "url"

  type t = {
    url     : string;
    kind    : repository_kind option;
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
  let s_darcs = "darcs"

  let valid_fields = [
    s_archive;
    s_checksum;
    s_git;
    s_darcs;
  ]

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let archive = OpamFormat.assoc_option s.file_contents s_archive OpamFormat.parse_string in
    let git = OpamFormat.assoc_option s.file_contents s_git OpamFormat.parse_string in
    let darcs = OpamFormat.assoc_option s.file_contents s_darcs OpamFormat.parse_string in
    let checksum = OpamFormat.assoc_option s.file_contents s_checksum OpamFormat.parse_string in
    let url, kind = match archive, git, darcs with
      | None  , None  , None   -> OpamGlobals.error_and_exit "Missing URL"
      | Some x, None  , None   -> x, None
      | None  , Some x, None   -> x, Some `git
      | None  , None  , Some x -> x, Some `darcs
      | _ -> OpamGlobals.error_and_exit "Too many URLS" in
    { url; kind; checksum }

  let to_string filename t =
    let url_name = match t.kind with
      | Some `git   -> "git"
      | Some `darcs -> "darcs"
      | None
      | Some `http  -> "archive"
      | Some `local -> OpamGlobals.error_and_exit "Local packages are not (yet) supported." in
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (url_name , OpamFormat.make_string t.url);
      ] @ match t.checksum with
        | None   -> []
        | Some c -> [Variable (s_checksum, OpamFormat.make_string c)]
    } in
    Syntax.to_string s

  let url t = t.url
  let kind t = t.kind
  let checksum t = t.checksum

  let with_checksum t checksum = { t with checksum = Some checksum }

end

module Export = struct

  let internal = "export"

  type t = package_set * package_set

  let empty = (OpamPackage.Set.empty, OpamPackage.Set.empty)

  let of_string _ s =
    let lines = Lines.of_string s in
    let installed = ref OpamPackage.Set.empty in
    let roots = ref OpamPackage.Set.empty in
    let add n v r =
      let nv = OpamPackage.create (OpamPackage.Name.of_string n) (OpamPackage.Version.of_string v) in
      installed := OpamPackage.Set.add nv !installed;
      if r then
        roots := OpamPackage.Set.add nv !roots;
    in
    List.iter (function
      | []        -> ()
      | [n; v]    -> add n v true
      | [n; v; r] -> add n v (r = "root")
      | l         ->
        OpamGlobals.error_and_exit
          "  Invalid line: %s\nThis is not a valid file to import."
          (String.concat " " l)
    ) lines;
    (!installed, !roots)

  let to_string _ (installed, roots) =
    let buf = Buffer.create 1024 in
    OpamPackage.Set.iter (fun nv ->
      Printf.bprintf buf "%s %s %s\n"
        (OpamPackage.Name.to_string (OpamPackage.name nv))
        (OpamPackage.Version.to_string (OpamPackage.version nv))
        (if OpamPackage.Set.mem nv roots then "root" else "noroot")
    ) installed;
    Buffer.contents buf

end

module Updated = struct

  let internal = "updated"

  type t = package_set

  let empty = OpamPackage.Set.empty

  let of_string _ s =
    let lines = Lines.of_string s in
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

module Installed_roots = struct

  include Installed

  let internal = "installed.roots"

end

module Reinstall = struct

  include Installed

  let internal = "reinstall"

end

module Repo_index = struct

  let internal = "repo-index"

  type t = repository_name list OpamPackage.Name.Map.t

  let empty = OpamPackage.Name.Map.empty

  let of_string _ str =
    let lines = Lines.of_string str in
    List.fold_left (fun map -> function
      | name_s :: repo_s ->
          let name = OpamPackage.Name.of_string name_s in
          if OpamPackage.Name.Map.mem name map then
            OpamGlobals.error_and_exit "multiple lines for package %s" name_s
          else
            let repo_s = List.map OpamRepositoryName.of_string repo_s in
            OpamPackage.Name.Map.add name repo_s map
      | [] -> map
    ) OpamPackage.Name.Map.empty lines

  let to_string _ map =
    let lines = OpamPackage.Name.Map.fold (fun name repo_s lines ->
      let repo_s = List.map OpamRepositoryName.to_string repo_s in
      (OpamPackage.Name.to_string name :: repo_s) :: lines
    ) map [] in
    Lines.to_string (List.rev lines)

end

module Pinned = struct

  let internal = "pinned"

  type t = pin_option OpamPackage.Name.Map.t

  let empty = OpamPackage.Name.Map.empty

  let of_string _ str =
    let lines = Lines.of_string str in
    let add name_s pin map =
      let name = OpamPackage.Name.of_string name_s in
      if OpamPackage.Name.Map.mem name map then
        OpamGlobals.error_and_exit "multiple lines for package %s" name_s
      else
        OpamPackage.Name.Map.add name pin map in
    List.fold_left (fun map -> function
      | []           -> map
      | [name_s; x]  -> add name_s (pin_option_of_string x) map
      | [name_s;k;x] ->
        let kind = Some (pin_kind_of_string k) in
        add name_s (pin_option_of_string ?kind x) map
      | _     -> OpamGlobals.error_and_exit "too many pinning options"
    ) OpamPackage.Name.Map.empty lines

  let to_string _ map =
    let lines = OpamPackage.Name.Map.fold (fun name pin lines ->
      let l = [
        OpamPackage.Name.to_string name;
        string_of_pin_kind (kind_of_pin_option pin);
        path_of_pin_option pin
      ] in
      l :: lines
    ) map [] in
    Lines.to_string (List.rev lines)

end

module Repo_config = struct

  let internal = "repo-config"

  type t = repository

  let empty = {
    repo_name     = OpamRepositoryName.of_string "<none>";
    repo_address  = OpamFilename.address_of_string "<none>";
    repo_kind     = `local;
    repo_priority = 0;
  }

  let s_name = "name"
  let s_kind = "kind"
  let s_address = "address"
  let s_priority = "priority"

  let of_string filename str =
    let s = Syntax.of_string filename str in
    let repo_name =
      OpamFormat.assoc s.file_contents s_name (OpamFormat.parse_string |> OpamRepositoryName.of_string) in
    let repo_address =
      OpamFormat.assoc s.file_contents s_address (OpamFormat.parse_string |> OpamFilename.address_of_string) in
    let repo_kind =
      OpamFormat.assoc s.file_contents s_kind (OpamFormat.parse_string |> repository_kind_of_string) in
    let repo_priority =
      OpamFormat.assoc_default 0 s.file_contents s_priority OpamFormat.parse_int in
    { repo_name; repo_address; repo_kind; repo_priority }

  let to_string filename t =
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_name    , OpamFormat.make_string (OpamRepositoryName.to_string t.repo_name));
        Variable (s_address , OpamFormat.make_string (OpamFilename.Dir.to_string t.repo_address));
        Variable (s_kind    , OpamFormat.make_string (string_of_repository_kind t.repo_kind));
        Variable (s_priority, OpamFormat.make_int t.repo_priority);
      ] } in
    Syntax.to_string s

end

module Descr = struct

  let internal = "descr"

  type t = string

  let empty = ""

  let synopsis str =
    match OpamMisc.cut_at str '\n' with
    | None       -> str
    | Some (s,_) -> s

  let full str = str

  let of_string _ x = x

  let to_string _ x = x

end

let s_opam_version = "opam-version"

module Aliases = struct

  let internal = "aliases"

  type t = compiler switch_map

  let empty = OpamSwitch.Map.empty

  let to_string _ t =
    let l =
      OpamSwitch.Map.fold (fun switch compiler lines ->
        [OpamSwitch.to_string switch; OpamCompiler.to_string compiler] :: lines
      ) t [] in
    Lines.to_string l

  let of_string _ s =
    let l = Lines.of_string s in
    List.fold_left (fun map -> function
      | []            -> map
      | [switch; comp] -> OpamSwitch.Map.add (OpamSwitch.of_string switch) (OpamCompiler.of_string comp) map
      | _             -> failwith "switches"
    ) OpamSwitch.Map.empty l

end

module Config = struct

    let internal = "config"

    type t = {
      opam_version  : opam_version ;
      repositories  : repository_name list ;
      switch        : switch;
      jobs          : int;
    }

    let with_repositories t repositories = { t with repositories }
    let with_switch t switch = { t with switch }
    let with_current_opam_version t = { t with opam_version = OpamVersion.current }

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let switch t = t.switch
    let jobs t = t.jobs

    let create opam_version switch repositories jobs =
      { opam_version ; repositories ; switch ; jobs }

    let empty = {
      opam_version = OpamVersion.of_string OpamGlobals.opam_version;
      repositories = [];
      switch = OpamSwitch.of_string "<empty>";
      jobs = OpamGlobals.default_jobs;
    }

    let s_repositories = "repositories"
    let s_switch = "switch"
    let s_switch1 = "alias"
    let s_switch2 = "ocaml-version"

    let s_jobs = "jobs"

    let s_cores = "cores"

    let s_system_version1 = "system_ocaml-version"
    let s_system_version2 = "system-ocaml-version"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_switch;
      s_jobs;

      (* this fields are no longer useful, but we keep it for backward
         compatibility *)
      s_switch1;
      s_switch2;
      s_system_version1;
      s_system_version2;
      s_cores;
    ]

    let of_string filename f =
      let s = Syntax.of_string filename f in
      Syntax.check s valid_fields;
      let opam_version =
        OpamFormat.assoc s.file_contents s_opam_version (OpamFormat.parse_string |> OpamVersion.of_string) in
      let repositories =
        OpamFormat.assoc_list s.file_contents s_repositories
          (OpamFormat.parse_list (OpamFormat.parse_string |> OpamRepositoryName.of_string)) in
      let mk_switch str =
        OpamFormat.assoc_option s.file_contents str (OpamFormat.parse_string |> OpamSwitch.of_string) in
      let switch  = mk_switch s_switch in
      let switch1 = mk_switch s_switch1 in
      let switch2 = mk_switch s_switch2 in
      let switch =
        match switch, switch1, switch2 with
        | Some v, _     , _
        | _     , Some v, _
        | _     , _     , Some v -> v
        | None  , None  , None   -> OpamGlobals.error_and_exit "No current switch defined." in

      let jobs =
        let mk str = OpamFormat.assoc_option s.file_contents str OpamFormat.parse_int in
        match mk s_jobs, mk s_cores with
        | Some i, _      -> i
        | _     , Some i -> i
        | _              -> 1 in
      { opam_version; repositories; switch; jobs }

   let to_string filename t =
     let s = {
       file_name     = OpamFilename.to_string filename;
       file_contents = [
         Variable (s_opam_version , OpamFormat.make_string (OpamVersion.to_string t.opam_version));
         Variable (s_repositories ,
                   OpamFormat.make_list
                     (OpamRepositoryName.to_string |> OpamFormat.make_string)
                     t.repositories);
         Variable (s_jobs , OpamFormat.make_int t.jobs);
         Variable (s_switch, OpamFormat.make_string (OpamSwitch.to_string t.switch))
       ]
     } in
     Syntax.to_string s
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
    others     : (string * value) list;
    ocaml_version: compiler_constraint option;
    os         : (bool * string) generic_formula;
    homepage   : string option;
    authors    : string list;
    license    : string option;
    doc        : string option;
    tags       : string list;
    build_test : command list;
    build_doc  : command list;
    depexts    : tags option;
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
    patches    = [];
    others     = [];
    ocaml_version = None;
    os         = Empty;
    homepage   = None;
    authors    = [];
    license    = None;
    doc        = None;
    tags       = [];
    build_test = [];
    build_doc  = [];
    depexts    = None;
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
  let s_ocaml_version = "ocaml-version"
  let s_patches     = "patches"
  let s_configure_style = "configure-style"
  let s_os          = "os"
  let s_homepage    = "homepage"
  let s_authors     = "authors"
  let s_license     = "license"
  let s_doc         = "doc"
  let s_tags        = "tags"
  let s_build_test  = "build-test"
  let s_build_doc   = "build-doc"
  let s_depexts     = "depexts"

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
    s_os;
    s_license;
    s_authors;
    s_homepage;
    s_doc;
    s_build_test;
    s_build_doc;
    s_depexts;
    s_tags;
  ]

  let valid_fields =
    useful_fields @ [
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
  let os t = t.os
  let homepage t = t.homepage
  let authors t = t.authors
  let license t = t.license
  let doc t = t.doc
  let tags t = t.tags
  let build_doc t = t.build_doc
  let build_test t = t.build_test
  let depexts t = t.depexts

  let with_depends t depends = { t with depends }
  let with_depopts t depopts = { t with depopts }
  let with_build t build = { t with build }
  let with_remove t remove = { t with remove }
  let with_libraries t libraries = { t with libraries }
  let with_substs t substs = { t with substs }
  let with_ocaml_version t ocaml_version = { t with ocaml_version }
  let with_maintainer t maintainer = { t with maintainer }
  let with_patches t patches = { t with patches }

  let to_string filename t =
    let make_file = OpamFormat.make_option (OpamFilename.Base.to_string |> OpamFormat.make_string) OpamFormat.make_filter in
    let option c s f = match c with
      | None   -> []
      | Some v -> [ Variable (s, f v) ] in
    let list c s f = match c with
      | [] -> []
      | l  -> [ Variable (s, f l) ] in
    let listm c s f = match c with
      | [] -> []
      | l  -> [ Variable (s, OpamFormat.make_list f l) ] in
    let formula c s f = match c with
      | Empty -> []
      | x     -> [ Variable (s, f x) ] in
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_opam_version, OpamFormat.make_string OpamGlobals.opam_version);
        Variable (s_maintainer  , OpamFormat.make_string t.maintainer);
      ] @ option  t.homepage      s_homepage      OpamFormat.make_string
        @ list    t.authors       s_authors       (String.concat ", " |> OpamFormat.make_string)
        @ option  t.license       s_license       OpamFormat.make_string
        @ option  t.doc           s_doc           OpamFormat.make_string
        @ list    t.tags          s_tags          OpamFormat.make_string_list
        @ listm   t.substs        s_substs        (OpamFilename.Base.to_string |> OpamFormat.make_string)
        @ listm   t.build_env     s_build_env     OpamFormat.make_env_variable
        @ listm   t.build         s_build         OpamFormat.make_command
        @ listm   t.remove        s_remove        OpamFormat.make_command
        @ formula t.depends       s_depends       OpamFormat.make_formula
        @ formula t.depopts       s_depopts       OpamFormat.make_opt_formula
        @ formula t.conflicts     s_conflicts     OpamFormat.make_formula
        @ listm   t.libraries     s_libraries     (OpamVariable.Section.to_string |> OpamFormat.make_string)
        @ listm   t.syntax        s_syntax        (OpamVariable.Section.to_string |> OpamFormat.make_string)
        @ list    t.patches       s_patches       (OpamFormat.make_list make_file)
        @ option  t.ocaml_version s_ocaml_version OpamFormat.make_compiler_constraint
        @ formula t.os            s_os            OpamFormat.make_os_constraint
        @ listm   t.build_test    s_build_test    OpamFormat.make_command
        @ listm   t.build_doc     s_build_doc     OpamFormat.make_command
        @ option  t.depexts       s_depexts       OpamFormat.make_tags
        @ List.rev (List.rev_map (fun (s, v) -> Variable (s, v)) t.others);
    } in
    Syntax.to_string
      ~indent_variable:(fun s -> List.mem s [s_build ; s_remove ; s_depends ; s_depopts])
      s

  let of_string filename str =
    let nv = OpamPackage.of_filename ~all:true filename in
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
              "Inconsistent naming scheme in %s"
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
              "Inconsistent versioning scheme in %s"
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
    let ocaml_version = OpamFormat.assoc_option s s_ocaml_version OpamFormat.parse_compiler_constraint in
    let os = OpamFormat.assoc_default OpamFormula.Empty s s_os OpamFormat.parse_os_constraint in
    let parse_file = OpamFormat.parse_option (OpamFormat.parse_string |> OpamFilename.Base.of_string) OpamFormat.parse_filter in
    let patches = OpamFormat.assoc_list s s_patches (OpamFormat.parse_list parse_file) in
    let homepage = OpamFormat.assoc_option s s_homepage OpamFormat.parse_string in
    let authors = OpamFormat.assoc_list s s_authors OpamFormat.parse_string_list in
    let license = OpamFormat.assoc_option s s_license OpamFormat.parse_string in
    let doc = OpamFormat.assoc_option s s_doc OpamFormat.parse_string in
    let tags = OpamFormat.assoc_list s s_tags OpamFormat.parse_string_list in
    let build_test = OpamFormat.assoc_list s s_build_test OpamFormat.parse_commands in
    let build_doc = OpamFormat.assoc_list s s_build_doc OpamFormat.parse_commands in
    let depexts = OpamFormat.assoc_option s s_depexts OpamFormat.parse_tags in
    let others     =
      OpamMisc.filter_map (function
        | Variable (x,v) -> if List.mem x useful_fields then None else Some (x,v)
        | _              -> None
      ) s in
    { name; version; maintainer; substs; build; remove;
      depends; depopts; conflicts; libraries; syntax; others;
      patches; ocaml_version; os; build_env;
      homepage; authors; license; doc; tags;
      build_test; build_doc; depexts;
    }
end

module Dot_install = struct

  let internal = ".install"

  type t =  {
    bin     : (basename optional * basename option) list;
    lib     : basename optional list;
    toplevel: basename optional list;
    share   : basename optional list;
    doc     : basename optional list;
    misc    : (basename optional * filename) list;
  }

  let empty = {
    lib      = [];
    bin      = [];
    toplevel = [];
    misc     = [];
    share    = [];
    doc      = [];
  }

  let bin t = t.bin
  let lib t = t.lib
  let toplevel t = t.toplevel
  let misc t = t.misc
  let share t = t.share
  let doc t = t.doc

  let s_lib      = "lib"
  let s_bin      = "bin"
  let s_misc     = "misc"
  let s_toplevel = "toplevel"
  let s_share    = "share"
  let s_doc      = "doc"

  let valid_fields = [
    s_opam_version;
    s_lib;
    s_bin;
    s_toplevel;
    s_misc;
    s_share;
    s_doc;
  ]

  (* Filenames starting by ? are not always present. *)
  let optional_of_string str =
    let mk = OpamFilename.Base.of_string in
    if String.length str > 0 && str.[0] = '?' then
      { optional = true;
        c        = mk (String.sub str 1 (String.length str - 1)) }
    else
      { optional = false;
        c        = mk str }

  let string_of_optional t =
    let o = if t.optional then "?" else "" in
    Printf.sprintf "%s%s" o (OpamFilename.Base.to_string t.c)

  let to_string filename t =
    let mk_bin =
      let aux (src, opt) =
        let src = String (string_of_optional src) in
        match opt with
        | None     -> src
        | Some dst -> Option (src, [String (OpamFilename.Base.to_string dst)]) in
      OpamFormat.make_list aux in
    let mk_misc =
      let aux (src, dst) =
        let src = String (string_of_optional src) in
        let dst = String (OpamFilename.to_string dst) in
        Option (src, [dst]) in
      OpamFormat.make_list aux in
    let mk =
      OpamFormat.make_list (string_of_optional |> OpamFormat.make_string) in
    let s = {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_bin     , mk_bin  t.bin);
        Variable (s_lib     , mk      t.lib);
        Variable (s_toplevel, mk      t.toplevel);
        Variable (s_share   , mk      t.share);
        Variable (s_doc     , mk      t.doc);
        Variable (s_misc    , mk_misc t.misc);
      ]
    } in
    Syntax.to_string ~indent_variable:(fun _ -> true) s

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let src = OpamFormat.parse_string |> optional_of_string in
    let mk field fn =
      OpamFormat.assoc_list s.file_contents field (OpamFormat.parse_list fn) in
    let bin =
      let dst = OpamFormat.parse_string |> OpamFilename.Base.of_string in
      let fn = OpamFormat.parse_single_option src dst in
      mk s_bin fn in
    let misc =
      let absolute_filename s =
        if not (Filename.is_relative s) then
          OpamFilename.of_string s
        else
          OpamSystem.internal_error "%s is not an absolute filename." str in
      let dst = OpamFormat.parse_string |> absolute_filename in
      let fn = OpamFormat.parse_pair src dst in
      mk s_misc fn in
    let fn = OpamFormat.parse_string |> optional_of_string in
    let lib      = mk s_lib      fn in
    let toplevel = mk s_toplevel fn in
    let share    = mk s_share    fn in
    let doc      = mk s_doc      fn in
    { lib; bin; misc; toplevel; share; doc }

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
      List.rev_map (fun (k,v) -> OpamVariable.of_string k, parse_value v) l in
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

  let to_string filename t =
    let of_value = function
      | B b -> Bool b
      | S s -> String s in
    let of_variables l =
      List.rev_map (fun (k,v) -> Variable (OpamVariable.to_string k, of_value v)) l in
    let make_require = OpamVariable.Section.to_string |> OpamFormat.make_string in
    let of_section s =
      Section
        { section_name  = OpamVariable.Section.to_string s.name;
          section_kind  = s.kind;
          section_items = [
            Variable (s_bytecomp, OpamFormat.make_string_list s.bytecomp);
            Variable (s_asmcomp , OpamFormat.make_string_list s.asmcomp);
            Variable (s_bytelink, OpamFormat.make_string_list s.bytelink);
            Variable (s_asmlink , OpamFormat.make_string_list s.asmlink);
            Variable (s_requires, OpamFormat.make_list make_require s.requires);
          ] @ of_variables s.lvariables
        } in
    Syntax.to_string {
      file_name     = OpamFilename.to_string filename;
      file_contents =
        of_variables t.variables
        @ List.rev (List.rev_map of_section t.sections)
    }

  let variables t = List.rev_map fst t.variables

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

    let available t = List.rev_map (fun s -> s.name) (M.get t)
    let kind t s = (find t s).kind
    let bytecomp t s = (find t s).bytecomp
    let asmcomp  t s = (find t s).asmcomp
    let bytelink t s = (find t s).bytelink
    let asmlink  t s = (find t s).asmlink
    let requires t s = (find t s).requires
    let variable t n s = List.assoc s (find t n).lvariables
    let variables t n = List.rev_map fst (find t n).lvariables
  end

  let filter t n = List.filter (fun s -> s.kind = n) t.sections
  module Library  = MK (struct let get t = filter t "library" end)
  module Syntax   = MK (struct let get t = filter t "syntax"  end)
  module Section  = MK (struct let get t = t.sections end)
end

module Comp = struct

  let internal = "comp"

  type t = {
    opam_version : opam_version ;
    name         : compiler ;
    version      : compiler_version ;
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
    tags         : string list;
  }

  let empty = {
    opam_version = OpamVersion.of_string OpamGlobals.opam_version;
    name         = OpamCompiler.of_string "<none>";
    version      = OpamCompiler.Version.of_string "<none>";
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
    tags      = [];
  }

  let create_preinstalled name version packages env =
    let mk n = Atom (n, Empty) in
    let aux accu t = match accu, t with
      | Empty, x  -> mk x
      | _    , x  -> And(accu, mk x) in
    let packages = List.fold_left aux OpamFormula.Empty packages in
    { empty with name; version; preinstalled = true; packages; env }

  let s_name      = "name"
  let s_version   = "version"
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
  let s_tags      = "tags"

  let valid_fields = [
    s_opam_version;
    s_name;
    s_version;
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
    s_tags;
  ]

  let name t = t.name
  let version t = t.version
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
  let tags t = t.tags

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
      OpamFormat.assoc s s_opam_version (OpamFormat.parse_string |> OpamVersion.of_string) in
    let name_d, version_d =
      let base = OpamCompiler.to_string (OpamCompiler.of_filename filename) in
      OpamCompiler.of_string base,
      match OpamMisc.cut_at base '+' with
      | None       -> OpamCompiler.Version.of_string base
      | Some (n,_) -> OpamCompiler.Version.of_string n in
    let name =
      OpamFormat.assoc_default name_d s s_name (OpamFormat.parse_string |> OpamCompiler.of_string) in
    if name_d <> name then
      OpamGlobals.warning "The file %s contains a bad 'name' field: %s instead of %s"
        (OpamFilename.to_string filename)
        (OpamCompiler.to_string name)
        (OpamCompiler.to_string name_d);
    let version =
      OpamFormat.assoc_default version_d s s_version
        (OpamFormat.parse_string |> OpamCompiler.Version.of_string) in
    if name <> OpamCompiler.system && version_d <> version then
      OpamGlobals.warning "The file %s contains a bad 'version' field: %s instead of %s"
        (OpamFilename.to_string filename)
        (OpamCompiler.Version.to_string version)
        (OpamCompiler.Version.to_string version_d);
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
    let tags = OpamFormat.assoc_string_list s s_tags in

    if build <> [] && (configure @ make) <> [] then
      OpamGlobals.error_and_exit "You cannot use 'build' and 'make'/'configure' \
                              fields at the same time.";
    if not preinstalled && src = None then
      OpamGlobals.error_and_exit "You should either specify an url (with 'sources')  \
                              or use 'preinstalled: true' to pick the already installed \
                              compiler version.";
    { opam_version; name; version; src;
      patches; configure; make; build;
      bytecomp; asmcomp; bytelink; asmlink; packages;
      requires; pp;
      preinstalled; env;
      tags;
    }

  let to_string filename s =
    let make_ppflag = function
      | Cmd l    -> OpamFormat.make_string_list l
      | Camlp4 l -> List (Symbol "CAMLP4" :: List.rev (List.rev_map OpamFormat.make_string l)) in
    Syntax.to_string {
      file_name     = OpamFilename.to_string filename;
      file_contents = [
        Variable (s_opam_version, OpamFormat.make_string (OpamVersion.to_string s.opam_version));
        Variable (s_name, OpamFormat.make_string (OpamCompiler.to_string s.name));
        Variable (s_version, OpamFormat.make_string (OpamCompiler.Version.to_string s.version));
      ] @ (match s.src with
          | None   -> []
          | Some s -> [Variable (s_src, OpamFormat.make_string (OpamFilename.to_string s))]
      ) @ [
        Variable (s_patches     , OpamFormat.make_list (OpamFilename.to_string |> OpamFormat.make_string) s.patches);
        Variable (s_configure   , OpamFormat.make_string_list s.configure);
        Variable (s_make        , OpamFormat.make_string_list s.make);
        Variable (s_build       , OpamFormat.make_list OpamFormat.make_string_list s.build);
        Variable (s_bytecomp    , OpamFormat.make_string_list s.bytecomp);
        Variable (s_asmcomp     , OpamFormat.make_string_list s.asmcomp);
        Variable (s_bytelink    , OpamFormat.make_string_list s.bytelink);
        Variable (s_asmlink     , OpamFormat.make_string_list s.asmlink);
        Variable (s_packages    , OpamFormat.make_formula s.packages);
        Variable (s_requires    , OpamFormat.make_list (OpamVariable.Section.to_string |> OpamFormat.make_string) s.requires);
        Variable (s_env         , OpamFormat.make_list OpamFormat.make_env_variable s.env);
        Variable (s_tags        , OpamFormat.make_string_list s.tags);
      ] @ (match s.pp with
        | None    -> []
        | Some pp -> [ Variable (s_pp, make_ppflag pp) ]
      ) @ (
        if s.preinstalled then
          [ Variable (s_preinstalled, OpamFormat.make_bool s.preinstalled) ]
        else
          []
      )
    }

end

module Comp_descr = struct

  let internal = "comp_descr"

  type t = string

  let empty = ""

  let of_string _ x = x

  let to_string _ x = x

end

module Subst = struct

  let internal = "subst"

  type t = string

  let empty = ""

  let of_string _ str = str

  let to_string _ t = t

  let replace t f =
    let subst str =
      let str = String.sub str 2 (String.length str - 4) in
      let v = OpamVariable.Full.of_string str in
      OpamVariable.string_of_variable_contents (f v) in
    let rex = Re_perl.compile_pat "%\\{[^%]+\\}%" in
    Re_pcre.substitute ~rex ~subst t

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

let reads = Hashtbl.create 64
let writes = Hashtbl.create 64
let incr tbl n =
  let v =
    try Hashtbl.find tbl n
    with _ -> 0 in
  Hashtbl.replace tbl n (v+1)

let reads_t = Hashtbl.create 64
let writes_t = Hashtbl.create 64
let incr_t tbl n t =
  let v =
    try Hashtbl.find tbl n
    with _ -> 0. in
  Hashtbl.replace tbl n (v+.t)

let print_stats () =
  Hashtbl.iter (fun n c -> Printf.eprintf "read(%s): %d - %.02fs\n" n c (Hashtbl.find reads_t n)) reads;
  Hashtbl.iter (fun n c -> Printf.eprintf "write(%s): %d - %02fs\n" n c (Hashtbl.find writes_t n)) writes

let with_time (tbl, tbl_t) n f =
    let t0 = Unix.gettimeofday () in
    let r = f () in
    let t1 =  Unix.gettimeofday () in
    incr tbl n;
    incr_t tbl_t n (t1 -. t0);
    r

let reads = (reads, reads_t)
let writes = (writes, writes_t)

module Make (F : F) = struct

  let log = OpamGlobals.log (Printf.sprintf "FILE(%s)" F.internal)

  let write f v =
    log "write %s" (OpamFilename.to_string f);
    with_time writes F.internal (fun () ->
      OpamFilename.write f (F.to_string f v)
    )

  let read f =
    let filename = OpamFilename.to_string f in
    log "read %s" filename;
    with_time reads F.internal (fun () ->
      if OpamFilename.exists f then
        try F.of_string f (OpamFilename.read f)
        with OpamFormat.Bad_format msg ->
          OpamGlobals.error_and_exit "File %s: %s" (OpamFilename.to_string f) msg
      else
        OpamGlobals.error_and_exit "File %s does not exist" (OpamFilename.to_string f)
    )

  let safe_read f =
    if OpamFilename.exists f then
      read f
    else (
      log "Cannot find %s" (OpamFilename.to_string f);
      F.empty
    )

  let dummy_file = OpamFilename.of_string "<dummy>"

  let read_from_channel ic =
    try F.of_string dummy_file (OpamSystem.string_of_channel ic)
    with OpamFormat.Bad_format msg ->
      OpamGlobals.error_and_exit "%s" msg

  let write_to_channel oc str =
    output_string oc (F.to_string dummy_file str)

end

open X

module type IO_FILE = sig
  type t
  val empty: t
  val write: filename -> t -> unit
  val read : filename -> t
  val safe_read: filename -> t
  val read_from_channel: in_channel -> t
  val write_to_channel: out_channel -> t -> unit
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

module Comp_descr = struct
  include Comp_descr
  include Make (Comp_descr)
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
end

module Dot_config = struct
  include Dot_config
  include Make (Dot_config)
end

module Export = struct
  include Export
  include Make(Export)
end

module Installed = struct
  include Installed
  include Make (Installed)
end

module Installed_roots = struct
  include Installed_roots
  include Make (Installed_roots)
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

module Prefix = struct
  include Prefix
  include Make(Prefix)
end
