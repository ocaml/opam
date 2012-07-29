(* Script to convert opam 0.3 repositories to 0.4 *)
open Types
open File_format
open Utils

module File_0_3 = struct

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

  let internal = "opam"

  type section = Types.section

  type t = {
    name       : N.t;
    version    : V.t;
    maintainer : string;
    substs     : basename list;
    build_env  : (string * string * string) list;
    build      : string list list;
    remove     : string list list;
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

  let create nv =
    let name = NV.name nv in
    let version = NV.version nv in
    { empty with name; version }

  let s_version     = "version"
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
  let s_opam_version = "opam-version"
    
  (* to convert to cudf *)
  (* see [Debcudf.add_inst] for more details about the format *)
  let s_status = "status" 
    
  (* see [Debcudf.add_inst] for more details about the format *)
  let s_installed   = "  installed" 

  let useful_fields = [
    s_opam_version;
    s_version;
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
        Variable ("opam-version", String Globals.opam_version);
        Section {
          File_format.kind = "package";
          name = N.to_string t.name;
          items = [
            Variable (s_version, String (V.to_string t.version));
            Variable (s_maintainer, String t.maintainer);
            Variable (s_substs, make_list (Basename.to_string |> make_string) t.substs);
            Variable (s_build_env, make_list make_env_variable t.build_env);
            Variable (s_build, make_list (make_list make_string) t.build);
            Variable (s_remove, make_list (make_list make_string) t.remove);
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
        }
      ] 
    } in
    Syntax.to_string filename s

  let parse_command =
    parse_or [
      ("string", parse_string);
      ("symbol", parse_symbol)
    ]

  let of_string filename str =
    let s = Syntax.of_string filename str in
    Syntax.check s valid_fields;
    let opam_version = assoc s.contents s_opam_version parse_string in
    if opam_version <> Globals.opam_version then
      Globals.error_and_exit "%s is not a supported OPAM version" opam_version;
    let package = get_section_by_kind s.contents "package" in
    let name = N.of_string package.File_format.name in
    let s = package.items in
    let parse_commands = parse_or [
      "list",      (fun x -> [parse_list parse_command x]);
      "list-list", parse_list (parse_list parse_command);
    ] in
    let version    = assoc s s_version (parse_string |> V.of_string) in
    let maintainer = assoc s s_maintainer parse_string in
    let substs     = 
      assoc_list s s_substs (parse_list (parse_string |> Basename.of_string)) in
    let build_env = assoc_list s s_build_env (parse_list parse_env_variable) in
    let build      =
      assoc_default Globals.default_build_command s s_build parse_commands in
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

module type F = sig
  val internal : string
  type t
  val empty : t
  val of_string : Filename.t -> Raw.t -> t
  val to_string : Filename.t -> t -> Raw.t
end

module Make (F : F) = struct

  let log = Globals.log (Printf.sprintf "FILE-0.3(%s)" F.internal)

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

module OPAM = struct
  include X
  include Make (X)
end

end

module Path_0_3 = struct

let log fmt = Globals.log "PATH-0.3" fmt

let available dir =
  let files = Filename.list dir in
  let files = List.filter (fun f -> Filename.check_suffix f ".opam") files in
  List.fold_left (fun set file ->
    match NV.of_filename file with
    | None    ->
        log "%s is not a valid package filename!" (Filename.to_string file);
        set
    | Some nv -> NV.Set.add nv set
  ) NV.Set.empty files

let compiler_list dir =
  if Dirname.exists dir then (
    let files = Filename.list dir in
    let files = List.filter (fun f -> Filename.check_suffix f ".comp") files in
    let l =
      List.map
        (Filename.chop_extension
         |> Filename.basename
         |> Basename.to_string
         |> OCaml_V.of_string)
        files in
    OCaml_V.Set.of_list l
  ) else
    OCaml_V.Set.empty

let versions nvset =
  NV.Set.fold (fun nv vset -> V.Set.add (NV.version nv) vset) nvset V.Set.empty

module R = struct

  type t = dirname (* [$opam/repo/$repo/] *)

  let create r =
    Dirname.of_string !Globals.root_path / "repo" / Repository.name r

  let of_path path = path
    
  let root t = t

  let config t = t // "config"

  let opam_dir t = t / "opam"

  let available t = available (opam_dir t)

  let available_versions t n =
    versions (NV.Set.filter (fun nv -> NV.name nv = n) (available t))

  let opam t nv = opam_dir t // (NV.to_string nv ^ ".opam")

  let descr_dir t = t / "descr"

  let descr t nv = descr_dir t // (NV.to_string nv)

  let archive_dir t = t / "archives"

  let archive t nv = archive_dir t // (NV.to_string nv ^ ".tar.gz")

  let updated t = t // "updated"

  let upload t = t / "upload"

  let upload_opam_dir t = upload t / "opam"

  let upload_descr_dir t = upload t / "descr"

  let upload_archives_dir t = upload t / "archives"

  let upload_opam t nv = upload_opam_dir t // (NV.to_string nv ^ ".opam")

  let upload_descr t nv = upload_descr_dir t // NV.to_string nv

  let upload_archives t nv = upload_archives_dir t // (NV.to_string nv ^ ".tar.gz")

  let compiler_dir t = t / "compilers"

  let compiler t ov = compiler_dir t // (OCaml_V.to_string ov ^ ".comp")

  let compiler_list t = compiler_list (compiler_dir t)

  let url_dir t = t / "url"

  let files_dir t = t / "files"

  let url t nv = url_dir t // NV.to_string nv

  let files t nv = files_dir t / NV.to_string nv

end
end

let () =
  let open File_0_3.X in
  let t3 = Path_0_3.R.of_path (Dirname.cwd ()) in
  let t4 = Path.R.of_dirname (Dirname.cwd ()) in
  NV.Set.iter (fun nv ->
    Globals.msg "Processing %s\n" (NV.to_string nv);
    let opam3 = File_0_3.OPAM.read (Path_0_3.R.opam t3 nv) in
    let maintainer =
      if opam3.maintainer = "<none>" then "contact@ocamlpro.com" else opam3.maintainer in
    let opam4 = File.OPAM.make
      ~name:opam3.name ~version:opam3.version ~maintainer
      ~substs:opam3.substs ~build_env:opam3.build_env ~build:opam3.build
      ~remove:opam3.remove ~depends:opam3.depends ~depopts:opam3.depopts
      ~conflicts:opam3.conflicts ~libraries:opam3.libraries ~syntax:opam3.syntax
      ~others:opam3.others ~ocaml_version:opam3.ocaml_version in
    File.OPAM.write (Path.R.opam t4 nv) opam4;
    let mv_file src dst =
    if Filename.exists (src t3 nv) then
      Filename.move (src t3 nv) (dst t4 nv) in
    let mv_dir src dst =
    if Dirname.exists (src t3 nv) then
      Dirname.move (src t3 nv) (dst t4 nv) in
    mv_file Path_0_3.R.descr Path.R.descr;
    mv_file Path_0_3.R.url Path.R.url;
    mv_dir Path_0_3.R.files Path.R.files;
  ) (Path_0_3.R.available t3)
