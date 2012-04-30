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

open ExtString
open ExtList
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
  val of_string : string -> t

  (** Return the content of a file as a string *)
  val to_string : t -> string

end

module Lines : IO_FILE with type t = string list list = struct

  let kind = "lines"

  (* Lines of space separated words *)
  type t = string list list

  let empty = []

  let of_string raw =
    let ic = BatIO.of_string (Raw.to_string raw) in
    let split s = String.nspint s " \t" in
    let lines = ref [] in
    let fini = ref false in
    let rec aux () =
      match
        try Some read_line ic
        with _ -> None
      with
      | None   -> ()
      | Some l -> lines := split l :: !lines; aux () in
    aux ();
    close_in ic;
    List.rev !lines

  let to_string lines =
    let buf = Buffer.create 1024 in
    List.iter (fun l ->
      Buffer.add_string buf (String.concat " " l);
      Buffer.add_string buf "\n"
    ) lines in
  Buffer.contents buf

end

module Installed : sig
  include IO_FILE with type t = name_version list
end = struct

  let kind = "installed"

  type t = NV.t list

  let empty = []
    
  let of_string s =
    let lines = Lines.of_string s in
    List.map (function
      | [name; version] -> NV.create (N.of_string name) (V.of_string version)
      | _               -> Globals.error_and_exit "installed"
    ) lines

    let to_string lines =
      let lines =
        List.map
          (fun (n,v) -> Printf.sprintf "%s %s" (N.to_string n) (V.to_string v))
          lines in
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
        (to_string f)
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
          Variable (s_substs, make_list (make_string |> File.to_string) t.substs);
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
    let ocaml_version = assoc s s_ocaml_version parse_string in
    match assoc_section s "package" with
    | [ name, s ] ->
        let s = s.items in
        let version = assoc s s_version (parse_string |> V.of_string) in
        let maintainer = assoc s s_maintainer parse_string in
        let substs = assoc s s_substs (parse_list (parse_string |> File.of_string)) in
        let build = assoc s s_build (parse_list (parse_list parse_string)) in
        let depends = assoc s s_build parse_or_formula in
        let conflicts = assoc s s_build parse_and_formula in
        let libraries = assoc s s_libraries (parse_list parse_string) in
        let syntax = assoc s s s_libraries (parse_list parse_string) in
        { name; version; maintainer; substs; build;
          depends; conflicts; libraries; syntax }
end

XXX


  module type TO_INSTALL =
  sig
    include IO_FILE

    (** destruct *)
    val lib : t -> path list
    val bin : t -> move list
    val misc : t -> move list

    val path_from : move -> path
    val path_to : move -> path
    val string_of_move : move -> string

    val filename_of_path_relative : Path.filename (* prefix *) -> path -> Path.filename list
    val filename_of_path_absolute : path -> Path.filename list
    val filename_of_path : Path.filename -> path -> Path.filename list

    (** construct *)
  end

  module To_install : TO_INSTALL =
  struct
    let internal_name = "to_install"

    type t = 
        { lib : path list
        ; bin : move list
        ; misc: move list }

    let lib t = t.lib
    let bin t = t.bin
    let misc t = t.misc
    let path_from m = m.p_from
    let path_to m = m.p_to

    let string_of_move m = 
      Printf.sprintf "from %s to %s" (string_of_path m.p_from) (string_of_path m.p_to)

    let filename_of_path f l_b suff = 
      let f = List.fold_left Path.concat f l_b in
      List.map (Path.concat f)
        (match suff with
          | Exact name -> [ B name ]
          | Suffix suff ->
            List.filter 
              (fun (B name) -> 
                (try Some (snd (BatString.split name ".")) with _ -> None) = Some suff)
              (match Path.find f with Path.Directory l -> l | _ -> []))

    let filename_of_path_relative f = function
      | Relative, l_b, suff -> filename_of_path f l_b suff
      | Absolute, _, _ -> assert false

    let filename_of_path_absolute = function
      | Absolute, l_b, suff -> filename_of_path Path.root l_b suff
      | _ -> assert false

    let filename_of_path f p = match p with
      | Absolute, _, _ -> filename_of_path_absolute p
      | Relative, _, _ -> filename_of_path_relative f p

    let empty = { lib  = []
                ; bin  = []
                ; misc = [] }

    let b_of_string abs s = 
      let l = String.nsplit (String.strip ~chars:" /" s) "/" in
      match List.rev l with
        | x :: xs ->
          abs,
          List.map (fun s -> B s) (List.rev xs), 
          (match BatString.Exceptionless.split x "*." with
            | Some ("", suff) -> Suffix suff
            | _ -> Exact x)
        | [] -> abs, [], Exact ""

    let relative_path_of_string = b_of_string Relative

    let parse s =
      let file = Parser.main Lexer.token (Lexing.from_string s) in
      let one accu s =
        if s.kind <> "install" then
          Globals.error_and_exit "Bad format: expecting 'install', got %s" s.kind;
        let cp = List.map relative_path_of_string in
        let mv f_to = List.map
          (fun (k,v) -> { p_from = relative_path_of_string k;
                          p_to   = f_to v }) in
        { lib  = cp (string_list "lib" s)  @ accu.lib;
          bin  = mv relative_path_of_string (pair_opt_list "bin"  s) @ accu.bin;
          misc = mv (b_of_string Absolute)  (pair_list "misc" s) @ accu.misc } in
      List.fold_left one empty file.statements

    let to_string t =

      let print (pref, l_base, base) = 
        let prefix = match pref with Absolute -> "/" | Relative -> "" in
        let path = match l_base with
          | [] -> ""
          | _  -> String.concat "/" (List.map (fun (B base) -> base) l_base) in
        let suffix = match base with Suffix s -> Printf.sprintf "*.%s" s | Exact s -> s in
        Printf.sprintf "%s%s%s" prefix path suffix in

      let print_list = List.map print in
      let print_moves = 
        List.map (fun m ->
          Printf.sprintf "%s %s" (print m.p_from) (print m.p_to)
        ) in
      Printf.sprintf "\
        lib: %s\n\
        bin: %s\n\
        misc:\n\
        %s"
        (String.concat ", " (print_list t.lib))
        (String.concat ", " (print_moves t.bin))
        (String.concat "\n" (print_moves t.misc))
  end



  (* Package config X.config *)
  module type PCONFIG =
  sig
    include IO_FILE

    val library_names   : t -> string list
    val link_options    : t -> string list
    val asmlink_options : t -> string list
    val bytelink_options: t -> string list
  end

  module PConfig : PCONFIG =
  struct
    let internal_name = "pconfig"

    type library =
        { name    : string
        ; link    : string list
        ; bytelink: string list
        ; asmlink : string list }

    type elt = Library of library

    type t = elt list

    open ExtString

    let libraries t =
      List.fold_left (fun accu -> function Library l -> l :: accu) [] t

    let library_names t = List.map (fun l -> l.name) (libraries t)

    let options f t = List.flatten (List.map (fun l -> f l) (libraries t))

    let link_options = options (fun l -> l.link)

    let asmlink_options = options (fun l -> l.asmlink)

    let bytelink_options = options (fun l -> l.bytelink)

    let parse_library s =
      { name     = s.File_format.name;
        link     = string_list "link" s;
        bytelink = string_list "bytelink" s;
        asmlink  = string_list "bytelink" s }

    let parse s =
      let file = Parser.main Lexer.token (Lexing.from_string s) in
      let parse_statement s = match s.kind with
        | "library" -> Library (parse_library s)
        | _         -> Globals.error_and_exit "Bad format: unknown kind '%s'" s.kind in
      List.map parse_statement file.statements

    let string_of_string_list l =
      let p = Printf.sprintf "%S" in
      match l with
      | [] -> "[]"
      | _  ->
        let elts = List.map p l in
        Printf.sprintf "[ %s ]" (String.concat "; " elts)

    let string_of_library t =
      let p (k,v) = match v with
        | [] -> ""
        | _  -> Printf.sprintf "  %s = %s;\n" k (string_of_string_list v) in
      let fields = List.map p [
        ("link"    , t.link);
        ("bytelink", t.bytelink);
        ("asmlink", t.asmlink);
      ] in
      Printf.sprintf "library %s {\n%s}\n" t.name (String.concat "" fields)

    let to_string l =
      let aux (Library l) = string_of_library l in
      String.concat "\n\n" (List.map aux l)

    let empty = []
  end

  module type SECURITY_KEY =
  sig
    include IO_FILE with type t = security_key
  end

  module Security_key : SECURITY_KEY =
  struct
    let internal_name = "security_key"

    type t = security_key

    let parse s = Random s
    let to_string (Random s) = s

    let empty = Random ""
  end

  module type COMPIL = sig
    include IO_FILE

    val name: t -> string
    val source: t -> url
    val configure: t -> string list
    val make: t -> string list
    val patches: t -> url list

  end

  module Compil : COMPIL = struct

    let internal_name = "compiler"

    type t = {
      name: string;
      source: url;
      patches: url list;
      configure: string list;
      make: string list;
    }

    let empty = {
      name = "<none>";
      source = url "<none>";
      patches = [];
      configure = [];
      make = [];
    }

    let name t = t.name
    let source t = t.source
    let patches t = t.patches
    let configure t = t.configure
    let make t = t.make

    let to_string t =
      Printf.sprintf "\
name: %s
source: %s
patches: %s
configure: %s
make: %s
"
        t.name
        (string_of_url t.source)
        (String.concat ", " (List.map string_of_url t.patches))
        (String.concat ", " t.configure)
        (String.concat ", " t.make)

    let parse contents =
      let file = Parse.colon contents in
      let name = Parse.assoc_parsed "name" file in
      let source = url (Parse.assoc_parsed "source" file) in
      let patches = match Parse.Exceptionless.assoc_parsed "patches" file with
      | None   -> []
      | Some s -> List.map url (Parse.split_comma s) in
      let configure = match Parse.Exceptionless.assoc_parsed "configure" file with
      | None   -> []
      | Some s -> Parse.split_comma s in
      let make = match Parse.Exceptionless.assoc_parsed "make" file with
      | None   -> []
      | Some s -> Parse.split_comma s in
      { name; source; patches; configure; make }
        
  end
end

exception Directory_found

module Make (F : Base.IO_FILE) = 
struct
  let log = Globals.log ("FILE." ^ F.internal_name)

  (** Add a file *)
  let add f v =
    log "add %s" (Path.string_of_filename f);
    Path.add f (Path.File (Binary (Raw_binary (F.to_string v))))

  (** Find a file. Return [None] if the file does not exists.
      Raise [Parsing] or [Directory_found] in case an error happens. *)
  let find f =
    let filename = Path.string_of_filename f in
    log "find %s" filename;
    let dirname = Filename.dirname filename in
    if not (Sys.file_exists dirname) then
      None
    else
      Run.U.in_dir (Filename.dirname filename) (function
        | Path.File (Raw_binary s)     -> Some (F.parse s)
        | Path.Not_found _             -> None
        | Path.Directory _             -> raise Directory_found
      ) (Path.find_binary f)


  (** Find a file. Exit the program if the file does not exists.
      Raise [Parsing] or [Directory] in case another error happen. *)
  let find_err = Path.read find

  module Exceptionless =
  struct
    (** Find a file. Return a default value [v0] if the file does not exists. 
        In general, forall [v1], [compare v0 v1] < 0. *)
    let default def f = 
      match try Some (find f) with _ -> None with
      | Some (Some t) -> t
      | _ -> def
          
    let find = default F.empty
  end
end


module File =
struct
  open Base

  module Config = struct include Config include Make (Config) end
  module Spec = struct include Spec include Make (Spec) end
  module To_install = struct include To_install include Make (To_install) end
  module PConfig = struct include PConfig include Make (PConfig) end
  module Security_key = struct include Security_key include Make (Security_key) end
  module Compil = struct include Compil include Make (Compil) end

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
