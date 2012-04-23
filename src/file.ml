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
open Namespace
open Path
open File_format
open Uri
open Protocol

type ('a, 'b) text = 
  | Parsed of 'a 
  | Raw of 'b


type basename_last = 
  | Suffix of string 
    (* Designates a file which have [string] as suffix, ie. cmo, cma, cmi, cmx...
       More generally, a file which name is "file.ssss" will have "ssss" as suffix. *)
    (** By default, every matching value will be considered (ie. the regexp equivalent to ".*" ). *)
  | Exact of string
    
type prefix = 
  | Absolute (** consider that the path begins at "/" *)
  | Relative

type path = prefix * basename list * basename_last

type move = { p_from : path ; p_to : path }

let string_of_path (pref, l, b) =
  Printf.sprintf "(%s, %s, %s)" 
    (match pref with Absolute -> "Absolute" | Relative -> "Relative")
    (BatIO.to_string (BatList.print (fun oc (B b) -> BatString.print oc b)) l)
    (match b with Suffix s -> Printf.sprintf "Suffix %s" s | Exact s -> Printf.sprintf "Exact %s" s)

module Parse =
struct

  let parse motif =
    List.map 
      (fun s -> 
        try Parsed (BatPair.map BatString.trim (BatString.split (BatString.trim s) motif)) with Not_found -> Raw s)

  let split motif s = parse motif (String.nsplit s "\n")

  let filter_parsed = 
    List.filter_map 
      (function 
        | Parsed x -> Some x
        | Raw _ -> None)
    
  let colon = split ":"
  let space = split " "

  let split_comma str =
    if str = "" then
      []
    else
      try List.map String.strip (String.nsplit str ",")
      with _ -> [str]

  let assoc f k0 =
    let rec aux = function
      | x :: xs -> 
        (match x with 
          | Parsed (k1, v) -> 
            if k0 = k1 then f v xs else aux xs
          | Raw _ -> aux xs)
      | [] -> raise Not_found in
    aux

  let assoc_parsed = assoc (fun v _ -> v)
(*
  let assoc_all = assoc (fun v xs -> v :: BatList.takewhile_map (function Raw x -> Some x | Parsed _ -> None) xs)
*)
  let map_parsed f = 
    List.map 
      (function
        | Parsed x -> Parsed (f x)
        | Raw r -> Raw r)       

  module Exceptionless = 
  struct
    let assoc_parsed k l = try Some (assoc_parsed k l) with Not_found -> None
(*  let assoc_all k l = try Some (assoc_all k l) with Not_found -> None *)

    let assoc_def def k l = 
      match assoc_parsed k l with
        | None -> def
        | Some v -> v
  end

end

module Cudf =
struct
  include Cudf
    
  let find_field x key = 
    try Some (List.assoc key x.Cudf.pkg_extra) with Not_found -> None
      
  let description p s_description = 
    match find_field p s_description with
      | Some (`String s) -> s
      | _ -> ""
end

module Base =
struct
  open Namespace

  exception Parsing of string

  module type IO_FILE =
  sig
    type t

    (** Parse a string or raise the exception [Parsing _]. *)
    val parse : string -> t

    val empty : t

    (** Return the content of a file as a string *)
    val to_string : t -> string

    val internal_name : string
  end

  module type CONFIG =
  sig
    include IO_FILE

    val version_of_string : string -> internal_version

    (** destruct *)
    val opam_version : t -> int
    val sources : t -> url list
    val ocaml_version : t -> internal_version

    (** construct *)
    val create :
      int  (* opam *) ->
      url list ->
      internal_version (* ocaml *) ->
      t

    val with_sources : t -> url list -> t
  end

  module Config : CONFIG =
  struct
    let internal_name = "config"

    type t =
        { version : int (* opam version *)
        ; sources : url list
        ; ocaml_version : internal_version }

    let version_of_string s = Version s

    let opam_version t = t.version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version

    let create version sources ocaml_version = { version ; sources ; ocaml_version }
    let with_sources t sources = { t with sources }

    let empty = {
      version = Globals.api_version;
      sources = [url Globals.default_hostname];
      ocaml_version = Version Sys.ocaml_version
    }

   let to_string t =
      Printf.sprintf "version: %d\nsources: %s\nocaml-version: %s\n"
        t.version
        (String.concat ", " (List.map string_of_url t.sources))
        (match t.ocaml_version with Version s -> s)

    let parse contents =
      let file = Parse.colon contents in
      let version = match Parse.Exceptionless.assoc_parsed "version" file with
        | None ->
            Globals.error_and_exit
              "Fatal error: Missing field 'version' in %s/config. Exit."
              !Globals.root_path
        | Some v ->
            try int_of_string v
            with _ ->
              Globals.error_and_exit
                "Fatal error: invalid value for 'version' field in %s/config. Exit"
                !Globals.root_path in
      let sources =
        Parse.assoc_parsed "sources" file in
      let sources = Parse.split_comma sources in
      let one source =
        let uri, hostname = uri_of_url source in
        url ?uri hostname in
      let sources = List.map one sources in
      let ocaml_version = try Parse.assoc_parsed "ocaml-version" file with Not_found -> Sys.ocaml_version in
      { version = version
      ; sources
      ; ocaml_version = Version ocaml_version }
  end

  module type CUDF =
  sig
    include IO_FILE

    (** Constructor *)
    val create: Cudf.preamble option -> Cudf.package list -> Cudf.request option -> t

    (** Getters *)
    val packages: t -> Cudf.package list
  end

  module type SPEC = sig
    include IO_FILE

    (** destruct *)
    val name        : t -> name
    val version     : t -> version
    val make        : t -> Run.command list
    val sources     : t -> links
    val patches     : t -> links

    (** Returns the list of sentences *)
    val description : t -> string list

    (** Convert to Debian packages to feed the solver *)
    val to_package : t -> bool (* true : installed *) -> Debian.Packages.package

    (** Delete in patches pointers to local filename *)
    val filter_external_ressources : t -> t

    (** Classic printing function *)
    val string_of_command : Run.command list -> string
  end

  module Spec : SPEC = struct

    let internal_name = "spec"

    module D = struct
      module D = Debian.Packages

      module D_add = struct
        let f g s = g ((Lexing.dummy_pos, Lexing.dummy_pos), s)
        let parse_vpkgformula = f D.parse_vpkgformula
        let parse_vpkglist = f D.parse_vpkglist
      end

      include D
      include D_add
    end

    open BatMap

    type t = {
      name       : string;
      version    : string;
      description: string list;
      sources    : links;
      patches    : links;
      make       : Run.command list;
      depends    : Debian.Format822.vpkgformula;
      conflicts  : Debian.Format822.vpkglist;
      fields     : (string * string) list;
    }

    let empty = {
      name        = "<none>";
      version     = "<none>";
      description = ["empty package"];
      sources     = [];
      make        = [];
      fields      = [];
      depends     = [];
      conflicts   = [];
      patches     = [];
    }

    let s_description = "description"
    let s_version     = "version"
    let s_status      = "status"      (* see [Debcudf.add_inst] for more details about the format *)
    let s_installed   = "  installed" (* see [Debcudf.add_inst] for more details about the format *)
    let s_depends     = "depends"
    let s_conflicts   = "conflicts"
    let s_sources     = "sources"
    let s_patches     = "patches"
    let s_make        = "make"

    let valid_fields = [
      s_description;
      s_version;
      s_depends;
      s_conflicts;
      s_sources;
      s_patches;
      s_make;
    ]
      
    let real_path = 
      List.map
        (function
          | Internal s, o -> Internal (Run.real_path s), o
          | External (s1, s2), o -> External (s1, s2), o)

    let description t = t.description
    let name t = Namespace.name_of_string t.name
    let version t = Namespace.version_of_string t.version
    let sources t = real_path t.sources
    let patches t = real_path t.patches
    let make t = t.make

    let default_package (t:t) =
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

    let to_string, string_of_command = 
      let open Printf in
      let pf p_S (k, v) = sprintf "  %s = %s\n" k (p_S v) in
      let p_concat f l = String.concat " ; " (List.map f l) in
      let p_s = sprintf "%s" in
      let p_S = sprintf "%S" in
      let pl_S = p_concat p_S in
      let pl_s = p_concat (sprintf "%s") in
      let plist = sprintf "[ %s ]" in
      let pl f_S k l = sprintf "  %s = %s\n" k (plist (f_S l)) in
      let plf k l = 
        pl 
          (p_concat (function 
            | s, None -> p_S s
            | s, Some s_to -> plist (pl_S [ s ; s_to ])))
          k
          (List.map
             (function s, o -> 
               (match s with
                 | Internal s        -> sprintf "local://%s" s
                 | External (uri, s) -> sprintf "%s%s" (string_of_uri uri) s), o) l) in
      let string_of_command l = 
        List.map (function 
          | Run.Sh l -> plist (pl_S l)
          | Run.OCaml s -> 
              Printf.sprintf "%c%s%c" 
                Lexer.escape_sharp
                (String.replace_chars
                   (fun c ->
                     sprintf "%s%c"
                       (if c = Lexer.escape_sharp then
                           "\\" 
                        else
                           "")
                       c) s)
                Lexer.escape_sharp) l in
      let plc k l = pl pl_s k (string_of_command l) in
      let pvpkgl = function
        | [] -> "[]"
        | l ->
          plist
            (p_concat (fun s -> plist (p_concat p_S s)) 
               (List.map (function
                 | s, None -> [ s ] 
                 | s, Some (rel, v) -> [s ; rel ; v]) l)) in
      (fun t ->
        sprintf "@%d\n\npackage %S {\n%s%s%s%s%s%s\n}\n"
          Globals.api_version t.name

          (String.concat "" (List.map (pf p_S) t.fields))
          (plc s_make t.make)
          (plf s_sources t.sources)
          (plf s_patches t.patches)
          (pl (fun l -> p_concat p_s (List.map pvpkgl l)) s_depends t.depends)
          (pf p_s (s_conflicts, pvpkgl t.conflicts))), 
      fun l -> pl_s (string_of_command l)


    let filter_external_ressources t = 
      { t with sources = [] ; patches = [] }

    let parse str =
      let lexbuf = Lexing.from_string str in
      let file = Parser.main Lexer.token lexbuf in
      if file.File_format.version <> Globals.api_version then
        Globals.error_and_exit "Incompatible software versions";
      let statement = match file.statements with
        | [s] -> s
        | []  -> Globals.error_and_exit "No package defined"
        | _   -> Globals.error_and_exit "Too many packages defined" in
      if statement.kind <> "package" then
        Globals.error_and_exit "%s: bad format (was waiting for 'package')" statement.kind;
      if not (is_valid statement valid_fields) then
        Globals.error_and_exit "%s contains some invalid fields" str;
      let version = string s_version statement in
      let description =
        try match List.assoc s_description statement.contents with
          | String s -> String.nsplit s "\\"
          | _        -> Globals.error_and_exit "Field 'description': bad format"  
        with Not_found -> [] in
      let sources = parse_l_url s_sources statement in
      let patches = parse_l_url s_patches statement in
      let make = 
        let make = command s_make statement in 
        if make = [] then [ Run.Sh [ "./build.sh" ] ] else make in
      let depends = vpkgformula s_depends statement in
      let conflicts = vpkglist s_conflicts statement in

      let fields = (* warning : 'List ...' values are not kept in [statement.contents] (only 'String ...') *)
        let unstring accu (k,v) =
          match v with
          | String s -> (k, s) :: accu
          | _        -> accu  in
        List.fold_left unstring [] statement.contents in
      { version
      ; description
      ; fields
      ; sources
      ; patches
      ; make
      ; depends
      ; conflicts
      ; name   = statement.File_format.name }
  end


  module type INSTALLED =
  sig
    include IO_FILE with type t = name_version list
  end

  module Installed : INSTALLED =
  struct
    let internal_name = "install"

    type t = name_version list
    let empty = []

    let parse s =
      List.map (fun (name, version) -> Name name, version_of_string version) (Parse.filter_parsed (Parse.space s))

    let to_string = 
      BatIO.to_string
        (BatList.print ~first:"" ~last:"" ~sep:"\n" 
           (fun oc (name, version) ->
             BatString.print oc
               (Printf.sprintf "%s %s" 
                  (Namespace.string_of_name name) 
                  (Namespace.string_of_version version))))
  end

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
      Run.in_dir (Filename.dirname filename) (function
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
