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
    val sources : t -> url
    val ocaml_version : t -> internal_version

    (** construct *)
    val create : int (* opam *) -> url -> internal_version (* ocaml *) -> t
  end

  module Config : CONFIG =
  struct
    let internal_name = "config"

    type t =
        { version : int (* opam version *)
        ; sources : url
        ; ocaml_version : internal_version }

    let version_of_string s = Version s

    let opam_version t = t.version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version

    let create version sources ocaml_version = { version ; sources ; ocaml_version }

    let empty = {
      version = Globals.api_version;
      sources = url Globals.default_hostname Globals.default_port ;
      ocaml_version = Version Sys.ocaml_version
    }

   let to_string t =
      Printf.sprintf "version: %d\nsources: %s\nocaml-version: %s\n"
        t.version
        (string_of_url t.sources)
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
        try
          let sources = Parse.assoc_parsed "sources" file in
          let hostname, port = BatString.split sources ":" in
          url hostname (try int_of_string port with Not_found -> Globals.default_port)
        with _ ->
          url Globals.default_hostname Globals.default_port in
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
    val name        : t -> string
    val version     : t -> Namespace.version
    val urls        : t -> string list
    val make        : t -> string list

    (** Returns the list of sentences *)
    val description : t -> string list

    (** Convert to Debian packages to feed the solver *)
    val to_package : t -> bool (* true : installed *) -> Debian.Packages.package

    val urls : t -> string list
    val make : t -> string list
    val patches : t -> string list
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
      urls       : string list;
      patches    : string list;
      make       : string list;
      fields     : (string * string) list;
    }

    let empty = {
      name        = "<none>";
      version     = "<none>";
      description = ["empty package"];
      urls        = [];
      make        = [];
      fields      = [];
      patches     = [];
    }

    let s_description = "description"
    let s_version     = "version"
    let s_status      = "status"      (* see [Debcudf.add_inst] for more details about the format *)
    let s_installed   = "  installed" (* see [Debcudf.add_inst] for more details about the format *)
    let s_depends     = "depends"
    let s_conflicts   = "conflicts"
    let s_urls        = "urls"
    let s_patches     = "patches"
    let s_make        = "make"

    let description t = t.description
    let name t = t.name
    let version t = {deb = t.version}
    let urls t = t.urls
    let patches t = t.patches
    let make t = t.make

    let default_package (t:t) =
      let assoc f s =
        try f (List.assoc s t.fields)
        with Not_found -> [] in

      { D.default_package with 
        D.name      = t.name ;
        D.version   = t.version ;
        D.extras    = [] ;
        D.depends   = assoc D.parse_vpkgformula s_depends ;
        D.conflicts = assoc D.parse_vpkglist s_conflicts }

    let to_package t installed =
      let p = default_package t in
      if installed then 
        { p with D.extras = (s_status, s_installed) :: p.D.extras }
      else
        p

    let to_string t =
      let pf (k, v) = Printf.sprintf "  %s = %S\n" k v in
      let ps = Printf.sprintf "%S"in
      let pl k l =
        Printf.sprintf "  %s = [%s]\n" k
          (String.concat "; " (List.map ps l)) in
      Printf.sprintf "@%d\n\npackage %S {\n%s%s%s\n}\n"
        Globals.api_version t.name
        (String.concat "" (List.map pf t.fields))
        (pl s_urls t.urls)
        (pl s_patches t.patches)

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
      let version = string s_version statement in
      let description =
        try match List.assoc s_description statement.contents with
          | String s -> String.nsplit s "\\"
          | _        -> Globals.error_and_exit "Fied 'description': bad format"  
        with Not_found -> [] in
      let urls = string_list s_urls statement in
      let patches = string_list s_patches statement in
      let make = 
        let make = string_list s_make statement in 
        if make = [] then [ "./build.sh" ] else make in
      let fields =
        let unstring accu (k,v) =
          match v with
          | String s -> (k, s) :: accu
          | _        -> accu  in
        List.fold_left unstring [] statement.contents in
      { version
      ; description
      ; fields
      ; urls
      ; patches
      ; make
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
          bin  = mv relative_path_of_string (pair_list "bin"  s) @ accu.bin;
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

end


exception Directory_found

module Make (F : Base.IO_FILE) = 
struct
  let log = Globals.log F.internal_name

  (** Add a file *)
  let add f v =
    log "add %s" (Path.string_of_filename f);
    Path.add f (Path.File (Binary (Raw_binary (F.to_string v))))

  (** Find a file. Return [None] if the file does not exists.
      Raise [Parsing] or [Directory_found] in case an error happens. *)
  let find f =
    log "find %s" (Path.string_of_filename f);
    match Path.find_binary f with
      | Path.File (Raw_binary s)     -> Some (F.parse s)
      | Path.Not_found _             -> None
      | Path.Directory _             -> raise Directory_found

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

  module Installed =
  struct
    module M_installed = Make (Installed)
      
    let find_map f = N_map.of_list (M_installed.Exceptionless.find f)
      
    let modify_def f f_map =
      M_installed.add f (N_map.bindings (f_map (find_map f)))
      
    include Installed
    include M_installed
  end
end
