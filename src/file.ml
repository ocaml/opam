open ExtString
open ExtList
open Namespace
open Path

type ('a, 'b) text = 
  | Parsed of 'a 
  | Raw of 'b

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
  let assoc_all = assoc (fun v xs -> v :: BatList.takewhile_map (function Raw x -> Some x | Parsed _ -> None) xs)

  let map_parsed f = 
    List.map 
      (function
        | Parsed x -> Parsed (f x)
        | Raw r -> Raw r)       

  module Exceptionless = 
  struct
    let assoc_parsed k l = try Some (assoc_parsed k l) with Not_found -> None
    let assoc_all k l = try Some (assoc_all k l) with Not_found -> None

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
    val opam_version : t -> internal_version
    val sources : t -> url
    val ocaml_version : t -> internal_version

    (** construct *)
    val create : internal_version (* opam *) -> url -> internal_version (* ocaml *) -> t
  end

  module Config : CONFIG =
  struct
    let internal_name = "config"

    type t =
        { version : internal_version (* opam version *)
        ; sources : url
        ; ocaml_version : internal_version }

    let version_of_string s = Version s

    let opam_version t = t.version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version

    let create version sources ocaml_version = { version ; sources ; ocaml_version }

    let empty = {
      version = Version Globals.version;
      sources = url Globals.default_hostname Globals.default_port ;
      ocaml_version = Version Sys.ocaml_version
    }

    let to_string t =
      Printf.sprintf "version: %s\nsources: %s\nocaml-version: %s\n"
        (match t.version with Version s -> s)
        (string_of_url t.sources)
        (match t.ocaml_version with Version s -> s)

    let parse contents =
      let file = Parse.colon contents in
      let version = Parse.Exceptionless.assoc_def Globals.opam_version "version" file in
      let sources =
        try
          let sources = Parse.assoc_parsed "sources" file in
          let hostname, port = BatString.split sources ":" in
          url hostname (try int_of_string port with Not_found -> Globals.default_port)
        with _ ->
          url Globals.default_hostname Globals.default_port in
      let ocaml_version = try Parse.assoc_parsed "ocaml-version" file with Not_found -> Sys.ocaml_version in
      { version = Version version
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

  module type OPAM = sig
    include IO_FILE

    (** destruct *)
    val opam_version : t -> internal_version
    val version : t -> Namespace.version
    val package : t -> bool (* true : installed *) -> Debian.Packages.package
    val description : t -> string

    (** construct *)
  end

  module Opam : OPAM = struct

    let internal_name = "opam"

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
      opam_version : internal_version ;
      version : Namespace.version ;
      list_stanza : (string * string, string) text list ;
    }

    let empty = { opam_version = Version "" ; version = { deb = "" } ; list_stanza = [] }

    let opam_version t = t.opam_version
    let version t = t.version
          
    let s_description = "description"
    let s_user_version = "version"
    let s_opam_version = "opam-version"
    let s_package = "package"
    let s_installed = "status" (* see [Debcudf.add_inst] for more details about the format *)
    let s_installed_true = "  installed" (* see [Debcudf.add_inst] for more details about the format *)
    let s_depends = "depends"
    let s_conflicts = "conflicts"

    let description t = 
      BatIO.to_string
        (BatList.print ~first:"" ~last:"" ~sep:"\n" BatString.print)
        (match Parse.Exceptionless.assoc_all s_description t.list_stanza with
          | None -> [] 
          | Some l -> l)
      
    let default_package t =
      let assoc f s = 
        match Parse.Exceptionless.assoc_parsed s t.list_stanza with
          | None -> []
          | Some l -> f l in

      { D.default_package with 
        D.name = Parse.assoc_parsed s_package t.list_stanza ;
        D.version = t.version.deb ;
        D.extras = [ s_description, description t ] ;
        D.depends = assoc D.parse_vpkgformula s_depends ;
        D.conflicts = assoc D.parse_vpkglist s_conflicts }

    let package t installed =
      let p = default_package t in
      if installed then 
        { p with D.extras = (s_installed, s_installed_true) :: p.D.extras }
      else
        p

    let parse str =
      let list_stanza = Parse.colon str in
      { opam_version = Version (Parse.Exceptionless.assoc_def Globals.version s_opam_version list_stanza)
      ; version = Namespace.version_of_string (Parse.assoc_parsed s_user_version list_stanza)
      ; list_stanza }

    let to_string t =
      BatIO.to_string
        (BatList.print ~first:"" ~last:"" ~sep:"\n" 
           (fun oc txt -> 
             BatString.print oc 
               (match txt with
                 | Raw s -> s
                 | Parsed (k, v) -> k ^ " : " ^ v))) t.list_stanza
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
                  (Namespace.string_user_of_name name) 
                  (Namespace.string_user_of_version version))))
  end

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

  type misc = { p_from : path ; p_to : path }

  let string_of_path (pref, l, b) =
    Printf.sprintf "(%s, %s, %s)" 
      (match pref with Absolute -> "Absolute" | Relative -> "Relative")
      (BatIO.to_string (BatList.print (fun oc (B b) -> BatString.print oc b)) l)
      (match b with Suffix s -> Printf.sprintf "Suffix %s" s | Exact s -> Printf.sprintf "Exact %s" s)

  module type TO_INSTALL =
  sig
    include IO_FILE

    (** destruct *)
    val lib : t -> path list
    val bin : t -> path option
    val misc : t -> misc list

    val path_from : misc -> path
    val path_to : misc -> path
    val string_of_misc : misc -> string

    val filename_of_path_relative : Path.t -> Path.filename (* prefix *) -> path -> Path.filename list
    val filename_of_path_absolute : Path.t -> path -> Path.filename list


    (** construct *)
  end

  module To_install : TO_INSTALL =
  struct
    let internal_name = "to_install"

    type t = 
        { lib : path list
        ; bin : path option
        ; misc : misc list }

    let lib t = t.lib
    let bin t = t.bin
    let misc t = t.misc
    let path_from m = m.p_from
    let path_to m = m.p_to

    let string_of_misc m = 
      Printf.sprintf "from %s to %s" (string_of_path m.p_from) (string_of_path m.p_to)

    let filename_of_path t f l_b suff = 
      let f = List.fold_left Path.concat f l_b in
      List.map (Path.concat f)
        (match suff with
          | Exact name -> [ B name ]
          | Suffix suff ->
            List.filter 
              (fun (B name) -> 
                (try Some (snd (BatString.split name ".")) with _ -> None) = Some suff)
              (match Path.find f with Path.Directory l -> l | _ -> []))

    let filename_of_path_relative t f = function
      | Relative, l_b, suff -> filename_of_path t f l_b suff
      | Absolute, _, _ -> assert false

    let filename_of_path_absolute t = function
      | Absolute, l_b, suff -> filename_of_path t Path.root l_b suff
      | _ -> assert false

    let empty = { lib = []
                ; bin = None
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
      let l_lib_bin, l_misc = 
        let l, f_while = 
          String.nsplit s "\n",
          fun s ->
            match try Some (BatString.split "misc" (BatString.trim s)) with _ -> None with
            | Some ("", _) -> false
            | _ -> true in
        List.takewhile f_while l, List.dropwhile f_while l in

      let l_lib_bin = Parse.parse ":" l_lib_bin in

      
      { lib = 
          (match Parse.Exceptionless.assoc_parsed "lib" l_lib_bin with
            | Some lib -> List.map relative_path_of_string (String.nsplit lib ",")
            | None -> [])
      ; bin = Option.map relative_path_of_string (Parse.Exceptionless.assoc_parsed "bin" l_lib_bin)
      ; misc = 
          List.map
            (fun (s_path, s_fname) -> 
              { p_from = relative_path_of_string s_path ; p_to = b_of_string Absolute s_fname })
            (Parse.filter_parsed (Parse.parse " " l_misc)) }

    let to_string t =

      let path_print oc (pref, l_base, base) = 
        begin
          BatString.print oc (match pref with Absolute -> "/" | Relative -> "");
          BatList.print ~first:"" ~last:"/" ~sep:"/" (fun oc (B base) -> BatString.print oc base) oc l_base;
          BatString.print oc (match base with Suffix s -> Printf.sprintf "*.%s" s | Exact s -> s);
        end in

      Printf.sprintf "
lib: %s
bin: %s
misc:
%s"
        (BatIO.to_string (BatList.print ~first:"" ~last:"" ~sep:", " path_print) t.lib)
        (BatIO.to_string (BatOption.print path_print) t.bin)
        (BatIO.to_string (BatList.print ~first:"" ~last:"" ~sep:"\n" 
                            (fun oc misc -> 
                              begin
                                path_print oc (misc.p_from);
                                BatString.print oc " ";
                                path_print oc (misc.p_to);
                              end)) t.misc)
  end



  module type DESCR =
  sig
    include IO_FILE

    val library : t -> string
    val requires : t -> string list
    val link : t -> string list
    val asmlink : t -> string list
  end

  module Descr : DESCR =
  struct
    let internal_name = "descr"

    type t = 
        { library : string
        ; requires : string list
        ; link : string list
        ; asmlink : string list }

    open ExtString

    let library t = t.library
    let requires t = t.requires
    let link t = t.link
    let asmlink t = t.asmlink

    let parse s = 
      let library, s = 
        let s_beg, s = BatString.split s "{" in
        (match List.filter (function "" -> false | _ -> true) (String.nsplit s_beg " ") with
          | "library" :: name :: _ -> name
          | _ -> failwith "The name of the library is not found"), s in
      let l = Parse.colon s in
      let f_dash dash key = List.map (String.strip ~chars:" ") (String.nsplit dash (Parse.assoc_parsed key l)) in
      { library 
      ; requires = f_dash "," "requires"
      ; link = f_dash "-" "link"
      ; asmlink = f_dash "-" "asmlink" }

    let to_string t = 
      let f_s print_beg motif l =
        BatIO.to_string (BatList.print ~first:(if print_beg then motif else "") ~last:"" ~sep:motif BatString.print) l in
      Printf.sprintf "
library %s {
  %s
  %s
  %s
}" t.library (f_s false ", " t.requires) (f_s true " -" t.link) (f_s true " -" t.asmlink)

    let empty = { library = "" ; requires = [] ; link = [] ; asmlink = [] }
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
  module Opam = struct include Opam include Make (Opam) end
  module To_install = struct include To_install include Make (To_install) end
  module Descr = struct include Descr include Make (Descr) end
  module Security_key = struct include Security_key include Make (Security_key) end

  module Installed =
  struct
    module M_installed = Make (Installed)
      
    let find_map f = N_map.of_list (M_installed.Exceptionless.find f)
      
    let modify_def f f_map = M_installed.add f (N_map.bindings (f_map (find_map f)))
      
    include Installed
    include M_installed
  end
end
