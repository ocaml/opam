open ExtString
open ExtList
open Namespace
open Path

module File =
struct
  open Namespace

  module type IO_FILE =
  sig
    type t

    (** Parse a string *)
    val parse: string -> t

    (** Return the content of a file as a string *)
    val to_string: t -> string

    (** Find a file. Return [None] if the file does not exists *)
    val find : Path.filename -> t option

    (** Find a file. Return a default value [v0] if the file does not exists. 
        In general, forall [v1], [compare v0 v1] < 0. *)
    val find_default : Path.filename -> t

    (** Add a file *)
    val add : Path.filename -> t -> unit
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

  type ('a, 'b) text = 
    | Parsed of 'a 
    | Raw of 'b

  module Parse =
  struct

    let parse motif =
      List.map 
        (fun s -> 
          try Parsed (BatPair.map BatString.trim (String.split (BatString.trim s) motif)) with Not_found -> Raw s)

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
    end

  end

  module Config : CONFIG =
  struct
    type t =
        { version : internal_version (* opam version *)
        ; sources : url
        ; ocaml_version : internal_version }

    let version_of_string s = Version s

    let opam_version t = t.version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version

    let create version sources ocaml_version = { version ; sources ; ocaml_version }

    let default = {
      version = Version Globals.version;
      sources = url Globals.default_hostname Globals.default_port ;
      ocaml_version = Version Sys.ocaml_version
    }

    let to_string t =
      Printf.sprintf "version: %s\nsources: %s\nocaml-version: %s\n"
        (match t.version with Version s -> s)
        (string_of_url t.sources)
        (match t.ocaml_version with Version s -> s)

    let log fmt =
      Globals.log "FILE.CONFIG" fmt

    let parse contents =
      let file = Parse.colon contents in
      let version = try Parse.assoc_parsed "version" file with Not_found -> Globals.opam_version in
      let sources =
        try
          let sources = Parse.assoc_parsed "sources" file in
          let hostname, port = String.split sources ":" in
          url hostname (try int_of_string port with Not_found -> Globals.default_port)
        with _ ->
          url Globals.default_hostname Globals.default_port in
      let ocaml_version = try Parse.assoc_parsed "ocaml-version" file with Not_found -> Sys.ocaml_version in
      { version = Version version
      ; sources
      ; ocaml_version = Version ocaml_version }

    let find f =
      log "find %s" (Path.string_of_filename f);
      match Path.find_binary f with
      | Path.File (Raw_binary s)     -> Some (parse s)
      | Path.Not_found _             -> None
      | Path.Directory _ -> failwith (Printf.sprintf "%s is a directory" (Path.string_of_filename f))

    let add f v =
      log "add %s" (Path.string_of_filename f);
      Path.add f (Path.File (Binary (Raw_binary (to_string v))))

    let find_default f = match find f with None -> default | Some t -> t
  end

  module type CUDF =
  sig
    include IO_FILE

    (** Constructor *)
    val create: Cudf.preamble option -> Cudf.package list -> Cudf.request option -> t

    (** Getters *)
    val packages: t -> Cudf.package list
  end

  (* If next modules wants to refer to Cudf *)
  module C = 
  struct
    include Cudf

    let find_field x key = 
      try Some (List.assoc key x.Cudf.pkg_extra) with Not_found -> None

    let description p s_description = 
      match find_field p s_description with
        | Some (`String s) -> s
        | _ -> ""
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

    module D = Debian.Packages

    open BatMap

    type t = {
      opam_version : internal_version ;
      version : Namespace.version ;
      list_stanza : (string * string, string) text list ;
    }

    let opam_version t = t.opam_version
    let version t = t.version
          
    let s_description = "description"
    let s_user_version = "version"
    let s_opam_version = "opam-version"
    let s_package = "package"
    let s_installed = "status" (* see [Debcudf.add_inst] for more details about the format *)
    let s_installed_true = "  installed" (* see [Debcudf.add_inst] for more details about the format *)

    let description t = 
      BatIO.to_string
        (BatList.print ~first:"" ~last:"" ~sep:"\n" BatString.print)
        (match Parse.Exceptionless.assoc_all s_description t.list_stanza with
          | None -> [] 
          | Some l -> l)
      
    let default_package t =
      { D.default_package with 
        D.name = Parse.assoc_parsed s_package t.list_stanza ;
        D.version = t.version.deb ;
        D.extras = [ s_description, description t ] }

    let package t installed =
      let p = default_package t in
      if installed then 
        { p with D.extras = (s_installed, s_installed_true) :: p.D.extras }
      else
        p

    let parse str =
      let list_stanza = Parse.colon str in
      { opam_version = Version (match Parse.Exceptionless.assoc_parsed s_opam_version list_stanza with None -> Globals.version | Some v -> v)
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

    let log fmt =
      Globals.log "FILE.CONFIG" fmt

    let find f =
      log "find %s" (Path.string_of_filename f);
      match Path.find_binary f with
        | Path.File (Raw_binary s) -> 
          (try Some (parse s)
           with _ -> failwith ("Error while parsing " ^ Path.string_of_filename f))
        | Path.Not_found _ -> None
        | _ -> raise Not_found

    let add f v =
      log "add %s" (Path.string_of_filename f);
      Path.add f (Path.File (Binary (Raw_binary (to_string v))))

    let find_default f = match find f with None -> failwith "to complete !" | Some t -> t
  end


  module type INSTALLED =
  sig
    include IO_FILE with type t = name_version list

    (** Same as [add] but the map we operating on is retrieved after a [find_default]. *)
    val modify_def : Path.filename -> (version N_map.t -> version N_map.t) -> unit

    (** see [find_default] *)
    val find_map : Path.filename -> version N_map.t
  end

  module Installed : INSTALLED =
  struct
    type t = name_version list
    let empty = []

    let parse s =
      List.map (fun (name, version) -> Name name, version_of_string version) (Parse.filter_parsed (Parse.space s))

    let find f = 
      match Path.find_binary f with
        | Path.File (Raw_binary s) -> Some (parse s)
        | Path.Not_found _         -> Some empty
        | _ -> assert false

    let to_string = 
      BatIO.to_string
        (BatList.print ~first:"" ~last:"" ~sep:"\n" 
           (fun oc (name, version) ->
             BatString.print oc
               (Printf.sprintf "%s %s" 
                  (Namespace.string_user_of_name name) 
                  (Namespace.string_user_of_version version))))

    let add f v = Path.add f (Path.File (Binary (Raw_binary (to_string v))))

    let find_default f = match find f with None -> empty | Some t -> t

    let find_map f = N_map.of_list (find_default f)

    let modify_def f f_map = add f (N_map.bindings (f_map (find_map f)))
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
    val bin : t -> path
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
    type t = 
        { lib : path list
        ; bin : path
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
                (try Some (snd (String.split name ".")) with _ -> None) = Some suff)
              (match Path.find f with Path.Directory l -> l | _ -> []))

    let filename_of_path_relative t f = function
      | Relative, l_b, suff -> filename_of_path t f l_b suff
      | Absolute, _, _ -> assert false

    let filename_of_path_absolute t = function
      | Absolute, l_b, suff -> filename_of_path t Path.root l_b suff
      | _ -> assert false

    let empty = { lib = []
                ; bin = Relative, [], Suffix ""
                ; misc = [] }

    let b_of_string abs s = 
      let l = String.nsplit (String.strip ~chars:"/" s) "/" in
      match List.rev l with
        | x :: xs ->
          abs,
          List.map (fun s -> B s) (List.rev xs), 
          (match try Some (String.split x "*.") with _ -> None with
            | Some ("", suff) -> Suffix suff
            | _ -> Exact x)
        | [] -> abs, [], Exact ""

    let relative_path_of_string = b_of_string Relative

    let parse s =
      let l_lib_bin, l_misc = 
        let l, f_while = 
          String.nsplit s "\n",
          fun s ->
            match try Some (String.split "misc" (BatString.trim s)) with _ -> None with
            | Some ("", _) -> true
            | _ -> false in
        List.takewhile f_while l, List.dropwhile f_while l in

      (match Parse.parse ":" l_lib_bin with 
      |  Parsed ("lib", lib)
      :: Parsed ("bin", bin) :: _ -> 
          { lib = List.map relative_path_of_string (String.nsplit lib ",")
          ; bin = relative_path_of_string bin 
          ; misc = 
              List.map
                (fun (s_path, s_fname) -> 
                  { p_from = relative_path_of_string s_path ; p_to = b_of_string Absolute s_fname })
                (Parse.filter_parsed (Parse.parse " " l_misc)) }
      | _ -> empty)

    let find f =
      match Path.find_binary f with
        | Path.File (Raw_binary s) -> Some (parse s)
        | Path.Not_found _         -> Some empty
        | _ -> assert false

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
        (BatIO.to_string path_print t.bin)
        (BatIO.to_string (BatList.print ~first:"" ~last:"" ~sep:"\n" 
                            (fun oc misc -> 
                              begin
                                path_print oc (misc.p_from);
                                BatString.print oc " ";
                                path_print oc (misc.p_to);
                              end)) t.misc)

    let add f v =
      Path.add f (Path.File (Binary (Raw_binary (to_string v))))

    let find_default f = match find f with None -> failwith "to complete !" | Some t -> t
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
        let s_beg, s = String.split s "{" in
        (match List.filter (function "" -> false | _ -> true) (String.nsplit s_beg " ") with
          | "library" :: name :: _ -> name
          | [] -> failwith "The name of the library is not found"), s in
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

    let empty = 
      { library = "" ; requires = [] ; link = [] ; asmlink = [] }

    let find f = 
      match Path.find_binary f with
        | Path.File (Raw_binary s) -> Some (parse s)
        | Path.Not_found _         -> Some empty
        | _ -> assert false


    let add f v =
      Path.add f (Path.File (Binary (Raw_binary (to_string v))))

    let find_default f = match find f with None -> failwith "to complete !" | Some t -> t
  end

end
