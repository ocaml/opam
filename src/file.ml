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

  let filter motif =
    BatList.filter_map 
      (fun s -> 
        try Some (BatPair.map BatString.trim (BatString.split (BatString.trim s) motif)) with Not_found -> None)

  let parse motif s = filter motif (BatString.nsplit s "\n")

  let parse_colon = parse ":"
  let parse_space = parse " "

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
      let file = parse_colon contents in
      let version = try List.assoc "version" file with _ -> Globals.opam_version in
      let sources =
        try
          let sources = List.assoc "sources" file in
          let hostname, port =  BatString.split sources ":" in
          url hostname (try int_of_string port with _ -> Globals.default_port)
        with _ ->
          url Globals.default_hostname Globals.default_port in
      let ocaml_version = try List.assoc "ocaml-version" file with _ -> Sys.ocaml_version in
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
  module C = Cudf

  module Cudf : CUDF = 
  struct
    type t = 
        { preamble : Cudf.preamble option
        ; pkgs : Cudf.package list
        ; request : Cudf.request option }

    let parse str =
      let preamble, pkgs, request =
        Cudf_parser.parse (Cudf_parser.from_IO_in_channel (IO.input_string str)) in
      { preamble; pkgs; request }

    let to_string t = 
      let oc = IO.output_string () in
      (match t.preamble with
      | Some preamble -> Cudf_printer.pp_io_preamble oc preamble
      | None -> ());
      IO.write oc '\n';
      List.iter (Cudf_printer.pp_io_package oc) t.pkgs;
      IO.write oc '\n';
      (match t.request with
      | Some request -> Cudf_printer.pp_io_request oc request
      | None -> ());
      IO.close_out oc

     let create preamble pkgs request =
       { preamble; pkgs; request }

    let packages p = p.pkgs

    let log fmt =
      Globals.log "FILE.CONFIG" fmt

    let find f =
      log "find %s" (Path.string_of_filename f);
      match Path.find_binary f with
      | Path.File (Raw_binary s) -> 
          (try Some (parse s)
           with _ -> failwith ("Error while parsing " ^ Path.string_of_filename f))
      | Path.Not_found _ -> None
      | _ -> assert false

    let add f v =
      log "add %s" (Path.string_of_filename f);
      Path.add f (Path.File (Binary (Raw_binary (to_string v))))

    let find_default f = match find f with None -> { preamble = None ; pkgs = [] ; request = None } | Some t -> t
  end

  module type OPAM = sig
    include IO_FILE

    (** destruct *)
    val opam_version: t -> internal_version
    val version : t -> Namespace.version
    val description : t -> string
    val package: t -> C.package

    (** construct *)
    val create : name_version -> string (* description *) -> t
  end

  module Opam : OPAM = struct

    type t = {
      opam_version: internal_version;
      version: Namespace.version;
      description: string;
      cudf: Cudf.t;
    }

    let opam_version t = t.opam_version
    let version t = t.version
    let description t = t.description

    let find_field key = function
      | [x] -> (try Some (List.assoc key x.C.pkg_extra) with Not_found -> None)
      | _   -> None
          
    let package t = 
      match Cudf.packages t with 
        | [ x ] -> x
        | _     -> failwith "package: Bad format"

    let s_description = "description"
    let s_user_version = "user-version"
    let s_opam_version = "opam-version"

    let opam_version_pkg p = 
      match find_field s_opam_version [ p ] with
        | Some (`String s) -> Version s
        | _ -> Version Globals.opam_version

    let version_pkg p =
      match find_field s_user_version [ p ] with
        | Some (`String s) -> { Namespace.deb = s; cudf = p.C.version }
        | _ -> failwith "Bad format"

    let description_pkg p = 
      match find_field s_description [ p ] with
        | Some (`String s) -> s
        | _ -> ""

    let package p =
      match Cudf.packages p.cudf with
      | []    -> failwith "Empty opam file"
      | [ p ] -> p
      | _     -> failwith "Too many packages"

    let default_preamble =
      Some { C.default_preamble with C.property = [ s_description, `String None ] }

    let create (Name name, version) description = 
      let pkg = {
        C.default_package with 
          C.package = name ;
          version = version.Namespace.cudf ; 
          pkg_extra = [
            s_description , `String description;
            s_user_version, `String version.deb;
            s_opam_version, `String Globals.opam_version;
          ] } in
      let cudf = Cudf.create default_preamble [pkg] None in
      { opam_version = Version Globals.opam_version
      ; version
      ; description
      ; cudf }

    let parse str =
      let pkg = match Cudf.packages (Cudf.parse str) with
      | [p] -> p
      | _   -> failwith ("parse:" ^ str) in
      let cudf =  Cudf.create default_preamble [pkg] None in
      { opam_version = opam_version_pkg pkg
      ; version = version_pkg pkg
      ; description = description_pkg pkg
      ; cudf }

    (* XXX: This need to be handled in a better way:
       * opam-version MUST appear on the first line,
       * and the version should be the user-version *)
    let to_string t =
      Cudf.to_string t.cudf

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
  end

  module Installed : INSTALLED =
  struct
    type t = name_version list
    let empty = []

    let parse s =
      BatList.map (fun (name, version) -> Name name, version_of_string name version) (parse_space s)

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

    let modify_def f f_map = add f (N_map.bindings (f_map (N_map.of_enum (BatList.enum (find_default f)))))
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
      BatList.map (Path.concat f)
        (match suff with
          | Exact name -> [ B name ]
          | Suffix suff ->
            BatList.filter 
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
                ; bin = Relative, [], Suffix ""
                ; misc = [] }

    let b_of_string abs s = 
      let l = BatString.nsplit (BatString.strip ~chars:"/" s) "/" in
      match List.rev l with
        | x :: xs ->
          abs,
          BatList.map (fun s -> B s) (List.rev xs), 
          (match try Some (BatString.split x "*.") with _ -> None with
            | Some ("", suff) -> Suffix suff
            | _ -> Exact x)
        | [] -> abs, [], Exact ""

    let relative_path_of_string = b_of_string Relative

    let parse s =
      let l_lib_bin, l_misc = 
        let l, f_while = 
          BatString.nsplit s "\n",
          fun s ->
            match try Some (BatString.split "misc" (BatString.trim s)) with _ -> None with
            | Some ("", _) -> true
            | _ -> false in
        BatList.take_while f_while l, BatList.drop_while f_while l in

      (match filter ":" l_lib_bin with 
      |  ("lib", lib)
        :: ("bin", bin) :: _ -> 
          { lib = BatList.map relative_path_of_string (BatString.nsplit lib ",")
          ; bin = relative_path_of_string bin 
          ; misc = 
              BatList.map
                (fun (s_path, s_fname) -> 
                  { p_from = relative_path_of_string s_path ; p_to = b_of_string Absolute s_fname })
                (filter " " l_misc) }
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
end
