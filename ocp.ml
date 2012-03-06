module Namespace =
struct
  open Printf

  type name = Name of Cudf_types.pkgname
  let name_compare = compare

  type version = { deb : Debian.Format822.version ; cudf : Cudf_types.version }
  let version_compare v1 v2 = compare v1.cudf v2.cudf

  let string_of_nv (Name n) version = sprintf "%s-%s" n version.deb
  let string_of_name (Name n) = n
  let string_user_of_name (Name n) = n
  let string_user_of_version version = version.deb

  let table = ref (Debian.Debcudf.init_tables [])

  let version_of_string n version = { deb = version ; cudf = Debian.Debcudf.get_cudf_version !table (n, version) }

  let nv_of_string s = 
    let n, version = BatString.split s "-" in
      Name n, version_of_string n version
end

type name_version = Namespace.name * Namespace.version

module N_map = BatMap.Make (struct open Namespace type t = name let compare = name_compare end)
module V_set = BatSet.Make (struct open Namespace type t = version let compare = version_compare end)

module NV_orderedtype = 
struct
  open Namespace
  type t = name_version
  let compare (n1, v1) (n2, v2) = 
    let c = name_compare n1 n2 in
      if c = 0 then
        version_compare v1 v2
      else
        c
end

module NV_map = BatMap.Make (NV_orderedtype)
module NV_set = BatSet.Make (NV_orderedtype)

type 'a ocaml_options = 
  | I of 'a

type binary_data = 
  | Binary of string (* contents *)
  | Filename of string (* pointer to the contents *)

type 'a archive = 
  | Tar_gz of 'a
  | Empty

type basename = B of string

type internal_version = Version of string
(** Type used to represent an internal form of version, which is in particular not related to the version of a particular package *)

module type PATH =
sig

  type t
  type filename
  type url

  type 'a contents = 
    | Directory of basename list
    | File of 'a
    | Not_exists

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) list
    | R_file of 'a
    | R_filename of filename list

  val init : url option (* [None] : local *) -> string (* $HOME_OPAM *) -> internal_version (* OVERSION *) -> t
  (* $HOME_OPAM_OVERSION = $HOME_OPAM/OVERSION *)

  (** definitions of some shortcuts *)
  val root : filename (* / *)
    (** the root of every path *)
  val package : t -> filename (* $PWD *)
    (** path in the packager filesystem, contains the collection of libraries and programs *)
  val lib : t -> Namespace.name -> filename (* $HOME_OPAM_OVERSION/lib/NAME *)
    (** installed libraries for the package (at most one version installed) *)
  val bin : t -> filename (* $HOME_OPAM_OVERSION/bin *)
    (** contain installed binaries *)
  val config : t -> filename (* $HOME_OPAM/config *)
    (** main configuration file *)
  val installed : t -> filename (* $HOME_OPAM_OVERSION/installed *)
    (** list of installed packages with their version *)
  val index_opam : t -> name_version option -> filename (* $HOME_OPAM/index/NAME-VERSION.opam *)
    (** OPAM files considered for an arbitrary version and package *)
  val index_opam_list : t -> name_version list (* [ $HOME_OPAM/index/NAME-VERSION.opam ] -> [ NAME, VERSION ] *)
    (** list of OPAM files *)
  val archives_targz : t -> name_version option -> filename (* $HOME_OPAM/archives/NAME-VERSION.tar.gz *)
    (** source archives for all versions of all packages *)
  val build : t -> name_version option -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION *)
    (** tempory folders used to decompress the corresponding archives *)
  val to_install : t -> name_version -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION/NAME.install *)
    (** compiled files in the extracted archive to install *)

  (** **)

  val find : t -> filename -> binary_data contents
    (** Retrieves the contents from the hard disk. *)

  val remove : t -> filename -> t
    (** Removes everything in [filename] if existed. *)

  val add : t -> filename -> binary_data contents -> t
    (** Removes everything in [filename] if existed, then write [contents] instead. *)

  val add_rec : t -> filename -> binary_data contents_rec -> t
    (** Removes everything in [filename] if existed, then write [contents_rec] inside [filename]. *)

  val extract_targz : t -> binary_data archive -> binary_data contents_rec
    (** Returns the same meaning as [archive] but in extracted form. *)

  val exec_buildsh : t -> name_version -> t
    (* $HOME_OPAM/build/NAME-VERSION/build.sh *)
    (** Executes this particularly named script. *)

  val dirname : filename -> filename
    (** see [Filename.dirname] *)

  val basename : filename -> basename
    (** see [Filename.basename] *)

  val chop_extension : basename -> string
    (** see [Filename.chop_extension] *)

  val concat : filename -> basename -> filename
    (** see [Filename.concat] *)

  val file_exists : filename -> bool
    (** see [Sys.file_exists] *)

  val ocaml_options_of_library : t -> Namespace.name -> string ocaml_options 
    (* $HOME_OPAM/lib/NAME *)
    (** Returns the exact path to give to the OCaml compiler (ie. -I ...) *)

  val url : string (* hostname *) -> int option (* port *) -> url
  val change_url : t -> url -> t
  val string_of_url : url -> string
    (** in the format "HOSTNAME:PORT" *)
  val compare_computer : t -> t -> int
end

module Path : PATH =
struct
  open Printf

  type url = U of string

  type filename = 
    | Normalized of string
    | Raw of string

  type t = { computer : url option (* [None] : local *)
           ; home : string
           ; home_ocamlversion : string }

  type 'a contents = 
    | Directory of basename list
    | File of 'a
    | Not_exists

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) list
    | R_file of 'a
    | R_filename of filename list

  let s_of_filename = function
    | Normalized s -> s
    | Raw s -> s

  let filename_map f = function
    | Normalized s -> Normalized (f s)
    | Raw s -> Raw (f s)

  let normalize s = 
    let getchdir s = 
      let p = Unix.getcwd () in
      let () = Unix.chdir s in
      p in

    Normalized (getchdir (getchdir s))

  let home = Unix.getenv "HOME"
  let (//) = sprintf "%s/%s"
  let concat f (B s) = filename_map (fun filename -> filename // s) f
  let (///) = concat
  let init o s (Version ocamlv) = 
    let home = home // s in
    { computer = o ; home ; home_ocamlversion = home // ocamlv }

  let root = Raw "/"
  let package _ = normalize "."
  let lib t (Namespace.Name n) = Raw (t.home_ocamlversion // "lib" // n)
  let bin t = Raw (t.home_ocamlversion // "bin")

  let mk_name_version t_home d ext n v = Raw (t_home // d // sprintf "%s%s" (Namespace.string_of_nv n v) ext)

  let mk_name_version_o t_home name ext = 
      function
        | None -> Raw (t_home // name)
        | Some (n, v) -> mk_name_version t_home name ext n v

  let index_opam t = mk_name_version_o t.home "index" ".opam"
  let archives_targz t = mk_name_version_o t.home "archives" ".tar.gz"

  let build t = mk_name_version_o t.home_ocamlversion "build" ""
  let installed t = Raw (t.home_ocamlversion // "installed")
  let config t = Raw (t.home // "config")

  let to_install t (n, v) = build t (Some (n, v)) /// B (Namespace.string_of_name n ^ ".install")

  let url x o = U (sprintf "%s%s" x (match o with None -> "" | Some i -> sprintf ":%d" i))

  let change_url t u = { t with computer = Some u }

  let contents f_dir f_fic f_not_exists t f = 
    match t.computer with 
      | None -> 
        let fic = s_of_filename f in
          if Sys.file_exists fic then
            (if Sys.is_directory fic then f_dir else f_fic) fic
          else
            f_not_exists
      | Some _ -> failwith "to complete !"


  let find = 
    contents
      (fun fic -> Directory (BatList.of_enum (BatEnum.map (fun s -> B s) (BatSys.files_of fic))))
      (fun fic -> File ((*Binary (BatFile.with_file_in fic BatIO.read_all)*)Filename fic))
      Not_exists

  let chop_extension (B s) = Filename.chop_extension s
  let file_exists f = Sys.file_exists (s_of_filename f)

  let index_opam_list t =
    BatList.map (fun fic -> Namespace.nv_of_string (chop_extension fic))
      (match find t (index_opam t None) with
        | Directory l -> l
        | _ -> [])

  let remove t f = 
    let rec aux fic = 
      match (Unix.lstat fic).Unix.st_kind with
        | Unix.S_DIR -> 
          let () = BatEnum.iter (fun f -> aux (fic // f)) (BatSys.files_of fic) in
          Unix.rmdir fic
        | Unix.S_REG -> Unix.unlink fic
        | _ -> failwith "to complete !" in
    let () = aux (s_of_filename f) in
    t

  let add t f =
    function 
      | Directory d -> failwith "to complete !"
      | File (Binary cts) -> 
          let () = contents (fun _ -> failwith "to complete !") Unix.unlink () t f in
          let fic = s_of_filename f in
          let () = BatFile.with_file_out fic (fun oc -> BatString.print oc cts) in
            t
      | File (Filename fic) -> failwith "to complete ! copy the given filename"
      | Not_exists -> failwith "to complete !"

  let compare_computer t1 t2 = compare t1.computer t2.computer

  let exec_buildsh t n_v = 
    let _ = Sys.chdir (s_of_filename (build t (Some n_v))) in
    let _ = Sys.command "build.sh" in
      t
  let basename s = B (Filename.basename (s_of_filename s))

  let extract_targz t = function
    | Tar_gz (Binary _) -> failwith "to complete ! check if the \"dose\" project has been configured with the correct option to extract the gzip or bz2, then use similars functions to extract" (*IO.read_all (Common.Input.open_file fic)*)
    | Tar_gz (Filename fic) -> R_filename [Raw fic]
    | Empty -> R_directory []

  let lstat s = Unix.lstat (s_of_filename s)
  let files_of f = BatSys.files_of (s_of_filename f)

  let dirname = filename_map Filename.dirname

  let add_rec t f = 
    let () = (* check that [f] is not a file *)
      contents
        (fun _ -> ())
        (fun _ -> failwith "to complete !") 
        () t f in

    let rec aux t f (* <- filename dir *) name (* name of the value that will be destructed*) = function
      | R_directory l -> 
        List.fold_left 
          (let f = f /// name in
           fun t (b, cts) -> aux t f b cts) t l
      | R_file cts -> add t (f /// name) (File cts)
      | R_filename l -> 
        List.fold_left
          (fun t fic -> 
            aux
              t
              f
              (basename fic)
              (match (lstat fic).Unix.st_kind with
                | Unix.S_DIR -> R_directory (BatList.map (fun f -> 
                  let f = B f in
                  f, R_filename [fic /// f]) (files_of fic))
                | Unix.S_REG -> R_file (Filename (s_of_filename fic))
                | _ -> failwith "to complete !")) t l in
    aux t (dirname f) (basename f)

  let ocaml_options_of_library t name = 
    I (Printf.sprintf "%s" (s_of_filename (lib t name)))

  let string_of_url (U s) = s
end

let filename_of_string s = 
  List.fold_left 
    (fun t s -> Path.concat t (B s)) 
    Path.root
    (BatString.nsplit (BatString.strip ~chars:"/" s) "/")
    
module File =
struct
  open Namespace

  module type PRINTF =
  sig
    type t
    type out_channel

    val init : unit -> t
    val read_line : t -> string * t
    val printf : t -> ('a, out_channel, t) format -> 'a
  end

  module P : PRINTF =
  struct
    type t = unit
    include Pervasives

    let init x = x
    let read_line () = 
      read_line (), ()
    let printf () = Printf.printf
  end

  module type IO_FILE =
  sig
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t
  end

  module type CONFIG =
  sig
    include IO_FILE

    val empty_package_manager : internal_version
    val empty_ocaml : internal_version
    val version_of_string : string -> internal_version

    (** destruct *)
    val package_manager : t -> internal_version
    val sources : t -> Path.url option
    val ocaml_version : t -> internal_version


    (** construct *)
    val config : internal_version (* opam *) -> Path.url option -> internal_version (* ocaml *) -> t
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
    type t = { version : internal_version ; sources : Path.url option ; ocaml_version : internal_version }

    let empty_package_manager = Version "1"
    let empty_ocaml = Version Sys.ocaml_version
    let version_of_string s = Version s

    let package_manager t = t.version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version
    let config version sources ocaml_version = { version ; sources ; ocaml_version }

    let ocamlpro_http = "opam.ocamlpro.com"
    let ocamlpro_port = 9999
    let empty1 = { version = Version "" ; sources = Some (Path.url ocamlpro_http (Some ocamlpro_port)) ; ocaml_version = Version Sys.ocaml_version }
    let empty2 = { version = Version "" ; sources = None ; ocaml_version = Version Sys.ocaml_version }

    let find t f = 
      match Path.find t f with
        | Path.File (Binary s) -> 
            (match parse_colon s with
               |  ("version", version)
               :: ("sources", sources)
               :: ("ocaml-version", ocaml_version)

               :: _ -> { version = Version version
                       ; sources = 
                          (try Some (let hostname, port = BatString.split sources ":" in
                                     Path.url hostname (try Some (int_of_string port) with _ -> None)) with _ -> None)
                       ; ocaml_version = Version ocaml_version }
               | _ -> empty1)
        | _ -> empty2

    let to_string t =
      Printf.sprintf "
version: %s
sources: %s" 
        (match t.version with Version s -> s)
        (match t.sources with None -> Printf.sprintf "%s:%d" ocamlpro_http ocamlpro_port | Some sources -> Path.string_of_url sources)

    let add t f v = Path.add t f (Path.File (Binary (to_string v)))
  end

  module type CUDF =
  sig
    include IO_FILE

    (** destruct *)
    val package : t -> Cudf.package
    val description : Cudf.package -> string

    (** construct *)
    val new_package : name_version -> string (* description *) -> Cudf.package
    val cudf : internal_version (* package manager *) -> Cudf.package -> t
  end

  module Cudf (F_config : CONFIG) : CUDF = 
  struct
    type package = 
        { preamble : Cudf.preamble option
        ; pkg : Cudf.package list
        ; request : Cudf.request option }
        
    type t = 
        { opam_version : internal_version
        ; package : package }

    let find_field key = function
      | [x] -> (try Some (List.assoc key x.Cudf.pkg_extra) with Not_found -> None)
      | _ -> None
          
    let package t = 
      match t.package.pkg with 
        | [ x ] -> x
        | _ -> Cudf.default_package

    let name p = match p.pkg with [x] -> x.Cudf.package | _ -> ""
    let version p = match p.pkg with [x] -> x.Cudf.version | _ -> min_int

    let s_description = "description"
    let description p = 
      match find_field s_description [ p ] with
        | Some (`String s) -> s
        | _ -> ""

    let new_package (Name name, version) description = 
      { Cudf.default_package with 
        Cudf.package = name ;
        Cudf.version = version.cudf ; 
        Cudf.pkg_extra = [ s_description, `String description ] }

    let cudf opam_version pkg = { opam_version ; package = { preamble = None ; pkg = [ pkg ] ; request = None } }

    let empty = 
      { opam_version = F_config.empty_package_manager
      ; package = { preamble = None ; pkg = [] ; request = None } }

    let find t f =
      match Path.find t f with
        | Path.File (Binary s) -> 
          (match 
              try
                Some (Cudf_parser.parse (Cudf_parser.from_IO_in_channel (IO.input_string s)))
              with _ -> None
           with
             | None -> empty
             | Some (preamble, pkg, request) -> 
               { opam_version = 
                   (match find_field "opam_version" pkg with
                      | Some (`String v) -> F_config.version_of_string v
                      | _ -> empty.opam_version)
               ; package = { preamble ; pkg ; request } })
        | _ -> empty

    let to_string t = 
      let oc = IO.output_string () in
      let () = 
        begin
          (match t.package.preamble with
            | Some preamble -> Cudf_printer.pp_io_preamble oc preamble
            | None -> ());
          List.iter (Cudf_printer.pp_io_package oc) t.package.pkg;
          (match t.package.request with
            | Some request -> Cudf_printer.pp_io_request oc request
            | None -> ());
        end in
      IO.close_out oc

    let add t f v = Path.add t f (Path.File (Binary (to_string v)))
  end

  module type INSTALLED =
  sig
    include IO_FILE with type t = name_version list
  end

  module Installed : INSTALLED =
  struct
    type t = name_version list
    let empty = []

    let find t f = 
      match Path.find t f with
        | Path.File (Binary s) -> 
            BatList.map (fun (name, version) -> Name name, version_of_string name version) (parse_space s)
        | _ -> empty

    let to_string = 
      BatIO.to_string (BatList.print (fun oc (name, version) -> BatString.print oc (Printf.sprintf "%s %s" 
                                                                                      (Namespace.string_user_of_name name) 
                                                                                      (Namespace.string_user_of_version version)))) 
    let add t f v = Path.add t f (Path.File (Binary (to_string v)))
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
              (match Path.find t f with Path.Directory l -> l | _ -> []))

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

    let find t f =
      match Path.find t f with
        | Path.File (Binary s) -> 

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

        | _ -> empty


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

    let add t f v = Path.add t f (Path.File (Binary (to_string v)))
  end
end

type 'a installed_status =
  | Was_installed of 'a
  | Was_not_installed

module type SOLVER =
sig
  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type ('a, 'b) action = 
    | To_change of 'a 
        (* Version to install. The package could have been present or not, 
           but if present, it is another version than the proposed solution. *)
    | To_delete of 'b (* The package has been installed. *)
    | To_recompile of 'b (* The package is already installed, we just recompile it. *)

  type 'a parallel = P of 'a list (* order irrelevant : elements are considered in parallel *)

  type 'a solution = 
      ( 'a (* old *) installed_status * 'a (* new *)
      , 'a (* old *) )
        action parallel list
      (** Sequence describing the action to perform.
          Order natural : first element to execute is the first element of the list. *)

  val solution_print : ('a BatIO.output -> 'b -> unit) -> 'a BatIO.output -> 'b solution -> unit
  val solution_map : ('a -> 'b) -> 'a solution -> 'b solution

  val resolve : Cudf.package list -> Cudf_types.vpkg request -> Cudf.package solution list
    (** Given a description of packages, it returns a list of solution preserving the consistency of the initial description. *)
end

module type SERVER =
sig
  type t
  type opam
  type package

  val init : Path.url option -> t

  val change_url : t -> Path.url -> t

  val getList : t -> name_version list
    (** Returns the list of the available versions for all
        packages. *)

  val getOpam : t -> name_version -> opam
    (** Returns the representation of
        the OPAM file for the corresponding package version. *)

  val getArchive : t -> opam -> binary_data archive
    (** Returns the corresponding package archive. *)

  val newArchive : t -> opam -> binary_data archive -> t
    (** Receives an upload, it contains an OPAM file and the
        corresponding package archive. *)

  val version_opam : t -> internal_version
  val version_ocaml : t -> internal_version

  val package : opam -> package option 
    (** [None] : the [opam] associated to the [(name, version)] does not exist. 
        Note that every [(name, version)] given by [getList] do exist. *)
end

module Server
  (F_config : File.CONFIG)
  (F_cudf : File.CUDF) 
  : SERVER with type package = Cudf.package =
struct
  module Path_map = BatMap.Make (struct type t = Path.t let compare = Path.compare_computer end)

  type t = 
      { current_repository : Cudf.package NV_map.t
      ; home : Path.t (* ~/.opam-server *)
      ; all_repository : Cudf.package NV_map.t Path_map.t
      ; version_package_manager : internal_version
      ; version_ocaml : internal_version }

  type opam = name_version * Cudf.package option
      (* [None] : the current repository does not contain the package associated to the [name] and [version] *)

  type package = Cudf.package

  let read_archives home =
    let archives = Path.archives_targz home None in
      List.fold_left
        (fun map x -> 
           NV_map.add
             (Namespace.nv_of_string (Path.chop_extension x)) 
             (F_cudf.package (F_cudf.find home (Path.concat archives x))) 
             map) NV_map.empty 
        (match Path.find home archives with
           | Path.Directory l -> l
           | _ -> [])

  let init o = 
    let version_ocaml = F_config.empty_ocaml in
    let home = Path.init o ".opam-server" version_ocaml in
    { current_repository = read_archives home
    ; home
    ; all_repository = Path_map.empty
    ; version_package_manager = F_config.empty_package_manager
    ; version_ocaml }

  let change_url t url = 
    let home = Path.change_url t.home url in
    { t with
        current_repository = (match Path_map.Exceptionless.find home t.all_repository with
                                | None -> read_archives home
                                | Some v -> v);
        home;
        all_repository = Path_map.add t.home t.current_repository t.all_repository }

  let getList t = BatList.map fst (NV_map.bindings t.current_repository)
  let getOpam t n_v = n_v, NV_map.Exceptionless.find n_v t.current_repository
  let getArchive t = function
    | _, None -> Empty
    | n_v, Some _ -> 
        match Path.find t.home (Path.archives_targz t.home (Some n_v)) with
          | Path.File s -> Tar_gz s
          | _ -> Empty

  let newArchive t (n_v, o_pack) arch = 
    let t = 
      { t with 
        home = 
          Path.add 
            t.home 
            (Path.archives_targz t.home (Some n_v)) 
            (Path.File (match arch with 
                          | Empty -> failwith "create an empty tar.gz here" 
                          | Tar_gz s -> s)) } in
      
    match o_pack with
      | None -> { t with current_repository = NV_map.add n_v (F_cudf.new_package n_v "") t.current_repository }
      | Some _ -> t

  let version_opam t = t.version_package_manager
  let version_ocaml t = t.version_ocaml
  let package = snd
end

module type CLIENT =
sig
  type t

  val init0 : unit -> t

  val init : t -> Path.url (* repository address *) option (* [None] : default is opam.ocamlpro.com, port = 9999 *) -> t
    (** Initializes in a consistent state. *)

  val info : t -> Namespace.name option -> t
    (** Displays the installed package. [None] : a general summary is given. *)

  type config_request = Dir
  val config : t -> config_request -> Namespace.name -> t
    (** Returns the directory where the package is installed,
        in a form suitable to OCaml compilers (i.e. like "-I ..."). *)

  val install : t -> Namespace.name -> t
    (** Installs the given package. *)

  val update : t -> t
    (** Downloads the latest packages available. *)

  val upgrade : t -> t
    (** Finds a consistent state where most of the installed packages are
        upgraded to their latest version. *)

  val upload : t -> Path.filename -> t
    (** Sends a new created package to the server. *)

  val remove : t -> Namespace.name -> t
    (** Removes the given package. *)
end

module Client 
  (F_config : File.CONFIG) 
  (F_installed : File.INSTALLED) 
  (F_cudf : File.CUDF) 
  (F_toinstall : File.TO_INSTALL)
  (Solver : SOLVER)
  (Server : SERVER with type package = Cudf.package) 
  (P : File.PRINTF)
  : CLIENT =
struct
  type t = 
      { server : Server.t
      ; home : Path.t (* ~/.opam *)
      ; stdout : P.t }

  let init0 x =
    let home = Path.init None ".opam" (F_config.empty_ocaml) in
      { server = Server.init (F_config.sources (F_config.find home (Path.config home)))
      ; home
      ; stdout = P.init x }

  let update t =
    let home, map =
      List.fold_left
        (fun (home, map) (n, v) -> 
           let index_nv = Path.index_opam t.home (Some (n, v)) in
             if Path.file_exists index_nv then
               home, map
             else
               F_cudf.add home index_nv
                 (F_cudf.cudf
                    (Server.version_opam t.server)
                    (match Server.package (Server.getOpam t.server (n, v)) with
                       | None -> assert false
                       | Some pkg -> pkg)),
           N_map.modify_def V_set.empty n (V_set.add v) map)
        (t.home, N_map.empty)
        (Server.getList t.server) in

      { t with home; stdout = 
          P.printf t.stdout "%s" (BatIO.to_string (N_map.print (fun oc name -> BatString.print oc (Namespace.string_user_of_name name))
                                                     (V_set.print (fun oc version -> BatString.print oc (Namespace.string_user_of_version version)))) map) }

  let init t o_url =
    update (match o_url with
              | None -> t
              | Some url -> { t with server = Server.change_url t.server url })

  let indent_left s nb = s ^ String.make nb ' '

  let indent_right s nb = String.make nb ' ' ^ s

  let find_from_name name l = 
    N_map.Exceptionless.find 
      name
      (List.fold_left
         (fun map (n, v) -> 
            N_map.modify_def V_set.empty n (V_set.add v) map) N_map.empty l)

  let info t = 
    let s_not_installed = "--" in

    function
    | None -> 
        let install_set = NV_set.of_enum (BatList.enum (F_installed.find t.home (Path.installed t.home))) in
        let map, max_n, max_v = 
          List.fold_left (fun (map, max_n, max_v) n_v -> 
                            let b = NV_set.mem n_v install_set in
                            NV_map.add n_v
                              (b, 
                               F_cudf.description (F_cudf.package (F_cudf.find t.home (Path.index_opam t.home (Some n_v)))))
                              map, 
                            max max_n (String.length (Namespace.string_user_of_name (fst n_v))), 
                            if b then max max_v (String.length (Namespace.string_user_of_version (snd n_v))) else max_v)
            (NV_map.empty, min_int, String.length s_not_installed)
            (Path.index_opam_list t.home) in

        { t with 
          stdout = 
            NV_map.fold (fun n_v (b, description) stdout -> 
                           P.printf stdout "%s %s %s" 
                             (indent_left (Namespace.string_user_of_name (fst n_v)) max_n)
                             (indent_right (if b then Namespace.string_user_of_version (snd n_v) else s_not_installed) max_v)
                             description) map t.stdout }

    | Some name -> 
        let find_from_name = find_from_name name in

        let o_v = 
          BatOption.map
            V_set.choose (* By definition, there is exactly 1 element, we choose it. *) 
            (find_from_name (F_installed.find t.home (Path.installed t.home))) in

        let v_set =
          let v_set = 
            match find_from_name (Path.index_opam_list t.home) with
              | None -> V_set.empty
              | Some v -> v in
            match o_v with
              | None -> v_set
              | Some v -> V_set.remove v v_set in

          { t with
            stdout = 
              List.fold_left
                (fun stdout (tit, desc) -> P.printf stdout "%s: %s" tit desc)
                t.stdout 
                [ "package", Namespace.string_user_of_name name
                ; "version", (match o_v with None -> s_not_installed | Some v -> Namespace.string_user_of_version v)
                ; "versions", BatIO.to_string (V_set.print ~first:"" ~last:"" ~sep:", " (fun oc v -> BatString.print oc (Namespace.string_user_of_version v))) v_set
                ; "description", "\n" ^ 
                  match o_v with None -> ""
                    | Some v -> 
                        F_cudf.description (F_cudf.package (F_cudf.find t.home (Path.index_opam t.home (Some (name, v))))) ] }

  let confirm_ msg chan = 
    match P.read_line
      (P.printf chan "%s\nContinue ? [y/N] " msg)
    with
      | ("y"|"Y"), chan -> true, chan
      | _, chan -> false, chan

  let confirm t msg = 
    let b, stdout = confirm_ msg t.stdout in
    b, { t with stdout }

  let fold_toinstall f_build f_add_rec t (name, v) = 
    let t, tgz = 
      let p_targz, p_build = 
        Path.archives_targz t.home (Some (name, v)),
        Path.build t.home (Some (name, v)) in
      if Path.file_exists p_targz then
        t, Path.R_filename (BatList.map (Path.concat p_build) (match Path.find t.home p_build with Path.Directory l -> l | _ -> []))
      else
        let tgz = Path.extract_targz t.home (Server.getArchive t.server (Server.getOpam t.server (name, v))) in
          { t with home = Path.add_rec t.home p_build tgz }, tgz in
      
    let t = f_build t tgz in
    let to_install = F_toinstall.find t.home (Path.to_install t.home (name, v)) in

    let filename_of_path_relative t path = 
      Path.R_filename (F_toinstall.filename_of_path_relative t.home
                         (Path.build t.home (Some (name, v))) 
                         path) in
      
    let add_rec f_lib t path = 
      f_add_rec t
        (f_lib t.home name (* warning : we assume that this result is a directory *))
        (filename_of_path_relative t path) in

    let t = (* lib *) 
      List.fold_left (add_rec Path.lib) t (F_toinstall.lib to_install) in
      
    let t = (* bin *) 
      add_rec (fun t _ -> Path.bin t) t (F_toinstall.bin to_install) in
      
    let t = (* misc *)
      List.fold_left 
        (fun t misc -> 
          let ok, t = confirm t (F_toinstall.string_of_misc misc) in
          if ok then
            let path_from = filename_of_path_relative t (F_toinstall.path_from misc) in
            List.fold_left 
              (fun t path_to -> { t with home = Path.add_rec t.home path_to path_from }) 
              t
              (F_toinstall.filename_of_path_absolute t.home (F_toinstall.path_to misc))
          else
            t) t (F_toinstall.misc to_install) in
    t

  let proceed_todelete t (n, v0) = 
    let map_installed = N_map.of_enum (BatList.enum (F_installed.find t.home (Path.installed t.home))) in
    match N_map.Exceptionless.find n map_installed with
      | Some v when v = v0 ->
        let t = 
          fold_toinstall
            (fun t _ -> t)
            (fun t file -> function
              | Path.R_filename l -> 
                { t with home = List.fold_left (fun t_home f -> Path.remove t_home (Path.concat file (Path.basename f))) t.home l }
              | _ -> failwith "to complete !")
            t
            (n, v) in

        let t = { t with home = F_installed.add t.home (Path.installed t.home) (N_map.bindings (N_map.remove n map_installed)) } in
        
        t
      | _ -> t

  let proceed_torecompile t (name, v) =
    fold_toinstall  
      (fun t tgz -> 
        { t with home = 
            Path.exec_buildsh
              (Path.add_rec t.home (Path.build t.home (Some (name, v))) tgz) 
              (name, v) })
      (fun t file contents -> { t with home = Path.add_rec t.home file contents })
      t
      (name, v)

  let proceed_tochange t (nv_old, nv) =
    proceed_torecompile
      (match nv_old with 
        | Was_installed n_v -> proceed_todelete t n_v
        | Was_not_installed -> t) 
      nv

  module PkgMap = BatMap.Make (struct type t = Cudf.package let compare = compare end)

  let resolve t l_index request = 

    let rec aux chan = function
      | x :: xs -> 
          let ok, chan = 
            confirm_ (Printf.sprintf "%s This solution will be performed or another will be tried if existed."
                        (if x = [] then
                            "Solution found : The current state of the repository can be kept to satisfy the constraints given."
                         else
                            BatIO.to_string (Solver.solution_print (fun oc (_, v) -> BatString.print oc (Namespace.string_user_of_version v))) x)) chan in
            if ok then
              chan, Some x
            else
              aux chan xs
                
      | [] -> chan, None in
      
    let stdout, o =
      aux t.stdout 
        (let l_pkg, map_pkg = 
           List.fold_left
             (fun (l, map) n_v -> 
               let pkg = F_cudf.package (F_cudf.find t.home (Path.index_opam t.home (Some n_v))) in
               pkg :: l, PkgMap.add pkg n_v map) ([], PkgMap.empty) l_index in
         (BatList.map (Solver.solution_map (fun p -> PkgMap.find p map_pkg)) (Solver.resolve l_pkg request)) ) in

    let t = { t with stdout } in
      match o with
        | Some sol -> 
            List.fold_left (fun t (Solver.P l) -> 
              List.fold_left (fun t -> function
                | Solver.To_change n_v -> proceed_tochange t n_v
                | Solver.To_delete n_v -> proceed_todelete t n_v
                | Solver.To_recompile n_v -> proceed_torecompile t n_v) t l) t sol
        | None -> t

  let vpkg_of_nv (name, v) = Namespace.string_of_name name, Some (`Eq, v.Namespace.cudf)

  let install t name = 
    let l_index = Path.index_opam_list t.home in
    match find_from_name name l_index with
      | None -> 
          let ok, t = confirm t (Printf.sprintf "Package \"%s\" not found. An update of package will be performed."
                                   (Namespace.string_user_of_name name)) in
            if ok then
              update t
            else
              t
      | Some v -> 
          resolve t l_index { Solver.wish_install = [ vpkg_of_nv (name, V_set.max_elt v) ]
                            ; wish_remove = [] 
                            ; wish_upgrade = [] }

  let remove t name = 
    match
      match BatList.Exceptionless.assoc name (F_installed.find t.home (Path.installed t.home)) with
        | None -> 
          let ok, t = confirm t (Printf.sprintf "Package \"%s\" not found. We will call the solver to see its output."
                                   (Namespace.string_user_of_name name)) in
          if ok then
            Some (t, None)
          else
            None
        | Some v -> Some (t, Some (`Eq, v.Namespace.cudf))
    with
      | Some (t, o_v) -> 
        let l_index = Path.index_opam_list t.home in
        resolve t l_index { Solver.wish_install = []
                          ; wish_remove = [ Namespace.string_of_name name, o_v ]
                          ; wish_upgrade = [] }
      | None -> t    

  let upgrade t =
      resolve t (Path.index_opam_list t.home) 
        { Solver.wish_install = [] ; wish_remove = [] ; wish_upgrade = BatList.map vpkg_of_nv (F_installed.find t.home (Path.installed t.home)) }
    
  let upload t filename = 
    { t with
      server = 
        Server.newArchive t.server
          (Server.getOpam t.server (Namespace.nv_of_string (Path.chop_extension (Path.basename filename))))
          (match Path.find t.home filename with
             | Path.File binary -> Tar_gz binary
             | _ -> Empty) }

  type config_request = Dir
  let config t Dir name =     
    match find_from_name name (Path.index_opam_list t.home) with
      | None -> 
        let ok, t = confirm t (Printf.sprintf "Package \"%s\" not found. An update of package will be performed."
                                 (Namespace.string_user_of_name name)) in
        if ok then
          update t
        else
          t
      | Some _ -> 
        
        { t with stdout = 
            P.printf t.stdout "-I %s" 
              (match Path.ocaml_options_of_library t.home name with
                | I s -> s) }
end

module Solver
  (F_cudf : File.CUDF)
  : SOLVER = 
struct

  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type ('a, 'b) action = 
    | To_change of 'a 
    | To_delete of 'b
    | To_recompile of 'b

  type 'a parallel = P of 'a list

  type 'a solution = 
      ( 'a (* old *) installed_status * 'a (* new *)
      , 'a (* old *) )
        action parallel list

  let solution_map f = 
    BatList.map (function P l -> P (BatList.map (function
      | To_change (o_p, p) -> To_change ((match o_p with
          |  Was_installed p -> Was_installed (f p)
          | Was_not_installed -> Was_not_installed), f p)
      | To_delete p -> To_delete (f p)
      | To_recompile p -> To_recompile (f p)) l))

  let solution_print f = 
    BatList.print ~first:"" ~last:"" ~sep:", " 
      (fun oc (P l) -> 
        BatList.print ~first:"" ~last:"" ~sep:", " 
          (fun oc act -> 
            let f_act s l_p = 
              begin
                BatString.print oc (Printf.sprintf "%s : " s);
                BatList.print f oc l_p;
              end in
            match act with
              | To_change (o_v_old, p_new) -> 
                f_act "change"
                  (match o_v_old with
                    | Was_not_installed -> [ p_new ]
                    | Was_installed p_old -> [ p_old ; p_new ])
              | To_recompile _ -> ()
              | To_delete v -> f_act "remove" [v]) oc l)

  module type CUDFDIFF = 
  sig
    val resolve_diff : Cudf.package list -> Cudf_types.vpkg request ->
      (Cudf.package installed_status * Cudf.package, Cudf.package) action list option

    val resolve_summary : Cudf.package list -> Cudf_types.vpkg request ->
      ( Cudf.package list
      * (Cudf.package * Cudf.package) list
      * (Cudf.package * Cudf.package) list
      * Cudf.package list ) option
  end

  module CudfDiff : CUDFDIFF =
  struct
    module type CUDFDIFF =
    sig
      type solution =
          { installed : Common.CudfAdd.Cudf_set.t
          ; removed : Common.CudfAdd.Cudf_set.t }
      val diff : Cudf.universe -> Cudf.universe -> (Common.CudfAdd.StringSet.elt, solution) ExtLib.Hashtbl.t
      val summary : Cudf.universe -> (Common.CudfAdd.StringSet.elt, solution) ExtLib.Hashtbl.t ->
        Cudf.package list * (Cudf.package * Cudf.package) list * (Cudf.package * Cudf.package) list * Cudf.package list
    end
      
    module CudfDiff : CUDFDIFF = 
    struct
      (**************************************************************************************)
      (*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
      (*  Copyright (C) 2010 Mancoosi Project                                               *)
      (*                                                                                    *)
      (*  This library is free software: you can redistribute it and/or modify              *)
      (*  it under the terms of the GNU Lesser General Public License as                    *)
      (*  published by the Free Software Foundation, either version 3 of the                *)
      (*  License, or (at your option) any later version.  A special linking                *)
      (*  exception to the GNU Lesser General Public License applies to this                *)
      (*  library, see the COPYING file for more information.                               *)
      (**************************************************************************************)


      open ExtLib
      open Common

      module Cudf_set = CudfAdd.Cudf_set
      module StringSet = CudfAdd.StringSet

      type solution = {
        installed : Cudf_set.t ;
        removed : Cudf_set.t ;
      }

      (* the 'package' is always taken from the universe *)
      let to_set univ l =
        List.fold_left (fun s p ->
          let q = Cudf.lookup_package univ (p.Cudf.package,p.Cudf.version) in
          Cudf_set.add q s
        ) Cudf_set.empty l
      ;;

      (* for each pkgname I've the list of all versions that were installed or removed *)
      let diff univ sol =
        let pkgnames = CudfAdd.pkgnames univ in
        let h = Hashtbl.create (StringSet.cardinal pkgnames) in
        StringSet.iter (fun pkgname ->
          let were_installed = to_set univ (Cudf.get_installed univ pkgname) in
          let are_installed = to_set univ (Cudf.get_installed sol pkgname) in
          let r = Cudf_set.diff were_installed are_installed in
          let i = Cudf_set.diff are_installed were_installed in
          let s = { removed = r ; installed = i } in
          Hashtbl.add h pkgname s
        ) pkgnames ;
        h

      (* 
         [all] : all versions of a package in the universe . 
         [s] : the set of version for version of a package in a solution 
         returns a list that contains for each version its status : installed, 
         removed, upgraded, etc
      *)
      type summary_t = {
        mutable i : Cudf.package list; (* installed *)
        mutable r : Cudf.package list; (* removed *)
        mutable u : (Cudf.package * Cudf.package) option ; (* upgraded *)
        mutable d : (Cudf.package * Cudf.package) option ; (* downgraded *)
        mutable nu : Cudf.package list; (* not upgraded *)
      }

      (* for one package *)
      let default_summary () = { u = None; d = None ; i = [] ; r = [] ; nu = [] }

      let uniqueversion all s =
        let l = default_summary () in
        let i = Cudf_set.filter (fun pkg -> pkg.Cudf.installed) all in
        if (Cudf_set.cardinal i <= 1) && ((Cudf_set.cardinal s.installed) <= 1) then
          begin
            if (Cudf_set.cardinal s.installed) = 1 then begin
              if (Cudf_set.cardinal i) = 1 then begin
                let np = Cudf_set.choose i in
                let op = Cudf_set.choose s.installed in
                if np.Cudf.version < op.Cudf.version
                then l.u <- Some(np,op)
                else l.d <- Some(op,np)
              end
              else
                l.i <- Cudf_set.elements s.installed;
            end else
              if not (Cudf_set.is_empty s.removed) then
                l.r <- Cudf_set.elements s.removed;
          end
        else begin
          if not (Cudf_set.is_empty s.removed) then
            l.r <- Cudf_set.elements s.removed;
          if not (Cudf_set.is_empty s.installed) then
            l.i <- Cudf_set.elements s.installed;
        end;
        l
      ;;

      let summary univ diff =
        let i = ref [] in
        let u = ref [] in
        let d = ref [] in
        let r = ref [] in
        let names = CudfAdd.pkgnames univ in
        StringSet.iter (fun pkgname ->
          let all = CudfAdd.to_set (Cudf.lookup_packages univ pkgname) in
          let s = Hashtbl.find diff pkgname in
          let l = uniqueversion all s in
          i := l.i @ !i ; 
          r := l.r @ !r ; 
          if not (Option.is_none l.u) then
            u := (Option.get l.u) :: !u;
          if not (Option.is_none l.d) then
            d := (Option.get l.d) :: !d;
        ) names;
        (!i,!u,!d,!r)
      ;;
    end

    let to_cudf_doc l_pkg req = 
      None, l_pkg, { Cudf.request_id = "" 
                   ; install = req.wish_install
                   ; remove = req.wish_remove
                   ; upgrade = req.wish_upgrade
                   ; req_extra = [] }


    let cudf_resolve l_pkg req = 
      let open Algo in
      let r = Depsolver.check_request (to_cudf_doc l_pkg req) in
      if Diagnostic.is_solution r then
        match r with
          | { Diagnostic.result = Diagnostic.Success f } -> Some (f ~all:true ())
          | _ -> assert false
      else
        None

    module Cudf_set =
    struct
      module S = Common.CudfAdd.Cudf_set

      let choose_one s = 
        match S.cardinal s with
          | 0 -> raise Not_found
          | 1 -> S.choose s
          | _ ->
            failwith "to complete ! Determine if it suffices to remove one arbitrary element from the \"removed\" class, or remove completely every element."

      include S
    end

    let resolve f_diff l_pkg_pb req = 
      BatOption.bind
        (fun l_pkg_sol -> 
          let univ_init = Cudf.load_universe l_pkg_pb in
          BatOption.bind 
            (f_diff univ_init)
            (try Some (CudfDiff.diff univ_init (Cudf.load_universe l_pkg_sol)) with Cudf.Constraint_violation _ -> None))
        (cudf_resolve l_pkg_pb req)

    let resolve_diff = 
      resolve
        (fun _ diff -> 
          match 
            Hashtbl.fold (fun pkgname s acc ->
              let add x = x :: acc in
              match 
                (try Some (Cudf_set.choose_one s.CudfDiff.removed) with Not_found -> None), 
                try Some (Cudf_set.choose s.CudfDiff.installed) with Not_found -> None
              with
                | None, Some p -> add (To_change (Was_not_installed, p))
                | Some p, None -> add (To_delete p)
                | Some p_old, Some p_new -> add (To_change (Was_installed p_old, p_new))
                | None, None -> acc) diff []
          with
            | [] -> None
            | l -> Some l)

    let resolve_summary = resolve (fun univ_init diff -> Some (CudfDiff.summary univ_init diff))
  end

  module Graph = 
  struct
    open Algo

    module PG = 
    struct
      module G = Defaultgraphs.PackageGraph.G
      let union g1 g2 =
        let g1 = G.copy g1 in
        let () = 
          begin
            G.iter_vertex (G.add_vertex g1) g2;
            G.iter_edges (G.add_edge g1) g2;
          end in
        g1
      include G
    end
    module PO = Defaultgraphs.GraphOper (PG)

    module PG_bfs = 
    struct
      include Graph.Traverse.Bfs (PG)
      let fold f acc g = 
        let rec aux acc iter = 
          match try Some (get iter, step iter) with Exit -> None with
            | None -> acc
            | Some (x, iter) -> aux (f acc x) iter in
        aux acc (start g)
    end

    module O_pkg = struct type t = Cudf.package let compare = compare end
    module PkgMap = BatMap.Make (O_pkg)
    module PkgSet = BatSet.Make (O_pkg)

    let dep_reduction v =
      let g = Defaultgraphs.PackageGraph.dependency_graph (Cudf.load_universe v) in
      let () = PO.transitive_reduction g in
      g

    let resolve l_pkg_pb req =
      [ match
      BatOption.bind 
        (let cons pkg act = Some (pkg, act) in
         fun l -> 
          let graph_installed = dep_reduction (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) (Cudf.load_universe l_pkg_pb)) in
          
          let l_del_p, l_del = 
            BatList.split
              (BatList.filter_map (function
                | To_delete pkg as act -> cons pkg act
                | _ -> None) l) in

          let map_add = 
            PkgMap.of_enum (BatList.enum (BatList.filter_map (function 
              | To_change (_, pkg) as act -> cons pkg act
              | To_delete _ -> None
              | To_recompile _ -> assert false) l)) in

          let _, l_act = 
            PG_bfs.fold
              (fun (set_recompile, l_act) pkg -> 
                let add_succ_rem pkg set act =
                  List.fold_left (fun set x -> PkgSet.add x set) (PkgSet.remove pkg set) (PG.succ graph_installed pkg), act :: l_act in
                
                match PkgMap.Exceptionless.find pkg map_add with
                  | Some act -> 
                    add_succ_rem pkg set_recompile act
                  | None ->
                    if PkgSet.mem pkg set_recompile then
                      add_succ_rem pkg set_recompile (To_recompile pkg)
                    else
                      set_recompile, l_act) (PkgSet.empty, List.rev l_del) 
              (let graph_installed = PG.copy graph_installed in
               let () = List.iter (PG.remove_vertex graph_installed) l_del_p in              
               PG.union graph_installed (dep_reduction (BatList.of_enum (PkgMap.keys map_add)))) in
          Some (List.rev l_act))
        (CudfDiff.resolve_diff l_pkg_pb req)
      with
        | None -> []
        | Some l -> BatList.map (fun x -> P [ x ]) l ]
  end

  let resolve = Graph.resolve
end

module M =
struct
  open File
  module Cudf = Cudf (Config)
  module Solv = Solver (Cudf)
  module S = Server (Config) (Cudf)
  module C = Client (Config) (Installed) (Cudf) (To_install) (Solv) (S) (P)
end

module C = M.C

open Namespace

let _ = 
  let client = C.init0 () in
  let f x = 
    let _ = Printf.printf "(* command not found *)\n%!" in
    x in
  match Array.to_list Sys.argv with
    | [] -> f client
    | _ :: l ->
      match l with
          
        | "init" :: url :: port :: _ -> C.init client (Some (Path.url url (Some (int_of_string port))))
        | "init" :: url :: _ -> C.init client (Some (Path.url url None))
        | "init" :: _ -> C.init client None
          
        | "info" :: name :: _ -> C.info client (Some (Name name))
        | "info" :: _ -> C.info client None
          
        | "config" :: name :: []
        | "config" :: _ :: name :: _ -> C.config client C.Dir (Name name)
          
        | "install" :: name :: _ -> C.install client (Name name)
          
        | "update" :: _ -> C.update client
          
        | "upgrade" :: _ -> C.upgrade client
          
        | "upload" :: s :: _ -> C.upload client (filename_of_string s)
          
        | "remove" :: name :: _ -> C.remove client (Name name)
          
        | _ -> f client
