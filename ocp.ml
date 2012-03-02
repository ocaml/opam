module Namespace =
struct
  open Printf

  type name = Name of string
  let name_compare (Name n1) (Name n2) = failwith "to complete !"

  type version = Version of string
  let version_compare (Version v1) (Version v2) = failwith "to complete !"

  let string_of_nv (Name n) (Version v) = sprintf "%s-%s" n v
  let string_of_name (Name n) = n
  let string_user_of_name (Name n) = n
  let string_user_of_version (Version v) = v

  let nv_of_string s = 
    let n, v = BatString.split s "-" in
      Name n, Version v
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

type binary_data = Binary of string

type 'a archive = 
  | Tar_gz of 'a
  | Empty

type basename = B of string

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

  val init : url option (* [None] : local *) -> string (* $HOME_OPAM *) -> t


  (** definitions of some shortcuts *)
  val root : filename (* / *)
    (** the root of every path *)
  val proot : t -> filename (* $PWD *)
    (** path in the packager filesystem, contains the collection of libraries and programs *)
  val lib : t -> Namespace.name -> filename (* $HOME_OPAM/lib/NAME *)
    (** installed libraries for the package (at most one version installed) *)
  val bin : t -> filename (* $HOME_OPAM/bin *)
    (** contain installed binaries *)
  val config : t -> filename (* $HOME_OPAM/config *)
    (** main configuration file *)
  val installed : t -> filename (* $HOME_OPAM/installed *)
    (** list of installed packages with their version *)
  val index_opam : t -> name_version option -> filename (* $HOME_OPAM/index/NAME-VERSION.opam *)
    (** OPAM files considered for an arbitrary version and package *)
  val index_opam_list : t -> name_version list (* [ $HOME_OPAM/index/NAME-VERSION.opam ] -> [ NAME, VERSION ] *)
    (** list of OPAM files *)
  val archives_targz : t -> name_version option -> filename (* $HOME_OPAM/archives/NAME-VERSION.tar.gz *)
    (** source archives for all versions of all packages *)
  val build : t -> name_version option -> filename (* $HOME_OPAM/build/NAME-VERSION *)
    (** tempory folders used to decompress the corresponding archives *)
  val to_install : t -> name_version -> filename (* $HOME_OPAM/build/NAME-VERSION/NAME.install *)
    (** compiled files in the extracted archive to install *)

  (** **)

  val find : t -> filename -> binary_data contents
    (** Retrieves the contents from the hard disk. *)

  val add : t -> filename -> binary_data contents -> t
    (** Removes everything in [filename] if existed, then write [contents] instead. *)

  val add_rec : t -> filename -> binary_data contents_rec -> t
    (** Removes everything in [filename] if existed, then write [contents_rec] inside [filename]. *)

  val extract_targz : t -> binary_data archive -> binary_data contents_rec
    (** Returns the same meaning as [archive] but in extracted form. *)

  val exec_buildsh : t -> name_version -> t
    (* $HOME_OPAM/build/NAME-VERSION/build.sh *)
    (** Executes this particularly named script. *)

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
           ; home : string }

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
  let init o s = { computer = o ; home = home // s }

  let root = Raw "/"
  let proot _ = normalize "."
  let lib t (Namespace.Name n) = Raw (t.home // "lib" // n)
  let bin t = Raw (t.home // "bin")

  let mk_name_version d ext t n v = Raw (t.home // d // sprintf "%s%s" (Namespace.string_of_nv n v) ext)

  let mk_name_version_o name ext t = 
      function
        | None -> Raw (t.home // name)
        | Some (n, v) -> mk_name_version name ext t n v

  let index_opam = mk_name_version_o "index" ".opam"
  let archives_targz = mk_name_version_o "archives" ".tar.gz"

  let build = mk_name_version_o "build" ""
  let installed t = Raw (t.home // "installed")
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
      (fun fic -> File (Binary (BatFile.with_file_in fic BatIO.read_all)))
      Not_exists

  let chop_extension (B s) = Filename.chop_extension s
  let file_exists f = Sys.file_exists (s_of_filename f)

  let index_opam_list t =
    BatList.map (fun fic -> Namespace.nv_of_string (chop_extension fic))
      (match find t (index_opam t None) with
        | Directory l -> l
        | _ -> [])

  let add t f =
    function 
      | Directory d -> failwith "to complete !"
      | File (Binary cts) -> 
          let () = contents (fun _ -> failwith "to complete !") Unix.unlink () t f in
          let fic = s_of_filename f in
          let () = BatFile.with_file_out fic (fun oc -> BatString.print oc cts) in
            t
      | Not_exists -> failwith "to complete !"

  let compare_computer t1 t2 = compare t1.computer t2.computer

  let exec_buildsh t n_v = 
    let _ = Sys.chdir (s_of_filename (build t (Some n_v))) in
    let _ = Sys.command "build.sh" in
      t
  let basename s = B (Filename.basename (s_of_filename s))

  let extract_targz _ = failwith "to complete !"
  let add_rec _ = failwith "to complete !"

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

    (** destruct *)
    val version : t -> version
    val sources : t -> Path.url option


    (** construct *)
    val config : version -> Path.url option -> t
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
    type t = { version : version ; sources : Path.url option }

    let version t = t.version
    let sources t = t.sources
    let config version sources = { version ; sources }

    let ocamlpro_http = "opam.ocamlpro.com"
    let ocamlpro_port = 9999
    let empty1 = { version = Version "" ; sources = Some (Path.url ocamlpro_http (Some ocamlpro_port)) }
    let empty2 = { version = Version "" ; sources = None }

    let find t f = 
      match Path.find t f with
        | Path.File (Binary s) -> 
            (match parse_colon s with
               |  ("version", version)
               :: ("sources", sources)

               :: _ -> { version = Version version
                       ; sources = 
                          try Some (let hostname, port = BatString.split sources ":" in
                                      Path.url hostname (try Some (int_of_string port) with _ -> None)) with _ -> None }
               | _ -> empty1)
        | _ -> empty2

    let to_string t =
      Printf.sprintf "
version: %s
sources: %s" 
        (Namespace.string_user_of_version t.version)
        (match t.sources with None -> Printf.sprintf "%s:%d" ocamlpro_http ocamlpro_port | Some sources -> Path.string_of_url sources)

    let add t f v = Path.add t f (Path.File (Binary (to_string v)))
  end

  module type CUDF =
  sig
    include IO_FILE

    type package

    (** destruct *)
    val opam_version : t -> version
    val package : t -> package

    val name : package -> name
    val version : package -> version
    val description : package -> string


    (** construct *)
    val new_package : name_version -> string (* description *) -> package
    val cudf : version -> package -> t
  end

  module Cudf : CUDF =
  struct
    type package =
        { name : name 
        ; version : version
        ; description : string }

    let name p = p.name
    let version p = p.version
    let description p = p.description
    let new_package (name, version) description = { name ; version ; description }

    type t = 
        { opam_version : version
        ; package : package }

    let opam_version t = t.opam_version
    let package t = t.package
    let cudf opam_version package = { opam_version ; package }

    let empty = 
      { opam_version = Version ""
      ; package = { name = Name "" ; version = Version "" ; description = "" } }

    let find t f =
      match Path.find t f with
        | Path.File (Binary s) -> 
            (match parse_colon s with
               |  ("opam-version", opam_version)
               :: ("package", name)
               :: ("version", version)
               :: ("description", description)

               :: _ -> { opam_version = Version opam_version
                       ; package = { name = Name name 
                                   ; version = Version version
                                   ; description } }
               | _ -> empty)
        | _ -> empty

    let to_string t = 
      Printf.sprintf "
opam-version: %s

package: %s
version: %s
description: %s" 
        (Namespace.string_user_of_version t.opam_version)

        (Namespace.string_user_of_name t.package.name)
        (Namespace.string_user_of_version t.package.version)
        t.package.description

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
            BatList.map (fun (name, version) -> Name name, Version version) (parse_space s)
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

module type SOLVER =
sig
  type package (* name, version, conflicts, dependencies *)

  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type 'a action = 
    | To_change of 'a 
        (* Version to install. The package could have been present or not, 
           but if present, it is another version than the proposed solution. *)
    | To_delete (* The package has been installed. *)
    | To_recompile (* The package is already installed, we just recompile it. *)

  type solution = (Namespace.name * Namespace.version action) list
      (** Sequence describing the action to perform.
          Order natural : first element to execute is the first element of the list. *)

  val resolve : package list -> name_version request -> solution list
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

  val version : t -> Namespace.version

  val package : opam -> package option 
    (** [None] : the [opam] associated to the [(name, version)] does not exist. 
        Note that every [(name, version)] given by [getList] do exist. *)
end

module Server
  (F_cudf : File.CUDF) 
  : SERVER with type package = F_cudf.package =
struct
  module Path_map = BatMap.Make (struct type t = Path.t let compare = Path.compare_computer end)

  type t = 
      { current_repository : F_cudf.package NV_map.t
      ; home : Path.t (* ~/.opam-server *)
      ; all_repository : F_cudf.package NV_map.t Path_map.t
      ; package_manager : Namespace.version }

  type opam = name_version * F_cudf.package option
      (* [None] : the current repository does not contain the package associated to the [name] and [version] *)

  type package = F_cudf.package

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
    let home = Path.init o ".opam-server" in
    { current_repository = read_archives home
    ; home
    ; all_repository = Path_map.empty
    ; package_manager = Namespace.Version "1.0" }

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

  let version t = t.package_manager
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
end

module Client 
  (F_config : File.CONFIG) 
  (F_installed : File.INSTALLED) 
  (F_cudf : File.CUDF) 
  (F_toinstall : File.TO_INSTALL)
  (Solver : SOLVER with type package = F_cudf.package)
  (Server : SERVER with type package = F_cudf.package) 
  (P : File.PRINTF)
  : CLIENT =
struct
  type t = 
      { server : Server.t
      ; home : Path.t (* ~/.opam *)
      ; stdout : P.t }

  let init0 x =
    let home = Path.init None ".opam" in
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
                    (Server.version t.server)
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

  let proceed_tochange v t name =
    let p_targz, p_build = 
      Path.archives_targz t.home (Some (name, v)),
      Path.build t.home (Some (name, v)) in
      
    let t, tgz = 
      if Path.file_exists p_targz then
        t, Path.R_filename (BatList.map (Path.concat p_build) (match Path.find t.home p_build with Path.Directory l -> l | _ -> []))
      else
        let tgz = Path.extract_targz t.home (Server.getArchive t.server (Server.getOpam t.server (name, v))) in
          { t with home = Path.add_rec t.home p_build tgz }, tgz in
      
    let to_install =
      F_toinstall.find 
        (Path.exec_buildsh
           (Path.add_rec t.home (Path.build t.home (Some (name, v))) tgz) 
           (name, v))
        (Path.to_install t.home (name, v)) in

    let filename_of_path_relative t path = 
      Path.R_filename (F_toinstall.filename_of_path_relative t.home
                         (Path.build t.home (Some (name, v))) 
                         path) in
      
    let add_rec f_lib t path = 
      { t with home = 
          Path.add_rec t.home 
            (f_lib t.home name (* warning : we assume that this result is a directory *))
            (filename_of_path_relative t path) } in

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

  let proceed_torecompile _ = failwith "to complete !"
  let proceed_todelete _ = failwith "to complete !"

  let resolve t l_index request = 

    let rec aux chan = function
      | x :: xs -> 
          let ok, chan = 
            confirm_ (BatIO.to_string (BatList.print ~first:"" ~last:"" ~sep:", " 
                                         (fun oc (name, act) -> 
                                            let f s = BatString.print oc (Printf.sprintf "%s : %s" (Namespace.string_user_of_name name) s) in
                                              match act with
                                                | Solver.To_change v -> f (Printf.sprintf "-> %s" (Namespace.string_user_of_version v))
                                                | Solver.To_recompile -> ()
                                                | Solver.To_delete -> f "remove"
                                         )) x) chan in
            if ok then
              chan, Some x
            else
              aux chan xs
                
      | [] -> chan, None in
      
    let stdout, o =
      aux t.stdout 
        (Solver.resolve 
           (BatList.map (fun n_v -> F_cudf.package (F_cudf.find t.home (Path.index_opam t.home (Some n_v)))) l_index)
           request) in

    let t = { t with stdout } in
      match o with
        | Some sol -> 
            List.fold_left (fun t (name, action) ->
                              (match action with 
                                  | Solver.To_change v -> proceed_tochange v
                                  | Solver.To_delete -> proceed_todelete
                                  | Solver.To_recompile -> proceed_torecompile) t name) t sol
        | None -> t

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
          resolve t l_index { Solver.wish_install = [ name, V_set.max_elt v ] ; wish_remove = [] ; wish_upgrade = [] }

  let upgrade t =
      resolve t (Path.index_opam_list t.home) 
        { Solver.wish_install = [] ; wish_remove = [] ; wish_upgrade = F_installed.find t.home (Path.installed t.home) }
    
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
    { t with stdout = 
        P.printf t.stdout "-I %s" 
          (match Path.ocaml_options_of_library t.home name with
             | I s -> s) }
end


(* ************* *)

module Solver
  (F_cudf : File.CUDF)
  : SOLVER with type package = F_cudf.package = 
struct
  type package = F_cudf.package
  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type 'a action = 
    | To_change of 'a 
    | To_delete
    | To_recompile

  type solution = (Namespace.name * Namespace.version action) list
  let resolve _ _ = []
end

open File
module Solv = Solver (Cudf)
module S = Server (Cudf)
module C = Client (Config) (Installed) (Cudf) (To_install) (Solv) (S) (P)

open Namespace

let _ = 
  let client = C.init0 () in
  match Array.to_list Sys.argv with

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

    | _ -> client
