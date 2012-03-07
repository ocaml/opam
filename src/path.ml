open Namespace

let log fmt =
  Globals.log "PATH" fmt

type url = {
  hostname: string;
  port: int;
}

let url hostname port = { hostname; port }

let string_of_url url =
  Printf.sprintf "%s:%d" url.hostname url.port

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

  type 'a contents = 
    | Directory of basename list
    | File of 'a
    | Not_exists

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) list
    | R_file of 'a
    | R_filename of filename list

  val init : string (* $HOME_OPAM *) -> t
  (* $HOME_OPAM_OVERSION = $HOME_OPAM/OVERSION *)

  (** definitions of some shortcuts *)

  (** the root of every path *)
  val root : filename (* ~/ *)

  (** path in the packager filesystem, contains the collection of libraries and programs *)
  val package : t -> string (* computed from $PWD *) -> filename

  (** installed libraries for the package (at most one version installed) *)
  val lib : t -> Namespace.name -> filename (* $HOME_OPAM_OVERSION/lib/NAME *)

  (** contain installed binaries *)
  val bin : t -> filename (* $HOME_OPAM_OVERSION/bin *)
  
  (** main configuration file *)
  val config : t -> filename (* $HOME_OPAM/config *)

  (** list of installed packages with their version *)
  val installed : t -> filename (* $HOME_OPAM_OVERSION/installed *)

  (** OPAM files considered for an arbitrary version and package *)
  val index_opam : t -> name_version option -> filename (* $HOME_OPAM/index/NAME-VERSION.opam *)
  (* THOMAS: why option *)

  (** list of OPAM files *)
  val index_opam_list : t -> name_version list (* [ $HOME_OPAM/index/NAME-VERSION.opam ] -> [ NAME, VERSION ] *)

  (** source archives for all versions of all packages *)
  val archives_targz : t -> name_version option -> filename (* $HOME_OPAM/archives/NAME-VERSION.tar.gz *)
  (* THOMAS: why option *)

  (** tempory folders used to decompress the corresponding archives *)
  val build : t -> name_version option -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION *)
  (* THOMAS: why option *)

  (** compiled files in the extracted archive to install *)
  val to_install : t -> name_version -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION/NAME.install *)


  (** **)

  (** Retrieves the contents from the hard disk. *)
  val find : t -> filename -> binary_data contents

  (** Removes everything in [filename] if existed. *)
  val remove : t -> filename -> unit

  (** Removes everything in [filename] if existed, then write [contents] instead. *)
  val add : t -> filename -> binary_data contents -> unit

  (** Removes everything in [filename] if existed, then write [contents_rec] inside [filename]. *)
  val add_rec : t -> filename -> binary_data contents_rec -> unit

  (** Returns the same meaning as [archive] but in extracted form. *)
  val extract_targz : t -> binary_data archive -> binary_data contents_rec

  (** Considers the given [filename] as the contents of an [archive] already extracted. *)
  val raw_targz : filename -> binary_data archive

  (** Executes this particularly named script. *)
  val exec_buildsh : t -> name_version -> unit
  (* $HOME_OPAM/build/NAME-VERSION/build.sh *)
  
  (** see [Filename.dirname] *)
  val dirname : filename -> filename

  (** see [Filename.basename] *)
  val basename : filename -> basename

  (** We iterate [Filename.chop_extension] on [basename] until a fix
      point is reached.  When [basename] is not of the form
      "NAME-VERSION", or when we can not extract the version, [string]
      is returned as version. *)
  val nv_of_extension : string (* version *) -> basename -> Namespace.name * Namespace.version

  (** see [Filename.concat] *)
  val concat : filename -> basename -> filename

  (** see [Sys.file_exists] *)
  val file_exists : filename -> bool

  (** Returns the exact path to give to the OCaml compiler (ie. -I ...) *)
  val ocaml_options_of_library : t -> Namespace.name -> string ocaml_options 
  (* $HOME_OPAM/lib/NAME *)

  val string_of_filename: filename -> string
end

module Path : PATH = struct
  open Printf

  type filename = 
    | Normalized of string
    | Raw of string

  type t = { home : string
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
  let init s = 
    let home = home // s in
    { home ; home_ocamlversion = home // Globals.default_ocaml_version }

  let root = Raw "/"
  let package _ s = Raw (Printf.sprintf "%s" s)
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

  let contents f_dir f_fic f_not_exists t f = 
    let fic = s_of_filename f in
    if Sys.file_exists fic then
      (if Sys.is_directory fic then f_dir else f_fic) fic
    else
      f_not_exists

  let find = 
    contents
      (fun fic -> Directory (BatList.of_enum (BatEnum.map (fun s -> B s) (BatSys.files_of fic))))
      (fun fic -> File ((*Binary (BatFile.with_file_in fic BatIO.read_all)*)Filename fic))
      Not_exists

  let nv_of_extension version (B s) = 
    let s = 
     let rec aux s =
       match try Some (Filename.chop_extension s) with _ -> None with 
       | Some s -> aux s
       | _ -> s in
     aux s in

    match try Some (Namespace.nv_of_string s) with _ -> None with
    | Some nv -> nv
    | None -> Namespace.Name s, Namespace.version_of_string s version

  let file_exists f = Sys.file_exists (s_of_filename f)

  let index_opam_list t =
    BatList.map (nv_of_extension Namespace.default_version)
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
    aux (s_of_filename f)

  module U = struct
    let mkdir f f_to = 
      let rec aux f_to = 
        if Sys.file_exists f_to then
          ()
        else begin
          aux (Filename.dirname f_to);
          Unix.mkdir f_to 0o755;
        end in
      aux (Filename.dirname f_to);
      f f_to
  
    let link f_from = mkdir (Unix.link f_from)

  end

  let add t f content =
    log "add %s" (s_of_filename f);
    match content with
    | Directory d -> failwith "to complete !"
    | File (Binary cts) -> 
        let () = contents (fun _ -> failwith "to complete !") Unix.unlink () t f in
        let fic = s_of_filename f in
        U.mkdir (fun fic -> BatFile.with_file_out fic (fun oc -> BatString.print oc cts)) fic
    | File (Filename fic) -> 
        begin match (Unix.lstat fic).Unix.st_kind with
        | Unix.S_DIR -> 
            let () = contents (fun _ -> ()) (fun _ -> failwith "to complete !") () t f in
            let rec aux f_from f_to = 
              (match (Unix.lstat f_from).Unix.st_kind with
              | Unix.S_DIR -> List.fold_left (fun _ b -> aux (f_from // b) (f_to // b)) () (BatSys.files_of f_from)
              | Unix.S_REG -> 
                  let () = 
                    if Sys.file_exists f_to then
                      Unix.unlink f_to
                    else
                      () in
                  U.link f_from f_to
              | _ -> failwith "to complete !") in
            aux fic (s_of_filename f)
        | _ -> Printf.kprintf failwith "to complete ! copy the given filename %s" fic
        end
    | Not_exists -> ()

  let exec_buildsh t n_v = 
    let _ = Sys.chdir (s_of_filename (build t (Some n_v))) in
    let _ = Sys.command "build.sh" in
    ()

  let basename s = B (Filename.basename (s_of_filename s))

  let extract_targz t = function
  | Tar_gz (Binary _) -> failwith "to complete ! check if the \"dose\" project has been configured with the correct option to extract the gzip or bz2, then use similars functions to extract" (*IO.read_all (Common.Input.open_file fic)*)
  | Tar_gz (Filename fic) -> R_filename [Raw fic]
  | Empty -> R_directory []

  let raw_targz f = Tar_gz (Filename (s_of_filename f))

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
        let f = f /// name in
        List.iter (fun (b, cts) -> aux t f b cts) l
    | R_file cts -> add t (f /// name) (File cts)
    | R_filename l -> 
        List.iter
          (fun fic -> 
            aux
              t
              f
              (basename fic)
              (match (lstat fic).Unix.st_kind with
              | Unix.S_DIR -> R_directory (BatList.map (fun f -> 
                let f = B f in
                f, R_filename [fic /// f]) (files_of fic))
              | Unix.S_REG -> R_file (Filename (s_of_filename fic))
              | _ -> failwith "to complete !")) l in
    aux t (dirname f) (basename f)

  let ocaml_options_of_library t name = 
    I (Printf.sprintf "%s" (s_of_filename (lib t name)))

  let string_of_filename = s_of_filename

end
