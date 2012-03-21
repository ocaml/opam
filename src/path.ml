open ExtList
open ExtString
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

type raw_binary = 
  | Raw_binary of string (* contents *)

type raw_filename =
  | Raw_filename of string (* pointer to the contents *)

type binary_data = 
  | Binary of raw_binary
  | Filename of raw_filename

let binary s = Binary (Raw_binary s)
let filename s = Filename (Raw_filename s)

type 'a archive = 
  | Tar_gz of 'a

type basename = B of string

(** Type used to represent an internal form of version, which is in
    particular not related to the version of a particular package *)
type internal_version = Version of string

type security_key = Random of string

module type RANDOM_KEY = sig
  val new_key : unit -> security_key
end

module Random_key : RANDOM_KEY = struct
  let make n = Random (String.implode (List.init n (fun _ -> char_of_int (Random.int 255))))

  let len = 128

  let n = ref (make len)

  let new_key _ = 
    let k = !n in
    let () = n := make len in
    k
end


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

  let copy src dst =
    log "Copying %s to %s" src dst;
    let n = 1024 in
    let b = String.create n in
    let read = ref 0 in
    let ic = open_in src in
    let oc = open_out dst in
    while !read <>0 do
      read := input ic b 0 n;
      output oc b 0 !read;
    done;
    close_in ic;
    close_out oc

  let read_content file =
    let ic = open_in file in
    let n = in_channel_length ic in
    let s = String.create n in
    really_input ic s 0 n;
    close_in ic;
    s

  (**************************)
  (* from ocplib-system ... *)
  (**************************)

  let in_dir dir fn =
    let cwd = Unix.getcwd () in
    Unix.chdir dir;
    try
      let r = fn () in
      Unix.chdir cwd;
      r
    with e ->
      Unix.chdir cwd;
      raise e

  let directories () =
    let d = Sys.readdir (Unix.getcwd ()) in
    let d = Array.to_list d in
    List.filter (fun f -> try Sys.is_directory f with _ -> false) d

  let files () =
    let d = Sys.readdir (Unix.getcwd ()) in
    let d = Array.to_list d in
    List.filter (fun f -> try not (Sys.is_directory f) with _ -> true) d

  let safe_unlink file =
    try Unix.unlink file
    with Unix.Unix_error _ -> ()

  let rec safe_rmdir dir =
    if Sys.file_exists dir then begin
      in_dir dir (fun () ->
        let dirs = directories () in
        let files = files () in
        List.iter safe_unlink files;
        List.iter safe_rmdir dirs;
      );
      Unix.rmdir dir;
    end

  let getchdir s = 
    let p = Unix.getcwd () in
    let () = Unix.chdir s in
    p
end


module type PATH =
sig

  type t
  type filename

  type 'a contents = 
    | Directory of basename list
    | File of 'a
    | Not_found of string

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) Enum.t
    | R_file of 'a
    | R_filename of filename list
    | R_lazy of (unit -> unit) (* The data describing the contents will appear as soon as : 
                                  1. the current directory is the directory where one would the contents appear,
                                  2. this function is executed. *)

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
  val index : t -> name_version option -> filename (* $HOME_OPAM/index/NAME-VERSION.spec *)
  (* [None] : $HOME_OPAM/index *)

  (** list of spec files *)
  val index_list : t -> name_version list (* [ $HOME_OPAM/index/NAME-VERSION.spec ] -> [ NAME, VERSION ] *)

  (** source archives for all versions of all packages *)
  val archives_targz : t -> name_version option -> filename (* $HOME_OPAM/archives/NAME-VERSION.tar.gz *)
  (* [None] : $HOME_OPAM/archives *)

  (** tempory folders used to decompress the corresponding archives *)
  val build : t -> name_version option -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION *)
  (* [None] : $HOME_OPAM_OVERSION/build *)

  (** compiled files in the extracted archive to install *)
  val to_install : t -> name_version -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION/NAME.install *)

  (** package configuration for compile and link options *)
  val pconfig : t -> name_version -> filename (* $HOME_OPAM_OVERSION/build/NAME-VERSION/NAME.config *)

  (** security keys related to package name *)
  val keys : t -> Namespace.name -> filename (* $HOME_OPAM/keys/NAME *)

  (** similar as [keys] *)
  val hashes : t -> Namespace.name -> filename (* $HOME_OPAM/hashes/NAME *)


  (** Path utilities **)

  (** Retrieves the contents from the hard disk. *)
  val find : filename -> binary_data contents

  (** see [find] *)
  val find_binary : filename -> raw_binary contents

  (** see [find] *)
  val find_filename : filename -> raw_filename contents

  (** Removes everything in [filename] if existed. *)
  val remove : filename -> unit

  (** Removes everything in [filename] if existed, then write [contents] instead. *)
  val add : filename -> binary_data contents -> unit

  (** Removes everything in [filename] if existed, then write [contents_rec] inside [filename]. *)
  val add_rec : filename -> binary_data contents_rec -> unit

  (** Returns the same meaning as [archive] but in extracted form. *)
  val extract_targz : binary_data archive -> binary_data contents_rec

  (** Considers the given [filename] as the contents of an [archive] already extracted. *)
  val raw_targz : filename -> binary_data archive

  (** Executes this particularly named script. For the [int], see [Sys.command]. *)
  val exec_buildsh : t -> name_version -> int
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

  (** [None] : not a directory *)
  val is_directory : filename -> raw_filename option

  (** Returns the exact path to give to the OCaml compiler (ie. -I ...) *)
  val ocaml_options_of_library : t -> Namespace.name -> string ocaml_options 
  (* $HOME_OPAM/lib/NAME *)

  val string_of_filename: filename -> string

  val file_not_found: filename -> 'a

  val read: (filename -> 'a option) -> filename -> 'a
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
    | Not_found of string

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) Enum.t
    | R_file of 'a
    | R_filename of filename list
    | R_lazy of (unit -> unit) 
        
  let s_of_filename = function
  | Normalized s -> s
  | Raw s -> s

  let filename_map f = function
  | Normalized s -> Normalized (f s)
  | Raw s -> Raw (f s)

  let normalize s = Normalized (U.getchdir (U.getchdir s))

  let (//) = sprintf "%s/%s"
  let concat f (B s) = filename_map (fun filename -> filename // s) f
  let (///) = concat
  let init home = 
    { home ; home_ocamlversion = home // Globals.ocaml_version }

  let root = Raw "/"
  let package _ s =
    (* REMARK this should be normalized *)
    Raw (Printf.sprintf "%s%s" (if s <> "" && s.[0] = '/' then "/" else "") (String.strip ~chars:"/" s))

  let lib t (Namespace.Name n) = Raw (t.home_ocamlversion // "lib" // n)
  let bin t = Raw (t.home_ocamlversion // "bin")

  let mk_name_version t_home d ext n v = Raw (t_home // d // sprintf "%s%s" (Namespace.string_of_nv n v) ext)

  let mk_name_version_o t_home name ext = 
    function
    | None -> Raw (t_home // name)
    | Some (n, v) -> mk_name_version t_home name ext n v

  let index t = mk_name_version_o t.home "index" ".spec"
  let archives_targz t = mk_name_version_o t.home "archives" ".tar.gz"

  let build t = mk_name_version_o t.home_ocamlversion "build" ""
  let installed t = Raw (t.home_ocamlversion // "installed")
  let config t = Raw (t.home // "config")

  let to_install t (n, v) = build t (Some (n, v)) /// B (Namespace.string_of_name n ^ ".install")

  let pconfig t (n, v) = build t (Some (n, v)) /// B (Namespace.string_of_name n ^ ".config")

  let keys t n = Raw (t.home // "keys" // Namespace.string_of_name n)
  let hashes t n = Raw (t.home // "hashes" // Namespace.string_of_name n)

  let contents f_dir f_fic f_notfound f =
    let fic = s_of_filename f in
    if Sys.file_exists fic then
      (if Sys.is_directory fic then f_dir else f_fic) fic
    else
      f_notfound fic

  let find_ f_fic = 
    contents
      (fun fic -> Directory (List.of_enum (Enum.map (fun s -> B s) (BatSys.files_of fic))))
      (fun fic -> File (f_fic fic))
      (fun fic -> Not_found fic)

  let find = find_ (fun fic -> Filename (Raw_filename fic))

  let find_binary = find_ (fun fic -> Raw_binary (BatFile.with_file_in fic BatIO.read_all))

  let find_filename = find_ (fun fic -> Raw_filename fic)

  let nv_of_extension version (B s) = 
    let s = 
      match BatString.right_chop s ".spec" with
        | Some s -> s
        | _ ->
          let rec aux s =
            match try Some (Filename.chop_extension s) with _ -> None with 
              | Some s -> aux s
              | _ -> s in
          aux s in

    match try Some (Namespace.nv_of_string s) with _ -> None with
    | Some nv -> nv
    | None -> Namespace.Name s, Namespace.version_of_string version

  let file_exists f = Sys.file_exists (s_of_filename f)

  let is_directory f = 
    let s = s_of_filename f in
    if Sys.is_directory s then
      Some (Raw_filename s)
    else
      None

  let index_list t =
    let files =
      match find (index t None) with
      | Directory l -> l
      | File _
      | Not_found _ -> [] in
    List.map (nv_of_extension Namespace.default_version) files

  let remove f = 
    let rec aux fic = 
      match (Unix.lstat fic).Unix.st_kind with
      | Unix.S_DIR -> 
          let () = Enum.iter (fun f -> aux (fic // f)) (BatSys.files_of fic) in
          Unix.rmdir fic
      | Unix.S_REG -> Unix.unlink fic
      | _ -> failwith "to complete !" in
    aux (s_of_filename f)

  let add f content =
    log "add %s" (s_of_filename f);
    match content with
    | Directory d -> failwith "to complete !"
    | File (Binary (Raw_binary cts)) -> 
        let fic = s_of_filename  f in
        U.mkdir
          (fun fic -> 
            begin
              U.safe_unlink fic; 
              BatFile.with_file_out fic (fun oc -> BatString.print oc cts);
            end)
          fic
    | File (Filename (Raw_filename fic)) ->
        begin match (Unix.lstat fic).Unix.st_kind with
        | Unix.S_DIR -> 
            U.safe_rmdir fic;
            let rec aux f_from f_to = 
              (match (Unix.lstat f_from).Unix.st_kind with
              | Unix.S_DIR -> Enum.fold (fun b _ -> aux (f_from // b) (f_to // b)) () (BatSys.files_of f_from)
              | Unix.S_REG -> 
                  let () = 
                    if Sys.file_exists f_to then
                      Unix.unlink f_to
                    else
                      () in
                  U.link f_from f_to
              | _ -> failwith "to complete !") in
            aux fic (s_of_filename f)
        | Unix.S_REG ->
          U.mkdir 
            (fun f_to -> 
              begin
                U.safe_unlink f_to;
                U.link fic f_to;
              end)
            (s_of_filename f)
        | _ -> Printf.kprintf failwith "to complete ! copy the given filename %s" fic
        end
    | Not_found s -> ()

  let exec_buildsh t n_v = 
    let _ = Sys.chdir (s_of_filename (build t (Some n_v))) in
    Sys.command "./build.sh"

  let basename s = B (Filename.basename (s_of_filename s))

  let extract_targz = function
  | Tar_gz (Binary (Raw_binary bin)) -> 
    R_lazy
      (fun () -> 
        let oc = BatUnix.open_process_out "tar xzv" in
        begin
          BatIO.write_string oc bin;
          BatIO.close_out oc;
        end)
  (*IO.read_all (Common.Input.open_file fic)*)
    
  | Tar_gz (Filename (Raw_filename fic)) -> failwith "to complete !" (* R_filename [Raw fic] *)

  let raw_targz f = Tar_gz (Filename (Raw_filename (s_of_filename f)))

  let lstat s = Unix.lstat (s_of_filename s)
  let files_of f = BatSys.files_of (s_of_filename f)

  let dirname = filename_map Filename.dirname

  let add_rec f = 
    log "add_rec %s" (s_of_filename f);
    let () = (* check that [f] is not a file *)
      contents
        (fun _ -> ())
        (fun _ -> failwith "to complete !") 
        (fun _ -> ())
        f in

    let f_filename f f_basename = 
      List.map
        (fun fic ->
          if Sys.file_exists (s_of_filename fic) then begin
            f, 
            f_basename fic,
            match (lstat fic).Unix.st_kind with
            | Unix.S_DIR ->
                R_directory (Enum.map 
                               (fun f -> 
                                 let f = B f in
                                 f, R_filename [fic /// f])
                               (files_of fic))
            | Unix.S_REG -> R_file (Filename (Raw_filename (s_of_filename fic)))
            | _ -> failwith "to complete !"
          end else
            Globals.error_and_exit "File %s does not exist." (s_of_filename fic))
    in

    let rec aux f (* <- filename dir *) name (* name of the value that will be destructed*) = function
    | R_directory l ->
        let f = f /// name in
        Enum.iter (fun (b, cts) -> aux f b cts) l
    | R_file cts -> add (f /// name) (File cts)
    | R_filename l -> 
        List.iter (fun (f, base_f, data) -> aux f base_f data) (f_filename f basename l)
    | R_lazy write_contents -> 
      U.mkdir
        (fun fic -> 
          let pwd = U.getchdir (Filename.dirname fic) in 
          begin 
            write_contents ();
            ignore (U.getchdir pwd);
          end) (s_of_filename (f /// name)) in

    function
      | R_filename l -> 
        List.iter (fun (f, base_f, data) -> aux f base_f data) (f_filename f (basename) l)
      | R_lazy _ as r_lazy -> 
        let f, name = dirname f, basename f in
        aux f name r_lazy
      | _ -> failwith "to complete !"

  let ocaml_options_of_library t name = 
    I (Printf.sprintf "%s" (s_of_filename (lib t name)))

  let string_of_filename = s_of_filename

  let file_not_found f =
    Globals.error_and_exit "%s not found" (string_of_filename f)

  let read f n = match f n with
  | None   -> file_not_found n
  | Some a -> a
end
