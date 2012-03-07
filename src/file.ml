open Namespace
open Path

let read_content file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

module File =
struct
  open Namespace

  module type IO_FILE =
  sig
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> unit
  end

  module type CONFIG =
  sig
    include IO_FILE

    val version_of_string : string -> internal_version

    (** destruct *)
    val package_manager : t -> internal_version
    val sources : t -> url
    val ocaml_version : t -> internal_version


    (** construct *)
    val config : internal_version (* opam *) -> url -> internal_version (* ocaml *) -> t
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
    type t = { version : internal_version ; sources : url ; ocaml_version : internal_version }

    let version_of_string s = Version s

    let package_manager t = t.version
    let sources t = t.sources
    let ocaml_version t = t.ocaml_version
    let config version sources ocaml_version = { version ; sources ; ocaml_version }

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

    let log fmt =
      Globals.log "FILE.CONFIG" fmt

    let find t f =
      log "read %s" (Path.string_of_filename f);
      let aux contents =
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
        { version = Version version; sources; ocaml_version = Version ocaml_version } in

      let t = match Path.find_binary t f with
      | Path.File (Raw_binary s)     -> aux s
      | Path.Directory _ -> failwith (Printf.sprintf "%s is a directory" (Path.string_of_filename f))
      | Path.Not_exists  -> failwith (Printf.sprintf "%s does not exist" (Path.string_of_filename f)) in

      log "contents:\n%s" (to_string t);
      t

    let add t f v = Path.add t f (Path.File (Binary (Raw_binary (to_string v))))
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

  module Cudf : CUDF = 
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

    let empty_preamble = Some { Cudf.default_preamble with Cudf.property = [ s_description, `String None ] }

    let cudf opam_version pkg = { opam_version ; package = { preamble = empty_preamble ; pkg = [ pkg ] ; request = None } }

    let empty = 
      { opam_version = Version Globals.opam_version
      ; package = { preamble = empty_preamble ; pkg = [] ; request = None } }

    let find t f =
      match Path.find_binary t f with
        | Path.File (Raw_binary s) -> 
          (match 
              try
                Some (Cudf_parser.parse (Cudf_parser.from_IO_in_channel (IO.input_string s)))
              with _ -> None
           with
             | None -> empty
             | Some (preamble, pkg, request) -> 
               { opam_version = 
                   (match find_field "opam_version" pkg with
                      | Some (`String v) -> Config.version_of_string v
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
          IO.write oc '\n';
          List.iter (Cudf_printer.pp_io_package oc) t.package.pkg;
          IO.write oc '\n';
          (match t.package.request with
            | Some request -> Cudf_printer.pp_io_request oc request
            | None -> ());
        end in
      IO.close_out oc

    let add t f v = Path.add t f (Path.File (Binary (Raw_binary (to_string v))))
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
      match Path.find_binary t f with
        | Path.File (Raw_binary s) -> 
            BatList.map (fun (name, version) -> Name name, version_of_string name version) (parse_space s)
        | _ -> empty

    let to_string = 
      BatIO.to_string
        (BatList.print (fun oc (name, version) ->
          BatString.print oc
            (Printf.sprintf "%s %s" 
               (Namespace.string_user_of_name name) 
               (Namespace.string_user_of_version version))))

    let add t f v = Path.add t f (Path.File (Binary (Raw_binary (to_string v))))
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
      match Path.find_binary t f with
        | Path.File (Raw_binary s) -> 

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

    let add t f v = Path.add t f (Path.File (Binary (Raw_binary (to_string v))))
  end
end
