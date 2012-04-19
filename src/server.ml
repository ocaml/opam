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


open Namespace
open Path
open File
open Unix
open Uri
open Protocol

module type SERVER =
sig
  type t

  (** [apiVersion t client] returns whether the API version for the
      server.  If the server cannot handle the [client] version, it
      raises an exception.  If the client cannot handle the server
      version, it should take the appropriate decisions.*)
  val apiVersion : t -> int -> int

  (** Return the list of the available versions for all packages. *)
  val getList : t -> name_version list

  (** Returns the spec file for the corresponding package version. *)
  val getSpec : t -> name_version -> raw_binary

  (** Returns the corresponding package archive. [None] means that the
      server does not mirror the associated archive. *)
  val getArchive : t -> name_version -> raw_binary option

  (** Process a first upload: it contains a spec file and a possible
      corresponding archive.
      The function returns the secret key associated to
      all the packages having the same name *)
  val newArchive : t -> name_version -> raw_binary -> raw_binary -> security_key

  (** Same as [newArchive] but for subsequent upload (using the secret key) *)
  val updateArchive : t -> name_version -> raw_binary -> raw_binary -> security_key -> unit

end

type server_state =
    { home : Path.t (* ~/.opam-server *)
    ; opam_version : int }

let server_init home = 
  { home = Path.init home
  ; opam_version = Globals.api_version }

exception Server_error of string

let error fmt =
  Printf.kprintf (fun s -> raise (Server_error s)) fmt

(* Used by the server to process things *)
module Server : SERVER with type t = server_state = struct

  type t = server_state

  (* Return all the .opam files *)
  let read_index home =
    List.fold_left
      (fun map nv ->
        let file = File.Spec.find_err (Path.index home (Some nv)) in
        NV_map.add nv file map)
      NV_map.empty
      (Path.index_list home)

  let string_of_nv (n, v) = Namespace.string_of_nv n v

  let apiVersion t client =
    if client <> Globals.api_version then
      error "incompatible API version. client=%d server=%d"
        client Globals.api_version
    else
      Globals.api_version

  let getList t =
    Path.index_list t.home

  let getSpec t n_v =
    let index = read_index t.home in
    try Raw_binary (File.Spec.to_string (File.Spec.filter_external_ressources (NV_map.find n_v index)))
    with Not_found -> error "%S not found" (string_of_nv n_v)

  let getArchive t n_v = 
    let spec = File.Spec.find_err (Path.index t.home (Some n_v)) in

    (* look at an archive in the same path
       having the right name *)
    let p = Path.archives_targz t.home (Some n_v) in
    match Path.find_binary p with
      | Path.File s -> Some s
      | _           -> 
        match File.Spec.sources spec with
          | [] -> error "Cannot find %S in local repository and no external url provided" (string_of_nv n_v)
          | urls ->
          (* if some urls are provided, check for external urls *)
          let external_urls =
            List.fold_left (fun accu -> function External (_,s), o -> (s, o) :: accu | _ -> accu) [] urls in
          if external_urls <> [] then
            (* clients can fetch archives *)
            None
          else
            error "No archive associated to package %S" (string_of_nv n_v)

  let storeArchive t n_v opam archive =
    let opam_file = Path.index t.home (Some n_v) in
    let archive_file = Path.archives_targz t.home (Some n_v) in
    begin match opam with
      | Raw_binary s -> File.Spec.add opam_file (File.Spec.parse s)
    end;
    Path.add archive_file (Path.File (Binary archive))

  let processArchive o_key t (name, v) spec archive = 
    let hashes = Path.hashes t.home name in
    let key = 
      match o_key, File.Security_key.find hashes with 
      | None, None ->
          let key = Random_key.new_key () in
          File.Security_key.add hashes key;
          key
      | Some k0, Some k1 -> 
          if k0 = k1 then
            k0
          else
            error
              "secret keys differ for package %S"
              (Namespace.string_of_name name)
      | Some _, None ->
          error
            "no previous keys stored for package %S"
            (Namespace.string_of_name name)
      | None, Some _ ->
          error
            "no key given and the package %S has already been uploaded"
            (Namespace.string_of_name name) in
    storeArchive t (name, v) spec archive;
    key

  let newArchive = processArchive None

  let updateArchive t n_v spec archive key =
    let (_ : security_key) = processArchive (Some key) t n_v spec archive in
    ()
end


exception Connection_error of string
let connection_error fmt =
  Printf.kprintf (fun str ->
    raise (Connection_error str)
  ) fmt

(* Used by the client to communicate with the server *)
module RemoteServer : SERVER with type t = url = struct

  open Protocol

  type t = url

  (* untyped message exchange *)
  let send url m =
    if url.uri = Some Git then
      connection_error "%s is not a valid OPAM server" url.hostname;
    let host =
      try (gethostbyname url.hostname).h_addr_list.(0)
      with Not_found ->
        connection_error "%s is not a valid host address" url.hostname in
    let port = match url.port with
      | None   -> Globals.error_and_exit "No port provided"
      | Some p -> p in
    let addr = ADDR_INET (host, port) in
    try
      Protocol.find (open_connection addr) m
    with _ ->
      connection_error
        "The server (%s) is unreachable. Please check your network configuration."
        (string_of_url url)

  let dyn_error str =
    Globals.error_and_exit "Protocol error: %S" str

  let error msg =
    Globals.error_and_exit "SERVER: %s" msg

  let apiVersion t s =
    match send t (C2S_apidVersion s) with
    | S2C_apiVersion nl -> nl
    | S2C_error s       -> error s
    | _                 -> dyn_error "apiVersion"

  let check_version t =
    let server_version = apiVersion t Globals.api_version in
    if server_version <> Globals.api_version then
     Globals.error_and_exit "API version error. client=%d server=%d"
       Globals.api_version server_version

  let getList t =
    check_version t;
    match send t C2S_getList with
    | S2C_getList nl -> nl
    | S2C_error s    -> error s
    | _              -> dyn_error "getList"

  let getSpec t name_version =
    check_version t;
    match send t (C2S_getSpec name_version) with
    | S2C_getSpec o -> o
    | S2C_error s   -> error s
    | _             -> dyn_error "getOpam"

  let getArchive t nv =
    check_version t;
    match send t (C2S_getArchive nv) with
    | S2C_getArchive a -> a
    | S2C_error s      -> error s
    | _                -> dyn_error "getArchive"

  let newArchive t nv opam archive =
    check_version t;
    match send t (C2S_newArchive (nv, opam, archive)) with
    | S2C_newArchive o -> o
    | S2C_error s      -> error s
    | _                -> dyn_error "newArchive"

  let updateArchive t nv opam archive k =
    check_version t;
    match send t (C2S_updateArchive (nv, opam, archive, k)) with
    | S2C_updateArchive -> ()
    | S2C_error s       -> error s
    | _                 -> dyn_error "updateArchive"

end

module Daemon = struct

  open Protocol

  let log id fmt =
    Globals.log (Printf.sprintf "[%s]" id) fmt

  let protect f x =
    try f x
    with e ->
      let msg = Printexc.to_string e in
      Globals.error "%s" msg;
      S2C_error msg

  let file = Filename.temp_file "lock" "dat"

  let flock id =
    let l = ref 0 in
    let rec loop () =
      if Sys.file_exists id && !l < 5 then begin
        log id "Filesytem busy. Waiting 1s (%d)" !l;
        sleep 1;
        loop ()
      end else if Sys.file_exists id then begin
        log id "Too many attemps. Cancelling ...";
        error "Too many attemps. Cancelling ...";
      end else begin
        let oc = open_out file in
        output_string oc id;
        flush oc;
        close_out oc;
        log id "lock %s" id;
      end in
    loop ()
    
  let funlock id =
    if Sys.file_exists file then
      let ic = open_in file in
      let s = input_line ic in
      if s = id then begin
        log id "unlock %s" id;
        Unix.unlink file;
      end

  let with_flock id f =
    try
      flock id;
      let r = f () in
      funlock id;
      r
    with e ->
      funlock id;
      raise e

  let process t id request =
    log id "Processing an incoming request";
    match request with

    | C2S_apidVersion s ->
        log id "acceptedVersion";
        protect (fun () -> S2C_apiVersion (Server.apiVersion t s)) ()

    | C2S_getList ->
        log id "getList";
        protect (fun () -> S2C_getList (Server.getList t)) ()

    | C2S_getSpec nv ->
        log id "getSpec";
        protect (fun () -> S2C_getSpec (Server.getSpec t nv)) ()

    | C2S_getArchive nv ->
        log id "getArchive";
        protect (fun () -> S2C_getArchive (Server.getArchive t nv)) ()

    | C2S_newArchive (nv, opam, archive) ->
        log id "newArchive";
        protect
          (with_flock id)
          (fun () -> S2C_newArchive (Server.newArchive t nv opam archive))

    | C2S_updateArchive (nv, opam, archive, k) ->
        log id "updateArchive [%s]"
          (Digest.to_hex (Digest.string (match k with Random s -> s)));
        protect
          (with_flock id)
          (fun () ->
            Server.updateArchive t nv opam archive k;
            S2C_updateArchive)

end
