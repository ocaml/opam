open Namespace
open Path
open Server

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

module Solver = struct

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

  module CudfDiff : CUDFDIFF = struct

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

    module Cudf_set = struct
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
            (try Some (Common.CudfDiff.diff univ_init (Cudf.load_universe l_pkg_sol))
             with Cudf.Constraint_violation _ -> None))
        (cudf_resolve l_pkg_pb req)

    let resolve_diff = 
      resolve
        (fun _ diff -> 
          match 
            Hashtbl.fold (fun pkgname s acc ->
              let add x = x :: acc in
              match 
                (try Some (Cudf_set.choose_one s.Common.CudfDiff.removed) with Not_found -> None), 
                try Some (Cudf_set.choose s.Common.CudfDiff.installed) with Not_found -> None
              with
                | None, Some p -> add (To_change (Was_not_installed, p))
                | Some p, None -> add (To_delete p)
                | Some p_old, Some p_new -> add (To_change (Was_installed p_old, p_new))
                | None, None -> acc) diff []
          with
            | [] -> None
            | l -> Some l)

    let resolve_summary = resolve (fun univ_init diff -> Some (Common.CudfDiff.summary univ_init diff))
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


module type CLIENT =
sig
  type t

  val init0 : unit -> t

  (** Initializes the client a consistent state. *)
  val init : t -> url (* repository address *) option (* [None] : default is opam.ocamlpro.com, port = 9999 *) -> t

  (** Displays the installed package. [None] : a general summary is given. *)
  val info : t -> Namespace.name option -> t

  type config_request = Dir

  (** Returns the directory where the package is installed,
      in a form suitable to OCaml compilers (i.e. like "-I ..."). *)
  val config : t -> config_request -> Namespace.name -> t

  (** Installs the given package. *)
  val install : t -> Namespace.name -> t

  (** Downloads the latest packages available. *)
  val update : t -> t

  (** Finds a consistent state where most of the installed packages are
      upgraded to their latest version. *)
  val upgrade : t -> t

  (** Sends a new created package to the server. *)
  val upload : t -> string -> t

  (** Removes the given package. *)
  val remove : t -> Namespace.name -> t
end

module Client = struct
  open File

  type t = 
      { server : url
      ; home   : Path.t (* ~/.opam *)
      ; stdout : P.t }

  let init0 x =
    let home = Path.init Globals.opam_path in
      { server = File.Config.sources (File.Config.find home (Path.config home))
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
               File.Cudf.add home index_nv
                 (File.Cudf.cudf
                    (Version Globals.default_opam_version)
                    (match Server.package (RemoteServer.getOpam t.server (n, v)) with
                       | None -> assert false
                       | Some pkg -> pkg)),
           N_map.modify_def V_set.empty n (V_set.add v) map)
        (t.home, N_map.empty)
        (RemoteServer.getList t.server) in

      { t with home; stdout = 
          P.printf t.stdout "%s"
            (BatIO.to_string 
               (N_map.print 
                  (fun oc name -> BatString.print oc (Namespace.string_user_of_name name))
                  (V_set.print (fun oc version -> BatString.print oc (Namespace.string_user_of_version version)))
               ) map) }

  let init t o_url =
    update (match o_url with
              | None -> t
              | Some url -> { t with server = url })

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
        let install_set = NV_set.of_enum (BatList.enum (File.Installed.find t.home (Path.installed t.home))) in
        let map, max_n, max_v = 
          List.fold_left (fun (map, max_n, max_v) n_v -> 
                            let b = NV_set.mem n_v install_set in
                            NV_map.add n_v
                              (b, 
                               File.Cudf.description (File.Cudf.package (File.Cudf.find t.home (Path.index_opam t.home (Some n_v)))))
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
            (find_from_name (File.Installed.find t.home (Path.installed t.home))) in

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
                        File.Cudf.description (File.Cudf.package (File.Cudf.find t.home (Path.index_opam t.home (Some (name, v))))) ] }

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
        let tgz = Path.extract_targz t.home
          (RemoteServer.getArchive t.server (RemoteServer.getOpam t.server (name, v))) in
        { t with home = Path.add_rec t.home p_build tgz }, tgz in
      
    let t = f_build t tgz in
    let to_install = File.To_install.find t.home (Path.to_install t.home (name, v)) in

    let filename_of_path_relative t path = 
      Path.R_filename (File.To_install.filename_of_path_relative t.home
                         (Path.build t.home (Some (name, v))) 
                         path) in
      
    let add_rec f_lib t path = 
      f_add_rec t
        (f_lib t.home name (* warning : we assume that this result is a directory *))
        (filename_of_path_relative t path) in

    let t = (* lib *) 
      List.fold_left (add_rec Path.lib) t (File.To_install.lib to_install) in
      
    let t = (* bin *) 
      add_rec (fun t _ -> Path.bin t) t (File.To_install.bin to_install) in
      
    let t = (* misc *)
      List.fold_left 
        (fun t misc -> 
          let ok, t = confirm t (File.To_install.string_of_misc misc) in
          if ok then
            let path_from = filename_of_path_relative t (File.To_install.path_from misc) in
            List.fold_left 
              (fun t path_to -> { t with home = Path.add_rec t.home path_to path_from }) 
              t
              (File.To_install.filename_of_path_absolute t.home (File.To_install.path_to misc))
          else
            t) t (File.To_install.misc to_install) in
    t

  let proceed_todelete t (n, v0) = 
    let map_installed = N_map.of_enum (BatList.enum (File.Installed.find t.home (Path.installed t.home))) in
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

        let t = { t with home = File.Installed.add t.home (Path.installed t.home) (N_map.bindings (N_map.remove n map_installed)) } in
        
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
               let pkg = File.Cudf.package (File.Cudf.find t.home (Path.index_opam t.home (Some n_v))) in
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
      match BatList.Exceptionless.assoc name (File.Installed.find t.home (Path.installed t.home)) with
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
        { Solver.wish_install = []
        ; wish_remove = []
        ; wish_upgrade = BatList.map vpkg_of_nv (File.Installed.find t.home (Path.installed t.home)) }
    
  let upload t s_filename = 
    let filename = Path.package t.home s_filename in
    let t, o =
      let f msg v = 
        let ok, t = confirm t (Printf.sprintf "Path \"%s\" %s. It will be uploaded anyway." s_filename msg) in
        if ok then
          t, Some v
        else
          t, None in
      match Path.find t.home filename with
        | Path.File binary -> t, Some (Tar_gz binary)
        | Path.Directory _ -> f "is a directory" (Path.raw_targz filename)
        | Path.Not_exists -> f "has not been found" Empty in

    match o with
      | Some v ->
        { t with server = 
            RemoteServer.newArchive t.server
              (RemoteServer.getOpam t.server
                 (Path.nv_of_extension Namespace.default_version (Path.basename filename))) v }
      | None -> t

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


open Namespace
(*
let filename_of_string s = 
  List.fold_left 
    (fun t s -> Path.concat t (B s)) 
    Path.root
    (BatString.nsplit (BatString.strip ~chars:"/" s) "/")
*)
let _ = 
  let client = Client.init0 () in
  let f x = 
    let _ = Printf.printf "(* command not found *)\n%!" in
    x in
  match Array.to_list Sys.argv with
    | [] -> f client
    | _ :: l ->
      match l with
          
        | "init" :: host :: port :: [] ->
            let port =
              try int_of_string port
              with _ -> failwith (port ^ " is not a valid port") in
            Client.init client (Some (url host port))
        | "init" :: host :: [] -> Client.init client (Some (url host Globals.default_port))
        | "init" :: _ -> Client.init client None
          
        | "info" :: name :: _ -> Client.info client (Some (Name name))
        | "info" :: _ -> Client.info client None
          
        | "config" :: name :: []
        | "config" :: _ :: name :: _ -> Client.config client Client.Dir (Name name)
          
        | "install" :: name :: _ -> Client.install client (Name name)
          
        | "update" :: _ -> Client.update client
          
        | "upgrade" :: _ -> Client.upgrade client
          
        | "upload" :: s :: _ -> Client.upload client s
          
        | "remove" :: name :: _ -> Client.remove client (Name name)
          
        | _ -> f client
