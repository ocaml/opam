open Namespace.Namespace
open Path
open Server
open Solver
open Client

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
          
        | "upload" :: s :: _ -> Client.upload client s; client
          
        | "remove" :: name :: _ -> Client.remove client (Name name)
          
        | _ -> f client
