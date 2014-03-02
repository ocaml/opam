(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)



type env = { env : plist StringMap.t }
and plist = (string * env) list

type 'a source_option = {
  get : env list -> 'a;
  set : 'a -> unit;
}

exception Var_not_found of string

let empty_env = { env = StringMap.empty }
let global_env = ref StringMap.empty
let set_global name v =
  global_env := StringMap.add name v !global_env

let set env name v = { env = StringMap.add name v env.env }
let rec get_local envs name =
  match envs with
    [] ->
      (*    Printf.eprintf "get_local %S failed\n%!" name; *)
    raise (Var_not_found name)
  | env :: envs ->
    try
      StringMap.find name env.env
    with Not_found ->
      get_local envs name

let get envs name =
  try get_local envs name
  with Var_not_found _ ->
    try
      StringMap.find name !global_env
    with Not_found ->
(*      Printf.eprintf "get_global %S failed\n%!" name; *)
      raise (Var_not_found name)

let true_value = ["", set empty_env "type" ["%bool", empty_env]]
let false_value = []

let plist_of_bool b =
  if b then true_value else false_value
let bool_of_plist v =
  match v with
  |  [] -> false
  | _ -> true

let plist_of_strings strings =
   (List.map (fun s -> s, { env = StringMap.empty }) strings)
let strings_of_plist list =
  List.map (fun (s,_) -> s) list

let plist_of_string_option option =
  match option with
  | None -> []
  | Some s -> [ s, { env = StringMap.empty } ]

let string_option_of_plist list = match list with
  | [] -> None
  | list -> Some (String.concat " " (strings_of_plist list))

let plist_of_string s = [ s, { env = StringMap.empty } ]
let string_of_plist list = (String.concat " " (strings_of_plist list))

let plist_of_path s = [ s, { env = StringMap.empty } ]
let path_of_plist list = String.concat "/" (strings_of_plist list)

let set_bool env name v = set env name (plist_of_bool v)
let get_bool env name = bool_of_plist (get env name)
let get_local_bool env name = bool_of_plist (get_local env name)

let set_strings env name v = set env name (plist_of_strings v)
let get_strings env name = strings_of_plist (get env name)
let get_local_strings env name = strings_of_plist (get_local env name)

let set_string env name v = set env name (plist_of_string v)
let get_string env name = string_of_plist (get env name)
let get_local_string env name = string_of_plist (get_local env name)

let set_string_option env name v = set env name (plist_of_string_option v)
let get_string_option env name = string_option_of_plist (get env name)
let get_local_string_option env name = string_option_of_plist (get_local env name)

let set_path env name v = set env name (plist_of_path v)
let get_path env name = path_of_plist (get env name)
let get_local_path env name = path_of_plist (get_local env name)

let get_with_default_fun f =
  fun env name default -> try f env name with Var_not_found _ -> default

let get_with_default = get_with_default_fun get

let get_local_with_default = get_with_default_fun get_local

let get_bool_with_default = get_with_default_fun get_bool
let get_strings_with_default = get_with_default_fun get_strings
let get_string_with_default = get_with_default_fun get_string
let get_string_option_with_default = get_with_default_fun get_string_option
let get_path_with_default = get_with_default_fun get_path

let get_local_bool_with_default = get_with_default_fun get_local_bool
let get_local_strings_with_default = get_with_default_fun get_local_strings
let get_local_string_with_default = get_with_default_fun get_local_string
let get_local_string_option_with_default = get_with_default_fun get_local_string_option
let get_local_path_with_default = get_with_default_fun get_local_path

let is_already_installed options =
  get_bool_with_default options "generated" false
  || get_bool_with_default options "installed" false

let new_option name v =
  set_global name v;
  {
    get = (fun env -> get env name);
    set = (fun v -> set_global name v);
  }

let new_bool_option name v =
  set_global name (plist_of_bool v);
  {
    get = (fun env -> get_bool env name);
    set = (fun v -> set_global name (plist_of_bool v));
  }

let new_strings_option name v =
  set_global name (plist_of_strings v);
  {
    get = (fun env -> get_strings env name);
    set = (fun v -> set_global name (plist_of_strings v));
  }

let new_string_option name v =
  set_global name (plist_of_string v);
  {
    get = (fun env -> get_string env name);
    set = (fun v -> set_global name (plist_of_string v));
  }

let new_path_option name v =
  set_global name (plist_of_path v);
  {
    get = (fun env -> get_path env name);
    set = (fun v -> set_global name (plist_of_path v));
  }

let iter f env =
  StringMap.iter f env.env
