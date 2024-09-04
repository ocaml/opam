(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes
open OpamTypesBase
open OpamStd.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "ENV" fmt
let slog = OpamConsole.slog


(* Path format & separator handling *)
let default_separator = if Sys.win32 then SSemiColon else SColon
let default_format = Target

(* Predefined default separators and format for some environment variables *)
let default_sep_fmt_str var =
  match String.uppercase_ascii var with
  | "PATH" when Sys.win32 ->
    SSemiColon, Target_quoted
  | "MANPATH" ->
    SColon, Host
  | _ -> default_separator, default_format

let default_sep_fmt var = default_sep_fmt_str (OpamStd.Env.Name.to_string var)

(* sepfmt argument:
   - None: no rewrite
   - Some None: rewrite with defaults for given variable
   - Some (Some (separator, path_format): use given separator & path format
*)
type sep_path_format = [
  | `norewrite (* not a path, rewrite *)
  | `rewrite_default of string (* path, default of variable *)
  | `rewrite of separator * path_format (* path, rewrite using sep & fmt *)
]

type transform = {
  tr_entry: string; (* Entry (directory) in native, normalised form *)
  tr_raw: string; (* Actual string to put into the final variable *)
  tr_sep: char; (* Separator to use if (and only if) any entries follow *)
}

let transform_format ~(sepfmt:sep_path_format) var =
  match sepfmt with
  | `norewrite ->
    fun arg ->
      { tr_entry = arg;
        tr_raw = arg;
        tr_sep = OpamTypesBase.char_of_separator (fst (default_sep_fmt var));
      }
  | (`rewrite_default _ | `rewrite _) as sepfmt ->
    let separator, format =
      match sepfmt with
      | `rewrite_default var -> default_sep_fmt_str var
      | `rewrite (sep, fmt) -> sep, fmt
    in
    let translate =
      match format with
      | Target | Target_quoted ->
        (match sepfmt with
         | `rewrite_default _ -> fun x -> x
         | `rewrite _ ->  OpamSystem.forward_to_back)
      | Host | Host_quoted ->
        (* noop on non windows *)
        (Lazy.force OpamSystem.get_cygpath_path_transform) ~pathlist:false
    in
    let separator = OpamTypesBase.char_of_separator separator in
    match format with
    | Target | Host ->
      fun arg ->
        let path = translate arg in
        { tr_entry = path;
          tr_raw = path;
          tr_sep = separator;
        }
    | Target_quoted | Host_quoted ->
      fun arg ->
        let path = translate arg in
        let quoted_path =
          if String.contains path separator then
            "\""^path^"\"" else path
        in
        { tr_entry = path;
          tr_raw = quoted_path;
          tr_sep = separator;
        }

let resolve_separator_and_format :
  type r. (r, 'a) env_update -> (spf_resolved, 'a) env_update =
  let env fv =
    let fv = OpamVariable.Full.variable fv in
    OpamStd.Option.(Op.(
        of_Not_found
          (OpamStd.List.assoc OpamVariable.equal fv)
          OpamSysPoll.variables >>= Lazy.force))
  in
  let resolve var to_str formula =
    let evaluated =
      OpamFormula.map (fun (x, filter) ->
          let eval = OpamFilter.eval_to_bool ~default:false env filter in
          if eval then Atom (x, FBool true) else Empty)
        formula
      |> OpamFormula.map_formula (function
          | Block x -> x
          | x -> x)
    in
    match evaluated with
    | Empty  -> None
    | Atom (x, FBool true) -> Some x
    | _ ->
      let sep, pfmt = default_sep_fmt_str var in
      OpamConsole.error
        "Formula can't be completely resolved : %s %s. Using default '%c' '%s'."
        var
        (OpamFormula.string_of_formula (fun (s, f) ->
             "\""^to_str s ^ "\" " ^
             OpamFilter.to_string f) formula)
        (char_of_separator sep)
        (string_of_path_format pfmt);
      None
  in
  fun upd ->
    let var = upd.envu_var in
    let envu_rewrite =
      match upd.envu_rewrite with
      | Some (SPF_Unresolved (sep_f, pfmt_f)) ->
        let def_sep, def_pfmt = default_sep_fmt_str var in
        let sep =
          resolve upd.envu_var
            (fun sep -> String.make 1 (char_of_separator sep))
            sep_f
        in
        let pfmt =
          resolve upd.envu_var string_of_path_format pfmt_f
        in
        let sep_pfmt =
          match sep, pfmt with
          | Some sep, Some pfmt -> Some (sep, pfmt)
          | Some sep, None -> Some (sep, def_pfmt)
          | None, Some pfmt -> Some (def_sep, pfmt)
          | None, None -> None
        in
        Some (SPF_Resolved (sep_pfmt))
      | Some (SPF_Resolved _) -> upd.envu_rewrite
      | None -> None
    in
    { upd with envu_rewrite }

let split_path_variable path sep =
  let length = String.length path in
  let rec f acc index current current_raw last normal =
    if (index : int) = length then
      let final = String.sub path last (index - last) in
      let current = current ^ final in
      let current_raw = current_raw ^ final in
      let elem = {tr_entry = current; tr_raw = current_raw; tr_sep = sep } in
      List.rev (elem::acc)
    else
    let c = path.[index]
    and next = succ index in
    if c = sep && normal || c = '"' then
      let segment = String.sub path last (index - last) in
      let current = current ^ segment in
      let current_raw = current_raw ^ segment in
      let elem = {tr_entry = current; tr_raw = current_raw; tr_sep = sep } in
      if c = '"' then
        f acc next current (current_raw ^ "\"") next (not normal)
      else if (next : int) = length then (* path ends with a separator *)
        let empty = { tr_entry = ""; tr_raw = ""; tr_sep = sep } in
        List.rev (empty::elem::acc)
      else (* c = sep; text follows *)
        f (elem::acc) next "" "" next true
    else
      f acc next current current_raw last normal
  in
  f [] 0 "" "" 0 true

(* - Environment and updates handling - *)
let split_var ~(sepfmt:sep_path_format) var value =
  match sepfmt with
  | `norewrite ->
    let sep = char_of_separator (fst (default_sep_fmt var)) in
    List.map (fun s ->
        { tr_entry = s; tr_raw = s; tr_sep = sep})
      (OpamStd.String.split_delim value sep)
  | (`rewrite_default _ | `rewrite _) as sepfmt ->
    let separator, format =
      match sepfmt with
      | `rewrite_default var -> default_sep_fmt_str var
      | `rewrite (sep, fmt) -> sep, fmt
    in
    let sep = OpamTypesBase.char_of_separator separator in
    if (value : string) = String.make 1 sep then
      [{ tr_entry = ""; tr_raw = value; tr_sep = sep }]
    else
    match format with
    | Target | Host ->
      List.map (fun s ->
          { tr_entry = s; tr_raw = s; tr_sep = sep})
        (OpamStd.String.split_delim value sep)
    | Target_quoted | Host_quoted ->
      split_path_variable value sep

(* Auxiliaries for join_var - cf. String.concat *)
let rec sum_lengths acc = function
  | [{ tr_raw = raw; _}] -> acc + String.length raw
  | { tr_raw = raw; _}::tl -> sum_lengths (acc + String.length raw + 1) tl
  | [] -> acc (* semantically unreachable *)

let rec unsafe_blits dst pos = function
  | [] ->
    Bytes.unsafe_to_string dst
  | [{ tr_raw = raw; _}] ->
    String.unsafe_blit raw 0 dst pos (String.length raw);
    Bytes.unsafe_to_string dst
  | { tr_raw = raw; tr_sep = sep; _}::tl ->
    let length = String.length raw in
    String.unsafe_blit raw 0 dst pos length;
    Bytes.unsafe_set dst (pos + length) sep;
    unsafe_blits dst (pos + length + 1) tl

let join_var values =
  if values = [] then "" else
    unsafe_blits (Bytes.create (sum_lengths 0 values)) 0 values

let separator_char_for ~sepfmt var =
  let (separator, _) =
    match sepfmt with
    | `norewrite -> default_sep_fmt var
    | `rewrite_default var -> default_sep_fmt_str var
    | `rewrite spf -> spf
  in
  OpamTypesBase.char_of_separator separator

(* To allow in-place updates, we store intermediate values of path-like as a
   pair of list [(rl1, l2)] such that the value is [List.rev_append rl1 l2] and
   the place where the new value should be inserted is in front of [l2] *)


let unzip_to ~sepfmt var elt current =
  (* If [r = l @ rs] then [remove_prefix l r] is [Some rs], otherwise [None] *)
  let rec remove_prefix l r =
    match l, r with
    | {tr_entry = l; _}::ls, { tr_entry = r; _}::rs when l = r ->
      remove_prefix ls rs
    | ([], rs) -> Some rs
    | _ -> None
  in
  (* Split elt if necessary *)
  let elts =
    if String.equal elt "" then
      [{ tr_entry = ""; tr_raw = "";
         tr_sep = separator_char_for ~sepfmt var }]
    else
    match sepfmt with
    | `norewrite ->
      (* Given FOO += "<value1><sep><value2>", then even with
         `norewrite it is necessary to split the value as FOO itself
         will have been split - i.e. if we don't split elt here then
         it cannot be reverted if it contains multiple directories
         (which would regression #4861) *)
      let sepfmt = `rewrite_default (var :> string) in
      split_var ~sepfmt var elt
    | `rewrite_default _ ->
      (* If no rewrite has been specified at all, then split elt as,
         again, #4861 would be regressed otherwise. *)
      split_var ~sepfmt var elt
    | `rewrite _ ->
      (* If a rewrite rule _is_ in effect, then opam 2.2's limited
         (but compatible) semantics for setenv and build-env mean that
         we're _assuming_ that elt only contains a single path. This
         should be addressed in opam 3.0 by having a somewhat richer
         syntax for environment changes to make clear the "type" of
         the value in FOO += "bar". *)
      [{ tr_entry = elt; tr_raw = elt;
         tr_sep = separator_char_for ~sepfmt var }]
  in
  match elts with
  | [] -> invalid_arg "OpamEnv.unzip_to"
  | { tr_entry = hd; _}::tl ->
    let rec aux acc = function
      | [] -> None
      | ({ tr_entry = x; _} as v)::r ->
        if String.equal x hd then
          match remove_prefix tl r with
          | Some r -> Some (acc, r)
          | None -> aux (v::acc) r
        else aux (v::acc) r
    in
    aux [] current

let rezip ?insert (l1, l2) =
  List.rev_append l1 (match insert with None -> l2 | Some i -> i::l2)

let rezip_to_string ?insert z =
  join_var (rezip ?insert z)

let cygwin_non_shadowed_programs =
  [ "bash.exe"; "make.exe"; "sort.exe"; "tar.exe";
    "install.exe"; (* from Vim for Windows *)
  ]

let apply_op_zip ~sepfmt var op arg (rl1,l2 as zip) =
  let arg = transform_format ~sepfmt var arg in
  let empty_tr = { tr_entry = ""; tr_raw = ""; tr_sep = arg.tr_sep } in
  let cygwin path =
    let contains_in {tr_entry = dir; _} item =
      Sys.file_exists (Filename.concat dir item)
    in
    let shadow_list =
      List.filter (contains_in arg) ("git.exe" :: cygwin_non_shadowed_programs)
    in
    let rec loop acc = function
      | [] -> acc, [arg]
      | (d::rest) as suffix ->
        if List.exists (contains_in d) shadow_list then
          acc, arg::suffix
        else
          loop (d::acc) rest
    in
    loop [] path
  in
  match op with
  | Eq ->
      (* Existing zip discarded - new value to l2; no prefix *)
      [], [arg]
  | PlusEq ->
    (* New value goes at head of existing list; no prefix *)
    begin match rezip zip with
      | [{ tr_entry = ""; tr_raw = raw; _}] ->
        if raw = "" then
          [], [arg]
        else
          [], [arg; empty_tr]
      | zip -> [], arg::zip
    end
  | EqPlus ->
    (* NB List.rev_append l2 rl1 is equivalent to
          List.rev (List.rev_append rl1 l2)
       Place new value at the end *)
    begin match List.rev_append l2 rl1 with
      | [{ tr_entry = ""; tr_raw = raw; _}] ->
        if raw = "" then
          [], [arg]
        else
          [], [empty_tr; arg]
      | zip -> zip, [arg]
    end
  | Cygwin ->
    cygwin (rezip zip)
  | EqPlusEq ->
    (* Add the value where the last value was reverted (i.e. as PlusEq but
       without the rezip) *)
    rl1, arg::l2
  | ColonEq ->
    begin match rezip zip with
      | [{ tr_entry = ""; _}] | [] -> (* empty or unset *)
        [], [arg; empty_tr]
      | ({ tr_entry = ""; _} as lead)::{ tr_entry = ""; _}::([] as zip) ->
        (* VAR=':' *)
        [], lead::arg::zip
      | zip ->
        [], arg::zip
    end
  | EqColon ->
    begin match List.rev_append l2 rl1 with
      | [{ tr_entry = ""; _}] | [] -> (* empty or unset *)
        [], [empty_tr; arg]
      | ({ tr_entry = ""; _} as lead)::{ tr_entry = ""; _}::([] as zip) ->
        (* VAR=':' *)
        [], List.rev (lead::arg::zip)
      | zip ->
        [], List.rev (arg::zip)
    end

(** Undoes previous updates done by opam, useful for not duplicating already
    done updates; this is obviously not perfect, as all operators are not
    reversible.

    [cur_value] is provided as a list split at path_sep.

    None is returned if the revert doesn't match. Otherwise, a zip (pair of lists
    [(preceding_elements_reverted, following_elements)]) is returned, to keep the
    position of the matching element and allow [=+=] to be applied later. A pair
    or empty lists is returned if the variable should be unset or has an unknown
    previous value. *)
let reverse_env_update ~sepfmt var op arg cur_value =
  let { tr_entry = arg; _} = transform_format ~sepfmt var arg in
  if String.equal arg "" && op <> Eq then None else
  match op with
  | Eq ->
    if arg = join_var cur_value
    then Some ([],[]) else None
  | PlusEq | EqPlusEq -> unzip_to var ~sepfmt arg cur_value
  | EqPlus | Cygwin ->
    (match unzip_to ~sepfmt var arg (List.rev cur_value) with
     | None -> None
     | Some (rl1, l2) -> Some (l2, List.rev rl1))
  | ColonEq ->
    (match unzip_to var ~sepfmt arg cur_value with
     | Some ([], [{ tr_entry = ""; _}]) -> Some ([], [])
     | r -> r)
  | EqColon ->
    (match unzip_to ~sepfmt var arg (List.rev cur_value) with
     | Some ([], [{ tr_entry = ""; _}]) -> Some ([], [])
     | Some (rl1, l2) -> Some (l2, List.rev rl1)
     | None -> None)

let map_update_names env_keys updates =
  let convert upd =
    let { envu_var = k; _ } = upd in
    let k =
      try
        let k = OpamStd.Env.Name.of_string k in
        (OpamStd.Env.Name.(Set.find (equal k) env_keys) :> string)
      with Not_found -> k
    in
    { upd with envu_var = k }
  in
  List.map convert updates

let global_env_keys = lazy (
  OpamStd.Env.list ()
  |> List.map fst
  |> OpamStd.Env.Name.Set.of_list)

let updates_from_previous_instance = lazy (
  let get_env env_file =
    OpamStd.Option.map
      (map_update_names (Lazy.force global_env_keys))
      (OpamFile.Environment.read_opt env_file)
  in
  let open OpamStd.Option.Op in
  (OpamStd.Env.getopt "OPAM_LAST_ENV"
   >>= fun env_file ->
   try
     OpamFilename.of_string env_file
     |> OpamFile.make
     |> get_env
   with e -> OpamStd.Exn.fatal e; None)
  >>+ (fun () ->
      OpamStd.Env.getopt "OPAM_SWITCH_PREFIX"
      >>= fun pfx ->
      let env_file =
        OpamPath.Switch.env_relative_to_prefix (OpamFilename.Dir.of_string pfx)
      in
      try get_env env_file
      with e -> OpamStd.Exn.fatal e; None))

let expand updates =
  let updates =
    if Sys.win32 then
      (* Preserve the case of updates which are already in env *)
      map_update_names (Lazy.force global_env_keys) updates
    else
      updates
  in
  let pick_assoc3 eq x l =
    let rec aux acc = function
      | [] -> None, l
      | (k,v,_) as b::r ->
        if eq k x then Some v, List.rev_append acc r
        else aux (b::acc) r
    in
    aux [] l
  in
  (* Reverse all previous updates, in reverse order, on current environment *)
  let reverts =
    match Lazy.force updates_from_previous_instance with
    | None -> []
    | Some updates ->
      List.fold_right (fun upd defs0 ->
          let { envu_var = var; envu_op = op; envu_value = arg;
                envu_rewrite; _} = upd
          in
          let sepfmt =
            match envu_rewrite with
            | None -> `norewrite
            | Some (SPF_Resolved None) -> `rewrite_default var
            | Some (SPF_Resolved (Some spf)) -> `rewrite spf
          in
          let var = OpamStd.Env.Name.of_string var in
          let v_opt, defs =
            pick_assoc3 OpamStd.Env.Name.equal var defs0
          in
          let v =
            match Option.map rezip v_opt with
            | Some v -> v
            | None ->
              OpamStd.Option.map_default (split_var ~sepfmt var) []
                (OpamStd.Env.getopt (var :> string))
          in
          match reverse_env_update ~sepfmt var op arg v with
          | Some v -> (var, v, sepfmt)::defs
          | None -> defs0)
        updates []
  in
  (* OPAM_LAST_ENV and OPAM_SWITCH_PREFIX must be reverted if they were set *)
  let reverts =
    if OpamStd.Env.getopt "OPAM_LAST_ENV" <> None then
      (OpamStd.Env.Name.of_string "OPAM_LAST_ENV", ([], []),
       `rewrite_default "OPAM_LAST_ENV")
      ::reverts
    else
      reverts
  in
  let reverts =
    if OpamStd.Env.getopt "OPAM_SWITCH_PREFIX" <> None then
      (OpamStd.Env.Name.of_string "OPAM_SWITCH_PREFIX", ([], []),
       `rewrite_default "OPAM_SWITCH_PREFIX")
      ::reverts
    else
      reverts
  in
  (* And apply the new ones *)
  let rec apply_updates reverts acc lst =
    match lst with
    | upd :: updates ->
      let { envu_var = svar; envu_op = op;
            envu_value = arg; envu_comment = doc;
            envu_rewrite } = upd
      in
      let sepfmt =
        match envu_rewrite with
        | None -> `norewrite
        | Some (SPF_Resolved None) -> `rewrite_default svar
        | Some (SPF_Resolved (Some spf)) -> `rewrite spf
      in
      let var = OpamStd.Env.Name.of_string svar in
      let zip, reverts =
        match OpamStd.List.find_opt (fun (v, _, _, _) ->
            OpamStd.Env.Name.equal var v) acc with
        | Some (_, z, _doc, _) -> z, reverts
        | None ->
          match pick_assoc3 OpamStd.Env.Name.equal var reverts with
          | Some z, reverts -> z, reverts
          | None, _ ->
            match OpamStd.Env.getopt svar with
            | Some s -> ([], split_var var s ~sepfmt), reverts
            | None -> ([], []), reverts
      in
      let acc =
        if String.equal arg "" && op <> Eq then acc else
          ((var, apply_op_zip ~sepfmt var op arg zip, doc, sepfmt)
           :: acc)
      in
      apply_updates
        reverts
        acc
        updates
    | [] ->
      List.rev
      @@ List.rev_append
        (List.rev_map (fun (var, z, doc, _sepfmt) ->
             var, rezip_to_string z, doc) acc)
      @@ List.rev_map (fun (var, z, _sepfmt) ->
          var, rezip_to_string z,
          Some "Reverting previous opam update")
        reverts
  in
  apply_updates reverts [] updates

let add (env: env) updates : env =
  let updates =
    if Sys.win32 then
      (* Preserve the case of updates which are already in env *)
      map_update_names (OpamStd.Env.Name.Set.of_list
                          (List.map (fun (k, _, _) -> k) env)) updates
    else
      updates
  in
  let updates = expand updates in
  let update_keys =
    List.fold_left (fun m (var, _, _) ->
        OpamStd.Env.Name.(Set.add var m))
      OpamStd.Env.Name.Set.empty updates
  in
  let env =
    List.filter (fun (k,_,_) ->
        not (OpamStd.Env.Name.Set.mem k update_keys))
      env
  in
  env @ updates

let env_expansion ?opam st upd =
  let fenv v =
    try OpamPackageVar.resolve st ?opam v
    with Not_found ->
      log "Undefined variable: %s" (OpamVariable.Full.to_string v);
      None
  in
  let s =
    OpamFilter.expand_string ~default:(fun _ -> "") fenv upd.envu_value
  in
  { upd with envu_value = s }

(* [env_update_resolved_with_default] creates an environment update with a fully
   evaluated rewrite rule. It's used internally because the updates in question
   are single directories only, which means that the update will then never be
   subject to splitting in [unzip_to] *)
let env_update_resolved_with_default ?comment var =
  let rewrite = Some (SPF_Resolved (Some (default_sep_fmt_str var))) in
  env_update_resolved ?comment ~rewrite var

let compute_updates ?(force_path=false) st =
  (* Todo: put these back into their packages!
  let perl5 = OpamPackage.Name.of_string "perl5" in
  let add_to_perl5lib =  OpamPath.Switch.lib t.root t.switch t.switch_config perl5 in
  let new_perl5lib = "PERL5LIB", "+=", OpamFilename.Dir.to_string add_to_perl5lib in
*)
  let bindir =
    OpamPath.Switch.bin st.switch_global.root st.switch st.switch_config
  in
  let path =
    env_update_resolved_with_default "PATH"
      (if force_path then PlusEq else EqPlusEq)
      (OpamFilename.Dir.to_string bindir)
      ~comment:("Binary dir for opam switch "^OpamSwitch.to_string st.switch)
  in
  let man_path =
    let open OpamStd.Sys in
    match os () with
    | OpenBSD | NetBSD | FreeBSD | Darwin | DragonFly ->
      [] (* MANPATH is a global override on those, so disabled for now *)
    | _ ->
      [ env_update_resolved_with_default "MANPATH" EqColon
          (OpamFilename.Dir.to_string
             (OpamPath.Switch.man_dir st.switch_global.root
                st.switch st.switch_config))
          ~comment:"Current opam switch man dir"
      ]
 in
 let switch_env =
   (env_update_resolved_with_default "OPAM_SWITCH_PREFIX" Eq
      (OpamFilename.Dir.to_string
         (OpamPath.Switch.root st.switch_global.root st.switch))
      ~comment:"Prefix of the current opam switch")
   ::
   List.map (env_expansion st) (OpamFile.Switch_config.env st.switch_config)
  in
  let pkg_env = (* XXX: Does this need a (costly) topological sort? *)
    let updates =
      OpamPackage.Set.fold (fun nv acc ->
          match OpamPackage.Map.find_opt nv st.opams with
          | Some opam ->
            List.map (env_expansion ~opam st) (OpamFile.OPAM.env opam) @ acc
          | None -> acc)
        st.installed []
    in
    List.map resolve_separator_and_format updates
  in
  switch_env @ pkg_env @ man_path @ [path]

let updates_common ~set_opamroot ~set_opamswitch root switch =
  let root =
    if set_opamroot then
      [ env_update_resolved_with_default "OPAMROOT" Eq
          (OpamFilename.Dir.to_string root)
          ~comment:"Opam root in use" ]
    else []
  in
  let switch =
    if set_opamswitch then
      [ env_update_resolved_with_default "OPAMSWITCH" Eq
          (OpamSwitch.to_string switch) ]
    else [] in
  root @ switch

let updates ~set_opamroot ~set_opamswitch ?force_path st =
  let common =
    updates_common ~set_opamroot ~set_opamswitch st.switch_global.root st.switch
  in
  common @ compute_updates ?force_path st

let get_pure ?(updates=[]) () =
  let env = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add env updates

let get_opam ~set_opamroot ~set_opamswitch ~force_path st =
  add [] (updates ~set_opamroot ~set_opamswitch ~force_path st)

let get_opam_raw_updates ~set_opamroot ~set_opamswitch ~force_path root switch =
  let env_file = OpamPath.Switch.environment root switch in
  let upd = OpamFile.Environment.safe_read env_file in
  let upd =
    let from_op, to_op =
      if force_path then
        EqPlusEq, PlusEq
      else
        PlusEq, EqPlusEq
    in
    List.map (function
        | { envu_var; envu_op; _} as upd when
            String.uppercase_ascii envu_var = "PATH" && envu_op = from_op ->
          { upd with envu_op = to_op }
        | e -> e) upd
  in
  updates_common ~set_opamroot ~set_opamswitch root switch @ upd

let get_opam_raw ~set_opamroot ~set_opamswitch ?(base=[]) ~force_path
  root switch =
  let upd =
    get_opam_raw_updates ~set_opamroot ~set_opamswitch ~force_path root switch
  in
  add base upd

let hash_env_updates upd =
  (* Should we use OpamFile.Environment.write_to_string ? cons: it contains
     tabulations *)
  let to_string { envu_var; envu_op; envu_value; _} =
    String.escaped envu_var
    ^ OpamPrinter.FullPos.env_update_op_kind (raw_of_op envu_op)
    ^ String.escaped envu_value
  in
  List.rev_map to_string upd
  |> String.concat "\n"
  |> Digest.string
  |> Digest.to_hex

let get_full
    ~set_opamroot ~set_opamswitch ~force_path ?updates:(u=[]) ?(scrub=[])
    st =
  let env =
    let env = OpamStd.Env.list () in
    let scrub =
      let add set elt =
        OpamStd.Env.Name.(Set.add (of_string elt) set)
      in
      List.fold_left add OpamStd.Env.Name.Set.empty scrub
    in
    List.filter (fun (name, _) -> not (OpamStd.Env.Name.Set.mem name scrub)) env
  in
  let env0 = List.map (fun (v,va) -> v,va,None) env in
  let u =
    (List.map resolve_separator_and_format u) in
  let updates =
    u @ updates ~set_opamroot ~set_opamswitch ~force_path st in
  add env0 updates

let is_up_to_date_raw ?(skip=OpamStateConfig.(!r.no_env_notice)) updates =
  skip ||
  let not_utd =
    List.fold_left (fun notutd upd ->
        let { envu_var = var; envu_op = op; envu_value = arg;
              envu_rewrite; _} = upd in
        let sepfmt =
          match envu_rewrite with
          | None -> `norewrite
          | Some (SPF_Resolved None) -> `rewrite_default var
          | Some (SPF_Resolved (Some spf)) -> `rewrite spf
        in
        let var = OpamStd.Env.Name.of_string var in
        match OpamStd.Env.getopt_full var with
        | _, None -> upd::notutd
        | var, Some v ->
          if reverse_env_update ~sepfmt var op arg
              (split_var ~sepfmt var v) = None then upd::notutd
          else List.filter (fun upd ->
              not (OpamStd.Env.Name.equal_string var upd.envu_var)) notutd)
      []
      updates
  in
  let r = not_utd = [] in
  if not r then
    log "Not up-to-date env variables: [%a]"
      (slog @@ String.concat " " @* List.map (fun upd -> upd.envu_var)) not_utd
  else log "Environment is up-to-date";
  r

let is_up_to_date_switch root switch =
  let env_file = OpamPath.Switch.environment root switch in
  try
    match OpamFile.Environment.read_opt env_file with
    | Some upd -> is_up_to_date_raw upd
    | None -> true
  with e -> OpamStd.Exn.fatal e; true

let switch_path_update ~force_path root switch =
  let bindir =
    OpamPath.Switch.bin root switch
      (OpamStateConfig.Switch.safe_load_t
         ~lock_kind:`Lock_read root switch)
  in
  [ env_update_resolved_with_default "PATH"
      (if force_path then PlusEq else EqPlusEq)
      (OpamFilename.Dir.to_string bindir)
      ~comment:"Current opam switch binary dir" ]

let path ~force_path root switch =
  let env = expand (switch_path_update ~force_path root switch) in
  let (_, path_value, _) =
    List.find (fun (v, _, _) -> OpamStd.Env.Name.equal_string v "PATH") env
  in
  path_value

let full_with_path ~force_path ?(updates=[]) root switch =
  let env0 = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add env0 (switch_path_update ~force_path root switch @ updates)

let is_up_to_date ?skip st =
  is_up_to_date_raw ?skip
    (updates ~set_opamroot:false ~set_opamswitch:false ~force_path:false st)

(** Returns shell-appropriate statement to evaluate [cmd]. *)
let shell_eval_invocation shell cmd =
  match shell with
  | SH_pwsh _ ->
    Printf.sprintf "(& %s) -split '\\r?\\n' | ForEach-Object { Invoke-Expression $_ }" cmd
  | SH_fish ->
    Printf.sprintf "eval (%s)" cmd
  | SH_csh ->
    Printf.sprintf "eval `%s`" cmd
  | SH_cmd ->
    Printf.sprintf {|for /f "tokens=*" %%i in ('%s') do @%%i|} cmd
  | _ ->
    Printf.sprintf "eval $(%s)" cmd

(** Returns if the file path needs to be quoted by any supported {!shell}.

    This function does not concern itself with how the file path should be
    quoted.

    This function treats variable expansions ($) and array expansions for
    PowerShell (@) and history expansions (!) as needing quotes.

    All other characters come from the following references:

    Bash (metacharacter)
      https://www.gnu.org/software/bash/manual/html_node/Definitions.html
      SPACE TAB | & ; ( ) < >

    PowerShell
      https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_special_characters?view=powershell-5.1
      https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-5.1
      SPACE `

    Command Prompt
      https://ss64.com/nt/syntax-esc.html
      SPACE TAB & \ < > ^ | % = ( )
*)
let filepath_needs_quote path =
  let f = function
    | '$' | '@' | '!'
    | ' ' | '\t' | '|' | '&' | ';' | '(' | ')' | '<' | '>'
    | '`'
    | '\\' | '^' | '%' -> true
    | _ -> false
  in
  OpamCompat.String.exists f path

(** Returns "opam env" invocation string together with optional root and switch
    overrides *)
let opam_env_invocation ?root ?switch ?(set_opamswitch=false) shell =
  let shell_arg argname pathval =
    let quoted = match shell with
    | SH_cmd | SH_pwsh _ ->
      Printf.sprintf " \"--%s=%s\"" argname
    | SH_sh | SH_bash | SH_zsh | SH_csh | SH_fish ->
      Printf.sprintf " '--%s=%s'" argname
    in
    if filepath_needs_quote pathval then
      quoted pathval
    else
      Printf.sprintf " --%s=%s" argname pathval
  in
  let root = OpamStd.Option.map_default (shell_arg "root") "" root in
  let switch = OpamStd.Option.map_default (shell_arg "switch") "" switch in
  let setswitch = if set_opamswitch then " --set-switch" else "" in
  Printf.sprintf "opam env%s%s%s" root switch setswitch

let eval_string gt ?(set_opamswitch=false) switch =
  let root =
    let opamroot_cur = OpamFilename.Dir.to_string gt.root in
    let opamroot_env =
      OpamStd.Option.Op.(
        OpamStateConfig.E.root () +!
        OpamFilename.Dir.to_string OpamStateConfig.(default.root_dir)
      ) in
    if opamroot_cur <> opamroot_env then
      Some opamroot_cur
    else
      None
  in
  let switch =
    (* Returns the switch only if it is different from the one determined by the
      environment *)
    let f sw =
      let sw_cur = OpamSwitch.to_string sw in
      let sw_env =
        OpamStd.Option.Op.(
          OpamStateConfig.E.switch () ++
          (OpamStateConfig.get_current_switch_from_cwd gt.root >>|
            OpamSwitch.to_string) ++
          (OpamFile.Config.switch gt.config >>| OpamSwitch.to_string)
        )
      in
      if Some sw_cur <> sw_env then Some sw_cur else None
    in
    OpamStd.Option.replace f switch
  in
  let shell = OpamStd.Sys.guess_shell_compat () in
  shell_eval_invocation shell (opam_env_invocation ?root ?switch ~set_opamswitch shell)


(* -- Shell and init scripts handling -- *)

(** The shells for which we generate init scripts (bash and sh are the same
    entry) *)
let shells_list = [ SH_sh; SH_zsh; SH_csh; SH_fish; SH_pwsh Powershell; SH_cmd ]

let complete_file = function
  | SH_sh | SH_bash -> Some "complete.sh"
  | SH_zsh -> Some "complete.zsh"
  | SH_csh | SH_fish | SH_pwsh _ | SH_cmd -> None

let env_hook_file = function
  | SH_sh | SH_bash -> Some "env_hook.sh"
  | SH_zsh -> Some "env_hook.zsh"
  | SH_csh -> Some "env_hook.csh"
  | SH_fish -> Some "env_hook.fish"
  | SH_pwsh _ | SH_cmd -> None

let variables_file = function
  | SH_sh | SH_bash | SH_zsh -> "variables.sh"
  | SH_csh -> "variables.csh"
  | SH_fish -> "variables.fish"
  | SH_pwsh _ -> "variables.ps1"
  | SH_cmd -> "variables.cmd"

let init_file = function
  | SH_sh | SH_bash -> "init.sh"
  | SH_zsh -> "init.zsh"
  | SH_csh -> "init.csh"
  | SH_fish -> "init.fish"
  | SH_pwsh _ -> "init.ps1"
  | SH_cmd -> "init.cmd"

let complete_script = function
  | SH_sh | SH_bash -> Some OpamScript.complete
  | SH_zsh -> Some OpamScript.complete_zsh
  | SH_csh | SH_fish -> None
  | SH_pwsh _ | SH_cmd -> None

let env_hook_script_base = function
  | SH_sh | SH_bash -> Some OpamScript.env_hook
  | SH_zsh -> Some OpamScript.env_hook_zsh
  | SH_csh -> Some OpamScript.env_hook_csh
  | SH_fish -> Some OpamScript.env_hook_fish
  | SH_pwsh _ | SH_cmd -> None

let export_in_shell shell =
  let make_comment comment_opt =
    OpamStd.Option.to_string (Printf.sprintf "# %s\n") comment_opt
  in
  let sh   (k,v,comment) =
    Printf.sprintf "%s%s=%s; export %s;\n"
      (make_comment comment) k v k in
  let csh  (k,v,comment) =
    Printf.sprintf "%sif ( ! ${?%s} ) setenv %s \"\"\nsetenv %s %s\n"
      (make_comment comment) k k k v in
  let fish (k,v,comment) =
    (* Fish converts some colon-separated vars to arrays, which have to be
       treated differently. MANPATH is handled automatically, so better not to
       set it at all when not already defined *)
    let to_arr_string v =
      OpamStd.List.concat_map " "
        (fun v ->
           if v = Printf.sprintf "\"$%s\"" k then
             "$"^k (* remove quotes *)
           else v)
        (OpamStd.String.split v ':')
    in
    match k with
    | "PATH" ->
      Printf.sprintf "%sset -gx %s %s;\n"
        (make_comment comment) k (to_arr_string v)
    | "MANPATH" ->
      Printf.sprintf "%sif [ (count $%s) -gt 0 ]; set -gx %s %s; end;\n"
        (make_comment comment) k k (to_arr_string v)
    | _ ->
      (* Regular string variables *)
      Printf.sprintf "%sset -gx %s %s;\n"
        (make_comment comment) k v
  in
  let pwsh (k,v,comment) =
    Printf.sprintf "%s$env:%s=%s\n"
      (make_comment comment) k v in
  let cmd (k,v,comment) =
    let make_cmd_comment comment_opt =
      OpamStd.Option.to_string (Printf.sprintf ":: %s\n") comment_opt
    in
    Printf.sprintf "%sset \"%s=%s\"\n"
      (make_cmd_comment comment) k v in
  match shell with
  | SH_zsh | SH_bash | SH_sh -> sh
  | SH_fish -> fish
  | SH_csh -> csh
  | SH_pwsh _ -> pwsh
  | SH_cmd -> cmd

let env_hook_script shell =
  OpamStd.Option.map (fun script ->
      export_in_shell shell ("OPAMNOENVNOTICE", "true", None)
      ^ script)
    (env_hook_script_base shell)

let source root shell f =
  let fname = OpamFilename.to_string (OpamPath.init root // f) in
  let unix_transform ?using_backslashes () =
    let cygpath = Lazy.force OpamSystem.get_cygpath_path_transform in
    cygpath ~pathlist:false fname
    |> OpamStd.Env.escape_single_quotes ?using_backslashes
  in
  match shell with
  | SH_csh ->
    let fname = unix_transform () in
    Printf.sprintf "if ( -f '%s' ) source '%s' >& /dev/null\n"
      fname fname
  | SH_fish ->
    let fname = unix_transform ~using_backslashes:true () in
    Printf.sprintf "test -r '%s' && source '%s' > /dev/null 2> /dev/null; or true\n" fname fname
  | SH_sh | SH_bash ->
    let fname = unix_transform () in
    Printf.sprintf "test -r '%s' && . '%s' > /dev/null 2> /dev/null || true\n"
      fname fname
  | SH_zsh ->
    let fname = unix_transform () in
    Printf.sprintf "[[ ! -r '%s' ]] || source '%s' > /dev/null 2> /dev/null\n"
      fname fname
  | SH_cmd ->
    Printf.sprintf "if exist \"%s\" call \"%s\" >NUL 2>NUL\n" fname fname
  | SH_pwsh _ ->
    Printf.sprintf "if Test-Path \"%s\" { . \"%s\" *> $null }\n" fname fname

let if_interactive_script shell t e =
  let ielse else_opt = match else_opt with
    |  None -> ""
    | Some e -> Printf.sprintf "else\n  %s" e
  in
  let ielse_cmd else_opt = match else_opt with
    |  None -> ""
    | Some e -> Printf.sprintf ") else (\n  %s" e
  in
  let ielse_pwsh else_opt = match else_opt with
    |  None -> ""
    | Some e -> Printf.sprintf "} else {\n  %s" e
  in
  match shell with
  | SH_sh| SH_bash ->
    Printf.sprintf "if [ -t 0 ]; then\n  %s%sfi\n" t @@ ielse e
  | SH_zsh ->
    Printf.sprintf "if [[ -o interactive ]]; then\n  %s%sfi\n" t @@ ielse e
  | SH_csh ->
    Printf.sprintf "if ( $?prompt ) then\n  %s%sendif\n" t @@ ielse e
  | SH_fish ->
    Printf.sprintf "if status is-interactive\n  %s%send\n" t @@ ielse e
  | SH_cmd ->
    Printf.sprintf "echo %%cmdcmdline%% | find /i \"%%~0\" >nul\nif errorlevel 1 (\n%s%s)\n" t @@ ielse_cmd e
  | SH_pwsh _ ->
    Printf.sprintf "if ([Environment]::UserInteractive) {\n  %s%s}\n" t @@ ielse_pwsh e

let init_script root shell =
  let interactive =
    List.map (source root shell) @@
    OpamStd.List.filter_some [complete_file shell; env_hook_file shell]
  in
  String.concat "\n" @@
  (if interactive <> [] then
     [if_interactive_script shell (String.concat "\n  " interactive) None]
   else []) @
  [source root shell (variables_file shell)]

let string_of_update st shell updates =
  let fenv = OpamPackageVar.resolve st in
  let aux { envu_var; envu_op; envu_value; envu_comment; envu_rewrite } =
    let string =
      OpamFilter.expand_string ~default:(fun _ -> "") fenv envu_value |>
      OpamStd.Env.escape_single_quotes ~using_backslashes:(shell = SH_fish)
    in
    let sepfmt =
      match envu_rewrite with
      | None -> `norewrite
      | Some (SPF_Resolved None) -> `rewrite_default envu_var
      | Some (SPF_Resolved (Some spf)) -> `rewrite spf
    in
    let { tr_raw = string; tr_sep = sep; _} =
      transform_format ~sepfmt (OpamStd.Env.Name.of_string envu_var) string
    in
    let key, value =
      envu_var, match (envu_op : euok_writeable env_update_op_kind) with
      | Eq ->
        (match shell with
         | SH_pwsh _ ->
           Printf.sprintf "'%s'" (OpamStd.Env.escape_powershell string)
         | SH_cmd -> string
         | _ -> Printf.sprintf "'%s'" string)
      | PlusEq | ColonEq | EqPlusEq ->
        (match shell with
         | SH_pwsh _ ->
           Printf.sprintf "'%s%c' + \"$env:%s\""
             (OpamStd.Env.escape_powershell string) sep envu_var
         | SH_cmd -> Printf.sprintf "%s%c%%%s%%" string sep envu_var
         | _ -> Printf.sprintf "'%s':\"$%s\"" string envu_var)
      | EqColon | EqPlus ->
        (match shell with
         | SH_pwsh _ -> Printf.sprintf "\"$env:%s\" + '%c%s'" envu_var sep string
         | SH_cmd -> Printf.sprintf "%%%s%%%c%s" envu_var sep string
         | _ -> Printf.sprintf "\"$%s\":'%s'" envu_var string)
    in
    export_in_shell shell (key, value, envu_comment) in
  OpamStd.List.concat_map "" aux updates

let write_script dir (name, body) =
  let file = dir // name in
  try OpamFilename.write file body
  with e ->
    OpamStd.Exn.fatal e;
    OpamConsole.error "Could not write %s" (OpamFilename.to_string file)

let write_init_shell_scripts root =
  let scripts =
    List.map (fun shell -> init_file shell, init_script root shell) shells_list
  in
  List.iter (write_script (OpamPath.init root)) scripts

let write_static_init_scripts root ?completion ?env_hook ?(inplace=false) () =
  write_init_shell_scripts root;
  let update_scripts filef scriptf enable =
    let scripts =
      OpamStd.List.filter_map (fun shell ->
          match filef shell, scriptf shell with
          | Some f, Some s -> Some (f, s)
          | _ -> None)
        shells_list
    in
    match enable, inplace with
    | Some true, _ ->
      List.iter (write_script (OpamPath.init root)) scripts
    | _, true ->
      List.iter (fun ((f,_) as fs) ->
          if OpamFilename.exists (OpamPath.init root // f) then
            write_script (OpamPath.init root) fs)
        scripts
    | Some false, _ ->
      List.iter (fun (f,_) ->
          OpamFilename.remove (OpamPath.init root // f)) scripts
    | None, _ -> ()
  in
  update_scripts complete_file complete_script completion;
  update_scripts env_hook_file env_hook_script env_hook

let write_custom_init_scripts root custom =
  let hookdir = OpamPath.hooks_dir root in
  let kind = `MD5 in
  List.iter (fun (name, script) ->
      let script_file = hookdir // name in
      let hash = OpamHash.compute_from_string ~kind script in
      let hash_name = name ^ ".hash" in
      let hash_file = hookdir // hash_name in
      if not (OpamFilename.exists hash_file)
      || (let same_hash =
          OpamHash.of_string_opt (OpamFilename.read hash_file) =
          Some (OpamHash.compute ~kind (OpamFilename.to_string script_file))
        in
        same_hash
        || not same_hash
           && OpamConsole.confirm ~default:false
             "%s contains local modification, overwrite ?"
             (OpamFilename.to_string script_file)) then
            (write_script hookdir (name, script);
            OpamFilename.chmod script_file 0o777;
            write_script hookdir (hash_name, OpamHash.to_string hash))
    ) custom

let write_dynamic_init_scripts st =
  let updates = updates ~set_opamroot:false ~set_opamswitch:false st in
  try
    if OpamStateConfig.is_newer_than_self
        ~lock_kind:`Lock_write st.switch_global then
      raise OpamSystem.Locked;
    OpamFilename.with_flock_upgrade `Lock_write ~dontblock:true
      st.switch_global.global_lock
    @@ fun _ ->
    List.iter
      (fun shell ->
         write_script (OpamPath.init st.switch_global.root)
           (variables_file shell, string_of_update st shell updates))
      [SH_sh; SH_csh; SH_fish; SH_pwsh Powershell; SH_cmd]
  with OpamSystem.Locked ->
    OpamConsole.warning
      "Global shell init scripts not installed (could not acquire lock)"

let clear_dynamic_init_scripts gt =
  List.iter (fun shell ->
      OpamFilename.remove (OpamPath.init gt.root // variables_file shell))
    [SH_sh; SH_csh; SH_fish; SH_pwsh Powershell; SH_cmd]

let dot_profile_needs_update root dot_profile =
  if not (OpamFilename.exists dot_profile) then `yes else
  let body = OpamFilename.read dot_profile in
  let pattern1 = "opam config env" in
  let pattern1b = "opam env" in
  let pattern2 = OpamFilename.to_string (OpamPath.init root // "init") in
  let pattern3 =
    OpamStd.String.remove_prefix ~prefix:(OpamFilename.Dir.to_string root)
      pattern2
  in
  let uncommented_re patts =
    Re.(compile (seq [bol; rep (diff any (set "#:"));
                      alt (List.map str patts)]))
  in
  if Re.execp (uncommented_re [pattern1; pattern1b; pattern2]) body then `no
  else if Re.execp (uncommented_re [pattern3]) body then `otherroot
  else `yes

let update_dot_profile root dot_profile shell =
  let pretty_dot_profile = OpamFilename.prettify dot_profile in
  let bash_src () =
    if (shell = SH_bash || shell = SH_sh)
    && OpamFilename.(Base.to_string (basename dot_profile)) <> ".bashrc" then
      OpamConsole.note "Make sure that %s is well %s in your ~/.bashrc.\n"
        pretty_dot_profile
        (OpamConsole.colorise `underline "sourced")
  in
  match dot_profile_needs_update root dot_profile with
  | `no        -> OpamConsole.msg "  %s is already up-to-date.\n" pretty_dot_profile; bash_src()
  | `otherroot ->
    OpamConsole.msg
      "  %s is already configured for another opam root.\n"
      pretty_dot_profile
  | `yes       ->
    let init_file = init_file shell in
    let old_body =
      if OpamFilename.exists dot_profile then
        OpamFilename.read dot_profile
      else
        "" in
    OpamConsole.msg "  Updating %s.\n" pretty_dot_profile;
    bash_src();
    let count_lines str = List.length (String.split_on_char '\n' str) in
    let opam_section =
      Printf.sprintf
        "\n\n\
         # BEGIN opam configuration\n\
         # This is useful if you're using opam as it adds:\n\
         #   - the correct directories to the PATH\n\
         #   - auto-completion for the opam binary\n\
         # This section can be safely removed at any time if needed.\n\
         %s\
         # END opam configuration\n"
        (source root shell init_file) in
    OpamFilename.write dot_profile (old_body ^ opam_section);
    OpamConsole.msg "  Added %d lines after line %d in %s.\n"
      (count_lines opam_section - 1) (count_lines old_body) pretty_dot_profile

let update_user_setup root ?dot_profile shell =
  if dot_profile <> None then (
    OpamConsole.msg "\nUser configuration:\n";
    OpamStd.Option.iter (fun f -> update_dot_profile root f shell) dot_profile
  )

let check_and_print_env_warning st =
  (* if you are trying to silence this warning,
     set the ~no_env_notice:true flag from OpamStateConfig,
     which is checked by (is_up_to_date st). *)
  if not (is_up_to_date st) &&
     (OpamFile.Config.switch st.switch_global.config = Some st.switch ||
      OpamStateConfig.(!r.switch_from <> `Command_line))
  then
    OpamConsole.formatted_msg
      "# To update the current shell environment, run: %s\n"
      (OpamConsole.colorise `bold (eval_string st.switch_global
                                     (Some st.switch)))

let setup
    root ~interactive ?dot_profile ?update_config ?env_hook ?completion
    ?inplace shell =
  let opam_root_msg =
    let current = OpamFilename.prettify_dir root in
    if root = OpamStateConfig.(default.root_dir) then
      current
    else
      let default = OpamFilename.prettify_dir OpamStateConfig.(default.root_dir) in
      Printf.sprintf "your opam root\n    (%s by default; currently %s)" default current
  in
  let shell, update_dot_profile, env_hook =
    match update_config, dot_profile, interactive with
    | Some false, _, _ -> shell, None, env_hook
    | Some true, dot_profile, _ -> shell, dot_profile, env_hook
    | None, _, false -> shell, None, env_hook
    | None, dot_profile, true ->
      OpamConsole.header_msg "Required setup - please read";

      OpamConsole.msg
        "\n\
        \  In normal operation, opam only alters files within %s.\n\
         \n\
        \  However, to best integrate with your system, some environment variables\n\
        \  should be set. "
        opam_root_msg;
      begin match dot_profile with
      | Some dot_profile ->
        OpamConsole.msg
          "If you allow it to, this initialisation step will update\n\
          \  your %s configuration by adding the following line to %s:\n\
           \n\
          \    %s\
           \n\
          \  Otherwise, every time"
        (OpamConsole.colorise `bold (string_of_shell shell))
        (OpamConsole.colorise `cyan @@ OpamFilename.prettify dot_profile)
        (OpamConsole.colorise `bold @@ source root shell (init_file shell));
      | None ->
        OpamConsole.msg "When"
      end;
      OpamConsole.msg
        " you want to access your opam installation, you will\n\
        \  need to run:\n\
         \n\
        \    %s\n\
         \n\
        \  You can always re-run this setup with 'opam init' later.\n\n"
        (OpamConsole.colorise `bold @@ shell_eval_invocation shell (opam_env_invocation shell));
      if OpamCoreConfig.answer_is_yes () then begin
        if dot_profile <> None then
          OpamConsole.warning "Shell not updated in non-interactive mode: use --shell-setup";
        shell, None, env_hook
      end else
      let rec menu shell dot_profile default =
        let colorised_shell = OpamConsole.colorise `bold (string_of_shell shell) in
        let opam_env_inv =
          OpamConsole.colorise `bold @@ shell_eval_invocation shell (opam_env_invocation shell)
        in
        let prompt () =
          match dot_profile with
          | Some dot_profile ->
              let options = [
                `Yes, Printf.sprintf "Yes, update %s"
                   (OpamConsole.colorise `cyan (OpamFilename.prettify dot_profile));
                `No_hooks, Printf.sprintf "Yes, but don't setup any hooks. You'll \
                                           have to run %s whenever you change \
                                           your current 'opam switch'"
                                          opam_env_inv;
                `Change_shell, "Select a different shell";
                `Change_file, "Specify another config file to update instead";
                `No, Printf.sprintf "No, I'll remember to run %s when I need opam"
                                     opam_env_inv
              ] in
            OpamConsole.menu "Do you want opam to configure %s?" colorised_shell
              ~default ~no:`No ~options
          | None ->
            if OpamConsole.confirm ~default:false
                "opam doesn't have any configuration options for %s; you will have to run %s \
                 whenever you change you current 'opam switch' or start a new terminal session. \
                 Alternatively, would you like to select a different shell?" colorised_shell opam_env_inv then
              `Change_shell
            else
              `No
        in
        match prompt () with
        | `No -> shell, None, env_hook
        | `Yes -> shell, dot_profile, Some true
        | `No_hooks -> shell, dot_profile, Some false
        | `Change_shell ->
          let shell =
            OpamConsole.menu ~default:shell ~no:shell
              "Please select a shell to configure"
              ~options: (List.map (fun s -> s, string_of_shell s) OpamStd.Sys.all_shells)
          in
          menu shell (OpamStd.Option.map OpamFilename.of_string (OpamStd.Sys.guess_dot_profile shell))
            default
        | `Change_file ->
          let open OpamStd.Option.Op in
          let dot_profile =
            (OpamConsole.read "Enter the name of the file to update:"
             >>| (fun f ->
                 if Filename.is_implicit f then Filename.concat (OpamStd.Sys.home ()) f
                 else f)
             >>| OpamFilename.of_string)
          in
          menu shell dot_profile `Yes
      in
      let default =
        if dot_profile = None then `No else
          match env_hook with
          | Some true -> `Yes
          | Some false -> `No_hooks
          | None -> `Yes
      in
      menu shell dot_profile default
  in
  update_user_setup root ?dot_profile:update_dot_profile shell;
  write_static_init_scripts root ?completion ?env_hook ?inplace ()

let hook_env root =
  let hook_vnam = OpamVariable.of_string "hooks" in
  let hook_vval = Some (OpamVariable.dirname (OpamPath.hooks_dir root)) in
  OpamVariable.Map.singleton hook_vnam hook_vval


(* -- Configuration eval variables -- *)
(* They are used in [OpamFormatUpgrade] and [OpamInitDefaults] *)

let sys_ocaml_eval_variables =
  (* For Windows, these only return results for OCaml 4.08+ *)
  List.map (fun (var, comment, unix, win32) ->
      let var = OpamVariable.of_string var in
      if Sys.win32 then var, win32, comment
      else var, unix, comment)
    [
      "sys-ocaml-system",
      "Target system of the OCaml compiler present on your system",
      ["sh"; "-c";
       "ocamlc -config 2>/dev/null | tr -d '\\r' | sed -n -e 's/system: //p'"],
      ["cmd"; "/d"; "/c"; "ocamlc -config-var system 2>nul"];
      "sys-ocaml-arch",
      "Target architecture of the OCaml compiler present on your system",
      ["sh"; "-c";
       "ocamlc -config 2>/dev/null | tr -d '\\r' | sed -n -e 's/i386/i686/;s/amd64/x86_64/;s/^architecture: //p'"],
      ["cmd"; "/d"; "/c";
       "for /f %f in ('ocamlc -config-var architecture 2^>nul') do @if '%f' equ 'i386' (echo i686) else if '%f' equ 'amd64' (echo x86_64) else (echo %f)"];
      "sys-ocaml-cc",
      "Host C Compiler type of the OCaml compiler present on your system",
      ["sh"; "-c";
       "ocamlc -config 2>/dev/null | tr -d '\\r' | sed -n -e 's/^ccomp_type: //p'"],
      ["cmd"; "/d"; "/c"; "ocamlc -config-var ccomp_type 2>nul"];
      "sys-ocaml-libc",
      "Host C Runtime Library type of the OCaml compiler present on your system",
      ["sh"; "-c";
       "ocamlc -config 2>/dev/null | tr -d '\\r' | sed -n -e 's/^os_type: Win32/msvc/p;s/^os_type: .*/libc/p'"],
      ["cmd"; "/d"; "/c";
       "for /f %f in ('ocamlc -config-var os_type 2^>nul') do @if '%f' equ 'Win32' (echo msvc) else (echo libc)"];
    ]
