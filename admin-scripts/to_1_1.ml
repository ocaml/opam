#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
#directory "+../re";;

(* Converts OPAM 1.2 packages for compat with OPAM 1.1
   * merge 'install' with 'build'
   * remove the new fields: install, flags and dev-repo
   * remove dependency flags ('build', 'test', 'doc')
   * set file version
   * replace inequality constraints with '> & <'
   * remove new global variables from filters in commands, messages, 'patches',
     'available'
*)

open OpamTypes
open OpamStd.Option.Op
;;

OpamFormatConfig.update ~all_parens:true;;

let rewrite_constraint ~conj = (* Rewrites '!=' *)
  OpamFormula.map OpamFormula.(function
      | (`Neq,v) ->
        if conj then And (Atom (`Lt,v), Atom (`Gt,v))
        else Or (Atom (`Lt,v), Atom (`Gt,v))
      | atom -> Atom atom)
;;

let vars_new_1_2 = [ "compiler"; "ocaml-native"; "ocaml-native-tools";
                     "ocaml-native-dynlink"; "arch" ]

let filter_string =
  let rex =
    Re.(compile ( seq [
        str "%{";
        rep (seq [opt (char '%'); opt (char '}'); diff notnl (set "}%")]);
        str "}%";
      ]))
  in
  Re_pcre.substitute ~rex ~subst:(fun s ->
      match String.sub s 2 (String.length s - 4) with
      | "compiler" -> "ocaml-version"
      | "ocaml-native" | "ocaml-native-tools" | "ocaml-native-dynlink" -> "true"
      | s when List.mem s vars_new_1_2 -> ""
      | s when String.contains s '?' -> (* new if/else printers: use default *)
        (try let i = String.rindex s ':' + 1 in String.sub s i (String.length s - i)
         with Not_found -> s)
      | _ -> s
    )

let rec filter_vars = function
  | FIdent ([],i,None) when List.mem (OpamVariable.to_string i) vars_new_1_2 ->
    None
  | FString s -> Some (FString (filter_string s))
  | FBool _ | FIdent _ as f -> Some f
  | FOp (f1,op,f2) ->
     (match filter_vars f1, filter_vars f2 with
      | Some f1, Some f2 -> Some (FOp (f1, op, f2))
      | _ -> Some (FBool false))
  | FAnd (f1,f2) ->
     (match filter_vars f1, filter_vars f2 with
      | Some f1, Some f2 -> Some (FAnd (f1, f2))
      | opt, None | None, opt -> Some (FBool false))
  | FOr (f1,f2) ->
     (match filter_vars f1, filter_vars f2 with
      | Some f1, Some f2 -> Some (FOr (f1, f2))
      | opt, None | None, opt -> opt)
  | FNot f ->
     (match filter_vars f with
      | Some f -> Some (FNot f)
      | None -> None)
  | FUndef -> None

let filter_vars_optlist ol =
  List.map
    (fun (x, filter) -> x, filter >>= filter_vars)
    ol

let filter_args sl =
  OpamStd.List.filter_map
    (fun (s, filter) -> match s with
       | CString s -> Some (CString (filter_string s),filter)
       | CIdent i when List.mem i vars_new_1_2 -> None
       | id -> Some (id,filter))
    sl

let filter_vars_commands ol =
  List.map
    (fun (args, filter) -> filter_args (filter_vars_optlist args),
                           filter >>= filter_vars)
    ol

let to_1_1 _ opam =
  let module OF = OpamFile.OPAM in
  if
    OpamVersion.compare (OF.opam_version opam) (OpamVersion.of_string "1.2")
    < 0
  then opam
  else
  let opam =
    OF.with_build opam
      (filter_vars_commands (OF.build opam @ OF.install opam)) in
  let opam = OF.with_install opam [] in
  let opam = OF.with_flags opam [] in
  let opam = OF.with_dev_repo opam None in
  let opam = OF.with_features opam [] in
  let opam = OF.with_opam_version opam (OpamVersion.of_string "1.1") in
  let remove_ext =
    OpamFormula.map (fun (n, (_,cstr)) ->
        OpamFormula.Atom (n, ([], rewrite_constraint ~conj:false cstr)))
  in
  let opam = OF.with_depends opam (remove_ext (OF.depends opam)) in
  let opam = OF.with_depopts opam (remove_ext (OF.depopts opam)) in
  let opam =
    OF.with_conflicts opam
      (OpamFormula.map (fun (n, cstr) ->
           OpamFormula.Atom (n, rewrite_constraint ~conj:false cstr))
          (OF.conflicts opam))
  in
  let opam = OF.with_available opam (filter_vars (OF.available opam) +! FBool true) in
  let opam = OF.with_patches opam (filter_vars_optlist (OF.patches opam)) in
  let opam = OF.with_libraries opam [] in
  let opam = OF.with_syntax opam [] in
  let opam = OF.with_messages opam (filter_vars_optlist (OF.messages opam)) in
  let opam =
    OF.with_post_messages opam (filter_vars_optlist (OF.post_messages opam)) in
  opam
;;

Opam_admin_top.iter_packages ~opam:to_1_1 ()
