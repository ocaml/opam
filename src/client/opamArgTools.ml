(**************************************************************************)
(*                                                                        *)
(*    Copyright 2021 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op
open Cmdliner
open OpamCLIVersion.Op

let cli2_0 = OpamCLIVersion.of_string "2.0"
let cli2_1 = OpamCLIVersion.of_string "2.1"
let cli2_2 = OpamCLIVersion.of_string "2.2"

type 'b validity_and_content = {
  valid: OpamCLIVersion.t;
  removed: (OpamCLIVersion.t * string option) option;
  content: 'b;
  default: bool;
  experimental: bool;
}

type 'a content = Valid of 'a | Removed of 'a
type 'a contented_validity = 'a content validity_and_content

type validity = unit validity_and_content
let default_validity =
  { valid = OpamCLIVersion.current;
    removed = None;
    content = ();
    default = false;
    experimental = false;
  }


let elem_of_vr = function Valid e | Removed e -> e

let contented_validity (validity:validity) content : 'a contented_validity =
  match validity.removed with
  | None -> { validity with content = Valid content}
  | Some _ -> { validity with content = Removed content}

let is_original_cli validity =
  OpamCLIVersion.compare validity.valid cli2_0 = 0

let cli_from ?(experimental=false) valid =
  { default_validity with
    valid;
    experimental;
  }
let cli_between ?option since ?replaced
    removal =
  if since >= removal then
    OpamConsole.error_and_exit `Internal_error
      "An option can't be added in %s and removed in %s"
      (OpamCLIVersion.to_string since)
      (OpamCLIVersion.to_string removal);
  let experimental, default =
    match option with
    | None -> false, false
    | Some `experimental -> true, false
    | Some `default -> false, true
  in
  { default_validity with
    valid = since;
    removed = Some (removal, replaced);
    default;
    experimental;
  }
let cli_original = cli_from cli2_0

let bold = OpamConsole.colorise `bold
let string_of_sourced_cli (c,_) = OpamCLIVersion.to_string c
let string_of_cli_option cli =
  if cli = cli2_0 then
    Printf.sprintf "set %s environment variable to %s"
      (bold "OPAMCLI") (bold "2.0")
  else
    Printf.sprintf "use --cli=%s"
      (bold @@ OpamCLIVersion.to_string cli)

let update_doc_w_cli doc ~cli validity =
  Printf.sprintf "%s%s"
    (if validity.experimental then "$(b,Experimental). " else "")
  @@ match validity with
  | { valid = c ; removed = None; _} ->
    if cli @< c then
      Printf.sprintf "(Since $(b,%s)) %s"
        (OpamCLIVersion.to_string c) doc
    else doc
  | { removed = Some (since, instead); _} ->
    if cli @< since then doc else
      Printf.sprintf "Removed in $(b,%s)%s"
        (OpamCLIVersion.to_string since)
        (match instead with
         | Some instead ->
           Printf.sprintf ", use $(i,%s) instead." instead
         | None -> ".")

(* Error messages *)
type target =
  | Flags of string list
  | Option of string
  | Verbatim of string

let get_long_form flags =
  List.fold_left (fun (lgth,long) f ->
      let flgth =  String.length f in
      if flgth > lgth then (flgth, f) else (lgth, long))
    (0,"") flags |> snd

let string_of_target = function
  | Flags flags -> bold "--"^get_long_form flags
  | Option o -> bold o
  | Verbatim s -> s

let newer_flag_msg cli valid_since target =
  Printf.sprintf
    "%s was added in version %s of the opam CLI, \
     but version %s has been requested, which is older."
    target (OpamCLIVersion.to_string valid_since)
    (string_of_sourced_cli cli)

let newer_flag_error cli valid_since targets =
  let target = string_of_target targets in
  let msg = newer_flag_msg cli valid_since target in
  `Error (false, msg)

let previously_str removal instead =
  let previous =
    string_of_cli_option (OpamCLIVersion.previous removal)
  in
  match instead with
  | Some ist ->
    Printf.sprintf  ". Use %s instead or %s"
      (bold ist) previous
  | None -> Printf.sprintf ", %s"  previous

let older_experimental_msg = function
  | [] -> ""
  | elems ->
    Printf.sprintf
      "\n%s %s experimental, so the behaviour may be removed completely \
       in a future release."
      (OpamStd.Format.pretty_list elems)
      (match elems with [_] -> "was" | _ -> "were")

let older_flag_msg ?(exp=false) cli removal instead target =
  Printf.sprintf
    "%s was removed in version %s of the opam CLI, \
     but version %s has been requested%s.%s"
    target (OpamCLIVersion.to_string removal)
    (string_of_sourced_cli cli)
    (if exp then "" else previously_str removal instead)
    (if exp then
       match instead with
       | None -> older_experimental_msg ["This flag"]
       | Some instead ->
         Printf.sprintf " It was experimental, use %s instead." (bold instead)
     else "")

let older_flag_error ?exp cli removal instead targets =
  let target = string_of_target targets in
  let msg = older_flag_msg ?exp cli removal instead target in
  `Error (false, msg)

let deprecated_warning removal instead targets =
  let target = string_of_target targets in
  OpamConsole.warning
    "%s was deprecated in version %s of the opam CLI%s."
    target (OpamCLIVersion.to_string removal)
    (previously_str removal instead)

let experimental_warning ?single = function
  | [] -> ()
  | elems ->
    let single =
      OpamStd.Option.default (match elems with [_] -> true | _ -> false) single
    in
    OpamConsole.warning
      "%s %s experimental, there is no guarantee that %s will be kept; \
       avoid using %s in scripts."
      (OpamStd.Format.pretty_list elems)
      (if single then "is" else "are")
      (if single then "it" else "they")
      (if single then "it" else "them")

(* Cli version check *)
let cond_new cli c = cli @< c
let cond_removed cli removal = cli @>= removal

let check_cli_validity_t ~newer ~default_cli ~older ~valid
    ?(cond=fun x -> x) cli validity =
  let exp = cond validity.experimental in
  match validity with
  | { removed = None ; valid = c; _ } when cond (cond_new cli c) ->
    newer c
  | { removed = Some (removal, instead); default = true; _ }
    when (snd cli = `Default)
      && OpamCLIVersion.default < removal
      && cond true ->
    default_cli removal instead
  | { removed = Some (removal, instead); _ }
    when cond (cond_removed cli removal) ->
    older ~exp removal instead
  | _ -> valid ~exp ()

let check_cli_validity cli validity ?cond elem targets =
  check_cli_validity_t cli validity ?cond
    ~newer:(fun c ->
        newer_flag_error cli c targets)
    ~default_cli:(fun removal instead ->
        deprecated_warning removal instead targets;
        `Ok elem)
    ~older:(fun ~exp removal instead ->
        older_flag_error ~exp cli removal instead targets)
    ~valid:(fun ~exp () ->
        if exp then
          experimental_warning ["Flag "^string_of_target targets];
        `Ok elem)

let term_cli_check ~check arg =
  Term.(ret ((const check) $ (Arg.value arg)))

(** Helpers for mk_vflag_all & mk_enum_opt_all *)
let preprocess_validity_for_all cli find flags elems =
  List.fold_left (fun (newer_cli,older_cli,valid) elem ->
      match OpamStd.List.find_opt (find elem) flags with
      | Some (validity, flags, _) ->
        check_cli_validity_t cli validity
          ~newer:(fun c -> (flags, c)::newer_cli, older_cli, valid)
          ~default_cli:(fun _ _ ->
              newer_cli, older_cli, (elem, flags, false)::valid)
          ~older:(fun ~exp removal instead ->
              newer_cli, (flags, (removal, instead), exp)::older_cli, valid)
          ~valid:(fun ~exp () ->
              newer_cli, older_cli, (elem, flags, exp)::valid)
      | None -> newer_cli, older_cli, valid)
    ([],[],[]) elems

let split_clis_all l =
  List.fold_left (fun (strs, clis, exps) (s,c,e) ->
      s::strs, c::clis,
      if e then s::exps else exps)
    ([],[],[]) l

(* Arguments *)

let mk_flag ~cli validity ~section flags doc =
  let doc = update_doc_w_cli doc ~cli validity in
  let doc = Arg.info ~docs:section ~doc flags in
  let check elem =
    check_cli_validity cli validity ~cond:(fun c -> c && elem) elem
      (Flags flags)
  in
  term_cli_check ~check Arg.(flag & doc)

let mk_opt ~cli validity ~section ?vopt flags value doc kind default =
  let doc = update_doc_w_cli doc ~cli validity in
  let doc = Arg.info ~docs:section ~docv:value ~doc flags in
  let check elem =
    check_cli_validity cli validity
      ~cond:(fun c -> c && default <> elem) elem (Flags flags)
  in
  term_cli_check ~check Arg.(opt ?vopt kind default & doc)

let mk_opt_all ~cli validity ~section ?vopt ?(default=[])
    flags value doc kind =
  let doc = update_doc_w_cli doc ~cli validity in
  let doc = Arg.info ~docs:section ~docv:value ~doc flags in
  let check elem =
    check_cli_validity cli validity
      ~cond:(fun c -> c && default <> elem) elem (Flags flags)
  in
  term_cli_check ~check Arg.(opt_all ?vopt kind default & doc)

let mk_vflag ~cli ~section default flags =
  let flags = List.map (fun (v,c,f,d) -> contented_validity v c, f, d) flags in
  let info_flags =
    List.map (fun (validity, flag, doc) ->
        let doc = update_doc_w_cli doc ~cli validity in
        validity.content, Arg.info ~docs:section flag ~doc)
      flags
  in
  let check elem =
    match
      OpamStd.List.find_opt (fun (validity, _, _) ->
          validity.content = elem)
        flags
    with
    | Some (validity, flags, _) ->
      check_cli_validity cli validity (elem_of_vr elem) (Flags flags)
    | None -> `Ok (elem_of_vr elem)
  in
  term_cli_check ~check Arg.(vflag (Valid default) info_flags)

let mk_flag_replaced ~cli ~section flags doc =
  let flags = List.map (fun (c,f) -> c, true, f, doc) flags in
  mk_vflag ~cli ~section false flags

let mk_vflag_all ~cli ~section ?(default=[]) flags =
  let flags = List.map (fun (v,c,f,d) -> contented_validity v c, f, d) flags in
  let info_flags =
    List.map (fun (validity, flag, doc) ->
        let doc = update_doc_w_cli doc ~cli validity in
        validity.content, Arg.info ~docs:section flag ~doc)
      flags
  in
  let check selected =
    let newer_cli, older_cli, valid =
      preprocess_validity_for_all cli (fun elem (validity, _, _) ->
          validity.content = elem) flags selected
    in
    let max_cli clis =
      OpamCLIVersion.to_string @@
      match clis with
      | [] -> assert false
      | c::cl -> List.fold_left max c cl
    in
    let lstring_of_options options =
      (List.map (fun o -> string_of_target (Flags o)) options)
    in
    let string_of_options options =
      OpamStd.Format.pretty_list (lstring_of_options options)
    in
    let valid_elems elems =
      let experimentals, elems =
        List.fold_left (fun (experimentals, elems) (elem, str, exp) ->
            let elems = elem_of_vr elem::elems in
            if exp then
              (bold "--"^get_long_form str)::experimentals, elems
            else experimentals,elems)
          ([],[]) elems
      in
      experimental_warning experimentals;
      `Ok elems
    in
    match newer_cli, older_cli with
    | [], [] -> valid_elems valid
    | [flags, c], [] ->
      newer_flag_error cli c (Flags flags)
    | [], [flags, (c, instead), exp] ->
      older_flag_error ~exp cli c instead (Flags flags)
    | _::_, []->
      let options, clis = List.split newer_cli in
      let msg =
        Printf.sprintf
          "%s can only be used with at least version %s of the opam \
           CLI, but version %s has been requested."
          (string_of_options options)
          (max_cli clis)
          (string_of_sourced_cli cli)
      in
      `Error (false, msg)
    | [], _::_->
      let options, clis, experimentals = split_clis_all older_cli in
      let clis = List.split clis |> fst in
      let in_all =
        match clis with
        | c::cs when List.for_all ((=) c) cs -> Some c
        | _ -> None
      in
      let msg =
        Printf.sprintf
          "%s %swere all removed by version %s of the opam CLI, \
           but version %s has been requested.%s"
          (string_of_options options)
          (OpamStd.Option.to_string
             (OpamCLIVersion.to_string
              @> Printf.sprintf "were all in %s, and ") in_all)
          (max_cli clis)
          (string_of_sourced_cli cli)
          (older_experimental_msg (lstring_of_options experimentals))
      in
      `Error (false, msg)
    | _,_ ->
      let newer, nclis = List.split newer_cli in
      let older, rclis_ist, o_experimentals = split_clis_all older_cli in
      let rclis, insteads = List.split rclis_ist in
      let msg =
        if List.for_all ((<>) None) insteads then
          Printf.sprintf
            "This combination of options is not possible: %s require \
             at least version %s of the opam CLI and the newer %s \
             flags must be used for %s respectively!"
            (string_of_options newer) (max_cli nclis)
            (string_of_options older)
            (OpamStd.Format.pretty_list
               (List.map (function Some f -> f | None -> assert false)
                    insteads))
        else
          Printf.sprintf
            "This combination of options is not possible: %s require \
             at least version %s of the opam CLI but %s were all \
             removed by version %s of the opam CLI!"
            (string_of_options newer) (max_cli nclis)
            (string_of_options older) (max_cli rclis)
      in
      let msg =
        Printf.sprintf "%s%s" msg
          (older_experimental_msg (lstring_of_options o_experimentals))
      in
      `Error (false, msg)
  in
  let default = List.map (fun x -> Valid x) default in
  term_cli_check ~check Arg.(vflag_all default info_flags)

let string_of_enum enum =
  Arg.doc_alts_enum (List.map (fun (_, s, v) -> s,v) enum)

let mk_enum_opt ~cli validity ~section flags value states doc =
  let doc = update_doc_w_cli doc ~cli validity in
  let doc = Arg.info ~docs:section ~docv:value ~doc flags in
  let check elem =
    (* first check validity of flag *)
    let flag_validity =
      check_cli_validity cli validity ~cond:(fun c -> c && elem <> None)
        elem (Flags flags)
    in
    (* then check validity of the argument *)
    match flag_validity with
    | `Ok (Some elem) ->
      let validity, str, _ = List.find (fun (_,_,v) -> v = elem) states in
      check_cli_validity cli validity (Some elem)
        (Verbatim
           (Printf.sprintf "the %s option for %s"
              (bold str) (bold "--"^get_long_form flags)))
    | _ -> flag_validity
  in
  let states = List.map (fun (_, s, v) -> s,v) states in
  term_cli_check ~check Arg.(opt (some (enum states)) None & doc)

let mk_enum_opt_all ~cli validity ~section flags value states doc =
  let doc = update_doc_w_cli doc ~cli validity in
  let doc = Arg.info ~docs:section ~docv:value ~doc flags in
  let check elems =
    (* first check validity of flag *)
    let flag_validity =
      check_cli_validity cli validity ~cond:(fun c -> c && elems <> [])
        elems (Flags flags)
    in
    (* then check validity of the argument *)
    match flag_validity with
    | `Error _ -> flag_validity
    | `Ok elems ->
      let newer_cli, older_cli, valid =
        preprocess_validity_for_all cli (fun elem (_,_,v) -> v = elem)
          states elems
      in
      let max_cli clis =
        OpamCLIVersion.to_string @@
        match clis with
        | [] -> assert false
        | c::cl -> List.fold_left max c cl
      in
      let long_form_flags = "--"^get_long_form flags in
      let to_str states =
        Printf.sprintf "The option%s %s for %s"
          (match states with [_] -> "" | _ -> "s")
          (bold @@ OpamStd.Format.pretty_list states)
          (bold long_form_flags)
      in
      let valid_flags elems =
        let experimentals, elems =
          List.fold_left (fun (experimentals, elems) (elem, str, exp) ->
              let elems = elem::elems in
              if exp then str::experimentals, elems
              else experimentals,elems)
            ([],[]) elems
        in
        if experimentals = [] then () else
          experimental_warning
            ~single:(match experimentals with | [_] -> true | _ -> false)
            [to_str experimentals];
        `Ok elems
      in
      match newer_cli, older_cli, valid with
      | [], [], elems -> valid_flags elems
      | [str, c], [], [] ->
        newer_flag_error cli c (Verbatim (to_str [str]))
      | [str, c], [], elems ->
        (OpamConsole.warning "%s"
           (newer_flag_msg cli c (to_str [str]));
         valid_flags elems)
      | [], [str, (c, instead), exp], [] ->
        older_flag_error ~exp cli c instead (Verbatim (to_str [str]))
      | [], [str, (c, instead), exp], elems ->
        (OpamConsole.warning "%s"
           (older_flag_msg ~exp cli c instead (to_str [str]));
         valid_flags elems)
      | _::_, [], elems ->
        let states, clis = List.split newer_cli in
        let msg =
          Printf.sprintf
            "%s can only be used with at least version %s of the opam \
             CLI, but version %s has been requested."
            (to_str states) (max_cli clis)
            (string_of_sourced_cli cli)
        in
        if elems = [] then `Error (false, msg) else
          (OpamConsole.warning "%s" msg; valid_flags elems)
      | [], _::_, elems->
        let states, clis, experimentals = split_clis_all older_cli in
        let clis = List.split clis |> fst in
        let in_all =
          match clis with
          | c::cs when List.for_all ((=) c) cs -> Some c
          | _ -> None
        in
        let msg =
          Printf.sprintf
            "%s %swere all removed by version %s of the opam CLI, \
             but version %s has been requested.%s"
            (to_str states)
            (OpamStd.Option.to_string
               (OpamCLIVersion.to_string
                @> Printf.sprintf "were all in %s, and ") in_all)
            (max_cli clis)
            (string_of_sourced_cli cli)
            (older_experimental_msg ([to_str experimentals]))
        in
        if elems = [] then `Error (false, msg) else
          (OpamConsole.warning "%s" msg; valid_flags elems)
      | _, _, elems ->
        let newer, nclis = List.split newer_cli in
        let older, rclis_ist, o_experimentals = split_clis_all older_cli in
        let rclis, insteads = List.split rclis_ist in
        let msg =
          if List.for_all ((<>) None) insteads then
            Printf.sprintf
              "This combination of %s options is not possible: %s require \
               at least version %s of the opam CLI and the newer %s \
               flags must be used for %s respectively!"
              (bold long_form_flags)
              (bold @@ OpamStd.Format.pretty_list newer)
              (max_cli nclis)
              (bold @@ OpamStd.Format.pretty_list older)
              (OpamStd.Format.pretty_list
                 (List.map (function Some f -> f | None -> assert false)
                      insteads))
          else
            Printf.sprintf
              "This combination of %s options is not possible: %s require \
               at least version %s of the opam CLI but %s were all \
               removed by version %s of the opam CLI!"
              (bold long_form_flags)
              (bold @@ OpamStd.Format.pretty_list newer) (max_cli nclis)
              (bold @@ OpamStd.Format.pretty_list older) (max_cli rclis)
        in
        let msg =
          Printf.sprintf "%s%s" msg
            (older_experimental_msg ([to_str o_experimentals]))
        in
        if elems = [] then `Error (false, msg) else
          (OpamConsole.warning "%s" msg; valid_flags elems)
  in
  let states = List.map (fun (_, s, v) -> s,v) states in
  term_cli_check ~check Arg.(opt_all (enum states) [] & doc)

(* Subcommands *)
type 'a subcommand = validity * string * 'a * string list * string
type 'a subcommands = 'a subcommand list

let mk_subdoc ~cli ?(defaults=[]) ?(extra_defaults=[]) commands =
  let bold s = Printf.sprintf "$(b,%s)" s in
  let it s = Printf.sprintf "$(i,%s)" s in
  `S Manpage.s_commands ::
  (List.map (function
       | "", name ->
         `P (Printf.sprintf "Without argument, defaults to %s."
               (bold name))
       | arg, default ->
         `I (it arg, Printf.sprintf "With a %s argument, defaults to %s %s."
               (it arg) (bold default) (it arg))
     ) defaults) @
  (List.map (fun (validity, arg, doc) ->
       `I (it arg, update_doc_w_cli doc ~cli validity))
      extra_defaults) @
  List.map (fun (validity, c, _, args,d) ->
      let cmds = bold c ^ " " ^ OpamStd.List.concat_map " " it args in
      let d = update_doc_w_cli d ~cli validity in
      `I (cmds, d)
    ) commands

let mk_subcommands_aux ~cli my_enum commands =
  let commands =
    List.map (fun (v,n,c,a,d) -> contented_validity v c, n, a, d) commands
  in
  let command =
    let doc = Arg.info ~docv:"COMMAND" [] in
    let scommand = List.rev_map (fun (v,f,_,_) -> f,v.content) commands in
    let check = function
      | None -> `Ok None
      | Some elem ->
        match OpamStd.List.find_opt (fun (validity, _, _, _) ->
            validity.content = elem) commands with
        | Some (validity, sbcmd, _,_) ->
          check_cli_validity cli validity (Some (elem_of_vr elem))
            (Option sbcmd)
        | None -> `Ok (Some (elem_of_vr elem))
    in

    term_cli_check ~check Arg.(pos 0 (some & my_enum scommand) None & doc)
  in
  let params =
    let doc = Arg.info ~doc:"Optional parameters." [] in
    Arg.(value & pos_right 0 string [] & doc)
  in
  command, params

let mk_subcommands ~cli commands =
  mk_subcommands_aux ~cli Arg.enum commands

type 'a default = [> `default of string] as 'a

let mk_subcommands_with_default ~cli commands =
  let enum_with_default_valrem sl =
    let parse, print = Arg.enum sl in
    let parse s =
      match parse s with
      | `Ok x -> `Ok (x)
      | _ -> `Ok (Valid (`default s)) in
    parse, print
  in
  mk_subcommands_aux ~cli enum_with_default_valrem commands

let bad_subcommand ~cli subcommands (command, usersubcommand, userparams) =
  match usersubcommand with
  | None ->
    `Error (false,
            Printf.sprintf "Missing subcommand. Valid subcommands are %s."
              (OpamStd.Format.pretty_list
                 (OpamStd.List.filter_map (fun (validity,sb,_,_,_) ->
                      match validity with
                      | {valid = c; removed = None; _} when cli @>= c -> None
                      | {removed = Some (c,_); _}  when cli @< c -> None
                      | _ -> Some sb)
                     subcommands)))
  | Some (`default cmd) ->
    `Error (true, Printf.sprintf "Invalid %s subcommand %S" command cmd)
  | Some usersubcommand ->
    let exe = Filename.basename Sys.executable_name in
    match
      List.find_all (fun (_,_,cmd,_,_) ->
          cmd = usersubcommand) subcommands
    with
    | [ _, name, _, args, _doc] ->
      let usage =
        Printf.sprintf "%s %s [OPTION]... %s %s"
          exe command name (String.concat " " args) in
      if List.length userparams < List.length args then
        `Error (false, Printf.sprintf "%s: Missing argument.\nUsage: %s\n"
                  exe usage)
      else
        `Error (false, Printf.sprintf "%s: Too many arguments.\nUsage: %s\n"
                  exe usage)
    | _ ->
      `Error (true, Printf.sprintf "Invalid %s subcommand" command)

(* Commands *)

type command = unit Term.t * Term.info

(* As [term_info] is defined later, we need to have it as argument *)
let mk_command ~cli validity term_info name ~doc ~man cmd =
  let doc = update_doc_w_cli doc ~cli validity in
  let info = term_info ~cli name ~doc ~man in
  let check =
    check_cli_validity cli validity () (Option name)
    |> Term.const
    |> Term.ret
  in
  Term.(cmd $ check), info

let mk_command_ret ~cli validity term_info name ~doc ~man cmd =
  let doc = update_doc_w_cli doc ~cli validity in
  let info = term_info ~cli name ~doc ~man in
  let check =
    check_cli_validity cli validity () (Option name)
    |> Term.const
    |> Term.ret
  in
  Term.(ret (cmd $ check)), info

(* Environment variables *)

let check_cli_env_validity cli validity var cons =
  let is_defined () = OpamStd.Config.env (fun x -> x) var <> None in
  let ovar = "OPAM"^var in
  match validity with
  | { removed = None ; valid = c; _ } when cond_new cli c ->
    if is_defined () then
      OpamConsole.warning
        "%s was ignored because CLI %s \
         was requested and it was introduced in %s."
        ovar (string_of_sourced_cli cli) (OpamCLIVersion.to_string c);
    None
  | { removed = Some (removal, instead); _ } when cond_removed cli removal ->
    if is_defined () then
      OpamConsole.warning
        "%s was ignored because CLI %s \
         was requested and it was removed in %s%s."
        ovar (string_of_sourced_cli cli) (OpamCLIVersion.to_string removal)
        (previously_str removal instead);
    None
  | _ -> Some (cons var)

let env_with_cli environment =
  let doc_env cli =
    List.map (fun (var, validity, _cons, doc) ->
        let doc = update_doc_w_cli doc ~cli validity in
        `P (Printf.sprintf "$(i,OPAM%s) %s" var doc))
      environment
  in
  let init_env cli =
    OpamStd.List.filter_map (fun (var, validity, cons, _doc) ->
        check_cli_env_validity cli validity var cons)
      environment
    |> OpamStd.Config.E.updates
  in
  doc_env, init_env
