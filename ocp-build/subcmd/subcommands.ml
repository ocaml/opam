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

module TYPES = struct

  type subcmd_spec = {
    subcmd_list : (string * Arg.spec * string) list;
    subcmd_usage : string list;
    subcmd_help : string list;
  }

  type subcmd_init = (unit -> unit)
  type subcmd_action = (string array -> unit)

end

exception Usage

open TYPES

module type SPEC = sig

  val subcmd_spec : subcmd_spec
  val subcmd_init : subcmd_init
  val subcmd_main : subcmd_action

end

let parse arg_list subcommands arg_usage =
  let arg_usage =
    String.concat "\n"
      (arg_usage :: "" ::
         "Available sub-commands are:" ::
         (List.map (fun (name, _, spec, _) ->
           Printf.sprintf "- %s [GLOBAL_OPTIONS] %s %s"
             (Filename.basename Sys.argv.(0))
             name
             (String.concat "\n    " spec.subcmd_usage)
          ) subcommands) @
         [
           Printf.sprintf "- %s help SUB-COMMAND : display help on sub-command"
             (Filename.basename Sys.argv.(0));
           "";
           "where GLOBAL_OPTIONS are:";
         ])
  in
  let subcmd_usage cmd spec =
    Printf.sprintf "%s %s [OPTIONS] %s"
      (Filename.basename Sys.argv.(0)) cmd
      (String.concat "\n" (spec.subcmd_usage @
                             [ ""; "where available OPTIONS are:"]))
  in
  let arg_anon s =
    List.iter (fun (cmd, init, spec, arg_action) ->
      if cmd = s then
        try
          init ();
          Sys.argv.(!Arg.current) <- Printf.sprintf "%s-%s" Sys.argv.(0) s;
          let arg_anon s =
            let current = !Arg.current in
            assert (s = Sys.argv.(current));
            let nargs = Array.length Sys.argv in
            arg_action (Array.sub Sys.argv current (nargs-current));
            exit 0
          in
          Arg.parse spec.subcmd_list arg_anon (subcmd_usage cmd spec);
          arg_action [||];
          exit 0
        with Usage ->
          Arg.usage spec.subcmd_list
            (Printf.sprintf "%s [GLOBAL_OPTIONS] %s %s"
               Sys.argv.(0) s
               (String.concat "\n" spec.subcmd_usage));
          exit 2
    ) subcommands;
    if s = "help" then begin
      Sys.argv.(!Arg.current) <- Printf.sprintf "%s-%s" Sys.argv.(0) s;
      let arg_anon s =
        List.iter (fun (cmd, init, spec, arg_action) ->
          if cmd = s then
            let help = match spec.subcmd_help with
              | _ :: _ -> String.concat "\n " spec.subcmd_help
              |  [] ->
                match spec.subcmd_usage with
                    [] ->
                      Printf.sprintf "No help on subcommand %S\n%!" s
                  | head :: tail ->
                    Arg.usage_string spec.subcmd_list (subcmd_usage cmd spec)
            in
            Printf.printf "%s\n%!" help;
            exit 0
        ) subcommands;
        exit 0
      in
      Arg.parse arg_list arg_anon arg_usage;
      Arg.usage arg_list arg_usage;
    end;

    Printf.eprintf "Fatal Error: no subcommand %S\n" s;
    Arg.usage arg_list arg_usage;
    exit 2
  in
  Arg.parse arg_list arg_anon arg_usage
