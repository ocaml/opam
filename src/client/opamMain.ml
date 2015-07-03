(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

let json_out () =
  match OpamStateConfig.(!r.json_out) with
  | None   -> ()
  | Some s ->
    let file_name () =
      match OpamStd.String.cut_at s '%' with
      | None -> OpamFilename.of_string s
      | Some (pfx, sfx) ->
        let rec getname i =
          let f = OpamFilename.of_string (Printf.sprintf "%s%d%s" pfx i sfx) in
          if OpamFilename.exists f then getname (i+1) else f
        in
        getname 1
    in
    try
      let f = OpamFilename.open_out (file_name ()) in
      OpamJson.flush f;
      close_out f
    with e ->
      OpamConsole.warning "Couldn't write json log: %s"
        (Printexc.to_string e)

let () =
  OpamStd.Sys.at_exit (fun () ->
      flush stderr;
      flush stdout;
      if OpamClientConfig.(!r.print_stats) then (
        OpamFile.print_stats ();
        OpamSystem.print_stats ();
      );
      json_out ()
    );
  OpamArg.(run default commands)
