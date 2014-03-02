(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Tools                             *)
(*                                                                        *)
(*                             OCamlPro                                   *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  See accompanying files for the terms under      *)
(*  which this file is distributed. In doubt, contact us at               *)
(*  contact@ocamlpro.com (http://www.ocamlpro.com/)                       *)
(**************************************************************************)

(* open BuildBase *)
(* open Stdlib2 *)

let term = try Sys.getenv "TERM" with Not_found -> "none"

let need_escape =
  if term <> "" then
    true
  else
    match MinUnix.os_type with
      MinUnix.WINDOWS | MinUnix.CYGWIN -> true
    | MinUnix.UNIX -> false

let term_escape s =
  if need_escape then String.escaped s else s

let has_colors =
  try
    match BuildMisc.get_stdout_lines [ "tput"; "-T" ^ term; "colors" ] [] with
    | 0, line :: _ -> (int_of_string line) >= 8
    | _ -> false
  with _ -> false

let ncolumns =
  try
    (* terminfo *)
    match BuildMisc.get_stdout_lines [ "tput"; "cols" ] [] with
    | 0, line :: _ -> int_of_string line
    | _ -> raise Not_found
  with _ ->
    try
      (* GNU stty *)
      match BuildMisc.get_stdout_lines [ "stty"; "size" ] [] with
      | 0, line :: _ -> begin
          match OcpString.split line ' ' with
          | [_ ; v] -> int_of_string v
          | _ -> failwith "stty"
        end
      | _ -> raise Not_found
    with MinUnix.Unix_error _ | End_of_file | Failure _ | Not_found ->
      try
        (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with Not_found | Failure _ ->
        80

let csi_m n = Printf.sprintf "\027[%sm" n

type ansi_sequences = {
  mutable esc_ansi : bool;
  mutable esc_bold : string;
  mutable esc_black_text : string;
  mutable esc_red_text : string;
  mutable esc_green_text : string;
  mutable esc_yellow_text : string;
  mutable esc_blue_text : string;
  mutable esc_magenta_text : string;
  mutable esc_cyan_text : string;
  mutable esc_white_text : string;
  mutable esc_end : string;
  mutable esc_linefeed : string;
  mutable esc_killline : string;
  mutable esc_columns : int;
}

let term = {
  esc_ansi = has_colors;

  esc_bold =  csi_m "1";
  esc_black_text = csi_m "30";
  esc_red_text = csi_m "31";
  esc_green_text = csi_m "32";
  esc_yellow_text = csi_m "33";
  esc_blue_text = csi_m "34";
  esc_magenta_text = csi_m "35";
  esc_cyan_text = csi_m "36";
  esc_white_text = csi_m "37";
  esc_end = csi_m "";
  esc_linefeed = "\r";
  esc_killline = "\027[K";
  esc_columns = ncolumns;
}

let set_ansi_term is_ansi =
  if is_ansi then begin
    term.esc_ansi <- true;

    term.esc_bold <-  csi_m "1";
    term.esc_black_text <- csi_m "30";
    term.esc_red_text <- csi_m "31";
    term.esc_green_text <- csi_m "32";
    term.esc_yellow_text <- csi_m "33";
    term.esc_blue_text <- csi_m "34";
    term.esc_magenta_text <- csi_m "35";
    term.esc_cyan_text <- csi_m "36";
    term.esc_white_text <- csi_m "37";
    term.esc_end <- csi_m "";
    term.esc_linefeed <- "\r";
    term.esc_killline <- "\027[K";
    term.esc_columns <- ncolumns;
  end else begin
    term.esc_ansi <- false;

    term.esc_bold <-  "";
    term.esc_black_text <- "";
    term.esc_red_text <- "";
    term.esc_green_text <- "";
    term.esc_yellow_text <- "";
    term.esc_blue_text <- "";
    term.esc_magenta_text <- "";
    term.esc_cyan_text <- "";
    term.esc_white_text <- "";
    term.esc_end <- "";
    term.esc_linefeed <- "";
    term.esc_killline <- "";
    term.esc_columns <- ncolumns;

  end


let term_bold s = term.esc_bold ^ s ^ term.esc_end
