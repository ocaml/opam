(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCompat

(* Global configuration *)

let debug () = OpamCoreConfig.(!r.debug_level) > 0

let verbose () = OpamCoreConfig.(!r.verbose_level) > 0

let dumb_term = lazy (
  try OpamStd.Env.get "TERM" = "dumb" with Not_found -> true
)

let color =
  let auto = lazy (
    OpamStd.Sys.tty_out && not (Lazy.force dumb_term)
  ) in
  fun () -> match OpamCoreConfig.(!r.color) with
    | `Always -> true
    | `Never -> false
    | `Auto -> Lazy.force auto

let disp_status_line () =
  match OpamCoreConfig.(!r.disp_status_line) with
  | `Always -> true
  | `Never -> false
  | `Auto -> OpamStd.Sys.tty_out && (color () || not (Lazy.force dumb_term))

let utf8, utf8_extended =
  let auto = lazy (
    let checkv v =
      try Some (OpamStd.String.ends_with ~suffix:"UTF-8" (OpamStd.Env.get v))
      with Not_found -> None
    in
    OpamStd.Option.Op.(checkv "LC_ALL" ++ checkv "LANG" +! false)
  ) in
  (fun () -> match OpamCoreConfig.(!r.utf8) with
     | `Always | `Extended -> true
     | `Never -> false
     | `Auto -> Lazy.force auto),
  (fun () -> match OpamCoreConfig.(!r.utf8) with
     | `Extended -> true
     | `Always | `Never -> false
     | `Auto -> Lazy.force auto && OpamStd.Sys.(os () = Darwin))

let timer () =
  if debug () then
    let t = Unix.gettimeofday () in
    fun () -> Unix.gettimeofday () -. t
  else
    fun () -> 0.

let global_start_time =
  Unix.gettimeofday ()

type text_style =
  [ `bold
  | `underline
  | `crossed
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white ]

let style_code (c: text_style) = match c with
  | `bold      -> "01"
  | `underline -> "04"
  | `crossed   -> "09"
  | `black     -> "30"
  | `red       -> "31"
  | `green     -> "32"
  | `yellow    -> "33"
  | `blue      -> "1;34" (* most terminals make blue unreadable unless bold *)
  | `magenta   -> "35"
  | `cyan      -> "36"
  | `white     -> "37"

(* not nestable *)
let colorise style s =
  if not (color ()) then s else
    Printf.sprintf "\027[%sm%s\027[m" (style_code style) s

let colorise' styles s =
  if not (color ()) then s else
    Printf.sprintf "\027[%sm%s\027[m"
      (String.concat ";" (List.map style_code styles))
      s

let acolor_with_width width c oc s =
  let str = colorise c s in
  output_string oc str;
  match width with
  | None   -> ()
  | Some w ->
    if String.length str >= w then ()
    else output_string oc (String.make (w-String.length str) ' ')

let acolor c oc s = acolor_with_width None c oc s
let acolor_w width c oc s = acolor_with_width (Some width) c oc s

let timestamp () =
  let time = Unix.gettimeofday () -. global_start_time in
  let tm = Unix.gmtime time in
  let msec = time -. (floor time) in
  Printf.ksprintf (colorise `blue) "%.2d:%.2d.%.3d"
    (tm.Unix.tm_hour * 60 + tm.Unix.tm_min)
    tm.Unix.tm_sec
    (int_of_float (1000.0 *. msec))

let log section ?(level=1) fmt =
  if level <= OpamCoreConfig.(!r.debug_level) then
    let () = flush stdout in
    Printf.fprintf stderr ("%s  %a  " ^^ fmt ^^ "\n%!")
      (timestamp ()) (acolor_w 30 `yellow) section
  else
    Printf.ifprintf stderr fmt

(* Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
   rather than [log "%s" (to_string x)] to avoid costly unneeded
   stringifications *)
let slog to_string channel x = output_string channel (to_string x)

let error fmt =
  Printf.ksprintf (fun str ->
    flush stdout;
    Printf.eprintf "%a %s\n%!" (acolor `red) "[ERROR]"
      (OpamStd.Format.reformat ~start_column:8 ~indent:8 str)
  ) fmt

let warning fmt =
  Printf.ksprintf (fun str ->
    flush stdout;
    Printf.eprintf "%a %s\n%!" (acolor `yellow) "[WARNING]"
      (OpamStd.Format.reformat ~start_column:10 ~indent:10 str)
  ) fmt

let note fmt =
  Printf.ksprintf (fun str ->
    flush stdout;
    Printf.eprintf "%a %s\n%!" (acolor `blue) "[NOTE]"
      (OpamStd.Format.reformat ~start_column:7 ~indent:7 str)
  ) fmt

let errmsg fmt =
  flush stdout;
  Printf.eprintf (fmt ^^ "%!")

let error_and_exit reason fmt =
  Printf.ksprintf (fun str ->
    error "%s" str;
    OpamStd.Sys.exit_because reason
  ) fmt

let msg fmt =
  flush stderr;
  Printf.printf (fmt ^^ "%!")

let formatted_msg ?indent fmt =
  flush stderr;
  Printf.ksprintf
    (fun s -> print_string (OpamStd.Format.reformat ?indent s); flush stdout)
    fmt

let last_status = ref ""
let status_line =
  let carriage_delete = "\r\027[K" in
  fun fmt ->
    if debug () || not (disp_status_line ()) then
      Printf.ksprintf
        (fun s -> if s <> !last_status then (last_status := s; print_endline s))
        fmt
    else
      Printf.ksprintf
        (fun s ->
           print_string carriage_delete;
           print_string s;
           flush stdout;
           print_string carriage_delete (* unflushed *))
        fmt

let header_width () = min 80 (OpamStd.Sys.terminal_columns ())

let header_msg fmt =
  let utf8camel = "\xF0\x9F\x90\xAB " in (* UTF-8 <U+1F42B, U+0020> *)
  let padding = "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\
                 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" in
  Printf.ksprintf (fun str ->
      flush stderr;
      print_char '\n';
      let wpad = header_width () - String.length str - 2 in
      let wpadl = 4 in
      print_string (colorise `cyan (String.sub padding 0 wpadl));
      print_char ' ';
      print_string (colorise `bold str);
      print_char ' ';
      let wpadr = wpad - wpadl - if utf8_extended () then 4 else 0 in
      if wpadr > 0 then
        print_string
          (colorise `cyan
             (String.sub padding (String.length padding - wpadr) wpadr));
      if wpadr >= 0 && utf8_extended () then
        (print_string "  ";
         print_string (colorise `yellow utf8camel));
      print_char '\n';
      flush stdout;
    ) fmt

let header_error fmt =
  let padding = "#=======================================\
                 ========================================#" in
  Printf.ksprintf (fun head fmt ->
      Printf.ksprintf (fun contents ->
          output_char stderr '\n';
          let wpad = header_width () - String.length head - 8 in
          let wpadl = 4 in
          output_string stderr (colorise `red (String.sub padding 0 wpadl));
          output_char stderr ' ';
          output_string stderr (colorise `bold "ERROR");
          output_char stderr ' ';
          output_string stderr (colorise `bold head);
          output_char stderr ' ';
          let wpadr = wpad - wpadl in
          if wpadr > 0 then
            output_string stderr
              (colorise `red
                 (String.sub padding (String.length padding - wpadr) wpadr));
          output_char stderr '\n';
          output_string stderr contents;
          output_char stderr '\n';
          flush stderr;
        ) fmt
    ) fmt


let confirm ?(default=true) fmt =
  Printf.ksprintf (fun s ->
      try
        if OpamCoreConfig.(!r.safe_mode) then false else
        let prompt () =
          formatted_msg "%s [%s] " s (if default then "Y/n" else "y/N")
        in
        if OpamCoreConfig.(!r.answer) = Some true then
          (prompt (); msg "y\n"; true)
        else if OpamCoreConfig.(!r.answer) = Some false ||
                OpamStd.Sys.(not tty_in)
        then
          (prompt (); msg "n\n"; false)
        else if OpamStd.Sys.(not tty_out || os () = Win32 || os () = Cygwin) then
          let rec loop () =
            prompt ();
            match String.lowercase_ascii (read_line ()) with
            | "y" | "yes" -> true
            | "n" | "no" -> false
            | "" -> default
            | _  -> loop ()
          in loop ()
        else
        let open Unix in
        prompt ();
        let buf = Bytes.create 1 in
        let rec loop () =
          let ans =
            try
              if read stdin buf 0 1 = 0 then raise End_of_file
              else Some (Char.lowercase_ascii (Bytes.get buf 0))
            with
            | Unix.Unix_error (Unix.EINTR,_,_) -> None
            | Unix.Unix_error _ -> raise End_of_file
          in
          match ans with
          | Some 'y' -> print_endline (Bytes.to_string buf); true
          | Some 'n' -> print_endline (Bytes.to_string buf); false
          | Some '\n' -> print_endline (if default then "y" else "n"); default
          | _ -> loop ()
        in
        let attr = tcgetattr stdin in
        let reset () =
          tcsetattr stdin TCSAFLUSH attr;
          tcflush stdin TCIFLUSH;
        in
        try
          tcsetattr stdin TCSAFLUSH
            {attr with c_icanon = false; c_echo = false};
          tcflush stdin TCIFLUSH;
          let r = loop () in
          reset ();
          r
        with e -> reset (); raise e
      with
      | Unix.Unix_error _ | End_of_file ->
        msg "%s\n" (if default then "y" else "n"); default
      | Sys.Break as e -> msg "\n"; raise e
    ) fmt

let read fmt =
  Printf.ksprintf (fun s ->
      formatted_msg "%s " s;
      if OpamCoreConfig.(!r.answer = None && not !r.safe_mode) then (
        try match read_line () with
          | "" -> None
          | s  -> Some s
        with
        | End_of_file ->
          msg "\n";
          None
        | Sys.Break as e -> msg "\n"; raise e
      ) else
        None
    ) fmt

(* This allows OpamStd.Config.env to display warning messages *)
let () =
  OpamStd.Sys.(set_warning_printer {warning})
