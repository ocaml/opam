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

let acolor_with_width width c () s =
  let str = colorise c s in
  str ^
  match width with
  | None   -> ""
  | Some w ->
    if String.length str >= w then ""
    else String.make (w-String.length str) ' '

let acolor c () = colorise c
let acolor_w width c oc s =
  output_string oc (acolor_with_width (Some width) c () s)

let print_message ch fmt =
  let output_string =
    let output_string ch s =
      output_string ch s;
      flush ch
    in
    match ch with
    | `stdout -> flush stderr; output_string stdout
    | `stderr -> flush stdout; output_string stderr
  in
  Printf.ksprintf output_string fmt

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
    print_message `stderr "%a %s\n" (acolor `red) "[ERROR]"
      (OpamStd.Format.reformat ~start_column:8 ~indent:8 str)
  ) fmt

let warning fmt =
  Printf.ksprintf (fun str ->
    print_message `stderr "%a %s\n" (acolor `yellow) "[WARNING]"
      (OpamStd.Format.reformat ~start_column:10 ~indent:10 str)
  ) fmt

let note fmt =
  Printf.ksprintf (fun str ->
    print_message `stderr "%a %s\n" (acolor `blue) "[NOTE]"
      (OpamStd.Format.reformat ~start_column:7 ~indent:7 str)
  ) fmt

let errmsg fmt = print_message `stderr fmt

let error_and_exit reason fmt =
  Printf.ksprintf (fun str ->
    error "%s" str;
    OpamStd.Sys.exit_because reason
  ) fmt

let msg fmt = print_message `stdout fmt

let formatted_msg ?indent fmt =
  Printf.ksprintf
    (fun s -> print_message `stdout "%s" (OpamStd.Format.reformat ?indent s))
    fmt

let carriage_delete () =
  print_string "\r\027[K"

let last_status = ref ""
let status_line fmt =
  if debug () || not (disp_status_line ()) then
    Printf.ksprintf
      (fun s -> if s <> !last_status then (last_status := s; print_endline s))
      fmt
  else
    let print_string s =
      print_string s;
      flush stdout;
      carriage_delete ()
    in
    Printf.ksprintf print_string ("\r\027[K" ^^ fmt)

let header_width () = min 80 (OpamStd.Sys.terminal_columns ())

let header_msg fmt =
  let utf8camel = "\xF0\x9F\x90\xAB " in (* UTF-8 <U+1F42B, U+0020> *)
  let padding = "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\
                 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" in
  Printf.ksprintf (fun str ->
      let wpad = header_width () - String.length str - 2 in
      let wpadl = 4 in
      let wpadr = wpad - wpadl - if utf8_extended () then 4 else 0 in
      print_message `stdout "\n%s %s %s%s\n"
        (colorise `cyan (String.sub padding 0 wpadl))
        (colorise `bold str)
        (if wpadr > 0 then
           let padding =
             String.sub padding (String.length padding - wpadr) wpadr
           in
           colorise `cyan padding
         else
           "")
        (if wpadr >= 0 && utf8_extended () then
           "  " ^ (colorise `yellow utf8camel)
         else
           "");
    ) fmt

let header_error fmt =
  let padding = "#=======================================\
                 ========================================#" in
  Printf.ksprintf (fun head fmt ->
      Printf.ksprintf (fun contents ->
          let wpad = header_width () - String.length head - 8 in
          let wpadl = 4 in
          let wpadr = wpad - wpadl in
          print_message `stderr "\n%s %s %s %s\n%s\n"
            (colorise `red (String.sub padding 0 wpadl))
            (colorise `bold "ERROR")
            (colorise `bold head)
            (if wpadr > 0 then
               let padding =
                 String.sub padding (String.length padding - wpadr) wpadr
               in
               colorise `red padding
             else
               "")
            contents
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

let print_table ?cut oc ~sep table =
  let open OpamStd.Format in
  let cut =
    match cut with
    | None -> if oc = stdout || oc = stderr then `Wrap "" else `None
    | Some c -> c
  in
  let output_string s =
    if oc = stdout then
      msg "%s\n" s
    else if oc = stderr then
      errmsg "%s\n" s
    else begin
      output_string oc s;
      output_char oc '\n'
    end
  in
  let replace_newlines by =
    Re.(replace_string (compile (char '\n')) ~by)
  in
  let print_line l = match cut with
    | `None ->
      let s = List.map (replace_newlines "\\n") l |> String.concat sep in
      output_string s;
    | `Truncate ->
      let s = List.map (replace_newlines " ") l |> String.concat sep in
      output_string (cut_at_visual s (OpamStd.Sys.terminal_columns ()));
    | `Wrap wrap_sep ->
      let width = OpamStd.Sys.terminal_columns () in
      let base_indent = 10 in
      let sep_len = visual_length sep in
      let wrap_sep_len = visual_length wrap_sep in
      let max_sep_len = max sep_len wrap_sep_len in
      let indent_string =
        String.make (max 0 (base_indent - wrap_sep_len)) ' ' ^ wrap_sep
      in
      let margin = visual_length indent_string in
      let min_reformat_width = 30 in
      let rec split_at_overflows start_col acc cur =
        let append = function
          | [] -> acc
          | last::r -> List.rev (OpamStd.String.strip last :: r) :: acc
        in
        function
        | [] -> List.rev (append cur)
        | cell::rest ->
          let multiline = String.contains cell '\n' in
          let cell_width =
            List.fold_left max 0
              (List.map visual_length (OpamStd.String.split cell '\n'))
          in
          let end_col = start_col + sep_len + cell_width in
          let indent ~sep n cell =
            let spc =
              if sep then
                String.make (max 0 (if sep then n - wrap_sep_len else n)) ' ' ^ wrap_sep
              else String.make n ' '
            in
            OpamStd.List.concat_map ("\n"^spc)
              OpamStd.String.strip_right
              (OpamStd.String.split cell '\n')
          in
          if end_col < width then
            if multiline then
              let cell =
                indent ~sep:true start_col (OpamStd.String.strip cell)
              in
              split_at_overflows margin (append (cell::cur)) [] rest
            else
              split_at_overflows end_col acc (cell::cur) rest
          else if rest = [] && acc = [] && not multiline &&
                  width - start_col - max_sep_len >= min_reformat_width
          then
            let cell =
              OpamStd.String.strip cell |> fun cell ->
              reformat ~width:(width - start_col - max_sep_len) cell |>
              indent ~sep:true start_col
            in
            split_at_overflows margin acc (cell::cur) []
          else if multiline || margin + cell_width >= width then
            let cell =
              OpamStd.String.strip cell |> fun cell ->
              reformat ~width:(width - margin) cell |> fun cell ->
              OpamStd.String.split cell '\n' |>
              OpamStd.List.concat_map ("\n" ^ indent_string)
                                      OpamStd.String.strip_right
            in
            split_at_overflows margin ([cell]::append cur) [] rest
          else
            split_at_overflows (margin + cell_width) (append cur) [cell] rest
      in
      let splits = split_at_overflows 0 [] [] l in
      let str =
        OpamStd.List.concat_map
          ("\n" ^ String.make base_indent ' ')
          (String.concat sep)
          splits
      in
      output_string str;
  in
  List.iter print_line table

(* This allows OpamStd.Config.env to display warning messages *)
let () =
  OpamStd.Sys.(set_warning_printer {warning})
