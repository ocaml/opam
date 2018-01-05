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
  try
    OpamStd.Env.get "TERM" = "dumb"
  with Not_found ->
    not Sys.win32
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

type win32_color_mode = Shim | VT100

type _ shim_return =
 | Handle : (OpamStubs.handle * win32_color_mode) shim_return
 | Mode   : win32_color_mode shim_return
 | Peek   : (win32_color_mode -> bool) shim_return

let enable_win32_vt100 ch =
  let hConsoleOutput =
    OpamStubs.getStdHandle ch
  in
  try
    let mode = OpamStubs.getConsoleMode hConsoleOutput in
    (* ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x4 *)
    let vt100_on = 0x4 in
    if mode land vt100_on <> 0 then
      (hConsoleOutput, VT100)
    else
      if OpamStubs.setConsoleMode hConsoleOutput (mode lor vt100_on) then begin
        let restore_console () =
          let mode =
            OpamStubs.getConsoleMode hConsoleOutput land (lnot vt100_on)
          in
          OpamStubs.setConsoleMode hConsoleOutput mode |> ignore
        in
        at_exit restore_console;
        (hConsoleOutput, VT100)
      end else
        (hConsoleOutput, Shim)
  with Not_found ->
    (hConsoleOutput, VT100)

let stdout_state = lazy (enable_win32_vt100 OpamStubs.STD_OUTPUT_HANDLE)
let stderr_state = lazy (enable_win32_vt100 OpamStubs.STD_ERROR_HANDLE)

let get_win32_console_shim :
  type s . [ `stdout | `stderr ] -> s shim_return -> s = fun ch ->
    let ch = if ch = `stdout then stdout_state else stderr_state in
    function
    | Handle ->
        Lazy.force ch
    | Mode ->
        Lazy.force ch |> snd
    | Peek ->
        fun mode ->
          if Lazy.is_val ch then
            snd (Lazy.force ch) = mode
          else
            false

(*
 * Layout of attributes (wincon.h)
 *
 * Bit 0 - Blue --\
 * Bit 1 - Green   } Foreground
 * Bit 2 - Red    /
 * Bit 3 - Bold -/
 * Bit 4 - Blue --\
 * Bit 5 - Green   } Background
 * Bit 6 - Red    /
 * Bit 7 - Bold -/
 * Bit 8 - Leading Byte
 * Bit 9 - Trailing Byte
 * Bit a - Top horizontal
 * Bit b - Left vertical
 * Bit c - Right vertical
 * Bit d - unused
 * Bit e - Reverse video
 * Bit f - Underscore
 *)

let is_windows_10 =
  lazy (let (v, _, _, _) = OpamStubs.getWindowsVersion () in v >= 10)

let win32_print_message ch msg =
  let ocaml_ch =
    match ch with
    | `stdout -> stdout
    | `stderr -> stderr
  in
  if get_win32_console_shim ch Peek VT100 then
    Printf.fprintf ocaml_ch "%s%!" msg
  else
    let (hConsoleOutput, mode) = get_win32_console_shim ch Handle in
    if mode = VT100 then begin
      output_string ocaml_ch msg;
      flush ocaml_ch
    end else
      let {OpamStubs.attributes; _} =
        OpamStubs.getConsoleScreenBufferInfo hConsoleOutput
      in
      let background = (attributes land 0b1110000) lsr 4 in
      let length = String.length msg in
      let execute_code =
        let color = ref (attributes land 0b1111) in
        let blend ?(inheritbold = true) bits =
          let bits =
            if inheritbold then
              (!color land 0b1000) lor (bits land 0b111)
            else
              bits in
          let result =
            (attributes land (lnot 0b1111))
            lor (bits land 0b1000)
            lor ((bits land 0b111) lxor background)
          in
          color := (result land 0b1111);
          result in
        fun code ->
          let l = String.length code in
          assert (l > 0 && code.[0] = '[');
          let attributes =
            OpamStd.String.split (String.sub code 1 (l - 1)) ';'
          in
          let attributes = if attributes = [] then [""] else attributes in
          let f attributes attribute =
            match attribute with
            | "1"
            | "01" ->
                blend ~inheritbold:false (!color lor 0b1000)
            | "4"
            | "04" ->
                if Lazy.force is_windows_10 then
                  attributes lor 0b1000000000000000
                else
                  (* Don't have underline, so change the background *)
                  (attributes land (lnot 0b11111111)) lor 0b01110000
            | "30" ->
                blend 0b000
            | "31" ->
                blend 0b100
            | "32" ->
                blend 0b010
            | "33" ->
                blend 0b110
            | "34" ->
                blend ~inheritbold:false 0b001
            | "35" ->
                blend 0b101
            | "36" ->
                blend 0b011
            | "37" ->
                blend 0b111
            | "" ->
                blend ~inheritbold:false 0b0111
            | _ -> assert false
          in
          let attrs = (List.fold_left f (blend !color) attributes) in
          OpamStubs.setConsoleTextAttribute hConsoleOutput attrs
      in
      let rec f index start in_code =
        if index < length
        then let c = msg.[index] in
             if c = '\027' then begin
               assert (not in_code);
               let fragment = String.sub msg start (index - start) in
               let index = succ index in
               if fragment <> "" then
                 Printf.fprintf ocaml_ch "%s%!" fragment;
               f index index true end
             else
               if in_code && c = 'm' then
                 let fragment = String.sub msg start (index - start) in
                 let index = succ index in
                 execute_code fragment;
                 f index index false
               else
                 f (succ index) start in_code
        else let fragment = String.sub msg start (index - start) in
             if fragment <> "" then
               if in_code then
                 execute_code fragment
               else
                 Printf.fprintf ocaml_ch "%s%!" fragment
             else
               flush ocaml_ch
      in
      flush ocaml_ch;
      f 0 0 false

let carriage_delete_unix _ =
  print_string "\r\027[K"

let carriage_delete_windows () =
  let (hConsoleOutput, mode) = get_win32_console_shim `stdout Handle in
  match mode with
  | Shim ->
      let {OpamStubs.size = (w, _); cursorPosition = (_, row); _} =
        OpamStubs.getConsoleScreenBufferInfo hConsoleOutput in
      Printf.printf "\r%!";
      OpamStubs.fillConsoleOutputCharacter hConsoleOutput '\000' w (0, row)
        |> ignore
  | VT100 ->
      carriage_delete_unix ()

let carriage_delete =
  if Sys.win32 then
    let carriage_delete = lazy (
      match get_win32_console_shim `stdout Mode with
      | Shim ->
          carriage_delete_windows
      | VT100 ->
          carriage_delete_unix)
    in
    fun () -> Lazy.force carriage_delete ()
  else
    carriage_delete_unix

let displaying_status = ref false

let clear_status_unix () =
  if !displaying_status then begin
    flush stdout;
    displaying_status := false
  end

let clear_status =
  if Sys.win32 then
    let clear_status = lazy (
      match get_win32_console_shim `stdout Mode with
      | Shim ->
          fun () ->
            carriage_delete_windows ();
            displaying_status := false
      | VT100 ->
          clear_status_unix)
    in
    fun () ->
      Lazy.force clear_status ()
  else
    clear_status_unix

let print_message =
  if Sys.win32 then
    fun ch fmt ->
      flush (if ch = `stdout then stderr else stdout);
      clear_status ();
      Printf.ksprintf (win32_print_message ch) (fmt ^^ "%!")
  else
    fun ch fmt ->
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
    let () = clear_status () in
    if Sys.win32 then begin
      (*
       * In order not to break [slog], split the output into two. A side-effect
       * of this is that logging lines may not use colour.
       *)
      win32_print_message `stderr (Printf.sprintf "%s  %a  "
        (timestamp ()) (acolor_with_width (Some 30) `yellow) section);
      Printf.fprintf stderr (fmt ^^ "\n%!") end
    else
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

let last_status = ref ""

let write_status_unix fmt =
  let print_string s =
    print_string s;
    flush stdout;
    carriage_delete_unix ();
    displaying_status := true
  in
  Printf.ksprintf print_string ("\r\027[K" ^^ fmt)

let write_status_windows fmt =
  let print_string s =
    carriage_delete ();
    win32_print_message `stdout s;
    displaying_status := true
  in
  Printf.ksprintf print_string fmt

let win32_print_functions = lazy (
  match get_win32_console_shim `stdout Mode with
  | Shim ->
      (true, (fun s -> win32_print_message `stdout (s ^ "\n")))
  | VT100 ->
      (false, print_endline))

let status_line fmt =
  let batch =
    debug () || not (disp_status_line ()) in
  let (use_shim, print_msg) =
    if Sys.win32 then
      Lazy.force win32_print_functions
    else
      (false, print_endline)
  in
  if batch then
    Printf.ksprintf
      (fun s -> if s <> !last_status then (last_status := s; print_msg s))
      fmt
  else
    if use_shim then
      write_status_windows fmt
    else
      write_status_unix fmt

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
