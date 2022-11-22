(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Global configuration *)

let debug () = abs OpamCoreConfig.(!r.debug_level) > 0

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
    if Sys.win32 then
      let attempt handle =
        let (info : OpamStubs.console_font_infoex) =
          let hConsoleOutput = OpamStubs.getStdHandle handle in
          OpamStubs.getCurrentConsoleFontEx hConsoleOutput false
        in
        (*
         * The Windows Console will be able to output Unicode as long as a
         * TrueType font has been selected (Consolas or Lucida Console are
         * installed by default) and the output code page has been set to
         * CP_UTF8 (65001)
         * TMPF_TRUETYPE = 0x4 (wingdi.h)
         *)
        info.fontFamily land 0x4 <> 0 && OpamStubs.getConsoleOutputCP () = 65001
      in
      try
        attempt OpamStubs.STD_OUTPUT_HANDLE
      with Not_found ->
        try
          attempt OpamStubs.STD_INPUT_HANDLE
        with Not_found ->
          false
    else
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
     | `Extended -> not Sys.win32
     | `Always | `Never -> false
     | `Auto -> Lazy.force auto && OpamStd.Sys.(os () = Darwin))

module Symbols = struct
  let rightwards_arrow = Uchar.of_int 0x2192
  let box_drawings_light_down_and_right = Uchar.of_int 0x250c
  let box_drawings_light_horizontal = Uchar.of_int 0x2500
  let box_drawings_light_vertical = Uchar.of_int 0x2502
  let box_drawings_light_up_and_right = Uchar.of_int 0x2514
  let box_drawings_light_right = Uchar.of_int 0x2576
  let box_drawings_light_vertical_and_right = Uchar.of_int 0x251C
  let circled_division_slash = Uchar.of_int 0x2298
  let asterisk_operator = Uchar.of_int 0x2217
  let north_east_arrow = Uchar.of_int 0x2197
  let south_east_arrow = Uchar.of_int 0x2198
  let clockwise_open_circle_arrow = Uchar.of_int 0x21bb
  let greek_small_letter_lambda = Uchar.of_int 0x03bb
  let latin_capital_letter_o_with_stroke = Uchar.of_int 0x00d8
  let six_pointed_black_star = Uchar.of_int 0x2736
  let upwards_arrow = Uchar.of_int 0x2191
  let downwards_arrow = Uchar.of_int 0x2193
  let up_down_arrow = Uchar.of_int 0x2195
  let downwards_double_arrow = Uchar.of_int 0x21d3
  let black_down_pointing_triangle = Uchar.of_int 0x25bc
  let downwards_black_arrow = Uchar.of_int 0x2b07
end

type win32_glyph_checker = {
  font: string;
  checker: OpamStubs.handle * OpamStubs.handle;
  glyphs: (Uchar.t, bool) Hashtbl.t;
}
let win32_glyph_checker = ref None
let () =
  if Sys.win32 then
    let release_checker checker () =
      match checker with
      | {contents = Some {checker; _}} ->
          OpamStubs.delete_glyph_checker checker
      | _ ->
        ()
    in
    at_exit (release_checker win32_glyph_checker)

let utf8_symbol main ?(alternates=[]) s =
  if utf8 () then
    try
      let scalar =
        if Sys.win32 then
          let current_font =
            let open OpamStubs in
            try
              let stdout = getStdHandle OpamStubs.STD_OUTPUT_HANDLE in
              (getCurrentConsoleFontEx stdout false).faceName
            with Not_found ->
              let stderr = getStdHandle OpamStubs.STD_ERROR_HANDLE in
              (getCurrentConsoleFontEx stderr false).faceName
          in
          let checker =
            let new_checker =
              lazy {font = current_font;
                    checker = OpamStubs.create_glyph_checker current_font;
                    glyphs = Hashtbl.create 16}
            in
            match win32_glyph_checker with
            | {contents = Some {font; checker; _}} when font <> current_font ->
                OpamStubs.delete_glyph_checker checker;
                let checker = Lazy.force new_checker in
                win32_glyph_checker := Some checker;
                checker
            | {contents = None} ->
                let checker = Lazy.force new_checker in
                win32_glyph_checker := Some checker;
                checker
            | {contents = Some checker} ->
                checker
          in
          let check_glyph scalar =
            try
              Hashtbl.find checker.glyphs scalar
            with Not_found ->
              let has_glyph = OpamStubs.has_glyph checker.checker scalar in
              Hashtbl.add checker.glyphs scalar has_glyph;
              has_glyph
          in
          List.find check_glyph (main::alternates)
        else
          main
      in
      let b = Buffer.create 4 in
      OpamCompat.Buffer.add_utf_8_uchar b scalar;
      Buffer.contents b
    with Failure _
       | Not_found ->
      s
  else
    s

let timer () =
  if OpamCoreConfig.(!r.debug_level) > 0 then
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
    Printf.sprintf "\027[%sm%s\027[0m" (style_code style) s

let colorise' styles s =
  if not (color ()) then s else
    Printf.sprintf "\027[%sm%s\027[0m"
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
let acolor_w width c f s =
  Format.pp_print_string f (acolor_with_width (Some width) c () s)

type win32_color_mode = Shim | VT100 of (unit -> unit)

type _ shim_return =
 | Handle : (OpamStubs.stdhandle * win32_color_mode) shim_return
 | Mode   : win32_color_mode shim_return
 | Peek   : bool shim_return

let force_win32_vt100 handle () =
  try
    let hConsoleOutput = OpamStubs.getStdHandle handle in
    let mode = OpamStubs.getConsoleMode hConsoleOutput in
    (* ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x4 *)
    let vt100_on = 0x4 in
    if mode land vt100_on = 0 then
      OpamStubs.setConsoleMode hConsoleOutput (mode lor vt100_on) |> ignore
  with Not_found -> ()

let enable_win32_vt100 ch =
  try
    let hConsoleOutput = OpamStubs.getStdHandle ch in
    let mode = OpamStubs.getConsoleMode hConsoleOutput in
    (* ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x4 *)
    let vt100_on = 0x4 in
    if mode land vt100_on <> 0 then
      (ch, VT100(force_win32_vt100 ch))
    else
      if OpamStubs.setConsoleMode hConsoleOutput (mode lor vt100_on) then begin
        let restore_console () =
          try
            let hConsoleOutput = OpamStubs.getStdHandle ch in
            let mode =
              OpamStubs.getConsoleMode hConsoleOutput land (lnot vt100_on)
            in
            OpamStubs.setConsoleMode hConsoleOutput mode |> ignore
          with Not_found -> ()
        in
        at_exit restore_console;
        (ch, VT100(force_win32_vt100 ch))
      end else
        (ch, Shim)
  with Not_found ->
    (ch, VT100 ignore)

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
        if Lazy.is_val ch then
          match Lazy.force ch with
          | (_, Shim) -> false
          | (_, VT100 force) -> force (); true
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
  if get_win32_console_shim ch Peek then
    Printf.fprintf ocaml_ch "%s%!" msg
  else
    let (ch, mode) = get_win32_console_shim ch Handle in
    match mode with
    | VT100 force ->
      force ();
      output_string ocaml_ch msg;
      flush ocaml_ch
    | Shim ->
      let hConsoleOutput = OpamStubs.getStdHandle ch in
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
            | "0"
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
  match get_win32_console_shim `stdout Handle with
  | (ch, Shim) ->
      let hConsoleOutput = OpamStubs.getStdHandle ch in
      let {OpamStubs.size = (w, _); cursorPosition = (_, row); _} =
        OpamStubs.getConsoleScreenBufferInfo hConsoleOutput in
      Printf.printf "\r%!";
      OpamStubs.fillConsoleOutputCharacter hConsoleOutput '\000' w (0, row)
        |> ignore
  | (_, VT100 force) ->
      force ();
      carriage_delete_unix ()

let carriage_delete =
  if Sys.win32 then
    let carriage_delete = lazy (
      match get_win32_console_shim `stdout Mode with
      | Shim ->
          carriage_delete_windows
      | VT100 force ->
          fun () ->
            force ();
            carriage_delete_unix ())
    in
    fun () -> Lazy.force carriage_delete ()
  else
    carriage_delete_unix

let rollback_terminal nlines =
  for _ = 1 to nlines do
    carriage_delete ();
    Printf.printf "\027[A"
  done

let left_1_char =
  let left_1_char_unix () = Printf.printf "\027[D%!" in
  if Sys.win32 then
    let f = lazy (
      match get_win32_console_shim `stdout Mode with
      | Shim -> fun () -> () (* unimplemented *)
      | VT100 force -> fun () -> force (); left_1_char_unix ()
    ) in
    fun () -> Lazy.force f ()
  else left_1_char_unix

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
      | VT100 force ->
          fun () ->
            force ();
            clear_status_unix ())
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
      (* win32_print_message *always* flushes *)
      Printf.ksprintf (win32_print_message ch) fmt
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
  Printf.ksprintf (colorise `blue) "%02.0f:%06.3f"
    (time /. 60.)
    (mod_float time 60.)

let log_formatter, finalise_output =
  if Sys.win32 then
    let b = Buffer.create 128 in
    let f _ =
      win32_print_message `stderr (Buffer.contents b);
      Buffer.clear b
    in
      Format.formatter_of_buffer b, f
  else
    Format.formatter_of_out_channel stderr, ignore

let () =
  Format.pp_set_margin log_formatter 0

let pending = Queue.create ()

let clear_pending debug_level =
  let f (level, timestamp, msg) =
    if level <= abs debug_level then
      let timestamp = if debug_level < 0 then "" else timestamp in
      Format.kfprintf finalise_output log_formatter "%s%s%!" timestamp msg
  in
  Queue.iter f pending;
  Queue.clear pending

let log section ?(level=1) fmt =
  let debug_level =
    let debug_level = OpamCoreConfig.(!r.debug_level) in
    let sections = OpamCoreConfig.(!r.debug_sections) in
    if OpamStd.String.Map.is_empty sections then
      debug_level
    else
      match OpamStd.String.Map.find section sections with
      | Some level -> level
      | None -> debug_level
      | exception Not_found -> 0
  in
  if not OpamCoreConfig.(!r.set) then
    let b = Buffer.create 128 in
    let timestamp = timestamp () ^ "  " in
    let k _ = Queue.push (level, timestamp, Buffer.contents b) pending in
    Format.kfprintf k (Format.formatter_of_buffer b) ("%a  " ^^ fmt ^^ "\n%!")
      (acolor_w 30 `yellow) section
  else if level <= abs debug_level then
    let () = clear_status () in
    let () = clear_pending debug_level in
    let timestamp = if debug_level < 0 then "" else timestamp () ^ "  " in
    Format.kfprintf finalise_output log_formatter ("%s%a  " ^^ fmt ^^ "\n%!")
      timestamp (acolor_w 30 `yellow) section
  else
    Format.ifprintf Format.err_formatter fmt

(* Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
   rather than [log "%s" (to_string x)] to avoid costly unneeded
   stringifications *)
let slog to_string f x = Format.pp_print_string f (to_string x)

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

let write_status_unix print_string fmt =
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
      (true, (fun s -> win32_print_message `stdout (s ^ "\n")), print_string)
  | VT100 force ->
      (false, (fun s -> force (); print_endline s), (fun s -> force (); print_string s)))

let status_line fmt =
  let batch =
    debug () || not (disp_status_line ()) in
  let (use_shim, print_msg, print_string) =
    if Sys.win32 then
      Lazy.force win32_print_functions
    else
      (false, print_endline, print_string)
  in
  if batch then
    Printf.ksprintf
      (fun s -> if s <> !last_status then (last_status := s; print_msg s))
      fmt
  else
    if use_shim then
      write_status_windows fmt
    else
      write_status_unix print_string fmt

let header_width () = min 80 (OpamStd.Sys.terminal_columns ())

let header_msg fmt =
  let utf8camel = "\xF0\x9F\x90\xAB " in (* UTF-8 <U+1F42B, U+0020> *)
  let padding = "<><><><><><><><><><><><><><><><><><><><>\
                 <><><><><><><><><><><><><><><><><><><><><>" in
  Printf.ksprintf (fun str ->
      let wpad = header_width () - String.length str - 2 in
      let wpadl = 4 in
      let wpadr = wpad - wpadl - if utf8_extended () then 4 else 0 in
      print_message `stdout "\n%s %s%s%s\n"
        (colorise `cyan (String.sub padding 0 wpadl))
        (colorise `bold str)
        (if wpadr > 0 then
           let padding =
             String.sub padding (String.length padding - wpadr) wpadr
           in
           " " ^ colorise `cyan padding
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

(* Reads a single char from the user when possible, a line otherwise *)
let short_user_input ~prompt ?default ?on_eof f =
  let on_eof = OpamStd.Option.Op.(on_eof ++ default) in
  let prompt () = match default with
    | Some x when OpamStd.Sys.tty_out ->
      msg "%s%s" prompt x;
      left_1_char ();
      carriage_delete ();
      (match List.rev (OpamStd.String.split prompt '\n') with
       | lastline::_ -> print_string lastline
       | [] -> ())
    | _ ->
      print_string prompt; flush stdout
  in
  try
    if OpamStd.Sys.(not tty_out || os () = Win32 || os () = Cygwin) then
      let rec loop () =
        prompt ();
        let input = match String.lowercase_ascii (read_line ()) with
          | "" -> default
          | s -> Some s
        in
        match OpamStd.Option.Op.(input >>= f) with
        | Some a -> a
        | None -> loop ()
      in
      loop ()
    else
    let open Unix in
    prompt ();
    let buf = Bytes.create 3 in
    let rec loop () =
      let input =
        match
          (* Some keystrokes, e.g. arrows, can return 3 chars *)
          let nr = read stdin buf 0 3 in
          if nr < 1 then raise End_of_file
          else String.uncapitalize_ascii (Bytes.sub_string buf 0 nr)
        with
        | "\n" -> default
        | s -> Some s
        | exception Unix.Unix_error (Unix.EINTR,_,_) -> None
        | exception Unix.Unix_error _ -> raise End_of_file
      in
      match input with
      | None -> loop ()
      | Some i -> match f i with
        | Some a ->
          if String.length i > 0 && i.[0] <> '\027' then print_endline i;
          a
        | None -> loop ()
    in
    let attr = tcgetattr stdin in
    let reset () =
      tcsetattr stdin TCSAFLUSH attr;
      tcflush stdin TCIFLUSH;
    in
    OpamStd.Exn.finally reset @@ fun () ->
    tcsetattr stdin TCSAFLUSH
      {attr with c_icanon = false; c_echo = false};
    tcflush stdin TCIFLUSH;
    loop ()
  with
  | Sys.Break as e -> OpamStd.Exn.finalise e (fun () -> msg "\n")
  | Unix.Unix_error _ | End_of_file ->
    match on_eof with
    | None -> OpamStd.Exn.finalise End_of_file (fun () -> msg "\n")
    | Some d ->
      msg "%s\n" d;
      match f d with
      | Some a -> a
      | None -> assert false

let pause fmt =
  if OpamStd.Sys.tty_in then
    Printf.ksprintf (fun s ->
        let prompt = OpamStd.Format.reformat s in
        short_user_input ~prompt ~default:""
        (function
        | "\027" -> OpamStd.Sys.exit_because `Aborted
        | _ -> Some ()))
      fmt
  else
    Printf.ifprintf () fmt

let confirm ?(require_unsafe_yes=false) ?(default=true) fmt =
  Printf.ksprintf (fun s ->
      if OpamCoreConfig.(!r.safe_mode) then false else
      let prompt =
        Printf.ksprintf OpamStd.Format.reformat "%s [%s/%s] " s
          (colorise' (`blue :: if default then [`underline] else []) "y")
          (colorise' (`blue :: if default then [] else [`underline]) "n")
      in
      if OpamCoreConfig.answer_is `unsafe_yes ||
         not require_unsafe_yes && OpamCoreConfig.answer_is_yes ()
      then
        (formatted_msg "%sy\n" prompt; true)
      else if OpamCoreConfig.answer_is `all_no ||
              OpamStd.Sys.(not tty_in)
      then
        (formatted_msg "%sn\n" prompt; false)
      else
        short_user_input ~prompt ~default:(if default then "y" else "n")
        (function
        | "y" | "yes" -> Some true
        | "n" | "no" -> Some false
        | "\027" -> Some false (* echap *)
        | _  -> None))
    fmt

let read fmt =
  Printf.ksprintf (fun s ->
      formatted_msg "%s " s;
      if OpamCoreConfig.(answer_is `ask && not !r.safe_mode) then (
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
  let cleanup_trailing sl =
    let rec clean acc = function
      | s::r ->
        let s' = OpamStd.String.strip_right s in
        if s' = "" then clean acc r else List.rev_append r (s'::acc)
      | [] -> acc
    in
    clean [] (List.rev sl)
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
          | last::r -> List.rev (OpamStd.String.strip_right last :: r) :: acc
        in
        function
        | [] -> List.rev (append cur)
        | cell::rest ->
          let multiline = String.contains cell '\n' in
          let cell_lines = OpamStd.String.split cell '\n' in
          let cell_width =
            List.fold_left max 0 (List.map visual_length cell_lines)
          in
          let text_width =
            List.fold_left max 0
              (List.map (fun s -> visual_length (OpamStd.String.strip_right s))
                 cell_lines)
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
          if start_col + sep_len + text_width <= width then
            if multiline then
              let cell =
                indent ~sep:true start_col (OpamStd.String.strip_right cell)
              in
              split_at_overflows margin (append (cell::cur)) [] rest
            else
              split_at_overflows end_col acc (cell::cur) rest
          else if rest = [] && acc = [] && not multiline &&
                  width - start_col - max_sep_len >= min_reformat_width
          then
            let cell =
              OpamStd.String.strip_right cell |> fun cell ->
              reformat ~width:(width - start_col - max_sep_len) cell |>
              indent ~sep:true start_col
            in
            split_at_overflows margin acc (cell::cur) []
          else if multiline || margin + cell_width >= width then
            let cell =
              OpamStd.String.strip_right cell |> fun cell ->
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
  List.iter (fun l -> print_line (cleanup_trailing l)) table

let menu ?default ?unsafe_yes ?yes ~no ~options fmt =
  assert (List.length options < 10);
  let options_nums =
    List.mapi (fun n (ans, _) -> ans, string_of_int (n+1)) options
  in
  let nums_options = List.map (fun (a, n) -> n, a) options_nums in
  let rec prev_option a0 = function
    | (a,_)::[] -> a
    | (a,_)::((a1,_)::_ as r) -> if a1 = a0 then a else prev_option a0 r
    | [] -> assert false
  in
  let rec menu default =
    let text =
      OpamStd.List.concat_map "" ~right:"\n" (fun (ans, n) ->
          Printf.ksprintf (OpamStd.Format.reformat ~indent:5) "%s %s. %s\n"
            (if ans = default then ">" else " ")
            (colorise `blue n)
            (List.assoc ans options))
        options_nums
    in
    let prompt =
      Printf.sprintf "[%s] "
        (OpamStd.List.concat_map "/" (fun (n, a) ->
             colorise'
               (`blue :: if a = default then [`underline] else [])
               n)
            nums_options)
    in
    let nlines = List.length Re.(all (compile (char '\n')) text) in
    msg "%s" text;
    let select a =
      msg "%s\n" (List.assoc a options_nums); a
    in
    let default_s = List.assoc default options_nums in
    let no_s = List.assoc no options_nums in
    if OpamCoreConfig.(!r.safe_mode) then no else
    match OpamCoreConfig.answer(), unsafe_yes, yes with
    | `unsafe_yes, Some a, _ -> print_string prompt; select a
    | #OpamStd.Config.yes_answer, _, Some a -> print_string prompt; select a
    | `all_no, _, _ -> print_string prompt; select no
    | _, _, _ ->
      let default_ref = ref default in
      let change_selection =
        fun opt ->
          rollback_terminal nlines; (* restore cursor pos *)
          default_ref := opt;
          raise Exit
      in
      try
        short_user_input ~prompt ~default:default_s ~on_eof:no_s @@ function
        | "" -> None
        | "\027" -> Some (select no) (* echap *)
        | "\027[A" (* up *) | "\027[D" (* left *) ->
          change_selection (prev_option default options)
        | "\027[B" (* down *) | "\027[C" (* right *) ->
          change_selection (prev_option default (List.rev options))
        | i -> OpamStd.List.assoc_opt i nums_options
      with Exit -> menu !default_ref
  in
  Printf.ksprintf (fun prompt_msg ->
      formatted_msg "%s\n" prompt_msg;
      let default = OpamStd.Option.default (fst (List.hd options)) default in
      menu default
    ) fmt


(** Tree printing *)
module Tree = struct

  type 'elt t = {
    value: 'elt;
    children: 'elt t list
  }

  let create ?(children=[]) value = { value; children }

  type symbols = {
    vert: string; (** |  *)
    hor:  string; (** -- *)
    tee:  string; (** |- *)
    hook: string; (** '- *)
  }

  let get_default_symbols () =
    let vert = utf8_symbol Symbols.box_drawings_light_vertical "|" in
    let hor  = utf8_symbol Symbols.box_drawings_light_horizontal "-" in
    let tee  = utf8_symbol Symbols.box_drawings_light_vertical_and_right "|-" in
    let hook = utf8_symbol Symbols.box_drawings_light_up_and_right "'-" in
    { vert; hor; tee; hook }

  let repeat count s =
    let rec aux count acc =
      if count < 1 then acc else aux (count -1) (acc ^ s)
    in
    aux count ""
  let spaces count = String.init count (fun _ -> ' ')
  let utf8_length s = Uutf.String.fold_utf_8 (fun state _ _ -> state+1) 0 s

  let to_parts { vert; hor; tee; hook } =
    let indent =
      vert ^ spaces (4 - utf8_length vert)
    in
    let child =
      let len = utf8_length tee in
      tee ^ repeat (3 - len) hor ^ " "
    in
    let child_end =
      let len = utf8_length hook in
      hook ^ repeat (3 - len) hor ^ " "
    in
    (indent, child, child_end)

  let print ?symbols ~printer { value; children } =
    let symbols =
      match symbols with
      | Some x -> x
      | None -> get_default_symbols ()
    in
    let indent, child, child_end = to_parts symbols in
    let blank = spaces 4 in
    let buff = Buffer.create 512 in
    let rec go indents (is_end: bool) { value; children } =
      let new_indents () =
        if is_end then indents ^ blank
        else indents ^ indent
      in
      match OpamStd.String.split (printer value) '\n' with
      | [] -> go_children (new_indents ()) children
      | first :: rest ->
        Buffer.add_string buff indents;
        Buffer.add_string buff (if is_end then child_end else child);
        Buffer.add_string buff first;
        Buffer.add_char buff '\n';
        let indents = new_indents () in
        List.iter (fun line ->
            Buffer.add_string buff indents;
            Buffer.add_string buff line;
            Buffer.add_char buff '\n';
          ) rest;
        go_children indents children
    and go_children indents children =
      let child_end = List.length children - 1 in
      List.iteri (fun i child ->
          go indents (OpamCompat.Int.equal i child_end) child)
        children
    in
    Buffer.add_string buff (printer value);
    Buffer.add_char buff '\n';
    go_children "" children;
    msg "%s" (Buffer.contents buff)

end

(* This allows OpamStd.Config.env to display warning messages *)
let () =
  OpamStd.Sys.(set_warning_printer {warning})
