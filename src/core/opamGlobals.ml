(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(* Convention:
   all the global OPAM variables can be set using environment variables
   using OPAM<variable> *)

open OpamCompat

let check ?(warn=true) var = ref (
    try
      match String.lowercase (OpamMisc.getenv ("OPAM"^var)) with
      | "" | "0" | "no" | "false" -> false
      | "1" | "yes" | "true" -> true
      | v ->
        if warn then
          Printf.eprintf "[WARNING] Invalid value %S for env variable OPAM%s, \
                          assumed true.\n" v var;
        true
    with Not_found -> false
  )

let when_var v =
    try (match OpamMisc.getenv ("OPAM"^v) with
      | "always" -> `Always
      | "never" -> `Never
      | _ -> `Auto
      )
    with
      | Not_found -> `Auto

let locale_utf8 () =
  let checkv v =
    try OpamMisc.ends_with ~suffix:"UTF-8" (OpamMisc.getenv v)
    with Not_found -> false in
  checkv "LANG" || checkv "LC_ALL"

let dumb_term =
  try OpamMisc.getenv "TERM" = "dumb" with Not_found -> true

let debug            = check ~warn:false "DEBUG"
let debug_level      =
  try ref (int_of_string (OpamMisc.getenv ("OPAMDEBUG")))
  with Not_found | Failure _ -> ref 1
let _ = if !debug_level > 1 then debug := true
let verbose          = check "VERBOSE"
let color_when       = when_var "COLOR"
let color            =
  ref (color_when = `Always ||
       color_when = `Auto && OpamMisc.tty_out && not dumb_term)
let disp_status_line_when = when_var "STATUSLINE"
let disp_status_line () =
  disp_status_line_when = `Always ||
  disp_status_line_when = `Auto &&
  OpamMisc.tty_out && (!color || not dumb_term)
let keep_build_dir   = check "KEEPBUILDDIR"
let no_base_packages = check "NOBASEPACKAGES"
let no_checksums     = check "NOCHECKSUMS"
let req_checksums    = check "REQUIRECHECKSUMS"
let yes              = check "YES"
let no               = check "NO"
let strict           = check "STRICT"
let build_test       = check "BUILDTEST"
let build_doc        = check "BUILDDOC"
let show             = check "SHOW"
let dryrun           = check "DRYRUN"
let fake             = check "FAKE"
let print_stats      = check "STATS"
let utf8_msgs        = check "UTF8MSGS"
let utf8_when        = when_var "UTF8"
let utf8             =
  ref (utf8_when = `Always || utf8_when = `Auto && locale_utf8 () || !utf8_msgs)
let autoremove       = check "AUTOREMOVE"
let do_not_copy_files = check "DONOTCOPYFILES"
let sync_archives    = check "SYNCARCHIVES"
let no_self_upgrade  = check "NOSELFUPGRADE"
let skip_version_checks = check "SKIPVERSIONCHECKS"
let safe_mode        = check "SAFE"
let all_parens       = ref false

(* Value set when opam calls itself *)
let self_upgrade_bootstrapping_value = "bootstrapping"
let is_self_upgrade =
  try OpamMisc.getenv "OPAMNOSELFUPGRADE" = self_upgrade_bootstrapping_value
  with Not_found -> false

let curl_command = try Some (OpamMisc.getenv "OPAMCURL") with Not_found -> None

let jobs = ref (
    try Some (int_of_string (OpamMisc.getenv "OPAMJOBS"))
    with Not_found | Failure _ -> None
  )

let dl_jobs = ref (
    try Some (int_of_string (OpamMisc.getenv "OPAMDOWNLOADJOBS"))
    with Not_found | Failure _ -> None
  )

let download_retry =
  try max 1 (int_of_string (OpamMisc.getenv "OPAMRETRY"))
  with Not_found | Failure _ -> 3

let cudf_file = ref (
    try Some (OpamMisc.getenv "OPAMCUDFFILE")
    with Not_found -> None
  )

let solver_timeout =
  try float_of_string (OpamMisc.getenv "OPAMSOLVERTIMEOUT")
  with Not_found | Failure _ -> 5.


type solver_criteria = [ `Default | `Upgrade | `Fixup ]

let default_preferences = function
  | `Default -> "-count(removed),-notuptodate(request),-count(down),-notuptodate(changed),-count(changed),-notuptodate(solution)"
  | `Upgrade -> "-count(down),-count(removed),-notuptodate(solution),-count(new)"
  | `Fixup -> "-count(changed),-notuptodate(solution)"

let compat_preferences = function (* Not as good, but for older solver versions *)
  | `Default -> "-removed,-notuptodate,-changed"
  | `Upgrade -> "-removed,-notuptodate,-changed"
  | `Fixup -> "-changed,-notuptodate"

let solver_preferences =
  let get prefs var kind =
    try (kind, OpamMisc.strip (OpamMisc.getenv var)) :: prefs
    with Not_found -> prefs
  in
  let prefs = [] in
  let prefs = get prefs "OPAMCRITERIA" `Default in
  let prefs = get prefs "OPAMUPGRADECRITERIA" `Upgrade in
  let prefs = get prefs "OPAMFIXUPCRITERIA" `Fixup in
  ref prefs

let get_solver_criteria action =
  try List.assoc action !solver_preferences
  with Not_found -> compat_preferences action

let default_external_solver = "aspcud"

let external_solver = ref(
  try Some (OpamMisc.strip (OpamMisc.getenv "OPAMEXTERNALSOLVER"))
  with Not_found -> None)

let use_external_solver =
  ref (not (!(check "NOASPCUD") || !(check "USEINTERNALSOLVER") ||
            !external_solver = Some ""))

let get_external_solver () =
  OpamMisc.Option.default default_external_solver !external_solver

let default_repository_name    = "default"
let default_repository_address = "https://opam.ocaml.org"

let search_files = ref ["findlib"]

let default_build_command = [ [ "./build.sh" ] ]

let global_config = "global-config"

let system = "system"

let switch: [`Env of string
            | `Command_line of string
            | `Not_set ] ref
  = ref (
    try `Env (OpamMisc.getenv "OPAMSWITCH")
    with Not_found -> `Not_set
  )

let external_tags = ref ([] : string list)

let home =
  try OpamMisc.getenv "HOME"
  with Not_found -> Sys.getcwd ()

let default_opam_dir =
  try OpamMisc.getenv "OPAMROOT"
  with Not_found -> Filename.concat home ".opam"

let root_dir_tmp =
  Filename.concat (Filename.get_temp_dir_name ())
    ("opam-" ^ string_of_int (Unix.getpid ()))

let root_dir = ref root_dir_tmp

let timer () =
  if !debug then
    let t = Sys.time () in
    fun () -> Sys.time () -. t
  else
    fun () -> 0.

(* For forked process, we want to get the time since the beginning of
   the parent process. *)
let global_start_time =
  Unix.gettimeofday ()

type text_style =
  [ `bold
  | `underline
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white ]

(* not nestable *)
let colorise (c: text_style) s =
  if not !color then s else
    let code = match c with
      | `bold      -> "01"
      | `underline -> "04"
      | `black     -> "30"
      | `red       -> "31"
      | `green     -> "32"
      | `yellow    -> "33"
      | `blue      -> "1;34"
      | `magenta   -> "35"
      | `cyan      -> "36"
      | `white     -> "37"
    in
    Printf.sprintf "\027[%sm%s\027[m" code s

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
  if !debug && level <= !debug_level then
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
    Printf.eprintf "%a %s\n%!" (acolor `red) "[ERROR]" str
  ) fmt

let warning fmt =
  Printf.ksprintf (fun str ->
    flush stdout;
    Printf.eprintf "%a %s\n%!" (acolor `yellow) "[WARNING]" str
  ) fmt

let note fmt =
  Printf.ksprintf (fun str ->
    flush stdout;
    Printf.eprintf "%a %s\n%!" (acolor `blue) "[NOTE]" str
  ) fmt

exception Exit of int

exception Exec of string * string array * string array

exception Package_error of string

let error_and_exit fmt =
  Printf.ksprintf (fun str ->
    error "%s" str;
    raise (Exit 66)
  ) fmt

let display_messages = ref true

let msg fmt =
  if !display_messages then (
    flush stderr;
    Printf.printf (fmt ^^ "%!")
  ) else (
    Printf.ifprintf stdout fmt
  )

let formatted_msg ?indent fmt =
  if !display_messages then (
    flush stderr;
    Printf.ksprintf
      (fun s -> print_string (OpamMisc.reformat ?indent s); flush stdout)
      fmt
  ) else (
    Printf.ksprintf ignore fmt
  )


let status_line fmt =
  let carriage_delete = "\r\027[K" in
  let endline = if !debug then "\n" else carriage_delete in
  if !display_messages && disp_status_line () then (
    flush stderr;
    Printf.kfprintf
      (fun ch -> output_string ch endline (* unflushed *))
      stdout
      ("%s" ^^ fmt ^^ "%!") carriage_delete
  ) else
    Printf.ifprintf stdout fmt

let header_width () = 80

let header_msg fmt =
  let utf8camel = "\xF0\x9F\x90\xAB " in (* UTF-8 <U+1F42B, U+0020> *)
  let padding = "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\
                 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" in
  Printf.ksprintf (fun str ->
    flush stderr;
    if !display_messages then (
      print_char '\n';
      let wpad = header_width () - String.length str - 2 in
      let wpadl = 4 in
        print_string (colorise `cyan (String.sub padding 0 wpadl));
      print_char ' ';
      print_string (colorise `bold str);
      print_char ' ';
      let wpadr = wpad - wpadl - if !utf8_msgs then 4 else 0 in
      if wpadr > 0 then
        print_string
          (colorise `cyan
             (String.sub padding (String.length padding - wpadr) wpadr));
      if wpadr >= 0 && !utf8_msgs then
        (print_string "  ";
         print_string (colorise `yellow utf8camel));
      print_char '\n';
      flush stdout;
    )
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

let editor = lazy (
  try OpamMisc.getenv "OPAM_EDITOR" with Not_found ->
  try OpamMisc.getenv "VISUAL" with Not_found ->
  try OpamMisc.getenv "EDITOR" with Not_found ->
    "nano"
)

type os =
  | Darwin
  | Linux
  | FreeBSD
  | OpenBSD
  | NetBSD
  | DragonFly
  | Cygwin
  | Win32
  | Unix
  | Other of string

let os =
  let os = lazy (
    match Sys.os_type with
    | "Unix" -> begin
        match OpamMisc.uname_s () with
        | Some "Darwin"    -> Darwin
        | Some "Linux"     -> Linux
        | Some "FreeBSD"   -> FreeBSD
        | Some "OpenBSD"   -> OpenBSD
        | Some "NetBSD"    -> NetBSD
        | Some "DragonFly" -> DragonFly
        | _                -> Unix
      end
    | "Win32"  -> Win32
    | "Cygwin" -> Cygwin
    | s        -> Other s
  ) in
  fun () -> Lazy.force os

let arch =
  let arch =
    lazy (OpamMisc.Option.default "Unknown" (OpamMisc.uname_m ()))
  in
  fun () -> Lazy.force arch

let string_of_os = function
  | Darwin    -> "darwin"
  | Linux     -> "linux"
  | FreeBSD   -> "freebsd"
  | OpenBSD   -> "openbsd"
  | NetBSD    -> "netbsd"
  | DragonFly -> "dragonfly"
  | Cygwin    -> "cygwin"
  | Win32     -> "win32"
  | Unix      -> "unix"
  | Other x   -> x

let os_string () =
  string_of_os (os ())

let makecmd = ref (fun () ->
    match os () with
    | FreeBSD
    | OpenBSD
    | NetBSD
    | DragonFly -> "gmake"
    | _ -> "make"
  )

let log_limit = 10
let log_line_limit = 5 * 80

let default_jobs = 4
let default_dl_jobs = 3

let exit i =
  raise (Exit i)

let confirm ?(default=true) fmt =
  Printf.ksprintf (fun s ->
    try
      if !safe_mode then false else
      let prompt () = msg "%s [%s] " s (if default then "Y/n" else "y/N") in
      if !yes then (prompt (); msg "y\n"; true)
      else if !no then (prompt (); msg "n\n"; false)
      else if os () = Win32 then
        let rec loop () =
          prompt ();
          match String.lowercase (read_line ()) with
          | "y" | "yes" -> true
          | "n" | "no" -> false
          | "" -> default
          | _  -> loop ()
        in loop ()
      else
      let open Unix in
      prompt ();
      let attr = tcgetattr stdin in
      tcsetattr stdin TCSAFLUSH {attr with c_icanon = false; c_echo = false};
      tcflush stdin TCIFLUSH;
      let buf = Bytes.create 1 in
      let rec loop () =
        if read stdin buf 0 1 = 0 then raise End_of_file
        else match Bytes.get  buf 0 with
        | 'y' | 'Y' -> print_endline (Bytes.to_string buf); true
        | 'n' | 'N' -> print_endline (Bytes.to_string buf); false
        | '\n' -> print_endline (if default then "y" else "n"); default
        | _ -> loop ()
      in
      let r = loop () in
      tcsetattr stdin TCSAFLUSH attr;
      tcflush stdin TCIFLUSH;
      r
    with
    | End_of_file -> msg "%s\n" (if default then "y" else "n"); default
    | Sys.Break as e -> msg "\n"; raise e
  ) fmt

let read fmt =
  Printf.ksprintf (fun s ->
    msg "%s %!" s;
    if not !yes || !no || !safe_mode then (
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
