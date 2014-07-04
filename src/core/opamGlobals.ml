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

let check var = ref (
    try OpamMisc.getenv ("OPAM"^var) <> ""
    with Not_found -> false
  )

let debug            = check "DEBUG"
let debug_level      =
  try ref (int_of_string (OpamMisc.getenv ("OPAMDEBUG")))
  with Not_found | Failure _ -> ref 1
let verbose          = check "VERBOSE"
let color_tri_state =
    try (match OpamMisc.getenv "OPAMCOLOR" with
      | "always" -> `Always
      | "never" -> `Never
      | _ -> `Auto
      )
    with
      | Not_found -> `Auto
let color            = ref (color_tri_state = `Always)
let keep_build_dir   = check "KEEPBUILDDIR"
let no_base_packages = check "NOBASEPACKAGES"
let no_checksums     = check "NOCHECKSUMS"
let yes              = check "YES"
let strict           = check "STRICT"
let build_test       = check "BUILDTEST"
let build_doc        = check "BUILDDOC"
let show             = check "SHOW"
let dryrun           = check "DRYRUN"
let fake             = check "FAKE"
let print_stats      = check "STATS"
let utf8_msgs        = check "UTF8MSGS"
let autoremove       = check "AUTOREMOVE"
let do_not_copy_files = check "DONOTCOPYFILES"
let sync_archives    = check "SYNCARCHIVES"
let compat_mode_1_0  = check "COMPATMODE_1_0"
let use_external_solver = ref (not (!(check "NOASPCUD") || !(check "USEINTERNALSOLVER")))
let no_self_upgrade  = check "NOSELFUPGRADE"

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
  with Not_found | Failure _ -> 10

let cudf_file = ref (
    try Some (OpamMisc.getenv "OPAMCUDFFILE")
    with Not_found -> None
  )

let solver_timeout =
  try float_of_string (OpamMisc.getenv "OPAMSOLVERTIMEOUT")
  with Not_found | Failure _ -> 5.

let default_preferences = (* "-removed,-notuptodate,-count(down),-new,-changed" *)
  "-removed,-count(up),-count(down),-notuptodate,-new"
let default_upgrade_preferences = (* "-removed,-notuptodate,-count(down),-new,-changed" *)
  "-removed,-notuptodate,-changed"
let default_fixup_preferences =
  "-changed"

let solver_preferences, solver_upgrade_preferences, solver_fixup_preferences =
  let get s =
    try Some (OpamMisc.strip (OpamMisc.getenv s)) with Not_found -> None in
  let prefs = get "OPAMCRITERIA"
  and upgrade_prefs = get "OPAMUPGRADECRITERIA"
  and fixup_prefs = get "OPAMFIXUPCRITERIA"
  in
  let fixup_prefs =
    OpamMisc.Option.default default_fixup_preferences fixup_prefs in
  let upgrade_prefs =
    OpamMisc.Option.default
      (OpamMisc.Option.default default_upgrade_preferences prefs)
      upgrade_prefs in
  let prefs =
    OpamMisc.Option.default default_preferences prefs in
  ref prefs, ref upgrade_prefs, ref fixup_prefs

let default_external_solver = "aspcud"

let external_solver = ref(
  try OpamMisc.strip (OpamMisc.getenv "OPAMEXTERNALSOLVER")
  with Not_found -> default_external_solver)

let default_repository_name    = "default"
let default_repository_address = "https://opam.ocaml.org"

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

let root_dir =
  ref (Filename.concat Filename.temp_dir_name
         ("opam-" ^ string_of_int (Unix.getpid ())))

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

let indent_left str n =
  if String.length str >= n then str
  else
    let nstr = String.make n ' ' in
    String.blit str 0 nstr 0 (String.length str);
    nstr

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
    Printf.fprintf stderr ("%s  %06d  %a  " ^^ fmt ^^ "\n%!")
      (timestamp ()) (Unix.getpid ()) (acolor_w 30 `yellow) section
  else
    Printf.ifprintf stderr fmt

(* Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
   rather than [log "%s" (to_string x)] to avoid costly unneeded
   stringifications *)
let slog to_string channel x = output_string channel (to_string x)

let error fmt =
  Printf.ksprintf (fun str ->
    Printf.eprintf "%a %s\n%!" (acolor `red) "[ERROR]" str
  ) fmt

let warning fmt =
  Printf.ksprintf (fun str ->
    Printf.eprintf "%a %s\n%!" (acolor `yellow) "[WARNING]" str
  ) fmt

let note fmt =
  Printf.ksprintf (fun str ->
    Printf.eprintf "%a %s\n%!" (acolor `blue) "[NOTE]" str
  ) fmt

exception Exit of int

exception Package_error of string

let error_and_exit fmt =
  Printf.ksprintf (fun str ->
    error "%s" str;
    raise (Exit 66)
  ) fmt

let display_messages = ref true

let msg =
  if !display_messages then (
    fun fmt ->
      flush stderr;
      Printf.kfprintf flush stdout fmt
  ) else (
    fun fmt ->
      Printf.ifprintf stdout fmt
  )

let header_width () = 80

let header_msg fmt =
  let utf8camel = "\xF0\x9F\x90\xAB " in (* UTF-8 <U+1F42B, U+0020> *)
  let padding = "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" in
  Printf.ksprintf (fun str ->
    flush stderr;
    if !display_messages then (
      print_char '\n';
      let wpad = header_width () - String.length str - 2 in
      let wpadl = max (wpad / 2) 3 in
      if !utf8_msgs then
        (print_string (colorise `yellow utf8camel);
         for _i = 1 to wpadl - 6 do print_char ' ' done;
         for _i = wpadl - 5 to wpadl - 3 do print_char '-' done)
      else
        print_string (colorise `cyan (String.sub padding 0 wpadl));
      print_char ' ';
      print_string (colorise `bold str);
      print_char ' ';
      let wpadr = wpad - wpadl in
      if wpadr > 0 then
        if !utf8_msgs then print_string "---" else
        print_string
          (colorise `cyan
             (String.sub padding (String.length padding - wpadr) wpadr));
      print_char '\n';
      flush stdout;
    )
  ) fmt

let header_error fmt =
  let padding = "#=========================================================#" in
  Printf.ksprintf (fun head fmt ->
      Printf.ksprintf (fun contents ->
          output_char stderr '\n';
          let wpad = header_width () - String.length head - 8 in
          let wpadl = max (wpad / 2) 3 in
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

let osref = ref None

let os () =
  match !osref with
  | None ->
    let os = match Sys.os_type with
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
      | s        -> Other s in
    osref := Some os;
    os
  | Some os -> os

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

let default_jobs = 1
let default_dl_jobs = 3

let exit i =
  raise (Exit i)
