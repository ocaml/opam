(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCompat
open OpamStd.Option.Op

let command_output c =
  match List.filter (fun s -> String.trim s <> "")
          (OpamSystem.read_command_output c)
  with
  | [""] -> None
  | [s] -> Some s
  | _ -> None
  | exception (OpamSystem.Process_error _ | OpamSystem.Command_not_found _) ->
    None

let norm s = if s = "" then None else Some (String.lowercase_ascii s)

let normalise_arch raw =
  match String.lowercase_ascii raw with
  | "x86" | "i386" | "i586" | "i686" -> "x86_32"
  | "x86_64" | "amd64" -> "x86_64"
  | "powerpc" | "ppc" | "ppcle" -> "ppc32"
  | "ppc64" | "ppc64le" -> "ppc64"
  | "aarch64_be" | "aarch64" | "armv8b" | "armv8l" -> "arm64"
  | a when List.exists (fun prefix -> OpamStd.String.starts_with ~prefix a)
        ["armv5"; "armv6"; "earmv6"; "armv7"; "earmv7"] -> "arm32"
  | s -> s

let arch_lazy = lazy (
  let raw = match Sys.os_type with
    | "Unix" | "Cygwin" -> OpamStd.Sys.uname "-m"
    | "Win32" ->
      (match OpamStd.Env.getopt "PROCESSOR_ARCHITECTURE" with
       | Some "X86" as a -> OpamStd.Env.getopt "PROCESSOR_ARCHITEW6432" ++ a
       | arch -> arch)
    | _ -> None
  in
  match raw with
  | None | Some "" -> None
  | Some a -> Some (normalise_arch a)
)
let arch () = Lazy.force arch_lazy

let os_lazy = lazy (
  match Sys.os_type with
  | "Unix" ->
    (match OpamStd.Sys.uname "-s" with
      | Some "Darwin" -> Some "macos"
      | Some s -> norm s
      | None -> None)
  | s -> norm s
)
let os () = Lazy.force os_lazy

let os_release_field =
  let os_release_file = lazy (
    List.find Sys.file_exists ["/etc/os-release"; "/usr/lib/os-release"] |>
    OpamProcess.read_lines |>
    OpamStd.List.filter_map (fun s ->
        try
          Scanf.sscanf s "%s@= %s" (fun x v ->
              let contents =
                try Scanf.sscanf v "\"%s@\"" (fun s -> s)
                with Scanf.Scan_failure _ | End_of_file -> v
              in
              Some (x, contents))
        with Scanf.Scan_failure _ | End_of_file -> None)
  ) in
  fun f ->
    try Some (List.assoc f (Lazy.force os_release_file))
    with Not_found -> None

let is_android, android_release =
  let prop = lazy (command_output ["getprop"; "ro.build.version.release"]) in
  (fun () -> Lazy.force prop <> None),
  (fun () -> Lazy.force prop)

let os_distribution_lazy = lazy (
  match os () with
  | Some "macos" as macos ->
    if OpamSystem.resolve_command "brew" <> None then Some "homebrew"
    else if OpamSystem.resolve_command "port" <> None then Some "macports"
    else macos
  | Some "linux" as linux ->
    (if is_android () then Some "android" else
     os_release_field "ID" >>= norm >>+ fun () ->
     command_output ["lsb_release"; "-i"; "-s"] >>= norm >>+ fun () ->
     try
       List.find Sys.file_exists ["/etc/redhat-release";
                                  "/etc/centos-release";
                                  "/etc/gentoo-release";
                                  "/etc/issue"] |>
       fun s -> Scanf.sscanf s " %s " norm
     with Not_found -> linux)
  | os -> os
)
let os_distribution () = Lazy.force os_distribution_lazy

let os_version_lazy = lazy (
  match os () with
  | Some "linux" ->
    android_release () >>= norm >>+ fun () ->
    command_output ["lsb_release"; "-s"; "-r"] >>= norm >>+ fun () ->
    os_release_field "VERSION_ID" >>= norm
  | Some "macos" ->
    command_output ["sw_vers"; "-productVersion"] >>= norm
  | Some ("win32" | "cygwin") ->
    (try
       command_output ["cmd"; "/C"; "ver"] >>= fun s ->
       Scanf.sscanf s "%_s@[ Version %s@]" norm
     with Scanf.Scan_failure _ | End_of_file -> None)
  | Some "freebsd" ->
    OpamStd.Sys.uname "-U" >>= norm
  | _ ->
    OpamStd.Sys.uname "-r" >>= norm
)
let os_version () = Lazy.force os_version_lazy

let os_family_lazy = lazy (
  match os () with
  | Some "linux" ->
    (os_release_field "ID_LIKE" >>= fun s ->
     Scanf.sscanf s " %s" norm (* first word *))
    >>+ os_distribution
  | Some ("freebsd" | "openbsd" | "netbsd" | "dragonfly") -> Some "bsd"
  | Some ("win32" | "cygwin") -> Some "windows"
  | os -> os
)
let os_family () = Lazy.force os_family_lazy

let variables =
  List.map
    (fun (n, v) ->
       OpamVariable.of_string n,
       lazy (Lazy.force v >>| fun v -> OpamTypes.S v))
    [
      "arch", arch_lazy;
      "os", os_lazy;
      "os-distribution", os_distribution_lazy;
      "os-version", os_version_lazy;
      "os-family", os_family_lazy;
    ]
