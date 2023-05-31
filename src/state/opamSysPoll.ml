(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Option.Op


let norm s = if s = "" then None else Some (String.lowercase_ascii s)

let normalise_arch raw =
  match String.lowercase_ascii raw with
  | "x86" | "i386" | "i486" | "i586" | "i686" -> "x86_32"
  | "x86_64" | "amd64" -> "x86_64"
  | "powerpc" | "ppc" | "ppcle" -> "ppc32"
  | "ppc64" | "ppc64le" -> "ppc64"
  | "aarch64_be" | "aarch64" -> "arm64"
  | a when a = "armv8b" || a = "armv8l" || List.exists (fun prefix -> OpamStd.String.starts_with ~prefix a)
        ["armv5"; "armv6"; "earmv6"; "armv7"; "earmv7"] -> "arm32"
  | s -> s

let normalise_os raw =
  match String.lowercase_ascii raw with
  | "darwin" | "osx" -> "macos"
  | s -> s

module type IncludeT = functor (Runnable : OpamStd.OpamSysRunnableT) (Runner : OpamStd.Runner) -> sig
  val poll_arch : unit -> string option Runnable(Runner).R.t
  val arch : string option Runnable(Runner).R.t Lazy.t

  val poll_os : unit -> string option Runnable(Runner).R.t
  val os : string option Runnable(Runner).R.t Lazy.t

  val poll_os_version : unit -> string option Runnable(Runner).R.t
  val os_version : string option Runnable(Runner).R.t Lazy.t

  val poll_os_distribution : unit -> string option Runnable(Runner).R.t
  val os_distribution : string option Runnable(Runner).R.t Lazy.t

  val variables : (OpamVariable.t * OpamVariable.variable_contents option Runnable(Runner).R.t lazy_t) list
end

module Include' (OpamStdSys : OpamStd.OpamSysRunnableT) (Runner : OpamStd.Runner) = struct
  module OpamStdSys = OpamStdSys(Runner)

  let command_output ~prog ~argv =
    let output = OpamStdSys.R.run ~prog ~argv in
    OpamStdSys.R.map output (function
      | None -> None
      | Some "" -> None
      | Some s -> Some s)

  let poll_arch () =
    let raw = match Sys.os_type with
      | "Unix" | "Cygwin" -> OpamStdSys.uname ["-m"]
      | "Win32" ->
        if Sys.word_size = 32 && not (OpamStubs.isWoW64 ()) then
          OpamStdSys.R.return (Some "i686")
        else
          OpamStdSys.R.return (Some "x86_64")
      | _ -> OpamStdSys.R.return None
    in
    OpamStdSys.R.map raw (function
      | None | Some "" -> None
      | Some a -> Some (normalise_arch a))
  let arch = Lazy.from_fun poll_arch

  let poll_os () =
    let raw =
      match Sys.os_type with
      | "Unix" -> OpamStdSys.uname ["-s"]
      | s -> OpamStdSys.R.return (norm s)
    in
    OpamStdSys.R.map raw (function
      | None | Some "" -> None
      | Some s -> Some (normalise_os s))
  let os = Lazy.from_fun poll_os

  let is_android, android_release =
    let prop = lazy (command_output ~prog:"getprop" ~argv:["ro.build.version.release"]) in
    (fun () ->
      OpamStdSys.R.map
        (Lazy.force prop)
        (fun prop -> prop <> None)),
    (fun () -> Lazy.force prop)

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
      try Some (OpamStd.List.assoc String.equal f (Lazy.force os_release_file))
      with Not_found -> None

  let poll_os_version () =
    let return = OpamStdSys.R.return in
    let lazy os = os in
    OpamStdSys.R.bind os (function
      | Some "linux" -> (
        OpamStdSys.R.bind (android_release ()) (function
        | Some android -> return (norm android)
        | None ->
          let release = command_output ~prog:"lsb_release" ~argv:["-s"; "-r"] in
          OpamStdSys.R.bind release (function
          | Some lsb -> return (norm lsb) 
          | None ->
            os_release_field "VERSION_ID" >>= norm |> return)))
      | Some "macos" ->
        let sw_vers = command_output ~prog:"sw_vers" ~argv:["-productVersion"] in
        OpamStdSys.R.map sw_vers (fun sw_vers -> sw_vers >>= norm)
      | Some "win32" ->
        let (major, minor, build, _) = OpamStubs.getWindowsVersion () in
        OpamStd.Option.some @@ Printf.sprintf "%d.%d.%d" major minor build
        |> return 
      | Some "cygwin" ->
        (try
          let cmd = command_output ~prog:"cmd" ~argv:["/C"; "ver"] in
          OpamStdSys.R.map cmd (fun cmd ->
            cmd >>= fun s ->
            Scanf.sscanf s "%_s@[ Version %s@]" norm)
         with Scanf.Scan_failure _ | End_of_file -> return None)
      | Some "freebsd" ->
        let uname = OpamStdSys.uname ["-U"] in
        OpamStdSys.R.map uname (fun uname -> Option.bind uname norm)
      | _ ->
        let uname = OpamStdSys.uname ["-r"] in
        OpamStdSys.R.map uname (fun uname -> Option.bind uname norm))
  let os_version = Lazy.from_fun poll_os_version

  let poll_os_distribution () =
    let lazy os = os in
    let return = OpamStdSys.R.return in
    OpamStdSys.R.bind os (function
      | Some "macos" as macos ->
        if OpamSystem.resolve_command "brew" <> None then return (Some "homebrew")
        else if OpamSystem.resolve_command "port" <> None then return (Some "macports")
        else (return macos)
      | Some "linux" as linux -> (
        let is_android = is_android () in
        OpamStdSys.R.bind is_android (function
          | true -> return @@ Some "android"
          | false -> (
            match os_release_field "ID" with
            | Some os_release_field -> return @@ norm os_release_field
            | None -> (
              let lsb_release = command_output ~prog:"lsb_release" ~argv:["-i"; "-s"] in
              OpamStdSys.R.bind lsb_release (function
                | Some lsb_release -> return @@ norm lsb_release
                | None -> (
                  try
                    List.find Sys.file_exists ["/etc/redhat-release";
                                               "/etc/centos-release";
                                               "/etc/gentoo-release";
                                               "/etc/issue"]
                    |> fun s -> Scanf.sscanf s " %s " norm
                    |> return
                  with Not_found -> return linux))))))
      | os -> return os)
  let os_distribution = Lazy.from_fun poll_os_distribution

  let poll_os_family () =
    let return = OpamStdSys.R.return in
    let lazy os = os in
    OpamStdSys.R.bind os (function
      | Some "linux" -> (
        let id_like = os_release_field "ID_LIKE" >>= fun s ->
          (* first word *)
          Scanf.sscanf s " %s" norm
        in
        match id_like with
        | Some _ as id_like -> return id_like
        | None -> Lazy.force os_distribution)
      | Some ("freebsd" | "openbsd" | "netbsd" | "dragonfly") ->
        return @@ Some "bsd"
      | Some ("win32" | "cygwin") ->
        return @@ Some "windows"
      | _ -> Lazy.force os_distribution)
  let os_family = Lazy.from_fun poll_os_family

  let variables =
    List.map
      (fun (name, value) ->
        let var_name = OpamVariable.of_string name in
        let opam_value = OpamCompat.Lazy.map (fun m ->
           OpamStdSys.R.map m (fun opt ->
             OpamStd.Option.map (fun v -> OpamTypes.S v)
             opt))
         value
        in
        (var_name, opam_value))
      [
        "arch", arch;
        "os", os;
        "os-distribution", os_distribution;
        "os-version", os_version;
        "os-family", os_family;
      ]

end

include Include'(OpamStd.OpamSysRunnable')(OpamStd.UnitRunner)

let variables =
  List.map (fun (k, v) ->
    let v = OpamCompat.Lazy.map (fun m ->
      OpamStd.UnitRunner.escape m) v
    in
    (k, v))
    variables

let cores =
  let v = Lazy.from_fun OpamSystem.cpu_count in
  fun () -> Lazy.force v

(* Exported functions *)
let resolve_or_poll var poll env =
  match OpamVariable.Full.read_from_env (OpamVariable.Full.of_string var) with
  | Some (S c) -> Some c
  | _ ->
    match OpamVariable.Map.find_opt (OpamVariable.of_string var) env with
    | Some (lazy (Some (OpamTypes.S c)), _) -> Some c
    | _ ->
      let m = Lazy.force poll in
      OpamStd.UnitRunner.escape m

let arch = resolve_or_poll "arch" arch
let os = resolve_or_poll "os" os
let os_distribution = resolve_or_poll "os-distribution" os_distribution
let os_version = resolve_or_poll "os-version" os_version
let os_family = resolve_or_poll "os-family" os_family

let to_string env =
  let open OpamStd.Option.Op in
  Printf.sprintf "arch=%s os=%s os-distribution=%s os-version=%s"
    (arch env +! "unknown")
    (os env +! "unknown")
    (os_distribution env +! "unknown")
    (os_version env +! "unknown")
