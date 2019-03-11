(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module SMap = OpamStd.String.Map
module SSet = OpamStd.String.Set

let fatal_error ?(exit_code=1) =
  OpamConsole.error_and_exit
    (List.find (fun (_,i) -> i = exit_code) OpamStd.Sys.exit_codes |> fst)

let string_split char str = OpamStd.String.split str char

(* system detection *)

let some = function Some k -> k | None -> assert false
let _arch = some (OpamSysPoll.arch ())
let os = some (OpamSysPoll.os ())
let distribution = some (OpamSysPoll.os_distribution ())
let family = some (OpamSysPoll.os_family ())

(* processing *)

let install_packages_commands ~interactive packages =
  let yes opt r =
    if not interactive then opt @ r else r
  in
  match family with
  | "homebrew" ->
    ["brew"::"install"::packages]
  | "macports" ->
    ["port"::"install"::packages]
  | "debian" ->
    ["apt-get"::"install"::yes ["-qq"; "-yy"] packages]
  | "rhel" | "centos" | "fedora" | "mageia" | "oraclelinux" | "ol" ->
    (* todo: check if they all declare "rhel" as primary family *)
    (* When opem-packages specify the epel-release package, usually it
       means that other dependencies require the EPEL repository to be
       already setup when yum-install is called. Cf. #70, #76. *)
    let epel_release = "epel-release" in
    let install_epel =
      try [
        "yum"::"install"::yes ["-y"] [List.find ((=) epel_release) packages];
      ] with _ -> [] in
    install_epel @
    ["yum"::"install"::yes ["-y"] (List.filter ((<>) epel_release) packages);
     "rpm"::"-q"::"--whatprovides"::packages]
  | "bsd" ->
    if distribution = "freebsd" then ["pkg"::"install"::packages]
    else ["pkg_add"::packages]
  | "archlinux" | "arch" ->
    ["pacman"::"-S"::"--noconfirm"::packages]
  | "gentoo" ->
    ["emerge"::packages]
  | "alpine" ->
    ["apk"::"add"::packages]
  | "suse" | "opensuse" ->
    ["zypper"::yes ["--non-interactive"] ("install"::packages)]
  | s ->
    fatal_error "Sorry, don't know how to install packages on your %s system" s

let update_command =
  match family with
  | "debian" ->
    ["apt-get";"update"]
  | "homebrew" ->
    ["brew"; "update"]
  | "rhel" | "centos" | "fedora" | "mageia" | "oraclelinux" | "ol" ->
    ["yum"; "makecache"]
  | "archlinux" | "arch" ->
    ["pacman"; "-Sy"]
  | "gentoo" ->
    ["emerge"; "--sync"]
  | "alpine" ->
    ["apk"; "update"]
  | "suse" | "opensuse" ->
    ["zypper"; "--non-interactive"; "update"]
  | _ -> ["echo"; "Skipping system update on this platform."]

exception Signaled_or_stopped of string list * int

let run_command cmd args f =
  let r = OpamProcess.run (OpamSystem.make_command cmd args) in
  let result = f r in
  OpamProcess.cleanup r;
  result

let run_query_command cmd args =
  run_command cmd args @@ fun r ->
  if OpamProcess.is_success r then
    r.r_stdout
  else []

(* filter 'packages' to retain only the installed ones *)
let get_installed_packages packages =
  match family with
  | "homebrew" ->
    let lines = run_query_command "brew" ["list"] in
    List.map (string_split ' ') lines
    |> List.flatten
    |> SSet.of_list
    |> SSet.inter packages
  | "suse" | "opensuse" ->
    let lines =
      (* get the second column of the table:
         zypper --quiet se -i -t package|grep '^i '|awk -F'|' '{print $2}'|xargs echo*)
      run_query_command "zypper" ["--quiet"; "se"; "-i"; "-t"; "package"]
      |> OpamStd.List.filter_map (fun s ->
          if OpamStd.String.starts_with ~prefix:"i" s then
            OpamStd.String.split s '|'
            |> function | _::x::_ -> Some (OpamStd.String.strip x) | _ -> None
          else None)
    in
    List.map (string_split ' ') lines
    |> List.flatten
    |> SSet.of_list
    |> SSet.inter packages
  | "debian" ->
    (* First query regular package *)
    let dpkg_args pkgs =
      (* XXX update to db status ? *)
      (* ${db:Status-Status} would give only the column we're interested in, but
         it's quite new in dpkg-query. *)
      ["-W"; "-f"; "'${Package} ${Status}\\n'"] @ pkgs @ ["2>/dev/null"]
    in
    let lines =
      run_query_command "dpkg-query" (dpkg_args (SSet.elements packages))
    in
    let installed =
      List.fold_left
        (fun acc l -> match string_split ' ' l with
           | [pkg;_;_;"installed"] -> SSet.add pkg acc
           | _ -> acc)
        SSet.empty lines in
    if SSet.cardinal installed
       = SSet.cardinal packages then
      installed
    else
    (* If package are missing look for virtual package. *)
    let missing = SSet.inter installed packages in
    let resolve_virtual name =
      let lines =
        run_query_command "apt-cache"
          ["apt-cache"; "--names-only"; "search"; "'^%s$'"; "2>/dev/null"; name]
      in
      List.fold_left
        (fun acc l -> match string_split ' ' l with
           | pkg :: _ ->  SSet.add pkg acc
           | [] -> acc)
        SSet.empty lines
    in
    let virtual_map =
      SSet.fold (fun vpkg acc ->
          SSet.fold (fun pkg acc ->
              let old =
                try SMap.find pkg acc
                with Not_found -> SSet.empty
              in
              SMap.add pkg (SSet.add vpkg old) acc)
            (resolve_virtual vpkg)acc)
        missing SMap.empty
    in
    let real_packages =
      List.map fst (SMap.bindings virtual_map)
    in
    let lines = run_query_command "dpkg-query" (dpkg_args real_packages) in
    List.fold_left
      (fun acc l -> match string_split ' ' l with
         | [pkg;_;_;"installed"] ->
           (match SMap.find_opt pkg virtual_map with
            | Some p -> SSet.union p acc
            | None -> acc)
         | _ -> acc)
      installed lines
  | "amzn" | "centos" | "fedora" | "mageia" | "archlinux" | "arch" | "gentoo"
  | "alpine" | "rhel" | "oraclelinux" | "ol" ->
    let cmd, get_args =
      match distribution with
      | "amzn" | "centos" | "fedora" | "mageia" | "rhel" | "oraclelinux" | "ol" ->
        "rpm", fun pkg_name -> ["-qi"; pkg_name]
      | "archlinux" | "arch" -> "pacman", fun pkg_name -> ["-Q"; pkg_name]
      | "gentoo" -> "sh", fun pkg_name -> ["-c"; "ls -d /var/db/pkg/*/* | cut -f5- -d/ | grep -q '^"^pkg_name^"-[0-9].*$'"]
      | "alpine" -> "apk", fun pkg_name -> ["info"; "-e"; pkg_name]
      | _ -> fatal_error "Distribution %s is not supported" distribution
    in
    SSet.filter
      (fun pkg_name ->
         let args = get_args pkg_name in
         run_command cmd args @@ fun r ->
         match r.r_code with
         | 0 -> true (* installed *)
         | 1 -> false (* not installed *)
         | c -> raise (Signaled_or_stopped ((cmd::args), c)))
      packages
  | "bsd" ->
    (match distribution with
     | "freebsd" ->
       run_query_command "pkg" ["query"; "%n"]
       |> SSet.of_list
       |> SSet.inter packages
     | "openbsd" ->
       run_query_command "pkg_info" ["-mqP"]
       |> SSet.of_list
       |> SSet.inter packages
     | _ -> SSet.empty)
  | "macports" -> SSet.empty
  | _ -> SSet.empty

let sudo_run_command ~su ~interactive cmd =
  let get_cmd = function
    | c::a -> c, a
    | _  -> assert false
  in
  let cmd, args =
    match os, distribution with
    | "openbsd", _ ->
      (* XXX!!! *)
      if Unix.getuid () <> 0 then (
        OpamConsole.msg
          "The following command needs to be run through %S:\n    %s\n%!"
          "doas" (String.concat " " cmd);
        if interactive && not (OpamConsole.confirm "Allow ?") then
          OpamStd.Sys.exit_because `Success;
        "doas", cmd
      ) else get_cmd cmd
    | ("linux" | "unix" | "freebsd" | "netbsd" | "dragonfly"), _
    | "macos", "macports" ->
      (* not sure about this list *)
      if Unix.getuid () <> 0 then (
        OpamConsole.msg
          "The following command needs to be run through %S:\n    %s\n%!"
          (if su then "su" else "sudo") (String.concat " " cmd);
        if interactive && not (OpamConsole.confirm "Allow ?") then
          exit 1;
        if su then
          "su", ["root"; "-c"; Printf.sprintf "%S" (String.concat " " cmd)]
        else
          "sudo", cmd
      ) else get_cmd cmd
    | _ -> get_cmd cmd
  in
  run_command cmd args OpamProcess.is_success

let update ~su ~interactive =
  if sudo_run_command ~su ~interactive update_command then
    OpamConsole.errmsg "# OS package update successful\n%!"
  else fatal_error "OS package update failed"

let install ~su ~interactive packages =
  if SSet.is_empty packages then ()
  else
  let cmds =
    install_packages_commands ~interactive (SSet.elements packages)
  in
  let ok =
    List.fold_left (fun ok cmd ->
        ok && (sudo_run_command ~su ~interactive cmd))
      true cmds
  in
  if ok then
  OpamConsole.errmsg "# OS packages installation successful\n%!"
  else
  fatal_error "OS package installation failed"
