(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log fmt = OpamConsole.log "XSYS" fmt

(* Run commands *)
(* Always call this function to run a command, as it handles `dryrun` option *)

let run_command
    ?vars ?(discard_err=false) ?allow_stdin ?verbose ?(dryrun=false) cmd args =
  let clean_output =
    if not discard_err then
      fun k -> k None
    else
    fun k -> OpamFilename.with_tmp_dir_job @@ fun dir ->
      let f = OpamFilename.Op.(dir // "out") in
      OpamFilename.touch f;
      k (Some (OpamFilename.to_string f))
  in
  let verbose =
    OpamStd.Option.default OpamCoreConfig.(!r.verbose_level >= 3) verbose
  in
  let env =
    match vars with
    | None -> None
    | Some vars ->
      let env = OpamStd.Env.list () in
      let set_vars, kept_vars, env =
        List.fold_left (fun (n,p,e) (op, (name, content as var)) ->
            match  OpamStd.List.assoc_opt name env, op with
            | Some c, `add when String.compare c content = 0 -> n, p, e
            | Some _, `set -> var::n, p, (List.remove_assoc name env)
            | Some _, _ -> n, var::p, e
            | None, _ -> var::n, p, e
          )
          ([],[], env) vars
      in
      let str_var (v,c) = Printf.sprintf "%s=%s" v c in
      if set_vars = [] then
        ((if kept_vars <> [] then
            log "Won't override %s"
              (OpamStd.List.to_string str_var kept_vars));
         None)
      else
        (log "Adding to env %s"
           (OpamStd.List.to_string str_var set_vars);
         Some (set_vars @ env
               |> List.rev_map str_var
               |> Array.of_list))
  in
  let run =
    if dryrun then OpamProcess.Job.dry_run else OpamProcess.Job.run
  in
  let open OpamProcess.Job.Op in
  run @@ clean_output @@ fun stdout ->
  OpamSystem.make_command
    ?env ?stdout ?allow_stdin ~verbose cmd args
  @@> fun r ->
  let code = r.r_code in
  let out = r.r_stdout in
  OpamProcess.cleanup r;
  Done (code, out)

let run_query_command ?vars cmd args =
  let vars = (`set, ("LC_ALL","C"))::OpamStd.Option.to_list vars in
  let code,out = run_command ~vars cmd args in
  if code = 0 then out
  else []

let run_command_exit_code ?vars ?allow_stdin ?verbose cmd args =
  let code,_ =
    run_command ?vars ?allow_stdin ?verbose ~dryrun:OpamStateConfig.(!r.dryrun)
      cmd args
  in
  code

(* Please keep this alphabetically ordered, in the type definition, and in
   below pattern matching *)
type families =
  | Alpine
  | Arch
  | Centos
  | Debian
  | Freebsd
  | Gentoo
  | Homebrew
  | Macports
  | Netbsd
  | Openbsd
  | Suse

(* System status *)
let family =
  let family = lazy (
    match OpamSysPoll.os_family () with
    | None ->
      Printf.ksprintf failwith
        "External dependency unusable, OS family not detected."
    | Some family ->
      match family with
      | "alpine" -> Alpine
      | "amzn" | "centos" | "fedora" | "mageia" | "oraclelinux" | "ol"
      | "rhel" -> Centos
      | "archlinux" | "arch" -> Arch
      | "bsd" ->
        begin match OpamSysPoll.os_distribution () with
          | Some ("freebsd" | "dragonfly") -> Freebsd
          | Some "netbsd" -> Netbsd
          | Some "openbsd" -> Openbsd
          | _ ->
            Printf.ksprintf failwith
              "External dependency handling not supported for OS family 'bsd'."
        end
      | "debian" | "ubuntu" -> Debian
      | "gentoo" -> Gentoo
      | "homebrew" -> Homebrew
      | "macports" -> Macports
      | "suse" | "opensuse" -> Suse
      | family ->
        Printf.ksprintf failwith
          "External dependency handling not supported for OS family '%s'."
          family
  ) in
  fun () -> Lazy.force family


let packages_status packages =
  let (+++) pkg set = OpamSysPkg.Set.add (OpamSysPkg.of_string pkg) set in
  (* Some package managers don't permit to request on available packages. In
     this case, we consider all non installed packages as [available]. *)
  let open OpamSysPkg.Set.Op in
  let compute_sets ?sys_available sys_installed =
    let installed = packages %% sys_installed in
    let available, not_found =
      match sys_available with
      | Some sys_available ->
        let available = (packages -- installed) %% sys_available in
        let not_found = packages -- installed -- available in
        available, not_found
      | None ->
        let available = packages -- installed in
        available, OpamSysPkg.Set.empty
    in
    available, not_found
  in
  let names_re ?str_pkgs () =
    let str_pkgs =
      OpamStd.Option.default
        OpamSysPkg.(Set.fold (fun p acc -> to_string p :: acc) packages [])
        str_pkgs
    in
    let need_escape = Re.(compile (group (set "+."))) in
    Printf.sprintf "^(%s)$"
      (OpamStd.List.concat_map "|"
         (Re.replace ~all:true need_escape ~f:(fun g -> "\\"^Re.Group.get g 1))
         str_pkgs)
  in
  let with_regexp_sgl re_pkg =
    List.fold_left (fun pkgs l ->
        try
          Re.(Group.get (exec re_pkg l) 1) +++ pkgs
        with Not_found -> pkgs) OpamSysPkg.Set.empty
  in
  let with_regexp_dbl ~re_installed ~re_pkg =
    List.fold_left (fun (inst,avail) l ->
        try
          let pkg = Re.(Group.get (exec re_pkg l) 1) in
          if Re.execp re_installed l then
            pkg +++ inst, avail
          else
            inst, pkg +++ avail
        with Not_found -> inst, avail)
      OpamSysPkg.Set.(empty, empty)
  in
  let package_set_of_pkgpath l =
    List.fold_left (fun set pkg ->
        let short_name =
          match String.rindex pkg '/' with
          | exception Not_found -> pkg
          | idx -> String.sub pkg idx (String.length pkg - idx)
        in
        set
        |> OpamSysPkg.Set.add (OpamSysPkg.of_string pkg)
        |> OpamSysPkg.Set.add (OpamSysPkg.of_string short_name)
      ) OpamSysPkg.Set.empty l
  in
  match family () with
  | Alpine ->
    let re_installed = Re.(compile (seq [str "[installed]"; eol])) in
    let re_pkg =
      (* packages form : libpeas-python3-1.22.0-r1 *)
      Re.(compile @@ seq
            [ bol;
              group @@ rep1 @@ alt [alnum; punct];
              char '-';
              rep1 digit;
              rep any ])
    in
    let sys_installed, sys_available =
      run_query_command "apk" ["list";"--available"]
      |> with_regexp_dbl ~re_installed ~re_pkg
    in
    compute_sets sys_installed ~sys_available
  | Arch ->
    (* output:
       >extra/cmake 3.17.1-1 [installed]
       >    A cross-platform open-source make system
       >extra/cmark 0.29.0-1
       >    CommonMark parsing and rendering library and program in C
    *)
    let re_installed = Re.(compile (seq [str "[installed]"; eol])) in
    let re_pkg =
      Re.(compile @@ seq
            [ bol;
              rep1 @@ alt [alnum; punct];
              char '/';
              group @@ rep1 @@ alt [alnum; punct];
              space;
            ])
    in
    let sys_installed, sys_available =
      run_query_command "pacman" ["-Ss" ; names_re ()]
      |> with_regexp_dbl ~re_installed ~re_pkg
    in
    compute_sets sys_installed ~sys_available
  | Centos ->
    (* XXX /!\ only checked on centos XXX *)
    let lines = run_query_command "yum" ["-q"; "-C"; "list"] in
    (* -C to retrieve from cache, no update but still quite long, 1,5 sec *)
    (* Return a list of installed packages then available ones:
       >Installed Packages
       >foo.arch    version   repo
       >Available Packages
       >bar.arch    version   repo
    *)
    let sys_installed, sys_available, _ =
      List.fold_left (fun (inst,avail,part) -> function
          (* beware of locales!! *)
          | "Installed Packages" -> inst, avail, `installed
          | "Available Packages" -> inst, avail, `available
          | l ->
            (match part, OpamStd.String.split l '.' with
             | `installed, pkg::_ ->
               pkg +++ inst, avail, part
             | `available, pkg::_ ->
               inst, pkg +++ avail, part
             | _ -> (* shouldn't happen *) inst, avail, part))
        OpamSysPkg.Set.(empty, empty, `preamble) lines
    in
    compute_sets sys_installed ~sys_available
  | Debian ->
    let sys_available, sys_provides, _ =
      let provides_sep = Re.(compile @@ str ", ") in
      let package_provided str =
        OpamSysPkg.of_string
          (match OpamStd.String.cut_at str ' ' with
           | None -> str
           | Some (p, _vc) -> p)
      in
      (* Output format:
         >Package: apt
         >Version: 2.1.7
         >Installed-Size: 4136
         >Maintainer: APT Development Team <deity@lists.debian.org>
         >Architecture: amd64
         >Replaces: apt-transport-https (<< 1.5~alpha4~), apt-utils (<< 1.3~exp2~)
         >Provides: apt-transport-https (= 2.1.7)
         > [...]
         >
         The `Provides' field contains provided virtual package(s) by current
         `Package:'.
         * manpages.debian.org/buster/apt/apt-cache.8.en.html
         * www.debian.org/doc/debian-policy/ch-relationships.html#s-virtual
      *)
      run_query_command "apt-cache"
        ["search"; names_re (); "--names-only"; "--full"]
      |> List.fold_left (fun (avail, provides, latest) l ->
          if OpamStd.String.starts_with ~prefix:"Package: " l then
            let p = String.sub l 9 (String.length l - 9) in
            p +++ avail, provides, Some (OpamSysPkg.of_string p)
          else if OpamStd.String.starts_with ~prefix:"Provides: " l then
            let ps =
              List.map package_provided (Re.split ~pos:10 provides_sep l)
              |> OpamSysPkg.Set.of_list
            in
            avail ++ ps,
            (match latest with
             | Some p -> OpamSysPkg.Map.add p ps provides
             | None -> provides (* Bad apt-cache output ?? *)),
            None
          else avail, provides, latest)
        (OpamSysPkg.Set.empty, OpamSysPkg.Map.empty, None)
    in
    let need_inst_check =
      OpamSysPkg.Map.fold (fun cp vps set ->
          if OpamSysPkg.Set.(is_empty (inter vps packages)) then set else
            OpamSysPkg.Set.add cp set
        ) sys_provides packages
    in
    let str_need_inst_check =
      OpamSysPkg.(Set.fold (fun p acc -> to_string p :: acc) need_inst_check [])
    in
    let sys_installed =
      (* ouput:
         >ii  uim-gtk3                 1:1.8.8-6.1  amd64    Universal ...
         >ii  uim-gtk3-immodule:amd64  1:1.8.8-6.1  amd64    Universal ...
      *)
      let re_pkg =
        Re.(compile @@ seq
              [ bol;
                str "ii";
                rep1 @@ space;
                group @@ rep1 @@ diff (alt [alnum; punct]) (char ':');
                (* pkg:arch convention *)
              ])
      in
      (* discard stderr as just nagging *)
      run_command ~discard_err:true "dpkg-query" ("-l" :: str_need_inst_check)
      |> snd
      |> with_regexp_sgl re_pkg
    in
    let sys_installed =
      (* Resolve installed "provides" packages;
         assumes provides are not recursive *)
      OpamSysPkg.Set.fold (fun p acc ->
          match OpamSysPkg.Map.find_opt p sys_provides with
          | None -> acc
          | Some ps -> OpamSysPkg.Set.union acc ps)
        sys_installed sys_installed
    in
    compute_sets sys_installed ~sys_available
  | Freebsd ->
    let sys_installed =
      run_query_command "pkg" ["query"; "%n\n%o"]
      |> List.map OpamSysPkg.of_string
      |> OpamSysPkg.Set.of_list
    in
    compute_sets sys_installed
  | Gentoo ->
    let sys_installed =
      let re_pkg =
        Re.(compile @@ seq
              [ group @@ rep1 @@ alt [alnum; punct];
                char '-';
                rep @@ seq [rep1 digit; char '.'];
                rep1 digit;
                rep any;
                eol ])
      in
      List.fold_left (fun inst dir ->
          List.fold_left (fun inst pkg ->
              let to_string d =
                OpamFilename.basename_dir d
                |> OpamFilename.Base.to_string
              in
              let pkg = Filename.concat (to_string dir) (to_string pkg) in
              try Re.(Group.get (exec re_pkg pkg) 1) :: inst
              with Not_found -> inst
            ) inst (OpamFilename.dirs dir))
        []
        (OpamFilename.dirs (OpamFilename.Dir.of_string "/var/db/pkg"))
      |> package_set_of_pkgpath
    in
    compute_sets sys_installed
  | Homebrew ->
    (* accept 'pkgname' and 'pkgname@version'
       exampe output
       >openssl@1.1
       >bmake
    *)
    let sys_installed =
      run_query_command "brew" ["list"; "--formula"]
      |> List.fold_left (fun res s ->
          List.fold_left (fun res spkg ->
              match OpamStd.String.cut_at spkg '@' with
              | Some (n,_v) -> n::spkg::res
              | None -> spkg::res)
            res (OpamStd.String.split s ' ')) []
      |> List.map OpamSysPkg.of_string
      |> OpamSysPkg.Set.of_list
    in
    compute_sets sys_installed
  | Macports ->
    let str_pkgs =
      OpamSysPkg.(Set.fold (fun p acc -> to_string p :: acc) packages [])
    in
    let sys_installed =
      (* output:
         >zlib @1.2.11_0 (active)
      *)
      let re_pkg =
        Re.(compile @@ seq
              [ bol;
                group @@ rep1 @@ alt [alnum; punct];
                rep1 space;
                rep any;
                str "(active)";
                eol
              ])
      in
      run_query_command "port" ("installed" :: str_pkgs)
      |> (function _::lines -> lines | _ -> [])
      |> with_regexp_sgl re_pkg
    in
    let sys_available =
      (* example output
         >diffutils  3.7  sysutils textproc devel  GNU diff utilities
         >--
         >No match for gcc found
      *)
      let re_pkg =
        Re.(compile @@ seq
              [ bol;
                group @@ rep1 @@ alt [alnum; punct];
                rep1 space;
                rep1 @@ alt [digit; punct];
              ])
      in
      run_query_command "port" (["search"; "--line"; "--exact" ] @ str_pkgs)
      |> with_regexp_sgl re_pkg
    in
    compute_sets sys_installed ~sys_available
  | Netbsd ->
    let sys_installed =
      run_query_command "pkg_info" ["-Q"; "PKGPATH"; "-a"]
      |> package_set_of_pkgpath
    in
    compute_sets sys_installed
  | Openbsd ->
    let sys_installed =
      run_query_command "pkg_info" ["-mqP"]
      |> package_set_of_pkgpath
    in
    compute_sets sys_installed
  | Suse ->
    (* get the second column of the table:
       zypper --quiet se -i -t package|grep '^i '|awk -F'|' '{print $2}'|xargs echo
       output:
       >S | Name                        | Summary
       >--+-----------------------------+-------------
       >  | go-gosqlite                 | Trivial SQLi
       >i | libqt4-sql-sqlite-32bit     | Qt 4 sqlite
    *)
    let re_pkg =
      Re.(compile @@ seq
            [ bol;
              rep1 any;
              char '|';
              rep1 space;
              group @@ rep1 @@ alt [alnum; punct];
              rep1 space;
              char '|';
            ])
    in
    let re_installed = Re.(compile @@ seq [bol ; char 'i']) in
    let sys_installed, sys_available =
      run_query_command "zypper" ["--quiet"; "se"; "-t"; "package"]
      |> with_regexp_dbl ~re_installed ~re_pkg
    in
    compute_sets sys_installed ~sys_available

(* Install *)

let install_packages_commands_t sys_packages =
  let yes ?(no=[]) yes r =
    if OpamStd.Config.env_bool "DEPEXTYES" = Some true then
      yes @ r else no @ r
  in
  let packages =
    List.map OpamSysPkg.to_string (OpamSysPkg.Set.elements sys_packages)
  in
  match family () with
  | Alpine -> ["apk", "add"::yes ~no:["-i"] [] packages], None
  | Arch -> ["pacman", "-S"::yes ["--noconfirm"] packages], None
  | Centos ->
    (* TODO: check if they all declare "rhel" as primary family *)
    (* When opam-packages specify the epel-release package, usually it
       means that other dependencies require the EPEL repository to be
       already setup when yum-install is called. Cf. opam-depext/#70,#76. *)
    let epel_release = "epel-release" in
    let install_epel rest =
      if List.mem epel_release packages then
        ["yum", "install"::yes ["-y"] [epel_release]] @ rest
      else rest
    in
    install_epel
      ["yum", "install"::yes ["-y"]
                (OpamStd.String.Set.of_list packages
                 |> OpamStd.String.Set.remove epel_release
                 |> OpamStd.String.Set.elements);
       "rpm", "-q"::"--whatprovides"::packages], None
  | Debian -> ["apt-get", "install"::yes ["-qq"; "-yy"] packages], None
  | Freebsd -> ["pkg", "install"::yes ["-y"] packages], None
  | Gentoo -> ["emerge", yes ~no:["-a"] [] packages], None
  | Homebrew ->
    ["brew", "install"::packages], (* NOTE: Does not have any interactive mode *)
    Some (["HOMEBREW_NO_AUTO_UPDATE","yes"])
  | Macports ->
    ["port", "install"::packages], (* NOTE: Does not have any interactive mode *)
    None
  | Netbsd -> ["pkgin", yes ["-y"] ("install" :: packages)], None
  | Openbsd -> ["pkg_add", yes ~no:["-i"] ["-I"] packages], None
  | Suse -> ["zypper", yes ["--non-interactive"] ("install"::packages)], None

let install_packages_commands sys_packages =
  fst (install_packages_commands_t sys_packages)

let sudo_run_command ?vars cmd args =
  let cmd, args =
    let not_root = Unix.getuid () <> 0  in
    match OpamSysPoll.os (), OpamSysPoll.os_distribution () with
    | Some "openbsd", _ when not_root ->
      "doas", cmd::args
    | Some ("linux" | "unix" | "freebsd" | "netbsd" | "dragonfly"), _
    | Some "macos", Some "macports" when not_root ->
      if OpamSystem.resolve_command "sudo" = None then
        "su",
        ["root"; "-c"; Printf.sprintf "%S" (String.concat " " (cmd::args))]
      else
        "sudo", cmd::args
    | _ -> cmd, args
  in
  match run_command_exit_code ?vars ~allow_stdin:true ~verbose:true cmd args with
  | 0 -> ()
  | code ->
    Printf.ksprintf failwith
      "failed with exit code %d at command:\n    %s"
      code (String.concat " " (cmd::args))

let install packages =
  if OpamSysPkg.Set.is_empty packages then
    log "Nothing to install"
  else
    let commands, vars = install_packages_commands_t packages in
    let vars = OpamStd.Option.map (List.map (fun x -> `add, x)) vars in
    List.iter
      (fun (cmd, args) ->
         try sudo_run_command ?vars cmd args
         with Failure msg -> failwith ("System package install " ^ msg))
      commands

let update () =
  let cmd =
    match family () with
    | Alpine -> Some ("apk", ["update"])
    | Arch -> Some ("pacman", ["-Sy"])
    | Centos -> Some ("yum", ["makecache"])
    | Debian -> Some ("apt-get", ["update"])
    | Gentoo -> Some ("emerge", ["--sync"])
    | Homebrew -> Some ("brew", ["update"])
    | Macports -> Some ("port", ["sync"])
    | Suse -> Some ("zypper", ["--non-interactive"; "update"])
    | Freebsd | Netbsd | Openbsd ->
      None
  in
  match cmd with
  | None ->
    OpamConsole.warning
      "Unknown update command for %s, skipping system update"
      OpamStd.Option.Op.(OpamSysPoll.os_family () +! "unknown")
  | Some (cmd, args) ->
    try sudo_run_command cmd args
    with Failure msg -> failwith ("System package update " ^ msg)
