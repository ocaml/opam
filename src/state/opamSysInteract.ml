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

type families =
  | Alpine
  | Arch
  | Centos
  | Debian
  | Freebsd
  | Gentoo
  | Homebrew
  | Macports
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
      | "bsd" when OpamSysPoll.os_distribution () = Some "freebsd" ->
        Freebsd
      | "bsd" when OpamSysPoll.os_distribution () = Some "openbsd" ->
        Openbsd
      | "debian" -> Debian
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
    let str_pkgs =
      OpamSysPkg.(Set.fold (fun p acc -> to_string p :: acc) packages [])
    in
    (* First query regular package *)
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
      run_command ~discard_err:true "dpkg-query" ("-l" :: str_pkgs)
      |> snd
      |> with_regexp_sgl re_pkg
    in
    let sys_available =
      run_query_command "apt-cache"
        ["search"; names_re ~str_pkgs (); "--names-only"]
      |> List.fold_left (fun avail l ->
          match OpamStd.String.cut_at l ' ' with
          | Some (pkg, _) -> pkg +++ avail
          | None ->  avail)
        OpamSysPkg.Set.empty
    in
    compute_sets sys_installed ~sys_available
  (* Disable for time saving
        let installed =
          if OpamSysPkg.Set.is_empty not_found then
            installed
          else
          (* If package are not_found look for virtual package. *)
          let resolve_virtual name =
            let lines =
              run_query_command "apt-cache"
                ["--names-only"; "search"; "^"^name^"$"]
                (* name need to be escaped, its a regexp *)
            in
            List.fold_left
              (fun acc l -> match OpamStd.String.split l ' ' with
                 | pkg :: _ ->
                 pkg +++ acc
                 | [] -> acc)
              OpamSysPkg.Set.empty lines
          in
          let virtual_map =
            OpamSysPkg.Set.fold (fun vpkg acc ->
                OpamSysPkg.Set.fold (fun pkg acc ->
                    let old =
                      try OpamSysPkg.Map.find pkg acc
                      with Not_found -> OpamSysPkg.Set.empty
                    in
                    OpamSysPkg.Map.add pkg
                    (OpamSysPkg.Set.add (OpamSysPkg.of_string vpkg) old) acc)
                  (resolve_virtual vpkg)acc)
              not_found OpamSysPkg.Map.empty
          in
          let real_packages =
            List.map fst (OpamSysPkg.Map.bindings virtual_map)
          in
          let dpkg_args pkgs = if pkgs = [] then [] else "-l" :: pkgs in
          let lines = run_query_command "dpkg-query" (dpkg_args real_packages) in
          List.fold_left
            (fun acc l -> match OpamStd.String.split l ' ' with
               | [pkg;_;_;"installed"] ->
                 (match OpamSysPkg.Map.find_opt pkg virtual_map with
                  | Some p -> p ++ acc
                  | None -> acc)
               | _ -> acc)
            installed lines
        in
  *)
  | Freebsd ->
    let sys_installed =
      run_query_command "pkg" ["query"; "%n"]
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
          let pkg =
            OpamFilename.basename_dir dir
            |> OpamFilename.Base.to_string
          in
          try Re.(Group.get (exec re_pkg pkg) 1) +++ inst
          with Not_found -> inst)
        OpamSysPkg.Set.empty
        (OpamFilename.rec_dirs (OpamFilename.Dir.of_string "/var/db/pkg"))
    in
    compute_sets sys_installed
  | Homebrew ->
    (* accept 'pkgname' and 'pkgname@version'
       exampe output
       >openssl@1.1
       >bmake
    *)
    let sys_installed =
      run_query_command "brew" ["list"]
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
  | Openbsd ->
    let sys_installed =
      run_query_command "pkg_info" ["-mqP"]
      |> List.map OpamSysPkg.of_string
      |> OpamSysPkg.Set.of_list
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
    if OpamCoreConfig.(!r.answer) = Some true then yes @ r else no @ r
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
  | Openbsd -> ["pkg_add", yes ~no:["-i"] ["-I"] packages], None
  | Suse -> ["zypper", ("install"::yes ["--non-interactive"] packages)], None

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
    | Freebsd | Openbsd ->
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
