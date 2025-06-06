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
    OpamStd.Option.default OpamCoreConfig.(!r.verbose_level > 3) verbose
  in
  let env =
    match vars with
    | None -> None
    | Some vars ->
      let env = OpamStd.Env.list () in
      let vars =
        List.map (fun (c, (n, v)) -> c, (OpamStd.Env.Name.of_string n, v)) vars
      in
      let set_vars, kept_vars, env =
        List.fold_left (fun (n,p,e) (op, (name, content as var)) ->
            match OpamStd.(List.assoc_opt Env.Name.equal name env), op with
            | Some c, `add when String.equal c content -> n, p, e
            | Some _, `set ->
              var::n, p, (List.filter (fun (k, _) ->
                  not (OpamStd.Env.Name.equal k name)) env)
            | Some _, _ -> n, var::p, e
            | None, _ -> var::n, p, e
          )
          ([],[], env) vars
      in
      let str_var (v,c) = Printf.sprintf "%s=%s" v c in
      if set_vars = [] then
        ((if kept_vars <> [] then
            log "Won't override %s"
              (OpamStd.List.to_string str_var (kept_vars :> (string * string) list)));
         None)
      else
        (log "Adding to env %s"
           (OpamStd.List.to_string str_var (set_vars :> (string * string) list));
         Some ((set_vars @ env :> (string * string) list)
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

type test_setup = {
  install: bool;
  installed: [ `all | `none | `set of OpamSysPkg.Set.t];
  available: [ `all | `none | `set of OpamSysPkg.Set.t];
}

(* Internal module to get package manager commands defined in global config file *)
module Commands = struct

  let get_cmd_opt config family =
    OpamStd.String.Map.find_opt family
            (OpamFile.Config.sys_pkg_manager_cmd config)

  let cygwin_t = "cygwin"
  let msys2_t = "msys2"

  let msys2 config =
    let override = get_cmd_opt config msys2_t in
    OpamStd.Option.map_default OpamFilename.to_string "pacman.exe" override

  let cygcheck config =
    let override = get_cmd_opt config cygwin_t in
    OpamStd.Option.map_default OpamFilename.to_string "cygcheck.exe" override
end

(* Please keep this alphabetically ordered, in the type definition, and in
   below pattern matching *)
type families =
  | Alpine
  | Altlinux
  | Arch
  | Centos
  | Cygwin
  | Debian
  | Dummy of test_setup
  | Freebsd
  | Gentoo
  | Homebrew
  | Macports
  | Msys2
  | Netbsd
  | Nix
  | Openbsd
  | Suse

(* System status *)
let family ~env () =
  match OpamSysPoll.os_family env with
  | None ->
    Printf.ksprintf failwith
      "External dependency unusable, OS family not detected."
  | Some family when OpamStd.String.starts_with ~prefix:"dummy-" family ->
    let error () =
      OpamConsole.error_and_exit `Bad_arguments
        "Syntax error on dummy depext test family. Syntax is \
         dummy-<success|failure>[:<*|0|pkgslist>:*|0|pkgslist>]"
    in
    let install, installed, available =
      match OpamStd.String.cut_at family ':' with
      | Some (install, packages) ->
        let installed, available =
          match OpamStd.String.cut_at packages ':' with
          | Some (installed, available) ->
            Some installed, Some available
          | None -> error ()
        in
        install, installed, available
      | None -> family, None, None
    in
    let install =
      match install with
      | "dummy-success" -> true
      | "dummy-failure" -> false
      | _ -> error()
    in
    let parse_packages ~default = function
      | Some "" | None -> default
      | Some "*" -> `all
      | Some "0" -> `none
      | Some set ->
        `set (OpamStd.String.split set ','
              |> List.map OpamSysPkg.of_string
              |> OpamSysPkg.Set.of_list)
    in
    let installed = parse_packages ~default:`none installed in
    let available = parse_packages ~default:`all available in
    Dummy { install; installed; available; }
  | Some family ->
    match family with
    | "alpine" | "wolfi" -> Alpine
    | "altlinux" -> Altlinux
    | "amzn" | "centos" | "fedora" | "mageia" | "oraclelinux" | "ol"
    | "rhel" -> Centos
    | "archlinux" | "arch" -> Arch
    | "bsd" ->
      begin match OpamSysPoll.os_distribution env with
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
    | "nixos" -> Nix
    | "macos" ->
      failwith
        "External dependency handling for macOS requires either \
         MacPorts or Homebrew - neither could be found"
    | "suse" | "opensuse" -> Suse
    | "windows" ->
      (match OpamSysPoll.os_distribution env with
       | Some "msys2" -> Msys2
       | Some "cygwin" -> Cygwin
       | _ ->
         failwith
           "External dependency handling not supported for Windows unless \
            MSYS2 or Cygwin is installed. In particular 'os-distribution' \
            must be set to 'msys2' or 'cygwin'.")
    | family ->
      Printf.ksprintf failwith
        "External dependency handling not supported for OS family '%s'."
        family

module Cygwin = struct
  open OpamFilename.Op

  let url_setupexe = OpamUrl.of_string "https://cygwin.com/setup-x86_64.exe"
  let url_setupexe_sha512 = OpamUrl.of_string "https://cygwin.com/sha512.sum"
  let mirror = "https://cygwin.mirror.constant.com/"

  (* Cygwin setup exe must be stored at Cygwin installation root *)
  let setupexe = "setup-x86_64.exe"
  let cygcheckexe = "cygcheck.exe"

  open OpamStd.Option.Op
  let cygbin_opt config =
    Commands.(get_cmd_opt config cygwin_t)
    >>| OpamFilename.dirname
  let msys2bin_opt config =
    Commands.(get_cmd_opt config msys2_t)
    >>| OpamFilename.dirname
  let cygroot_opt config =
    cygbin_opt config
    >>| OpamFilename.dirname_dir
  let cygroot config =
    match cygroot_opt config with
    | Some c -> c
    | None ->
      match OpamSystem.resolve_command "cygcheck.exe" with
      | Some cygcheck ->
        OpamFilename.dirname_dir (OpamFilename.Dir.of_string (Filename.dirname cygcheck))
      | None ->
        failwith "Cygwin install not found"

  let internal_cygwin =
    let internal =
      Lazy.from_fun @@ fun () -> (OpamStateConfig.(!r.root_dir) / ".cygwin")
    in
    fun () -> Lazy.force internal
  let internal_cygroot () = internal_cygwin () / "root"
  let internal_cygcache () = internal_cygwin () / "cache"
  let cygsetup () = internal_cygwin () // setupexe
  let is_internal config =
    OpamStd.Option.equal OpamFilename.Dir.equal
      (cygroot_opt config)
      (Some (internal_cygroot ()))

  let download_setupexe dst =
    let overwrite = true in
    let kind = `SHA512 in
    let dst_exists = OpamFilename.exists dst in
    let current_checksum =
      if dst_exists then
        Some (OpamHash.compute ~kind (OpamFilename.to_string dst))
      else
        None
    in
    let open OpamProcess.Job.Op in
    log "Downloading Cygwin setup checksums";
    if OpamConsole.disp_status_line () then
      if dst_exists then
        OpamConsole.status_line "Checking if Cygwin setup is up-to-date"
      else
        OpamConsole.status_line "Downloading Cygwin setup from cygwin.com";
    OpamFilename.with_tmp_dir_job @@ fun dir ->
    OpamProcess.Job.catch
      (fun exn ->
         let backtrace = Printexc.get_raw_backtrace () in
         if dst_exists then
           OpamConsole.warning "%s failed to update" setupexe
         else
           begin match OpamEmbeddedCygwinSetup.content with
           | None -> Printexc.raise_with_backtrace exn backtrace
           | Some setup_exe_content ->
             OpamConsole.warning "Cygwin setup failed to download - \
                                  using embedded copy instead";
             OpamFilename.write dst setup_exe_content
           end;
         Done ()
      )
    @@ fun () ->
    OpamDownload.download ~overwrite url_setupexe_sha512 dir @@+ fun file ->
    let checksum =
      let content = OpamFilename.read file in
      let re =
        (* File content:
           >SHA512  setup-x86.exe
           >SHA512  setup-x86_64.exe
        *)
        Re.(compile @@ seq [
            group @@ repn
              (alt [ digit ; rg 'A' 'F'; rg 'a' 'f' ]) 128 (Some 128);
            rep space;
            str "setup-x86_64.exe"
          ])
      in
      try Some (OpamHash.sha512 Re.(Group.get (exec re content) 1))
      with Not_found -> None
    in
    if OpamStd.Option.equal OpamHash.equal current_checksum checksum &&
       dst_exists &&
       OpamStd.Option.equal OpamHash.equal current_checksum
         (Some (OpamHash.compute ~kind (OpamFilename.to_string dst))) then begin
      log "Up-to-date";
      OpamConsole.clear_status ();
      Done ()
    end else begin
      log "Downloading setup-x86_64.exe";
      if OpamConsole.disp_status_line () then
        OpamConsole.status_line "Downloading Cygwin setup from cygwin.com";
      OpamDownload.download_as ~overwrite ?checksum url_setupexe dst @@+
        fun () ->
          OpamConsole.clear_status ();
          Done ()
    end

  let set_fstab_noacl =
    let orig = "binary," in
    let re = Re.compile (Re.str orig) in
    fun fstab ->
      let content = OpamFilename.read fstab in
      let content = Re.replace_string re ~by:("noacl,"^orig) content in
      OpamFilename.with_open_out_bin_atomic fstab
        (fun oc -> Stdlib.output_string oc content)

  let install packages =
    let open OpamProcess.Job.Op in
    let cygwin_root = internal_cygroot () in
    let cygwin_bin = cygwin_root / "bin" in
    let fstab = cygwin_root / "etc" // "fstab" in
    let cygcheck = cygwin_bin // cygcheckexe in
    let local_cygwin_setupexe = cygsetup () in
    if OpamFilename.exists cygcheck then
      OpamConsole.warning "Cygwin already installed in root %s"
        (OpamFilename.Dir.to_string cygwin_root)
    else
      (* rjbou: dry run ? there is no dry run on install, from where this
         function is called *)
      (OpamProcess.Job.run @@
       (* download setup.exe *)
       download_setupexe local_cygwin_setupexe @@+ fun () ->
       (* launch install *)
       let args = [
         "--root"; OpamFilename.Dir.to_string cygwin_root;
         "--arch"; "x86_64";
         "--only-site";
         "--site"; mirror;
         "--local-package-dir";
         OpamFilename.Dir.to_string (internal_cygcache ());
         "--no-admin";
         "--no-desktop";
         "--no-replaceonreboot";
         "--no-shortcuts";
         "--no-startmenu";
         "--no-write-registry";
         "--no-version-check";
         "--quiet-mode"; "noinput";
       ] @
         match packages with
         | [] -> []
         | spkgs ->
           [ "--packages";
             OpamStd.List.concat_map "," OpamSysPkg.to_string spkgs ]
       in
       let args =
         if Unix.has_symlink () then
           "--symlink-type" :: "native" :: args
         else
           args
       in
       OpamSystem.make_command
         (OpamFilename.to_string local_cygwin_setupexe)
         args @@> fun r ->
       OpamSystem.raise_on_process_error r;
       set_fstab_noacl fstab;
       Done ())

  let analysis_cache = Hashtbl.create 17

  let analyse_install path =
    let cygbin =
      if not (Sys.file_exists path) then
        Error (path ^ " not found!")
      else if Filename.remove_extension (Filename.basename path)
              = "cygcheck" then
        (* path refers to cygcheck directly *)
        Ok (Filename.dirname path)
      else if not (Sys.is_directory path) then
        Error (Printf.sprintf "%s neither a directory nor cygcheck.exe" path)
      else
      (* path is a directory - search path, path\bin and path\usr\bin *)
      let contains_cygcheck dir =
        Sys.file_exists (Filename.concat dir "cygcheck.exe")
      in
      let tests = [
        path;                     (* e.g. C:\cygwin64\bin / C:\msys64\usr\bin *)
        Filename.concat path "bin";                       (* e.g. C:\cygwin64 *)
        Filename.concat (Filename.concat path "usr") "bin"  (* e.g. C:\msys64 *)
      ] in
      match List.filter contains_cygcheck tests with
      | [] ->
        Error (Printf.sprintf
                 "cygcheck.exe not found in %s, or subdirectories \
                  bin and usr\\bin" path)
      | _::_::_ ->
        Error (Printf.sprintf
                 "cygcheck.exe found in multiple places in %s which suggests \
                  it is not a Cygwin/MSYS2 installation" path)
      | [path] ->
        Ok path
    in
    let identify dir =
      try Hashtbl.find analysis_cache dir
      with Not_found ->
        let result =
          let cygpath = Filename.concat dir "cygpath.exe" in
          if not (Sys.file_exists cygpath) then
            Error (Printf.sprintf
                     "cygcheck.exe found in %s, but cygpath.exe was not" dir)
          else
          match OpamStd.Sys.get_windows_executable_variant
                  ~search_in_first:dir cygpath with
          | `Native | `Tainted _ ->
            Error (Printf.sprintf
                     "cygcheck.exe found in %s; but it does not appear \
                      to be part of a Cygwin or MSYS2 installation" dir)
          | (`Msys2 | `Cygwin) as kind ->
            (* Check that pacman.exe is present with MSYS2: it is typically
               not present with a Git-for-Windows Git Bash session, and as
               these are basically unusable (they don't have all the required
               tools, and we have no package manager with which to add them),
               it's better to exclude them). *)
            if kind = `Msys2
            && not (Sys.file_exists (Filename.concat dir "pacman.exe")) then
              Error (Printf.sprintf
                       "cygcheck.exe found in %s, which appears to be from \
                        an MSYS2 installation, but pacman.exe was not" dir)
            else
            let r =
              OpamProcess.run
                (OpamProcess.command ~name:(OpamSystem.temp_file "command")
                   ~allow_stdin:false cygpath ["-w"; "--"; "/"])
            in
            OpamProcess.cleanup ~force:true r;
            if OpamProcess.is_success r then
              match r.OpamProcess.r_stdout with
              | [] ->
                Error ("Unexpected error translating \"/\" with " ^ cygpath)
              | l::_ ->
                Ok (kind, OpamFilename.Dir.of_string l)
            else
              Error ("Could not determine the root for " ^ cygpath)
        in
        Hashtbl.add analysis_cache dir result;
        result
    in
    Result.bind cygbin identify

  let bindir_for_root kind root =
    let open OpamFilename.Op in
    match kind with
    | `Msys2 -> root / "usr" / "bin"
    | `Cygwin -> root / "bin"

  (* Set setup.exe in the good place, ie in .opam/.cygwin/ *)
  let check_setup ~update =
    let dst = cygsetup () in
    if update || not (OpamFilename.exists dst) then
      OpamProcess.Job.run @@ download_setupexe dst
end

let yum_cmd = lazy begin
  if OpamSystem.resolve_command "yum" <> None then
    "yum"
  else if OpamSystem.resolve_command "dnf" <> None then
    "dnf"
  else
    raise (OpamSystem.Command_not_found "yum or dnf")
end

let packages_status ?(env=OpamVariable.Map.empty) config packages =
  let (+++) pkg set = OpamSysPkg.Set.add (OpamSysPkg.of_string pkg) set in
  (* Some package managers don't permit to request on available packages. In
     this case, we consider all non installed packages as [available]. *)
  let open OpamSysPkg.Set.Op in
  let compute_sets ?sys_available sys_installed =
    let installed = packages %% sys_installed in
    match sys_available with
    | Some sys_available ->
      let s_available = (packages -- installed) %% sys_available in
      let s_not_found = packages -- installed -- s_available in
      { OpamSysPkg.s_available; s_not_found }
    | None ->
      let s_available = packages -- installed in
      { OpamSysPkg.status_empty with s_available }
  in
  let to_string_list pkgs =
    OpamSysPkg.(Set.fold (fun p acc -> to_string p :: acc) pkgs [])
  in
  let names_re ?str_pkgs () =
    let str_pkgs =
      OpamStd.Option.default (to_string_list packages) str_pkgs
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
  let package_set_of_pkgpath l =
    List.fold_left (fun set pkg ->
        let short_name =
          match String.rindex pkg '/' with
          | exception Not_found -> pkg
          | idx -> String.sub pkg (idx+1) (String.length pkg - idx - 1)
        in
        let no_flavor =
          match String.index short_name ',' with
          | exception Not_found -> short_name
          | idx -> String.sub short_name 0 idx
        in
        set
        |> OpamSysPkg.Set.add (OpamSysPkg.of_string pkg)
        |> OpamSysPkg.Set.add (OpamSysPkg.of_string short_name)
        |> OpamSysPkg.Set.add (OpamSysPkg.of_string no_flavor)
      ) OpamSysPkg.Set.empty l
  in
  let compute_sets_with_virtual get_avail_w_virtuals get_installed  =
    let sys_available, sys_provides = get_avail_w_virtuals () in
    let need_inst_check =
      OpamSysPkg.Map.fold (fun cp vps set ->
          if OpamSysPkg.Set.(is_empty (inter vps packages)) then set else
            OpamSysPkg.Set.add cp set)
        sys_provides packages
    in
    let str_need_inst_check = to_string_list need_inst_check in
    let sys_installed = get_installed str_need_inst_check in
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
  in
  let compute_sets_for_arch ~pacman =
    let get_avail_w_virtuals () =
      let package_provided str =
        OpamSysPkg.of_string
          (match OpamStd.String.cut_at str '=' with
           | None -> str
           | Some (p, _vc) -> p)
      in
      (* Output format:
         >Repository      : core
         >Name            : python
         >Version         : 3.9.6-1
         >Description     : Next generation of the python high-level scripting language
         >Architecture    : x86_64
         >URL             : https://www.python.org/
         >Licenses        : custom
         >Groups          : None
         >Provides        : python3
         >Depends On      : bzip2  expat  gdbm  libffi  libnsl  libxcrypt  openssl
         >Optional Deps   : python-setuptools
         >                  python-pip
         >[...]

         Format partially described in https://archlinux.org/pacman/PKGBUILD.5.html
      *)
      (* Discard stderr to not have it pollute output. Plus, exit code is the
         number of packages not found. *)
      run_command ~discard_err:true pacman ["-Si"]
      |> snd
      |> List.fold_left (fun ((avail, provides, latest) as acc) l ->
          if OpamStd.String.starts_with ~prefix:"Name" l then
            match OpamStd.String.split l ' ' with
            | "Name"::":"::p::_ ->
              p +++ avail, provides, Some (OpamSysPkg.of_string p)
            | _ -> acc
          else if OpamStd.String.starts_with ~prefix:"Provides" l then
            match OpamStd.String.split l ' ' with
            | "Provides"::":"::"None"::[] -> acc
            | "Provides"::":"::pkgs ->
              let ps = OpamSysPkg.Set.of_list (List.map package_provided pkgs) in
              let provides =
                match latest with
                | Some p -> OpamSysPkg.Map.add p ps provides
                | None -> provides (* Bad pacman output ?? *)
              in
              ps ++ avail, provides, None
            | _ -> acc
          else
            acc)
        (OpamSysPkg.Set.empty, OpamSysPkg.Map.empty, None)
      |> (fun (a,p,_) -> a,p)
    in
    let get_installed str_pkgs =
      (* output:
         >extra/cmake 3.17.1-1 [installed]
         >    A cross-platform open-source make system
         >extra/cmark 0.29.0-1
         >    CommonMark parsing and rendering library and program in C
      *)
      let re_pkg =
        Re.(compile @@ seq
              [ bol;
                rep1 @@ alt [alnum; punct];
                char '/';
                group @@ rep1 @@ alt [alnum; punct];
                space;
              ])
      in
      run_query_command pacman ["-Qs" ; names_re ~str_pkgs ()]
      |> with_regexp_sgl re_pkg
    in
    compute_sets_with_virtual get_avail_w_virtuals get_installed
  in
  match family ~env () with
  | Alpine ->
    (* Output format
       >capnproto policy:
       >  0.8.0-r1:
       >    lib/apk/db/installed
       >    @edgecommunity https://dl-cdn.alpinelinux.org/alpine/edge/community
       >at policy:
       >  3.2.1-r1:
       >    https://dl-cdn.alpinelinux.org/alpine/v3.13/community
       >vim policy:
       >  8.2.2320-r0:
       >    lib/apk/db/installed
       >    https://dl-cdn.alpinelinux.org/alpine/v3.13/main
       >  8.2.2852-r0:
       >    @edge https://dl-cdn.alpinelinux.org/alpine/edge/main
       >hwids-udev policy:
       >  20201207-r0:
       >    https://dl-cdn.alpinelinux.org/alpine/v3.13/main
       >    @edge https://dl-cdn.alpinelinux.org/alpine/v3.13/main
       >    https://dl-cdn.alpinelinux.org/alpine/edge/main
       >    @edge https://dl-cdn.alpinelinux.org/alpine/edge/main
    *)
    let sys_installed, sys_available =
      let pkg_name =
        Re.(compile @@ seq
              [ bol;
                group @@ rep1 @@ alt [ alnum; punct ];
                space;
                str "policy:";
                eol
              ])
      in
      let repo_name =
        Re.(compile @@ seq
              [ bol;
                repn space 4 (Some 4);
                char '@';
                group @@ rep1 @@ alt [ alnum; punct ];
                space
              ])
      in
      let add_pkg pkg repo installed (inst,avail) =
        let pkg = match repo with Some r -> pkg^"@"^r | None -> pkg in
        if installed then pkg +++ inst, avail else inst, pkg +++ avail
      in
      to_string_list packages
      |> List.map (fun s ->
          match OpamStd.String.cut_at s '@' with
          | Some (pkg, _repo) -> pkg
          | None -> s)
      |> (fun l -> run_query_command "apk" ("policy"::l))
      |> List.fold_left (fun (pkg, installed, instavail) l ->
          try (* package name *)
            Re.(Group.get (exec pkg_name l) 1), false, instavail
          with Not_found ->
            if l.[2] <> ' ' then (* only version field is after two spaces *)
              pkg, false, instavail
            else if l = "    lib/apk/db/installed" then
              (* from https://git.alpinelinux.org/apk-tools/tree/src/database.c#n58 *)
              pkg, true, instavail
            else (* repo (tagged and non-tagged) *)
            let repo =
              try Some Re.(Group.get (exec repo_name l) 1)
              with Not_found -> None
            in
            pkg, installed, add_pkg pkg repo installed instavail)
        ("", false, OpamSysPkg.Set.(empty, empty))
      |> (fun (_,_, instavail) -> instavail)
    in
    compute_sets sys_installed ~sys_available
  | Arch ->
    compute_sets_for_arch ~pacman:"pacman"
  | Centos | Altlinux | Suse ->
    (* Output format:
       >crypto-policies
       >python3-pip-wheel
    *)
    let sys_installed =
      run_query_command "rpm" ["-qa"; "--qf"; "%{NAME}\\n"]
      |> List.map OpamSysPkg.of_string
      |> OpamSysPkg.Set.of_list
    in
    compute_sets sys_installed
  | Cygwin ->
    (* Output format:
       >Cygwin Package Information
       >Package         Version
       >git             2.35.1-1
       >binutils        2.37-2
    *)
    let sys_installed =
      run_query_command (Commands.cygcheck config)
      ([ "-c"; "-d" ] @ to_string_list packages)
      |> (function | _::_::l -> l | _ -> [])
      |> OpamStd.List.filter_map (fun l ->
          match OpamStd.String.split l ' ' with
          | pkg::_ -> Some pkg
          | _ -> None)
      |> List.map OpamSysPkg.of_string
      |> OpamSysPkg.Set.of_list
    in
    compute_sets sys_installed
  | Debian ->
    let get_avail_w_virtuals () =
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
      |> List.fold_left (fun ((avail, provides, latest) as acc) l ->
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
          else acc)
        (OpamSysPkg.Set.empty, OpamSysPkg.Map.empty, None)
      |> (fun (a,p,_) -> a,p)
    in
    let get_installed str_pkgs =
      (* ouput:
         >ii  uim-gtk3                 1:1.8.8-6.1  amd64    Universal ...
         >ri  uim-gtk3-immodule:amd64  1:1.8.8-6.1  amd64    Universal ...

         First column is <desired action><package status>
         * Desired action:
           u = Unknown           h = Hold             p = Purge
           i = Install           r = Remove
         * Package status:
           n = Not-installed    U = Unpacked          t = Triggers-pending
           c = Config-files     F = Half-configured   i = Installed
           H = Half-installed   W = Triggers-awaiting

         We focus on the second element of the column
      *)
      let re_pkg =
        Re.(compile @@ seq
              [ bol;
                alpha;
                char 'i';
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
    compute_sets_with_virtual get_avail_w_virtuals get_installed
  | Dummy test ->
    let sys_installed =
      match test.installed with
      | `all -> packages
      | `none -> OpamSysPkg.Set.empty
      | `set pkgs -> pkgs %% packages
    in
    let sys_available =
      match test.available with
      | `all -> packages
      | `none -> OpamSysPkg.Set.empty
      | `set pkgs -> pkgs %% packages
    in
    compute_sets ~sys_available sys_installed
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
       >koekeishiya/formulae/skhd
    *)
    let sys_installed =
      run_query_command "brew" ["list"; "--full-name"]
      |> List.fold_left (fun res s ->
          List.fold_left (fun res spkg ->
              let parse_fullname pkg =
                match OpamStd.String.rcut_at pkg '/' with
                | None -> [pkg]
                | Some (_, simple_name) -> [pkg; simple_name]
              in
              match OpamStd.String.cut_at spkg '@' with
              | Some (n,_v) -> parse_fullname n@parse_fullname spkg@res
              | None -> parse_fullname spkg@res)
            res (OpamStd.String.split s ' ')) []
      |> List.map OpamSysPkg.of_string
      |> OpamSysPkg.Set.of_list
    in
    compute_sets sys_installed
  | Macports ->
    let variants_map, packages =
      OpamSysPkg.(Set.fold (fun spkg (map, set) ->
          match OpamStd.String.cut_at (to_string spkg) ' ' with
          | Some (pkg, variant) ->
            OpamStd.String.Map.add pkg variant map,
            pkg +++ set
          | None -> map, Set.add spkg set)
          packages (OpamStd.String.Map.empty, Set.empty))
    in
    let str_pkgs = to_string_list packages in
    let sys_installed =
      (* output:
         >  zlib @1.2.11_0 (active)
         >  gtk3 @3.24.21_0+quartz (active)
      *)
      let re_pkg =
        Re.(compile @@ seq
              [ bol;
                rep space;
                group @@ rep1 @@ alt [alnum; punct];
                rep1 space;
                char '@';
                rep1 @@ diff any (char '+');
                opt @@ group @@ rep1 @@ alt [alnum; punct];
                rep1 space;
                str "(active)";
                eol
              ])
      in
      run_query_command "port" ("installed" :: str_pkgs)
      |> (function _::lines -> lines | _ -> [])
      |> List.fold_left (fun pkgs l ->
          try
            let pkg = Re.(Group.get (exec re_pkg l) 1) in
            (* variant handling *)
            match OpamStd.String.Map.find_opt pkg variants_map with
            | Some variant ->
              (try
                 if Re.(Group.get (exec re_pkg l) 2) = variant then
                   (pkg ^ " " ^ variant) +++ pkgs
                 else pkgs
               with Not_found -> pkgs)
            | None -> pkg +++ pkgs
          with Not_found -> pkgs)
        OpamSysPkg.Set.empty
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
      let avail =
        run_query_command "port"
          [ "search"; "--line"; "--regex"; names_re ~str_pkgs () ]
        |> with_regexp_sgl re_pkg
      in
      (* variants handling *)
      let variants =
        OpamStd.String.Map.filter
          (fun p _ -> OpamSysPkg.Set.mem (OpamSysPkg.of_string p) avail)
          variants_map
        |> OpamStd.String.Map.keys
      in
      run_query_command "port" ([ "info"; "--name"; "--variants" ] @ variants)
      |> List.fold_left (fun (prec, avail) l ->
          match prec, OpamStd.String.split l ' ' with
          | _, "name:"::pkg::[] -> Some pkg, avail
          | Some pkg, "variants:"::variants ->
            None,
            List.fold_left (fun avail v ->
                (pkg ^ " +" ^ (OpamStd.String.remove_suffix ~suffix:"," v))
                +++ avail) avail variants
          | _ -> None, avail
        ) (None, avail)
      |> snd
    in
    compute_sets sys_installed ~sys_available
  | Msys2 ->
    compute_sets_for_arch ~pacman:(Commands.msys2 config)
  | Netbsd ->
    let sys_installed =
      run_query_command "pkg_info" ["-Q"; "PKGPATH"; "-a"]
      |> package_set_of_pkgpath
    in
    compute_sets sys_installed
  | Nix ->
      (* We say all requested packages are available but uninstalled.
         We could check that these packages are available in Nixpkgs,
         but that would involve an expensive Nixpkgs evaluation.
         Saying no packages are installed results in a warning that
         conf packages depend on a 'system package that can no longer
         be found.' But omitting them will mean that they won't be
         added to the Nix derivation.
      *)
      let s_available = packages in
      let s_not_found = OpamSysPkg.Set.empty in
      { OpamSysPkg.s_available; s_not_found }
  | Openbsd ->
    let sys_installed =
      run_query_command "pkg_info" ["-qP"]
      |> package_set_of_pkgpath
    in
    compute_sets sys_installed

let stateless_install ?(env=OpamVariable.Map.empty) () =
  match family ~env () with
  | exception Failure _ -> true (* no depexts *)
  | Nix -> true
  | Alpine | Altlinux | Arch | Centos | Cygwin | Debian | Dummy _
  | Freebsd | Gentoo | Homebrew | Macports | Msys2 | Netbsd
  | Openbsd | Suse -> false

(* Install *)

let package_manager_name_t ?(env=OpamVariable.Map.empty) config =
  match family ~env () with
  | Alpine -> `AsAdmin "apk"
  | Altlinux -> `AsAdmin "apt-get"
  | Arch -> `AsAdmin "pacman"
  | Centos -> `AsAdmin (Lazy.force yum_cmd)
  | Cygwin -> `AsUser (OpamFilename.to_string (Cygwin.cygsetup ()))
  | Debian -> `AsAdmin "apt-get"
  | Dummy test ->
    if test.install then
      `AsUser "echo"
    else
      `AsUser "false"
  | Freebsd -> `AsAdmin "pkg"
  | Gentoo -> `AsAdmin "emerge"
  | Homebrew -> `AsUser "brew"
  | Macports -> `AsAdmin "port"
  | Msys2 -> `AsUser (Commands.msys2 config)
  | Netbsd -> `AsAdmin "pkgin"
  | Nix -> `AsUser "nix-build"
  | Openbsd -> `AsAdmin "pkg_add"
  | Suse -> `AsAdmin "zypper"

(* Perform some action for Nix and Cygwin *)
let install_packages_commands_t ?(env=OpamVariable.Map.empty) ~to_show st
    config sys_packages =
  let unsafe_yes = OpamCoreConfig.answer_is `unsafe_yes in
  let yes ?(no=[]) yes r =
    if unsafe_yes then
      yes @ r else no @ r
  in
  let packages =
    List.map OpamSysPkg.to_string
      (OpamSysPkg.Set.elements sys_packages.OpamSysPkg.ti_new)
  in
  let pm = package_manager_name_t ~env config in
  match family ~env () with
  | Alpine -> [pm, "add"::yes ~no:["-i"] [] packages], None
  | Altlinux ->
    [pm, "install"::yes ["-qq"; "-yy"] packages], None
  | Arch -> [pm, "-Su"::yes ["--noconfirm"] packages], None
  | Centos ->
    (* TODO: check if they all declare "rhel" as primary family *)
    (* Kate's answer: no they don't :( (e.g. Fedora, Oraclelinux define Nothing and "fedora" respectively)  *)
    (* When opam-packages specify the epel-release package, usually it
       means that other dependencies require the EPEL repository to be
       already setup when yum-install is called. Cf. opam-depext/#70,#76. *)
    let epel_release = "epel-release" in
    let install_epel rest =
      if List.mem epel_release packages then
        [pm, "install"::yes ["-y"] [epel_release]] @ rest
      else rest
    in
    install_epel
      [pm, "install"::yes ["-y"]
             (OpamStd.String.Set.of_list packages
              |> OpamStd.String.Set.remove epel_release
              |> OpamStd.String.Set.elements);
       `AsUser "rpm", "-q"::"--whatprovides"::packages], None
  | Cygwin ->
    (* We use setup_x86_64 to install package instead of `cygcheck` that is
       stored in `sys-pkg-manager-cmd` field *)
    Cygwin.check_setup ~update:(not to_show);
    let is_internal = Cygwin.is_internal config in
    [`AsUser (OpamFilename.to_string (Cygwin.cygsetup ())),
     [ "--root"; (OpamFilename.Dir.to_string (Cygwin.cygroot config));
       "--quiet-mode"; (if is_internal then "noinput" else "unattended");
       "--no-shortcuts";
       "--no-startmenu";
       "--no-desktop";
       "--no-admin";
       "--no-version-check";
       "--no-write-registry";
       "--packages";
       String.concat "," packages;
     ] @ (if is_internal then
            let common =
              [ "--upgrade-also";
                "--only-site";
                "--site"; Cygwin.mirror;
                "--local-package-dir";
                OpamFilename.Dir.to_string (Cygwin.internal_cygcache ());
              ]
            in
            if Unix.has_symlink () then
              "--symlink-type" :: "native" :: common
            else
              common
          else [])
    ],
    None
  | Debian ->
    [pm, "install"::yes ["-qq"; "-yy"] packages],
    (if unsafe_yes then Some ["DEBIAN_FRONTEND", "noninteractive"] else None)
  | Dummy test ->
    if test.install then
      [pm, packages], None
    else
      [pm, []], None
  | Freebsd -> [pm, "install"::yes ["-y"] packages], None
  | Gentoo -> [pm, yes ~no:["-a"] [] packages], None
  | Homebrew ->
    [pm, "install"::packages], (* NOTE: Does not have any interactive mode *)
    Some (["HOMEBREW_NO_AUTO_UPDATE","yes"])
  | Macports ->
    let packages = (* Separate variants from their packages *)
      List.map (fun p -> OpamStd.String.split p ' ')  packages
      |> List.flatten
    in
    [pm, yes ["-N"] ("install"::packages)],
    None
  | Msys2 ->
    (* NOTE: MSYS2 interactive mode may break (not show output until key pressed)
       when called from opam. Confer
       https://www.msys2.org/wiki/Terminals/#mixing-msys2-and-windows. *)
    [`AsUser (Commands.msys2 config),
     "-Su"::"--noconfirm"::packages], None
  | Netbsd -> [pm, yes ["-y"] ("install" :: packages)], None
  | Nix ->
    (match st with
     | None ->
       log "Nix depext must be passed switch";
       [], None
     | Some (st : _ OpamStateTypes.switch_state) ->
       let dir = OpamPath.Switch.meta st.switch_global.root st.switch in
       let drvFile =
         OpamFilename.create dir
           (OpamFilename.Base.of_string "env.nix")
       in
       let packages =
         String.concat " "
           (OpamSysPkg.Set.fold (fun p l -> OpamSysPkg.to_string p :: l)
              OpamSysPkg.Set.Op.(sys_packages.ti_new ++ sys_packages.ti_required) [])
       in
       (* We exclude variables from
            https://github.com/NixOS/nix/blob/e4bda20918ad2af690c2e938211a7d362548e403/src/nix/develop.cc#L308-L325
          append to variables from
            https://github.com/NixOS/nix/blob/e4bda20918ad2af690c2e938211a7d362548e403/src/nix/develop.cc#L347-L353
          and exclude some other regarding the Nix derivation *)
       let contents =
         {|{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "opam-nix-env";
  nativeBuildInputs = with buildPackages; [ |} ^ packages ^ {| ];

  phases = [ "buildPhase" ];

  buildPhase = ''
while IFS='=' read -r var value; do
  escaped="''$(echo "$value" | sed -e 's/^$/@/' -e 's/ /\\ /g')"
  echo "$var	=	$escaped	Nix" >> "$out"
done < <(env \
  -u BASHOPTS \
  -u HOME \
  -u NIX_BUILD_TOP \
  -u NIX_ENFORCE_PURITY \
  -u NIX_LOG_FD \
  -u NIX_REMOTE \
  -u PPID \
  -u SHELLOPTS \
  -u SSL_CERT_FILE \
  -u TEMP \
  -u TEMPDIR \
  -u TERM \
  -u TMP \
  -u TMPDIR \
  -u TZ \
  -u UID \
  -u PATH \
  -u XDG_DATA_DIRS \
  -u self-referential \
  -u excluded_vars \
  -u excluded_pattern \
  -u phases \
  -u buildPhase \
  -u outputs)

echo "PATH	+=	$PATH	Nix" >> "$out"
echo "XDG_DATA_DIRS	+=	$XDG_DATA_DIRS	Nix" >> "$out"
  '';

  preferLocalBuild = true;
}
|} in
       OpamFilename.write drvFile contents;
       let envFile = OpamPath.Switch.nix_env st.switch_global.root st.switch in
       [pm,
        [ OpamFilename.to_string drvFile;
          "--out-link"; OpamFile.to_string envFile ] ],
       None)
  | Openbsd -> [pm, yes ~no:["-i"] ["-I"] packages], None
  | Suse -> [pm, yes ["--non-interactive"] ("install"::packages)], None

let install_packages_commands ?env st config sys_packages =
  fst (install_packages_commands_t ?env ~to_show:true st config sys_packages)

let package_manager_name ?env config =
  match package_manager_name_t ?env config with
  | `AsAdmin pkgman | `AsUser pkgman -> pkgman

let sudo_run_command ?(env=OpamVariable.Map.empty) ?vars cmd args =
  let cmd, args =
    let not_root = Unix.getuid () <> 0  in
    match cmd, OpamSysPoll.os env with
    | `AsAdmin cmd, Some "openbsd" when not_root -> (* TODO: alpine is also switching to doas in 3.16 *)
      "doas", cmd::args
    | `AsAdmin cmd, Some ("linux" | "unix" | "freebsd" | "netbsd" | "dragonfly" | "macos") when not_root ->
      if OpamSystem.resolve_command "sudo" = None then
        "su",
        ["root"; "-c"; Printf.sprintf "%S" (String.concat " " (cmd::args))]
      else
        "sudo", cmd::args
    | (`AsUser cmd | `AsAdmin cmd), _ -> cmd, args
  in
  match run_command_exit_code ?vars ~allow_stdin:true ~verbose:true cmd args with
  | 0 -> ()
  | code ->
    Printf.ksprintf failwith
      "failed with exit code %d at command:\n    %s"
      code (String.concat " " (cmd::args))

let install ?env st config (packages : OpamSysPkg.to_install) =
  if OpamSysPkg.Set.is_empty packages.ti_new
  && OpamSysPkg.Set.is_empty packages.ti_required then
    log "Nothing to install"
  else
    let commands, vars =
      install_packages_commands_t ?env ~to_show:false st config packages
    in
    let vars = OpamStd.Option.map (List.map (fun x -> `add, x)) vars in
    List.iter
      (fun (cmd, args) ->
         try sudo_run_command ?env ?vars cmd args
         with Failure msg -> failwith ("System package install " ^ msg))
      commands

let update ?(env=OpamVariable.Map.empty) config =
  let family = family ~env () in
  let cmd =
    match family with
    | Alpine -> Some (`AsAdmin "apk", ["update"])
    | Arch -> Some (`AsAdmin "pacman", ["-Sy"])
    | Centos -> Some (`AsAdmin (Lazy.force yum_cmd), ["makecache"])
    | Cygwin -> None
    | Debian | Altlinux -> Some (`AsAdmin "apt-get", ["update"])
    | Dummy test ->
      if test.install then None else Some (`AsUser "false", [])
    | Freebsd -> None
    | Gentoo -> Some (`AsAdmin "emerge", ["--sync"])
    | Homebrew -> Some (`AsUser "brew", ["update"])
    | Macports -> Some (`AsAdmin "port", ["sync"])
    | Msys2 -> Some (`AsUser (Commands.msys2 config), ["-Sy"])
    | Netbsd -> None
    | Nix -> Some (`AsUser "nix-channel", ["--update"])
    | Openbsd -> None
    | Suse -> Some (`AsAdmin "zypper", ["--non-interactive"; "refresh"])
  in
  match cmd with
  | None ->
    (* Cygwin doesn't have an update database per se, but one is supposed to use
       the most current setup program when downloading setup.ini (which is the
       package database (cf. the --no-version-check option).
       Also, when #5839 is addressed, we'll need to cache setup.ini, and that
       will want to be updated here too. *)
    if family = Cygwin then
      Cygwin.check_setup ~update:true
    else
      OpamConsole.warning
        "Unknown update command for %s, skipping system update"
        OpamStd.Option.Op.(OpamSysPoll.os_family env +! "unknown")
  | Some (cmd, args) ->
    try sudo_run_command ~env cmd args
    with Failure msg -> failwith ("System package update " ^ msg)

let repo_enablers ?(env=OpamVariable.Map.empty) config =
  if family ~env () <> Centos then None else
  let status =
    packages_status ~env config
      (OpamSysPkg.raw_set
         (OpamStd.String.Set.singleton "epel-release"))
  in
  if OpamSysPkg.Set.is_empty status.s_available then None
  else
    Some
      "On CentOS/RHEL, many packages may assume that the Extra Packages for \
       Enterprise Linux (EPEL) repository has been enabled. \
       This is typically done by installing the 'epel-release' package. \
       Please see https://fedoraproject.org/wiki/EPEL for more information"
