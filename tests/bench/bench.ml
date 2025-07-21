let fmt = Printf.sprintf

let time_cmd ~exit cmd =
  let cmd = cmd ^ " 2> /dev/null > /dev/null" in
  let before = Unix.gettimeofday () in
  let code = Sys.command cmd in
  let timer = Unix.gettimeofday () -. before in
  if Int.equal code exit then
    timer
  else
    failwith (fmt "Command %S exited with error code %d" cmd code)

let () =
  let bin = "./opam" in
  let bin_size = (Unix.stat bin).st_size in
  let launch cmd  =
    let code = Sys.command cmd in
    if not (Int.equal code 0) then
      failwith (fmt "Preliminary command %S exited with error code %d" cmd code)
  in
  let time_misspelled_cmd =
    (* NOTE: https://github.com/ocaml/opam/issues/5479 *)
    Gc.compact ();
    time_cmd ~exit:2 (fmt "%s sitch" bin)
  in
  let time_install_cmd =
    (* NOTE: https://github.com/ocaml/opam/issues/5502 *)
    Gc.compact ();
    launch "opam switch create one --empty";
    time_cmd ~exit:0 (fmt "%s install magic-trace -y --fake --sw one" bin)
  in
  let time_install_cmd_w_invariant =
    (* NOTE: https://github.com/ocaml/opam/issues/5502 *)
    Gc.compact ();
    launch "opam switch create two --empty";
    launch "opam switch set-invariant core -n --sw two";
    time_cmd ~exit:0 (fmt "%s install magic-trace -y --fake --sw two" bin)
  in
  let time_OpamSystem_read_100 =
    (* NOTE: https://github.com/ocaml/opam/pull/5896 *)
    let files =
      let ic = Stdlib.open_in_bin "/home/opam/all-opam-files" in
      let rec loop files =
        match Stdlib.input_line ic with
        | file -> loop (file :: files)
        | exception End_of_file -> files
      in
      loop []
    in
    let n = 100 in
    let l = List.init n (fun _ ->
        Gc.compact ();
        let before = Unix.gettimeofday () in
        List.iter (fun file -> ignore (OpamSystem.read file)) files;
        Unix.gettimeofday () -. before)
    in
    List.fold_left (+.) 0.0 l /. float_of_int n
  in
  let time_deps_only_installed_pkg =
    (* NOTE: https://github.com/ocaml/opam/pull/5908 *)
    Gc.compact ();
    launch (fmt "%s switch create three --fake ocaml-base-compiler.4.14.0" bin);
    launch (fmt "%s install -y --fake core.v0.15.0" bin);
    time_cmd ~exit:0 (fmt "%s install --show --deps-only core.v0.15.0" bin)
  in
  let time_OpamPackage_Version_compare_100 =
    (* NOTE: https://github.com/ocaml/opam/pull/5518 *)
    let ic = Stdlib.open_in_bin "/home/opam/all-packages" in
    let pkgs =
      let rec loop pkgs =
        match Stdlib.input_line ic with
        | pkg -> loop (OpamPackage.version (OpamPackage.of_string pkg) :: pkgs)
        | exception End_of_file -> pkgs
      in
      loop []
    in
    let n = 100 in
    let l = List.init n (fun _ ->
        Gc.compact ();
        let before = Unix.gettimeofday () in
        let _ = List.stable_sort OpamPackage.Version.compare pkgs in
        Unix.gettimeofday () -. before)
    in
    List.fold_left (+.) 0.0 l /. float_of_int n
  in
  let time_install_check_installed =
    Gc.compact ();
    launch (fmt "%s switch create four --fake ocaml-base-compiler.4.14.0" bin);
    launch (fmt "%s install -y --fake core.v0.15.0" bin);
    time_cmd ~exit:0 (fmt "%s install --check core.v0.15.0" bin)
  in
  let time_install_check_not_installed =
    Gc.compact ();
    launch (fmt "%s switch create five --fake ocaml-base-compiler.4.14.0" bin);
    time_cmd ~exit:1 (fmt "%s install --check core.v0.15.0" bin)
  in
  let time_list_installed_noninstalled_packages =
    Gc.compact ();
    launch (fmt "%s switch create six --empty" bin);
    time_cmd ~exit:0 (fmt "%s list --installed --short --safe --color=never ocp-indent ocp-index merlin" bin)
  in
  launch (fmt "%s switch create seven --empty" bin);
  launch (fmt "%s install -y --fake dune" bin);
  let time_show_installed =
    Gc.compact ();
    time_cmd ~exit:0 (fmt "%s show dune" bin)
  in
  let time_show_with_depexts =
    Gc.compact ();
    time_cmd ~exit:0 (fmt "%s show conf-llvm" bin)
  in
  let time_show_raw =
    Gc.compact ();
    time_cmd ~exit:0 (fmt "%s show --raw conf-llvm" bin)
  in
  let time_show_precise =
    Gc.compact ();
    time_cmd ~exit:0 (fmt "%s show --raw conf-llvm.14.0.6" bin)
  in
  let time_OpamStd_String_split_10 =
    let lines =
      let ic = Stdlib.open_in_bin "/home/opam/all-opam-content" in
      let rec loop files =
        match Stdlib.input_line ic with
        | file -> loop (file :: files)
        | exception End_of_file -> files
      in
      loop []
    in
    let n = 10 in
    let l = List.init n (fun _ ->
        Gc.compact ();
        let before = Unix.gettimeofday () in
        List.iter (fun line -> ignore (OpamStd.String.split line ' ')) lines;
        Unix.gettimeofday () -. before)
    in
    List.fold_left (+.) 0.0 l /. float_of_int n
  in
  let time_update_local_plain =
    (* This should be the latest test *)
    launch (fmt "%s repo set-url default /rep/opam-repository-after-phase3" bin);
    launch "rm -rf ~/.opam/repo/default && cp -r /rep/opam-repository-before-phase1 ~/.opam/repo/default";
    time_cmd ~exit:0 (fmt "opam update")
  in
  let json = fmt {|{
  "results": [
    {
      "name": "Timings",
      "metrics": [
        {
          "name": "Misspelled command",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "Fake install with no invariant",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "Fake install with invariant",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "OpamSystem.read amortised over 100 runs",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "Deps-only install of an already installed package",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "OpamPackage.Version.compare amortised over 100 runs",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam install --check on an already installed package",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam install --check on a non-installed package",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam list --installed on non-installed packages",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam show of an installed package",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam show with depexts",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam show --raw pkgname",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam show --raw pkgname.version",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "OpamStd.String.split amortised over 10 runs",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "opam update local plain repository",
          "value": %f,
          "units": "secs"
        }
      ]
    },
    {
      "name": "Misc",
      "metrics": [
        {
          "name": "Size of opam.exe",
          "value": %d,
          "units": "bytes"
        }
      ]
    }
  ]
}|}
      time_misspelled_cmd
      time_install_cmd
      time_install_cmd_w_invariant
      time_OpamSystem_read_100
      time_deps_only_installed_pkg
      time_OpamPackage_Version_compare_100
      time_install_check_installed
      time_install_check_not_installed
      time_list_installed_noninstalled_packages
      time_show_installed
      time_show_with_depexts
      time_show_raw
      time_show_precise
      time_OpamStd_String_split_10
      time_update_local_plain
      bin_size
  in
  print_endline json
