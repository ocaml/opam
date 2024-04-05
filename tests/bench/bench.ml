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
    time_cmd ~exit:2 (fmt "%s sitch" bin)
  in
  let time_install_cmd =
    (* NOTE: https://github.com/ocaml/opam/issues/5502 *)
    launch "opam switch create one --empty";
    time_cmd ~exit:0 (fmt "%s install magic-trace -y --fake --sw one" bin)
  in
  let time_install_cmd_w_invariant =
    (* NOTE: https://github.com/ocaml/opam/issues/5502 *)
    launch "opam switch create two --empty";
    launch "opam switch set-invariant core -n --sw two";
    time_cmd ~exit:0 (fmt "%s install magic-trace -y --fake --sw two" bin)
  in
  let time_OpamSystem_read_10 =
    (* NOTE: https://github.com/ocaml/opam/pull/5896 *)
    let time_OpamSystem_read () =
      let ic = Stdlib.open_in_bin "/home/opam/all-opam-files" in
      let before = Unix.gettimeofday () in
      let rec loop () =
        match Stdlib.input_line ic with
        | file -> ignore (OpamSystem.read file); loop ()
        | exception End_of_file -> Unix.gettimeofday () -. before
      in
      loop ()
    in
    let n = 10 in
    let l = List.init n (fun _ -> time_OpamSystem_read ()) in
    List.fold_left (+.) 0.0 l /. float_of_int n
  in
  let time_deps_only_installed_pkg =
    (* NOTE: https://github.com/ocaml/opam/pull/5908 *)
    launch (fmt "%s switch create three --fake ocaml-base-compiler.4.14.0" bin);
    launch (fmt "%s install -y --fake core.v0.15.0" bin);
    time_cmd ~exit:0 (fmt "%s install --show --deps-only core.v0.15.0" bin)
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
          "name": "OpamSystem.read amortised over 10 runs",
          "value": %f,
          "units": "secs"
        },
        {
          "name": "Deps-only install of an already installed package",
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
      time_OpamSystem_read_10
      time_deps_only_installed_pkg
      bin_size
  in
  print_endline json
