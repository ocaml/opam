(* Utility helper to check if a given set of packages is installed *)

open Types

let usage = "opam-check [--root root] [-l label] <package>+"

let label = ref ""
let spec = Arg.align [
  ("--root", Arg.Set_string Globals.root_path, " set opam path");
  ("-l"    , Arg.Set_string label            , " set a test label");
]

let packages = ref []
let ano x = packages := x :: !packages

let () = Arg.parse spec ano usage

let packages = NV.Set.of_list (List.map NV.of_string !packages)

let installed () =
  let config = File.Config.read (Path.G.config (Path.G.create ())) in
  let version = File.Config.ocaml_version config in
  let installed = File.Installed.read (Path.C.installed (Path.C.create version)) in
  NV.Set.filter (fun nv -> N.to_string (NV.name nv) <> Globals.default_package) installed

let () =
  let installed = installed () in
  let diff1 = NV.Set.diff packages installed in
  let diff2 = NV.Set.diff installed packages in
  let diff = NV.Set.union diff1 diff2 in
  let label = if !label = "" then "" else Printf.sprintf "[%s] " !label in
  if not (NV.Set.is_empty diff) then (
    Globals.error "%swaiting for: %s" label (NV.Set.to_string diff1);
    Globals.error "%sgot:         %s" label (NV.Set.to_string diff2);
    exit 1
  )
