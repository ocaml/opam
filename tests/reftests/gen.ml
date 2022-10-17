let first_line ~path =
  let ic = open_in path in
  let s = input_line ic in
  close_in ic;
  s

let null_hash= "N0REP0"
let default_repo = "opam-repo-"^null_hash

let diff_rule base_name ~condition =
  Format.sprintf
    {|
(rule
 (alias reftest-%s)%s
 (action
  (diff %s.test %s.out)))

(alias
 (name reftest)%s
 (deps (alias reftest-%s)))
|}
    base_name condition base_name base_name condition base_name

let tgz_name ~archive_hash =
  Printf.sprintf "opam-archive-%s.tar.gz" archive_hash

let repo_directory ~archive_hash =
  Printf.sprintf "opam-repo-%s" archive_hash

let opamroot_directory ~archive_hash =
  Printf.sprintf "root-%s" archive_hash

let run_rule ~base_name ~archive_hash ~condition =
  Format.sprintf {|
(rule
 (targets %s)
 (deps %s)%s
 (action
  (with-stdout-to
   %%{targets}
   (run ./run.exe %%{exe:../../src/client/opamMain.exe} %%{dep:%s.test} %%{read-lines:testing-env}))))
|} (base_name^".out") (opamroot_directory ~archive_hash) condition base_name

let archive_download_rule archive_hash =
   Format.sprintf {|
(rule
 (targets %s)
 (action (run curl --silent -Lo %%{targets} https://github.com/ocaml/opam-repository/archive/%s.tar.gz)))
|} (tgz_name ~archive_hash) archive_hash

let default_repo_rule =
  Format.sprintf {|
(rule
 (targets %s)
 (action
  (progn
   (run mkdir -p %%{targets}/packages)
   (write-file repo "opam-version:\"2.0\"")
   (run cp repo %%{targets}/repo))))
|} default_repo

(* XXX this fails if the directory already exists ?! *)
let archive_unpack_rule archive_hash =
  Format.sprintf {|
(rule
  (targets %s)
  (action
   (progn
    (run mkdir -p %%{targets})
    (run tar -C %%{targets} -xzf %%{dep:%s} --strip-components=1))))
|} (repo_directory ~archive_hash) (tgz_name ~archive_hash)

let opam_init_rule archive_hash =
  Format.sprintf {|
(rule
  (targets %s)
  (action
   (progn
    (ignore-stdout
    (run %%{bin:opam} init --root=%%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%%{dep:%s})))))
|} (opamroot_directory ~archive_hash) (repo_directory ~archive_hash)

module StringSet = Set.Make(String)

let () =
  let () = set_binary_mode_out stdout true in
  let contents =
    Sys.readdir "."
    |> Array.to_list
    |> List.sort String.compare
  in
  let process archive_hashes filename =
    let base_name = OpamStd.String.remove_suffix ~suffix:".test" filename in
    let condition = match Filename.extension base_name with
      | "" -> ""
      | os ->
        Printf.sprintf "\n (enabled_if (= %%{os_type} %S))"
          String.(capitalize_ascii (sub os 1 (length os - 1)))
    in
    if base_name = filename then archive_hashes else
      (print_string (diff_rule base_name ~condition);
       let archive_hash = first_line ~path:filename in
       if archive_hash = null_hash then
         (print_string (run_rule ~base_name ~archive_hash:null_hash ~condition);
          archive_hashes)
       else
         (print_string (run_rule ~base_name ~archive_hash ~condition);
          StringSet.add archive_hash archive_hashes))
  in
  let archive_hashes =
    List.fold_left process StringSet.empty contents
  in
  print_string default_repo_rule;
  print_string (opam_init_rule null_hash);
  StringSet.iter
    (fun archive_hash ->
       print_string (archive_download_rule archive_hash);
       print_string (archive_unpack_rule archive_hash);
       print_string (opam_init_rule archive_hash)
    )
    archive_hashes
