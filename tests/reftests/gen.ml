let first_line ~path =
  let ic = open_in path in
  let s = input_line ic in
  close_in ic;
  s

let diff_rule base_name =
  Format.sprintf
    {|
(alias
 (name reftest-%s)
 (action
  (diff %s.test %s.out)))

(alias
 (name reftest)
 (deps (alias reftest-%s)))
|}
    base_name base_name base_name base_name

let tgz_name ~archive_hash =
  Printf.sprintf "opam-archive-%s.tar.gz" archive_hash

let repo_directory ~archive_hash =
  Printf.sprintf "opam-repo-%s" archive_hash

let opamroot_directory ~archive_hash =
  Printf.sprintf "root-%s" archive_hash

let run_rule ~base_name ~archive_hash =
  Format.sprintf {|
(rule
 (deps %s)
 (action
  (with-stdout-to
   %s.out
   (run ./run.exe %%{bin:opam} %%{dep:%s.test} %%{read-lines:testing-env}))))
|} (opamroot_directory ~archive_hash) base_name base_name

let archive_download_rule archive_hash =
   Format.sprintf {|
(rule
 (targets %s)
 (action (run wget --quiet -O %%{targets} https://github.com/ocaml/opam-repository/archive/%s.tar.gz)))
|} (tgz_name ~archive_hash) archive_hash

let archive_unpack_rule archive_hash =
  Format.sprintf {|
(rule
  (targets %s)
  (action
   (progn
    (run mkdir %%{targets})
    (run tar -C %%{targets} -xzf %%{dep:%s} --strip-components=1))))
|} (repo_directory ~archive_hash) (tgz_name ~archive_hash)

let opam_init_rule archive_hash =
  Format.sprintf {|
(rule
  (targets %s)
  (action
   (progn
    (run %%{bin:opam} init --root=%%{targets}
           --no-setup --bypass-checks --no-opamrc --bare -vv
           file://%%{dep:%s}))))
|} (opamroot_directory ~archive_hash) (repo_directory ~archive_hash)

module StringSet = Set.Make(String)

let () =
  let contents =
    Sys.readdir "."
    |> Array.to_list
    |> List.sort String.compare
  in
  let archive_hashes = ref StringSet.empty in
  let process filename =
    let base_name = OpamStd.String.remove_suffix ~suffix:".test" filename in
    if base_name = filename then () else
      (print_string (diff_rule base_name);
       let archive_hash = first_line ~path:filename in
       print_string (run_rule ~base_name ~archive_hash);
       archive_hashes := StringSet.add archive_hash !archive_hashes)
  in
  List.iter process contents;
  StringSet.iter
    (fun archive_hash ->
       print_string (archive_download_rule archive_hash);
       print_string (archive_unpack_rule archive_hash);
       print_string (opam_init_rule archive_hash)
    )
    !archive_hashes
