(**************************************************************************)
(*                                                                        *)
(*    Copyright 2021 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Generator script for the GitHub Actions workflows. Primary aim is to
   eliminate duplicated YAML steps (e.g. the build step for a cache miss) *)

open Lib

let ocamls = [
  (* Legacy versions - core libraries only *)
  "4.03.0"; "4.04.2"; "4.05.0"; "4.06.1"; "4.07.1"; 
  (* Fully supported versions *)
   "4.08.1"; "4.09.1"; "4.10.2"; "4.11.2"; "4.12.1"; "4.13.1"; "4.14.0";
]

(* Entry point for the workflow. Workflows are specified as continuations where
   each job is passed as a continuation to the [workflow], terminated with
   {!end_workflow}. *)
let workflow ~oc ~env name f =
  fprintf oc
{|name: %s

on:
  push:
    paths:
    - 'src/**'
    - '!src/tools/**'
    - 'src_ext/**'
    - 'dune'
    - 'dune-project'
    - '*.opam'
    - 'Makefile*'
    - 'configure*'
  pull_request:
    paths:
    - 'src/**'
    - '!src/tools/**'
    - 'src_ext/**'
    - 'dune'
    - 'dune-project'
    - '*.opam'
    - 'Makefile*'
    - 'configure*'
    - '.github/scripts/**'
    - '.github/workflows/main.yml'
    - 'tests/**'
#    paths-ignore:
#    - 'release/**'
#    - 'shell/**'
#    - 'admin-scripts/**'
#    - 'doc/**'
#    - 'CHANGES'
#    - 'LICENSE'
#    - 'CONTRIBUTING.md'
#    - 'master_changes.md'
#    - 'README.md'
|} name;
  if env <> [] then begin
    output_char oc '\n';
    emit_env ~oc ~indent:0 env
  end;
  output_string oc
{|
defaults:
  run:
    shell: bash

jobs:
|};
  f ~oc ~workflow:name

let end_workflow ~oc:_ ~workflow:_ = ()

let ocamls =
  List.map (fun v -> Scanf.sscanf v "%u.%u.%u" (fun major minor _ -> ((major, minor), v))) ocamls

let latest_ocaml = List.fold_left (fun _ (v, _) -> v) (0, 0) ocamls

let platform_ocaml_matrix ?(dir=List.drop_while) ~fail_fast start_version =
  (fail_fast,
   [("ocamlv", List.map snd (dir (fun ocaml -> fst ocaml < start_version) ocamls))],
   [])

let git_lf_checkouts ?(title="Configure Git") ?cond ?shell () =
  run title ?cond ?shell
    ["git config --system core.autocrlf false";
     "git config --system core.eol lf"]

type cache = {
  name: string;
  key: string -> string;
  id: string;
  force_gzip: bool;
  paths: string list;
  always_build: bool;
  build: string list;
  build_shell: string option;
}

type _ cache_name =
| Archives : ((cache -> 'a) -> 'a) cache_name
| Secondary : ((cache -> 'a) -> 'a) cache_name
| Cygwin : ((cache -> 'a) -> [ `x86 | `x64 ] -> 'a) cache_name
| OCaml : ((cache -> 'a) -> _ platform -> string -> string -> 'a) cache_name
| OpamBS : ((cache -> 'a) -> string -> string -> 'a) cache_name
| OpamRoot : ((cache -> 'a) -> string -> 'a) cache_name
| Opam12Root : ((cache -> 'a) -> 'a) cache_name

let cygwin_cache_directory = {|D:\Cache\cygwin|}

let get_cache_cont : type s . s cache_name -> s = function
  | Archives ->
      fun f -> f
        {name = "src_ext/archives and opam-repository";
         key = Printf.sprintf "${{ %s.outputs.archives }}";
         id = "archives";
         force_gzip = true;
         paths = ["src_ext/archives"; "~/opam-repository"];
         always_build = false;
         build = ["bash -exu .github/scripts/main/archives-cache.sh"];
         build_shell = None}
  | Secondary ->
      fun f -> f
        {name = "ocaml-secondary-compiler";
         key = Printf.sprintf "${{ runner.os }}-${{ %s.outputs.ocaml-secondary-compiler }}";
         id = "legacy";
         force_gzip = false;
         paths = ["~/.opam"];
         always_build = false;
         build = ["bash -exu .github/scripts/main/legacy-cache.sh"];
         build_shell = None}
  | Cygwin ->
      fun f arch ->
        let (arch_name, key_arch) = match arch with `x86 -> ("i686", "32") | `x64 -> ("x86_64", "64") in f
        {name = (match arch with `x86 -> "Cygwin32" | `x64 -> "Cygwin64");
         key = Printf.sprintf "cygwin%s-${{ %s.outputs.cygwin }}" key_arch;
         id = "cygwin" ^ key_arch;
         force_gzip = true;
         paths = [Printf.sprintf "%s\\%s-pc-cygwin" cygwin_cache_directory arch_name];
         always_build = false;
         build = [Printf.sprintf {|.github\scripts\cygwin.cmd %s-pc-cygwin %s create|} arch_name cygwin_cache_directory];
         build_shell = Some "cmd"}
  | OCaml ->
      fun f (type a) (platform : a platform) version host ->
        let if_windows =
          match platform with
          | Windows
          | Specific (Windows, _) -> fun a _ -> a
          | _ -> fun _ b -> b in
        let is_windows = if_windows true false in f
        {name = Printf.sprintf "OCaml %s" version;
         key = Printf.sprintf "${{ runner.os }}%s-ocaml-%s-${{ %s.outputs.ocaml-cache }}" (if_windows ("-" ^ host) "") version;
         id = "ocaml-cache";
         force_gzip = is_windows;
         paths = [if_windows {|D:\Cache\ocaml-local.tar|} "~/.cache/ocaml-local/**"];
         always_build = is_windows;
         build = [Printf.sprintf "bash -exu .github/scripts/main/ocaml-cache.sh ${{ runner.os }} %s%s" version (if_windows (" " ^ host) "")];
         build_shell = None}
  | OpamBS ->
      fun f version key_prefix -> f
        {name = "opam bootstrap";
         key = Printf.sprintf "opam%s-${{ runner.os }}-${{ env.OPAMBSVERSION }}-%s-${{ env.OPAM_REPO_SHA }}-${{ %s.outputs.opam-bs-cache }}" key_prefix version;
         id = "opam-bootstrap";
         force_gzip = false;
         paths = ["${{ env.OPAMBSROOT }}/**"; "~/.cache/opam-local/bin/**"];
         always_build = false;
         build = ["bash -exu .github/scripts/main/opam-bs-cache.sh"];
         build_shell = None}
  | OpamRoot ->
      fun f version -> f
        {name = "opam-rt";
         key = Fun.const (Printf.sprintf "${{ runner.os }}-opam-rt-%s" version);
         id = "opam-rt";
         force_gzip = false;
         paths = ["~/.cache/opam-rt/**"];
         always_build = false;
         build = [];
         build_shell = None}
  | Opam12Root ->
      fun f -> f
        {name = "opam 1.2 root";
         key = Fun.const ("${{ runner.os }}-opam1.2-root");
         id = "";
         force_gzip = false;
         paths = ["${{ env.OPAM12CACHE }}"];
         always_build = false;
         build = [];
         build_shell = None}

let get_cache name = get_cache_cont name Fun.id

let cache ?cond ?(key_prefix="needs.Analyse") ?(check_only=false) name =
  get_cache_cont name (fun cache ->
    let action =
      if cache.force_gzip || check_only then
        "ocaml-opam/cache@opam"
      else
        "actions/cache@v3"
    in
    let withs =
      if cache.force_gzip then
        [("force-gzip", Literal ["true"])]
      else
        [] in
    let withs =
      if check_only then
        ("check-only", Literal ["true"]) :: withs
      else
        withs in
    let withs =
      ("path", Literal cache.paths)::("key", Literal [cache.key key_prefix])::withs in
    let id =
      if cache.id = "" then None else Some cache.id
    in
    uses (cache.name ^ " Cache") ?cond ?id ~withs action)

let build_cache ?cond name =
  get_cache_cont name (fun cache ->
  let miss = Predicate (true, CacheMiss cache.id) in
  let cond =
    if cache.always_build then
      cond
    else
      Option.map_default (fun cond -> Some (And (cond, miss))) (Some miss) cond
  in
  run ?cond ?shell:cache.build_shell (Printf.sprintf "%s %s%s" (if cache.always_build then "Unpack" else "Create") cache.name (if cache.always_build then "" else " cache")) cache.build)

let unpack_cygwin ?cond build host =
  run ?cond ~shell:"cmd" "Unpack Cygwin" [Printf.sprintf {|.github\scripts\cygwin.cmd %s %s %s|} build cygwin_cache_directory host]

let install_sys_packages packages ~descr ?cond platforms =
  let packages = String.concat " " packages in
  let linux_command = "sudo apt install " ^ packages in
  let macos_command = "brew install " ^ packages in
  match platforms with
  | [Windows] -> skip_step
  | _ ->
      let not_windows = Predicate(false, Runner Windows) in
      let cond =
        if List.mem Windows platforms then
          Option.map_default (fun cond -> Some (And (not_windows, cond))) (Some not_windows) cond
        else
          cond
      in
      let commands =
        match platforms with
        | [Linux] -> [linux_command ^ " || true"]
        | [MacOS] -> [macos_command ^ " || true"]
        | _ ->
            let commands =
              if List.mem MacOS platforms then
                [Printf.sprintf "( test '${{ runner.os }}' == macOS && %s ) || true" macos_command]
              else
                []
            in
            if List.mem Linux platforms then
              (Printf.sprintf "( test '${{ runner.os }}' == Linux && %s ) || true" linux_command)::commands
            else
              commands
      in
        run ?cond descr commands

let install_sys_opam ?cond = install_sys_packages ["opam"] ~descr:"Install system's opam package" ?cond
let install_sys_dune ?cond = install_sys_packages ["dune"; "ocaml"] ~descr:"Install system's dune and ocaml packages" ?cond

let analyse_job ~oc ~workflow ~platforms ~keys f =
  let outputs =
    let f (key, _) = (key, Printf.sprintf "${{ steps.keys.outputs.%s }}" key) in
    List.map f keys
  in
  let keys =
    let set_key (name, value) =
      [Printf.sprintf "echo %s=%s" name value;
       Printf.sprintf "echo ::set-output name=%s::%s" name value]
    in
    List.flatten (List.map set_key keys)
  in
  let only_with platform step =
    if List.mem platform platforms then
      step
    else
      skip_step
  in
  let linux_guard =
    match platforms with
    | [Linux] -> None
    | _ -> Some (Predicate(true, Runner Linux))
  in
  let not_windows_guard =
    if List.mem Windows platforms then
      Some (Predicate(false, Runner Windows))
    else
      None
  in
  job ~oc ~workflow ~runs_on:(Runner platforms) ~outputs ~section:"Caches" "Analyse"
    ++ only_with Windows (git_lf_checkouts ~cond:(Predicate(true, Runner Windows)) ~shell:"cmd" ~title:"Configure Git for Windows" ())
    ++ checkout ()
    ++ run "Determine cache keys" ~id:"keys" keys
    ++ cache ?cond:linux_guard ~key_prefix:"steps.keys" ~check_only:true Archives 
    ++ build_cache ?cond:not_windows_guard Archives
    ++ cache ?cond:not_windows_guard ~key_prefix:"steps.keys" ~check_only:true Secondary 
    ++ install_sys_opam ~cond:(Predicate(true, CacheMiss (get_cache Secondary).id)) platforms
    ++ build_cache ?cond:not_windows_guard Secondary
    ++ end_job f

let cygwin_job ~analyse_job ~oc ~workflow f =
  let cygwin32 = get_cache Cygwin `x86 in
  let cygwin64 = get_cache Cygwin `x64 in
  let either_missed =
    Or(Predicate(true, CacheMiss cygwin32.id), Predicate(true, CacheMiss cygwin64.id))
  in
  job ~oc ~workflow ~runs_on:(Runner [Windows]) ~needs:[analyse_job] "Cygwin"
    ++ cache ~check_only:true Cygwin `x86 
    ++ cache ~check_only:true Cygwin `x64
    ++ checkout ~cond:either_missed ()
    ++ build_cache Cygwin `x86
    ++ build_cache Cygwin `x64
    ++ end_job f

let main_build_job ~analyse_job ~cygwin_job ?section platform start_version ~oc ~workflow f =
  let only_on target = only_on platform target in
  let not_on target = not_on platform target in
  let shell =
    if platform = Windows then
      Some {|D:\cygwin\bin\bash.exe {0}|}
    else None
  in
  (* Intentionally fail fast, no need to run all build if there is a
   * problem in a given version; usually it is functions not defined in lower
   * versions of OCaml. *)
  let (_fail_fast, matrix, _) = platform_ocaml_matrix ~fail_fast:true start_version in
  let (matrix, includes) =
    if platform = Windows then
      let includes =
        (* Annoyingly, it does not appear to be possible to exclude or include by pattern, i.e. to
           have
           include:
              - host: i686-pc-cygwin
                build: i686-pc-cygwin
            and have GHA take the cross-product of that with ocamlv from the main matrix
            So we have to duplicate the ocamlv below... *)
        List.map (fun v -> [("ocamlv", v); ("host", "i686-pc-cygwin"); ("build", "i686-pc-cygwin")]) (snd (List.hd matrix))
      in
      (("host", ["x86_64-pc-cygwin"; "i686-w64-mingw32"; "x86_64-w64-mingw32"; "i686-pc-windows"; "x86_64-pc-windows"]) ::
       ("build", ["x86_64-pc-cygwin"]) ::
        matrix, includes)
    else
      (matrix, []) in
  let matrix = ((platform <> Windows), matrix, includes) in
  let needs = if platform = Windows then [analyse_job; cygwin_job] else [analyse_job] in
  let host = host_of_platform platform in
  job ~oc ~workflow ~runs_on:(Runner [platform]) ?shell ?section ~needs ~matrix ("Build-" ^ name_of_platform platform)
    ++ only_on Linux (run "Install bubblewrap" ["sudo apt install bubblewrap"])
    ++ only_on Windows (git_lf_checkouts ~cond:(Predicate(true, EndsWith("matrix.host", "-pc-cygwin"))) ~shell:"cmd" ~title:"Configure LF checkout for Cygwin" ())
    ++ checkout ()
    ++ only_on Windows (cache ~cond:(Predicate(true, Compare("matrix.build", "i686-pc-cygwin"))) Cygwin `x86)
    ++ only_on Windows (cache ~cond:(Predicate(true, Compare("matrix.build", "x86_64-pc-cygwin"))) Cygwin `x64)
    ++ cache Archives
    ++ cache OCaml platform "${{ matrix.ocamlv }}" host
    ++ only_on Windows (unpack_cygwin "${{ matrix.build }}" "${{ matrix.host }}")
    ++ build_cache OCaml platform "${{ matrix.ocamlv }}" host
    ++ run "Build" ["bash -exu .github/scripts/main/main.sh " ^ host]
    ++ not_on Windows (run "Test (basic)" ["bash -exu .github/scripts/main/test.sh"])
    ++ only_on Windows (run "Test (basic - Cygwin)" ~cond:(Predicate(true, EndsWith("matrix.host", "-pc-cygwin"))) ["bash -exu .github/scripts/main/test.sh"])
    ++ only_on Windows (run "Test (basic - native Windows)" ~env:[("OPAMROOT", {|D:\a\opam\opam\.opam|})] ~shell:"cmd" ~cond:(Predicate(false, EndsWith("matrix.host", "-pc-cygwin")))
         ({|set Path=D:\Cache\ocaml-local\bin;%Path%|} ::
          {|if "${{ matrix.host }}" equ "x86_64-pc-windows" call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Auxiliary\Build\vcvars64.bat"|} ::
          {|if "${{ matrix.host }}" equ "i686-pc-windows" call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Auxiliary\Build\vcvars32.bat"|} ::
          run_or_fail [
           {|opam init --yes --bare default git+file://D:/opam-repository#${{ env.OPAM_TEST_REPO_SHA }}|};
           {|opam switch --yes create default ocaml-system|};
           {|opam env|};
           {|opam install --yes lwt|};
           {|opam list|};
           {|opam config report|};
          ]))
    ++ only_on Windows (run "Test (reftests)" ["bash -exu .github/scripts/main/reftests.sh ${{ matrix.host }}"])
    ++ end_job f

let legacy_build_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ~(platforms : os_only platform list) ~oc ~workflow f =
  let needs = analyse_job :: List.map (function Linux -> build_linux_job | Windows -> build_windows_job | MacOS -> build_macOS_job) platforms in
  let (fail_fast, matrix, _) = platform_ocaml_matrix ~dir:List.take_until ~fail_fast:true (4, 08) in
  let matrix =
    (fail_fast, ("os", List.map os_name_of_platform platforms)::matrix, [])
  in
  (* Not needed, so not implemented *)
  let _ = assert (not (List.mem Windows platforms)) in
  job ~oc ~workflow ~runs_on:(Matrix "${{ matrix.os }}-latest") ~section:"Legacy" ~needs ~matrix "Legacy"
    ++ install_sys_opam platforms
    ++ checkout ()
    ++ cache Secondary
    ++ build_cache Secondary
    (* Little bit hacky, but the host and platform parameters for the OCaml cache affect Windows only *)
    ++ cache OCaml Linux "${{ matrix.ocamlv }}" ""
    ++ build_cache OCaml Linux "${{ matrix.ocamlv }}" ""
    ++ run "Build" ["bash -exu .github/scripts/main/legacy.sh"]
    ++ end_job f

let main_test_job ~analyse_job ~build_linux_job ~build_windows_job:_ ~build_macOS_job:_ ?section platform ~oc ~workflow f =
  let _ = assert (platform <> Windows) in
  let only_on target = only_on platform target in
  let needs =
    match platform with
    | Windows -> assert false (* Not at present implemented *)
    | MacOS -> [analyse_job]  (* This isn't gated on build_macOS_job for speed *)
    | Linux -> [analyse_job; build_linux_job]
  in
  let matrix = platform_ocaml_matrix ~fail_fast:false latest_ocaml in
  let host = host_of_platform platform in
  let ocamlv = "${{ matrix.ocamlv }}" in
  job ~oc ~workflow ?section ~runs_on:(Runner [platform]) ~env:[("OPAM_TEST", "1")] ~matrix ~needs ("Test-" ^ name_of_platform platform)
    ++ checkout ()
    ++ only_on Linux (run "Install bubblewrap" ["sudo apt install bubblewrap"])
    ++ cache Archives
    ++ cache OCaml platform ocamlv host
    ++ build_cache OCaml platform ocamlv host
    ++ cache OpamBS ocamlv ""
    ++ build_cache OpamBS ocamlv ""
    ++ cache OpamRoot ocamlv
    ++ run "Build (and test)" ["bash -exu .github/scripts/main/main.sh " ^ host]
    ++ run "Test (opam-rt)" ["bash -exu .github/scripts/main/opam-rt.sh"]
    ++ end_job f

let cold_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ?section platform ~oc ~workflow f =
  (* TODO Windows steps not all here *)
  let host = host_of_platform platform in
  let only_on target = only_on platform target in
  let needs = [analyse_job; (match platform with Linux -> build_linux_job | Windows -> build_windows_job | MacOS -> build_macOS_job)] in
  job ~oc ~workflow ?section ~runs_on:(Runner [platform]) ~env:[("OPAM_COLD", "1")] ~needs ("Cold-" ^ name_of_platform platform)
    ++ only_on Linux (run "Install bubblewrap" ["sudo apt install bubblewrap"])
    ++ checkout ()
    ++ cache Archives
    ++ run "Cold" [
         "make compiler";
         "make lib-pkg";
         "bash -exu .github/scripts/main/main.sh " ^ host]
    ++ run "Test (basic)" ["bash -exu .github/scripts/main/test.sh"]
    ++ end_job f

let solvers_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ?section platform ~oc ~workflow f =
  (* TODO Windows steps not all here *)
  let host = host_of_platform platform in
  let only_on target = only_on platform target in
  let needs = [analyse_job; (match platform with Linux -> build_linux_job | Windows -> build_windows_job | MacOS -> build_macOS_job)] in
  let env = [("SOLVER", "${{ matrix.solver }}"); ("OPAMBSROOT", "~/.cache/opam.${{ matrix.solver }}.cached")] in
  let (fail_fast, matrix, _) = platform_ocaml_matrix ~fail_fast:false latest_ocaml in
  let matrix =
    (fail_fast, ("solver", ["z3"; "0install"])::matrix, [])
  in
  let ocamlv = "${{ matrix.ocamlv }}" in
  job ~oc ~workflow ?section ~runs_on:(Runner [platform]) ~env ~needs ~matrix ("Solvers-" ^ name_of_platform platform)
    ++ only_on Linux (run "Install bubblewrap" ["sudo apt install bubblewrap"])
    ++ checkout ()
    ++ cache Archives
    ++ cache OCaml platform ocamlv host
    ++ build_cache OCaml platform ocamlv host
    ++ cache OpamBS ocamlv "-${{ matrix.solver }}"
    ++ build_cache OpamBS ocamlv "-${{ matrix.solver }}"
    ++ run "Compile" ["bash -exu .github/scripts/main/solvers.sh"]
    ++ end_job f

let upgrade_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ?section platform ~oc ~workflow f =
  let _ = assert (platform <> Windows) in (* No opam 1.x for Windows *)
  let host = host_of_platform platform in
  let only_on target = only_on platform target in
  let needs = [analyse_job; (match platform with Linux -> build_linux_job | Windows -> build_windows_job | MacOS -> build_macOS_job)] in
  let matrix = platform_ocaml_matrix ~fail_fast:false latest_ocaml in
  let ocamlv = "${{ matrix.ocamlv }}" in
  job ~oc ~workflow ?section ~runs_on:(Runner [platform]) ~needs ~matrix ("Upgrade-" ^ name_of_platform platform)
    ++ only_on Linux (run "Install bubblewrap" ["sudo apt install bubblewrap"])
    ++ checkout ()
    ++ cache Opam12Root
    ++ cache OCaml platform ocamlv host
    ++ build_cache OCaml platform ocamlv host
    ++ run "Build" ~env:[("OPAM_UPGRADE", "1")] ["bash -exu .github/scripts/main/main.sh " ^ host]
    ++ run "Test (upgrade)" ["bash -exu .github/scripts/main/upgrade.sh"]
    ++ end_job f

let hygiene_job (type a) ~analyse_job (platform : a platform) ~oc ~workflow f =
  job ~oc ~workflow ~section:"Around opam tests" ~runs_on:(Runner [platform]) ~needs:[analyse_job] "Hygiene"
    ++ install_sys_dune [os_of_platform platform]
    ++ checkout ()
    ++ cache Archives
    ++ uses "Get changed files" ~id:"files" ~continue_on_error:true (* see https://github.com/jitterbit/get-changed-files/issues/19 *) "jitterbit/get-changed-files@v1"
    ++ run "Changed files list" [
         "for changed_file in ${{ steps.files.outputs.modified }}; do";
         "  echo \"M  ${changed_file}.\"";
         "done";
         "for changed_file in ${{ steps.files.outputs.removed }}; do";
         "  echo \"D  ${changed_file}.\"";
         "done";
         "for changed_file in ${{ steps.files.outputs.added }}; do";
         "  echo \"A  ${changed_file}.\"";
         "done";
         "for changed_file in ${{ steps.files.outputs.renamed }}; do";
         "  echo \"AD ${changed_file}.\"";
         "done";
       ]
    ++ run "Hygiene" ~cond:(Or(Predicate(true, Contains("steps.files.outputs.modified", "configure.ac")), Predicate(true, Contains("steps.files.outputs.all", "src_ext")))) ~env:[("BASE_REF_SHA", "${{ github.event.pull_request.base.sha }}"); ("PR_REF_SHA", "${{ github.event.pull_request.head.sha }}")] ["bash -exu .github/scripts/main/hygiene.sh"]
    ++ end_job f

let main oc : unit =
  let env = [
    ("OPAMBSVERSION", "2.1.0");
    ("OPAMBSROOT", "~/.cache/.opam.cached");
    ("OPAM12CACHE", "~/.cache/opam1.2/cache");
    (* These should be identical to the values in appveyor.yml *)
    ("OPAM_REPO", "https://github.com/ocaml/opam-repository.git");
    ("OPAM_TEST_REPO_SHA", "59a65e144ad843d97ddd3e3e600c2a33c02527fd");
    ("OPAM_REPO_SHA", "59a65e144ad843d97ddd3e3e600c2a33c02527fd");
    ("SOLVER", "");
    (* Cygwin configuration *)
    ("CYGWIN_MIRROR", "http://mirrors.kernel.org/sourceware/cygwin/");
    ("CYGWIN_ROOT", "D:\\cygwin");
    ("CYGWIN", "winsymlinks:native");
    ("CYGWIN_EPOCH", "3");
  ] in
  let keys = [
    ("archives", "archives-1-${{ hashFiles('src_ext/Makefile.sources', 'src_ext/Makefile', '.github/scripts/common/preamble.sh', '.github/scripts/main/preamble.sh', '.github/scripts/main/archives-cache.sh') }}-${{ env.OPAM_REPO_SHA }}");
    ("ocaml-secondary-compiler", "legacy-${{ env.OPAM_REPO_SHA }}");
    ("ocaml-cache", "${{ hashFiles('.github/scripts/main/ocaml-cache.sh', '.github/scripts/main/preamble.sh') }}");
    ("cygwin", "${{ hashFiles('.github/scripts/cygwin.cmd') }}-${{ env.CYGWIN_EPOCH }}");
    ("opam-bs-cache", "${{ hashFiles('.github/scripts/main/opam-bs-cache.sh', '*.opam', '.github/scripts/main/preamble.sh') }}");
  ] in
  workflow ~oc ~env "Builds, tests & co"
    ++ analyse_job ~keys ~platforms:[Linux] (fun analyse_job ->
       cygwin_job ~analyse_job (fun cygwin_job ->
       main_build_job ~analyse_job ~cygwin_job ~section:"Build" Linux (4, 08) (fun build_linux_job ->
       main_build_job ~analyse_job ~cygwin_job Windows latest_ocaml (fun build_windows_job ->
       main_build_job ~analyse_job ~cygwin_job MacOS latest_ocaml (fun build_macOS_job ->
       legacy_build_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ~platforms:[Linux] (fun _ ->
       main_test_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ~section:"Opam tests" Linux (fun _ ->
       main_test_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job MacOS (fun _ ->
       cold_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ~section:"Opam cold" Linux (fun _ ->
       solvers_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ~section:"Compile solver backends" Linux (fun _ ->
       solvers_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job MacOS (fun _ ->
       upgrade_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job ~section:"Upgrade from 1.2 to current" Linux (fun _ ->
       upgrade_job ~analyse_job ~build_linux_job ~build_windows_job ~build_macOS_job MacOS (fun _ ->
       hygiene_job ~analyse_job (Specific (Linux, "22.04")) (fun _ ->
       end_workflow))))))))))))))

let () =
  let oc = open_out "main.yml" in
  main oc;
  close_out oc
