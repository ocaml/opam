(**************************************************************************)
(*                                                                        *)
(*    Copyright 2021 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Option = struct
  include Option

  let map_default f dft = function
  | Some x -> f x
  | None -> dft
end

module List = struct
  include List

  let rec drop_while f = function
  | [] -> []
  | (h::tl) as l -> if f h then drop_while f tl else l

  let rec take_until f = function
  | [] -> []
  | h::tl -> if f h then h::take_until f tl else []
end

let fprintf = Printf.fprintf

type _ platform = MacOS : os_only platform
                | Linux : os_only platform
                | Windows : os_only platform
                | Specific : os_only platform * string -> with_runner_version platform
and os_only = Os_only
and with_runner_version = With_runner_version

let rec name_of_platform (type a) (platform : a platform) =
  match platform with
  | MacOS -> "macOS"
  | Linux -> "Linux"
  | Windows -> "Windows"
  | Specific (platform, _) -> name_of_platform platform

let os_of_platform (type a) (platform : a platform) =
  match platform with
  | MacOS
  | Linux
  | Windows as platform -> platform
  | Specific (platform, _) -> platform

let rec os_name_of_platform (type a) (platform : a platform) =
  match platform with
  | MacOS -> "macos"
  | Linux -> "ubuntu"
  | Windows -> "windows"
  | Specific (platform, _) -> os_name_of_platform platform

let emit_map ~oc ?(indent=4) name map =
  let indent = String.make indent ' ' in
  fprintf oc "%s%s:\n" indent name;
  let emit_binding (key, value) =
    let value = if value = "" then "" else " " ^ value in
    fprintf oc "%s  %s:%s\n" indent key value
  in
  List.iter emit_binding map

(* Prints an `env:` block *)
let emit_env ?indent = emit_map ?indent "env"

(* Prints an `outputs:` block *)
let emit_outputs ?indent = emit_map ?indent "outputs"

(* Prints a string list field (not printed if value = []) *)
let emit_yaml_list ?(indent=4) ?(force_list=false) ~oc name value =
  if value <> [] then
    let indent = String.make indent ' ' in
    match value with
    | [elt] when not force_list ->
        fprintf oc "%s%s: %s\n" indent name elt
    | elts ->
        fprintf oc "%s%s: [ %s ]\n" indent name (String.concat ", " elts)

let print_include ~oc values =
  fprintf oc "          - %s\n" (String.concat "\n            " (List.map (fun (key, value) -> Printf.sprintf "%s: %s" key value) values))

(* Prints a strategy block for a job *)
let emit_strategy ~oc (fail_fast, matrix, includes) =
  output_string oc
{|    strategy:
      matrix:
|};
  let print_matrix (key, elts) = emit_yaml_list ~oc ~indent:8 ~force_list:true key elts in
  List.iter print_matrix matrix;
  if includes <> [] then begin
    output_string oc "        include:\n";
    List.iter (print_include ~oc) includes
  end;
  fprintf oc "      fail-fast: %b\n" fail_fast

type 'a runs_on = Runner of 'a platform list | Matrix of string

type job = ..
let jobs = Hashtbl.create 15

let find_need need = Hashtbl.find jobs need

let emit_runs_on ~oc runs_on =
  let runner_of_platform (type a) (platform : a platform) =
    match platform with
    | Windows -> "windows-2019"
    | MacOS
    | Linux as platform -> os_name_of_platform platform ^ "-latest"
    | Specific (platform, version) -> os_name_of_platform platform ^ "-" ^ version
  in
  let value =
    match runs_on with
    | Runner [platform] ->
        runner_of_platform platform
    | Runner platforms ->
        Printf.sprintf "[%s]" (String.concat ", " (List.map runner_of_platform platforms))
    | Matrix entry -> entry
  in
  fprintf oc "    runs-on: %s\n" value

(* Continuation for a job. Steps are specified as continuations as for
   the jobs within the workflow, terminated with {!end_job}. *)
let job ~oc ~workflow ?shell ?section ?(needs = []) ?matrix ?env ?outputs ~runs_on name f =
  let module M = struct type job += Key end in
  Hashtbl.add jobs M.Key name;
  output_char oc '\n';
  let emit_section =
    fprintf oc
{|####
# %s
####
|} in
  Option.iter emit_section section;
  fprintf oc "  %s:\n" name;
  emit_runs_on ~oc runs_on;
  emit_yaml_list ~oc "needs" (List.map find_need needs);
  Option.iter (emit_strategy ~oc) matrix;
  Option.iter (emit_env ~oc) env;
  Option.iter (emit_outputs ~oc) outputs;
  Option.iter (fprintf oc "    defaults:\n      run:\n        shell: %s\n") shell;
  output_string oc "    steps:\n";
  f ~oc ~workflow ~job:M.Key

let end_job ~oc ~workflow ~job f = f job ~oc ~workflow

(* Left-associative version of (@@) which allows combining jobs and steps
   without parentheses. *)
let (++) = (@@)

type condition =
| And of condition * condition
| Or of condition * condition
| Predicate of bool * variable
and variable =
| Runner of os_only platform
| CacheMiss of string
| EndsWith of string * string
| Contains of string * string
| Compare of string * string

let emit_condition ~oc ~indent =
  let indent = String.make indent ' ' in
  let rec to_yaml condition =
    match condition with
    | And ((Predicate(_, _) as l), (Predicate(_, _) as r)) -> recurse l ^ " && " ^ recurse r
    | Or ((Predicate(_, _) as l), (Predicate(_, _) as r)) -> recurse l ^ " || " ^ recurse r
    | cond -> recurse cond
  and recurse = function
    | And (l, r) -> Printf.sprintf "((%s) && (%s))" (recurse l) (recurse r)
    | Or (l, r) -> Printf.sprintf "((%s) || (%s))" (recurse l) (recurse r)
    | Predicate (op, EndsWith(variable, constant)) ->
        let op = if op then "" else " == false" in
        Printf.sprintf "endsWith(%s, '%s')%s" variable constant op
    | Predicate (op, Contains(variable, constant)) ->
        let op = if op then "" else " == false" in
        Printf.sprintf "contains(%s, '%s')%s" variable constant op
    | Predicate (op, Compare(variable, constant)) ->
        let op = if op then '=' else '!' in
        Printf.sprintf "%s %c= '%s'" variable op constant
    | Predicate (op, Runner platform) ->
        let op = if op then '=' else '!' in
        Printf.sprintf "runner.os %c= '%s'" op (name_of_platform platform)
    | Predicate (op, CacheMiss id) ->
        let op = if op then '!' else '=' in
        Printf.sprintf "steps.%s.outputs.cache-hit %c= 'true'" id op
  in
  let convert cond = fprintf oc "%sif: %s\n" indent (to_yaml cond) in
  Option.iter convert

let run name ?id ?cond ?shell ?env run ~oc ~workflow ~job f =
  fprintf oc "    - name: %s\n" name;
  Option.iter (emit_env ~indent:6 ~oc) env;
  Option.iter (fprintf oc "      id: %s\n") id;
  emit_condition ~oc ~indent:6 cond;
  Option.iter (fprintf oc "      shell: %s\n") shell;
  begin match run with
  | [command] ->
      fprintf oc "      run: %s\n" command
  | commands ->
      fprintf oc "      run: |\n        %s\n" (String.concat "\n        " commands)
  end;
  f ~oc ~workflow ~job

type with_entry =
| Literal of string list
| Expression of string

let yaml_of_with_entry = function
| Literal [entry] -> entry
| Literal entries -> "|\n          " ^ String.concat "\n          " entries
| Expression expr -> Printf.sprintf "${{ %s }}" expr

let uses name ?id ?cond ?(continue_on_error=false) ?(withs=[]) action ~oc ~workflow ~job f =
  fprintf oc "    - name: %s\n" name;
  Option.iter (fprintf oc "      id: %s\n") id;
  emit_condition ~oc ~indent:6 cond;
  fprintf oc "      uses: %s\n" action;
  if continue_on_error then
    output_string oc "      continue-on-error: true\n";
  if withs <> [] then begin
    fprintf oc "      with:\n";
    List.iter (fun (key, value) -> fprintf oc "        %s: %s\n" key (yaml_of_with_entry value)) withs
  end;
  f ~oc ~workflow ~job

let checkout ?cond () =
  uses "Checkout tree" ?id:None ?cond "actions/checkout@v2"

let skip_step ~oc ~workflow ~job f = f ~oc ~workflow ~job

(* Appends `|| exit /b 1` to a series of cmd commands *)
let run_or_fail = List.map (Fun.flip (^) " || exit /b 1")

let rec host_of_platform (type a) (platform : a platform) =
  match platform with
  | Windows -> "${{ matrix.host }}"
  | Linux -> "x86_64-pc-linux-gnu"
  | MacOS -> "x86_64-apple-darwin"
  | Specific (platform, _) -> host_of_platform platform

let gen_on op platform target step =
  if op target platform then
    step
  else
    skip_step
let only_on platform = gen_on (=) platform
let not_on platform = gen_on (<>) platform
