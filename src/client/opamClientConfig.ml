(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module E = struct

  type OpamStd.Config.E.t +=
    | ASSUMEDEPEXTS of bool option
    | AUTOREMOVE of bool option
    | CLI of string option
    | DROPWORKINGDIR of bool option
    | EDITOR of string option
    | FAKE of bool option
    | IGNOREPINDEPENDS of bool option
    | INPLACEBUILD of bool option
    | JSON of string option
    | KEEPBUILDDIR of bool option
    | NOAGGREGATE of bool option
    | NOAUTOUPGRADE of bool option
    | NOSELFUPGRADE of string option
    | PINKINDAUTO of bool option
    | REUSEBUILDDIR of bool option
    | ROOTISOK of bool option
    | SHOW of bool option
    | SKIPUPDATE of bool option
    | STATS of bool option
    | WORKINGDIR of bool option

  open OpamStd.Config.E
  let assumedepexts = value (function ASSUMEDEPEXTS b -> b | _ -> None)
  let autoremove = value (function AUTOREMOVE b -> b | _ -> None)
  let cli = value (function CLI s -> s | _ -> None)
  let dropworkingdir = value (function DROPWORKINGDIR b -> b | _ -> None)
  let editor = value (function EDITOR s -> s | _ -> None)
  let fake = value (function FAKE b -> b | _ -> None)
  let ignorepindepends = value (function IGNOREPINDEPENDS b -> b | _ -> None)
  let inplacebuild = value (function INPLACEBUILD b -> b | _ -> None)
  let json = value (function JSON s -> s | _ -> None)
  let keepbuilddir = value (function KEEPBUILDDIR b -> b | _ -> None)
  let noaggregate = value (function NOAGGREGATE b -> b | _ -> None)
  let noautoupgrade = value (function NOAUTOUPGRADE b -> b | _ -> None)
  let noselfupgrade = value (function NOSELFUPGRADE s -> s | _ -> None)
  let pinkindauto = value (function PINKINDAUTO b -> b | _ -> None)
  let reusebuilddir = value (function REUSEBUILDDIR b -> b | _ -> None)
  let rootisok = value (function ROOTISOK b -> b | _ -> None)
  let show = value (function SHOW b -> b | _ -> None)
  let skipupdate = value (function SKIPUPDATE b -> b | _ -> None)
  let stats = value (function STATS b -> b | _ -> None)
  let workingdir = value (function WORKINGDIR b -> b | _ -> None)

end

type t = {
  print_stats: bool;
  pin_kind_auto: bool;
  autoremove: bool;
  editor: string;
  keep_build_dir: bool;
  reuse_build_dir: bool;
  inplace_build: bool;
  working_dir: bool;
  drop_working_dir: bool;
  ignore_pin_depends: bool;
  show: bool;
  fake: bool;
  skip_dev_update: bool;
  json_out: string option;
  root_is_ok: bool;
  no_auto_upgrade: bool;
  assume_depexts: bool;
  cli: OpamCLIVersion.t;
  scrubbed_environment_variables: string list;
}

let default = {
  print_stats = false;
  pin_kind_auto = true;
  autoremove = false;
  editor = "nano";
  keep_build_dir = false;
  reuse_build_dir = false;
  inplace_build = false;
  working_dir = false;
  drop_working_dir = false;
  ignore_pin_depends = false;
  show = false;
  fake = false;
  skip_dev_update = false;
  json_out = None;
  root_is_ok = false;
  no_auto_upgrade = false;
  assume_depexts = false;
  cli = OpamCLIVersion.current;
  scrubbed_environment_variables = [];
}

type 'a options_fun =
  ?print_stats:bool ->
  ?pin_kind_auto:bool ->
  ?autoremove:bool ->
  ?editor:string ->
  ?keep_build_dir:bool ->
  ?reuse_build_dir:bool ->
  ?inplace_build:bool ->
  ?working_dir:bool ->
  ?drop_working_dir:bool ->
  ?ignore_pin_depends:bool ->
  ?show:bool ->
  ?fake:bool ->
  ?skip_dev_update:bool ->
  ?json_out:string option ->
  ?root_is_ok:bool ->
  ?no_auto_upgrade:bool ->
  ?assume_depexts:bool ->
  ?cli:OpamCLIVersion.t ->
  ?scrubbed_environment_variables:string list ->
  'a

let setk k t
    ?print_stats
    ?pin_kind_auto
    ?autoremove
    ?editor
    ?keep_build_dir
    ?reuse_build_dir
    ?inplace_build
    ?working_dir
    ?drop_working_dir
    ?ignore_pin_depends
    ?show
    ?fake
    ?skip_dev_update
    ?json_out
    ?root_is_ok
    ?no_auto_upgrade
    ?assume_depexts
    ?cli
    ?scrubbed_environment_variables
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    print_stats = t.print_stats + print_stats;
    pin_kind_auto = t.pin_kind_auto + pin_kind_auto;
    autoremove = t.autoremove + autoremove;
    editor = t.editor + editor;
    keep_build_dir = t.keep_build_dir + keep_build_dir;
    reuse_build_dir = t.reuse_build_dir + reuse_build_dir;
    inplace_build = t.inplace_build + inplace_build;
    working_dir = t.working_dir + working_dir;
    drop_working_dir = t.drop_working_dir + drop_working_dir;
    ignore_pin_depends = t.ignore_pin_depends + ignore_pin_depends;
    show = t.show + show;
    fake = t.fake + fake;
    skip_dev_update = t.skip_dev_update + skip_dev_update;
    json_out = t.json_out + json_out;
    root_is_ok = t.root_is_ok + root_is_ok;
    no_auto_upgrade = t.no_auto_upgrade + no_auto_upgrade;
    assume_depexts = t.assume_depexts + assume_depexts;
    cli = t.cli + cli;
    scrubbed_environment_variables = t.scrubbed_environment_variables + scrubbed_environment_variables
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd.Option.Op in
  Random.self_init ();
  let editor =
    E.editor () ++ OpamStd.Env.(getopt "VISUAL" ++ getopt "EDITOR")
  in
  setk (setk (fun c -> r := c; k)) !r
    ?print_stats:(E.stats ())
    ?pin_kind_auto:(E.pinkindauto ())
    ?autoremove:(E.autoremove ())
    ?editor
    ?keep_build_dir:(E.keepbuilddir ())
    ?reuse_build_dir:(E.reusebuilddir ())
    ?inplace_build:(E.inplacebuild ())
    ?working_dir:(E.workingdir ())
    ?drop_working_dir:(E.dropworkingdir ())
    ?ignore_pin_depends:(E.ignorepindepends ())
    ?show:(E.show ())
    ?fake:(E.fake ())
    ?skip_dev_update:(E.skipupdate ())
    ?json_out:(E.json () >>| function "" -> None | s -> Some s)
    ?root_is_ok:(E.rootisok ())
    ?no_auto_upgrade:(E.noautoupgrade ())
    ?assume_depexts:(E.assumedepexts ())
    ?cli:None
    ?scrubbed_environment_variables:None

let init ?noop:_ = initk (fun () -> ())

let search_files = ["findlib"]

open OpamStd.Op

let opam_init ?root_dir ?strict ?solver =
  let open OpamStd.Option.Op in

  (* (i) get root dir *)
  let root = OpamStateConfig.opamroot ?root_dir () in

  (* (ii) load conf file and set defaults *)
  (* the init for OpamFormat is done in advance since (a) it has an effect on
     loading the global config (b) the global config has no effect on it *)
  OpamFormatConfig.initk ?strict @@ fun ?log_dir ->
  let config = OpamStateConfig.load_defaults ~lock_kind:`Lock_read root in
  let initialised = config <> None in
  (* !X fixme: don't drop the loaded config file to reload it afterwards (when
     loading the global_state) like that... *)

  let solver =
    if solver = None && OpamSolverConfig.E.externalsolver () = None then
      (* fixme: in order to not revert config file solver value, we need to
         check it here *)
      (config >>= OpamFile.Config.solver >>|
       fun s -> lazy (OpamCudfSolver.custom_solver s))
    else solver
  in
  begin match config with
    | None -> ()
    | Some conf ->
      let criteria kind =
        let c = OpamFile.Config.criteria conf in
        OpamStd.(List.assoc_opt Compare.equal kind c)
      in
      OpamSolverConfig.update
        ?solver
        ?solver_preferences_default:(criteria `Default >>| fun s-> lazy(Some s))
        ?solver_preferences_upgrade:(criteria `Upgrade >>| fun s-> lazy(Some s))
        ?solver_preferences_fixup:(criteria `Fixup >>| fun s -> lazy (Some s))
        ?solver_preferences_best_effort_prefix:
          (OpamFile.Config.best_effort_prefix conf >>| fun s -> lazy (Some s))
        ();
      OpamStateConfig.update
        ()
  end;

  (* (iii) load from env and options using OpamXxxConfig.init *)
  let log_dir =
    OpamStd.Option.map OpamFilename.Dir.to_string @@
    if log_dir = None && initialised
       && OpamCoreConfig.E.logs () = None then
      (* fixme: in order to not revert [OPAMLOGS] value,
         we need to check it here *)
      Some (OpamPath.log root)
    else log_dir
  in
  (fun () -> ()) |>
  OpamCoreConfig.initk ?log_dir |>
  OpamRepositoryConfig.initk |>
  OpamSolverConfig.initk ?solver |>
  OpamStateConfig.initk ~root_dir:root |>
  initk
