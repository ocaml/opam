(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

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
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  Random.self_init ();
  let editor =
    env_string "EDITOR" ++ OpamStd.Env.(getopt "VISUAL" ++ getopt "EDITOR")
  in
  setk (setk (fun c -> r := c; k)) !r
    ?print_stats:(env_bool "STATS")
    ?pin_kind_auto:(env_bool "PINKINDAUTO")
    ?autoremove:(env_bool "AUTOREMOVE")
    ?editor
    ?keep_build_dir:(env_bool "KEEPBUILDDIR")
    ?reuse_build_dir:(env_bool "REUSEBUILDDIR")
    ?inplace_build:(env_bool "INPLACEBUILD")
    ?working_dir:(env_bool "WORKINGDIR")
    ?drop_working_dir:(env_bool "DROPWORKINGDIR")
    ?ignore_pin_depends:(env_bool "IGNOREPINDEPENDS")
    ?show:(env_bool "SHOW")
    ?fake:(env_bool "FAKE")
    ?skip_dev_update:(env_bool "SKIPUPDATE")
    ?json_out:(env_string "JSON" >>| function "" -> None | s -> Some s)
    ?root_is_ok:(env_bool "ROOTISOK")
    ?no_auto_upgrade:(env_bool "NOAUTOUPGRADE")
    ?assume_depexts:(env_bool "ASSUMEDEPEXTS")
    ?cli:None

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
  let config = OpamStateConfig.load_defaults root in
  let initialised = config <> None in
  (* !X fixme: don't drop the loaded config file to reload it afterwards (when
     loading the global_state) like that... *)

  let solver =
    if solver = None && OpamStd.Config.env_string "EXTERNALSOLVER" = None then
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
        try Some (List.assoc kind c) with Not_found -> None
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
       && OpamStd.Config.env_string "LOGS" = None then
      (* fixme: in order to not revert [OPAMLOGS] value,
         we need to check it here *)
      Some (OpamPath.log root)
    else log_dir
  in
  (fun () -> ()) |>
  OpamStd.Config.initk ?log_dir |>
  OpamRepositoryConfig.initk |>
  OpamSolverConfig.initk ?solver |>
  OpamStateConfig.initk ~root_dir:root |>
  initk
