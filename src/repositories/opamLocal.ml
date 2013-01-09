(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open OpamFilename.OP

let log fmt = OpamGlobals.log "RSYNC" fmt

let rsync ?(delete=true) src dst =
  log "rsync: delete:%b src:%s dst:%s" delete src dst;
  if src <> dst then (
    OpamSystem.mkdir src;
    OpamSystem.mkdir dst;
    let delete = if delete then ["--delete"] else [] in
    try
      let lines = OpamSystem.read_command_output (["rsync" ; "-arv"; "--exclude"; ".git/*"; "--exclude"; "_darcs/*"; src; dst] @ delete) in
      match OpamMisc.rsync_trim lines with
      | []    -> Up_to_date []
      | lines -> Result lines
    with _ ->
      Not_available
  ) else
    Up_to_date []

let rsync_dirs ?delete src dst =
  let src_s = Filename.concat (OpamFilename.Dir.to_string src) "" in
  let dst_s = OpamFilename.Dir.to_string dst in
  match rsync ?delete src_s dst_s with
  | Not_available -> Not_available
  | Up_to_date _  -> Up_to_date []
  | Result lines  -> Result lines

let rsync_file src dst =
  log "rsync_file src=%s dst=%s" (OpamFilename.to_string src) (OpamFilename.to_string dst);
  try
    let lines = OpamSystem.read_command_output [
      "rsync"; "-av"; OpamFilename.to_string src; OpamFilename.to_string dst;
    ] in
    match OpamMisc.rsync_trim lines with
    | []  -> Up_to_date dst
    | [_] -> Result dst
    | l   ->
        OpamSystem.internal_error
          "unknown rsync output: {%s}"
          (String.concat ", " l)
  with _ ->
    Not_available

module B = struct

  let init ~address:_ = ()

  let download_file ?checksum nv remote_file =
    let local_path = OpamRepository.local_repo () in
    let tmp_dir = OpamPath.Repository.tmp_dir local_path nv in
    let local_file = OpamFilename.create tmp_dir (OpamFilename.basename remote_file) in
    let up_to_date = match checksum with
      | None   -> false
      | Some c -> OpamFilename.exists local_file && OpamFilename.digest local_file = c in
    if up_to_date then
      Up_to_date local_file
    else
      rsync_file remote_file local_file

  let download_dir nv ?dst remote_dir =
    let local_repo = OpamRepository.local_repo () in
    let local_dir = match dst with
      | None   ->
        let tmp_dir = OpamPath.Repository.tmp_dir local_repo nv in
        let basename = OpamFilename.Base.to_string (OpamFilename.basename_dir remote_dir) in
        tmp_dir / basename
      | Some d -> d in
    OpamGlobals.msg "Synchronizing %s with %s.\n"
      (OpamFilename.Dir.to_string local_dir)
      (OpamFilename.Dir.to_string remote_dir);
    match rsync_dirs ~delete:true remote_dir local_dir with
    | Up_to_date _  -> Up_to_date local_dir
    | Result _      -> Result local_dir
    | Not_available -> Not_available

  let download_archive ~address nv =
    let remote_file = OpamPath.Repository.archive address nv in
    let local_repo = OpamRepository.local_repo () in
    let local_file = OpamPath.Repository.archive local_repo nv in
    rsync_file remote_file local_file

  let update ~address =
    let local_repo = OpamRepository.local_repo () in
    OpamGlobals.msg "Synchronizing %s with %s.\n"
      (OpamFilename.Dir.to_string (OpamPath.Repository.root local_repo))
      (OpamFilename.Dir.to_string address);
    let sync_dir fn =
      match rsync_dirs ~delete:true (fn address) (fn local_repo) with
      | Not_available
      | Up_to_date _ -> OpamFilename.Set.empty
      | Result lines ->
          let files = List.map OpamFilename.of_string lines in
          OpamFilename.Set.of_list files in
    let archives =
      let available_packages = OpamRepository.packages local_repo in
      let updates = OpamPackage.Set.filter (fun nv ->
        let archive = OpamPath.Repository.archive local_repo nv in
        if not (OpamFilename.exists archive) then
          false
        else match download_archive ~address nv with
        | Not_available ->
            OpamFilename.remove archive;
            false
        | Up_to_date _  -> false
        | Result _      -> true
      ) available_packages in
      List.map (OpamPath.Repository.archive local_repo) (OpamPackage.Set.elements updates) in
    let (++) = OpamFilename.Set.union in
    let updates = OpamFilename.Set.of_list archives
    ++ sync_dir OpamPath.Repository.packages_dir
    ++ sync_dir OpamPath.Repository.compilers_dir in
    ignore (rsync_file (OpamPath.Repository.version address) (OpamPath.Repository.version local_repo));
    updates

  let upload_dir ~address local_dir =
    (* we assume that rsync is only used locally *)
    if OpamFilename.exists_dir (OpamFilename.dirname_dir address)
    && not (OpamFilename.exists_dir address) then
      OpamFilename.mkdir address;
    if OpamFilename.exists_dir local_dir then
      match rsync_dirs ~delete:false local_dir address with
      | Not_available ->
          OpamGlobals.error_and_exit "Cannot upload %s to %s"
            (OpamFilename.Dir.to_string local_dir)
            (OpamFilename.Dir.to_string address)
      | Up_to_date _ -> OpamFilename.Set.empty
      | Result _     ->
          let files = OpamFilename.list_files local_dir in
          OpamFilename.Set.of_list files
    else
      OpamFilename.Set.empty

end

let register () =
  OpamRepository.register_backend `local (module B: OpamRepository.BACKEND)

