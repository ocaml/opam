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

(* Note: this module is heavily inspired by (and tries to stay close to) OpamDarcs *)

open OpamTypes
open OpamFilename.OP

let log fmt = OpamGlobals.log "Bazaar" fmt

let bazaar_init address =
  let repo = OpamFilename.Dir.to_string address in
  (* TODO: to speedup maybe use "--lightweight" option
   * http://doc.bazaar.canonical.com/beta/en/user-guide/using_checkouts.html#getting-a-lightweight-checkout
   *)
  OpamSystem.commands [ [ "bzr" ; "branch" ; repo ] ]

let get_revno () =
  let xs = OpamSystem.read_command_output ["bzr"; "revno"] in
  assert (List.length xs = 1);
  int_of_string (List.hd xs) (* TODO: Handle exception Invalid_string there *)

let check_updates local_path remote_address =
  if OpamFilename.exists_dir (local_path / ".bzr") then begin
    (* pull command automatically changes to the last revision *)
    log "pulling changes";
    Some (OpamFilename.in_dir local_path begin fun () ->
      let cur_revno = get_revno () in
      OpamSystem.commands [ [ "bzr"; "pull" ] ];
      let last_revno = get_revno () in
      if cur_revno = last_revno then OpamFilename.Set.empty
      else begin
        let lines = OpamSystem.read_command_output
          [ "bzr"; "status"; "-r"; Printf.sprintf "%d..%d" cur_revno last_revno; "-S" ] in
        (* This lines are of that format
         *  M  debian/libyaml-syck-ocaml-dev.dirs
         * -D  debian/svn-deblayout
         * That's why I remove leading 4 spaces
         * *)
        let lines = List.map (fun s -> String.sub s 4 (String.length s -4)) lines in
        log "changed files are: [%s]" (String.concat "," lines);
        OpamFilename.(Set.of_list (List.map of_string lines))
      end
    end)
  end else
    None

module B = struct

  let updates r =
    OpamPath.Repository.root r // "last-bazaar-updates"

  let check_file file =
    let local_repo = OpamRepository.local_repo () in
    let updates = OpamFile.Filenames.read (updates local_repo) in
    if OpamFilename.Set.mem file updates then
      Result file
    else if OpamFilename.exists file then
      Up_to_date file
    else
      Not_available

  let init ~address =
    log "init";
    let local_repo = OpamRepository.local_repo () in
    bazaar_init address;
    OpamFile.Filenames.write (updates local_repo) (OpamFilename.Set.empty)

  let download_archive ~address:_ nv =
    log "download_archive";
    let local_repo = OpamRepository.local_repo () in
    let archive = OpamPath.Repository.archive local_repo nv in
    check_file archive

  let download_file ?checksum:_ nv filename =
    log "download_file";
    let local_repo = OpamRepository.local_repo () in
    let basename = OpamFilename.basename filename in
    let file = OpamPath.Repository.tmp_dir local_repo nv // OpamFilename.Base.to_string basename in
    check_file file

  let rec download_dir nv ?dst remote_address =
    let local_repo = OpamRepository.local_repo () in
    let basename = OpamFilename.Base.to_string (OpamFilename.basename_dir remote_address) in
    let dir = match dst with
      | None   ->  OpamPath.Repository.tmp_dir local_repo nv
      | Some d -> d in
    (* Difference between bazaar and git/darcs is that in git and darcs we create a directory,
     * initialize repo there and than fetch sources. In Bazaar `bzr branch url` automatically creates
     * repo because every bazaar branch on the disk is located in different directories *)
    log "download_dir";
    match OpamFilename.in_dir dir (fun () -> check_updates (dir/basename) remote_address) with
    | None ->
      OpamFilename.in_dir dir (fun () -> bazaar_init remote_address);
      download_dir nv ?dst remote_address
    | Some f ->
      if OpamFilename.Set.empty = f then
        Up_to_date dir
      else
        Result dir

  let update ~address =
    let local_repo = OpamRepository.local_repo () in
    let local_dir = OpamPath.Repository.root local_repo in
    log "update ";
    match check_updates local_dir address with
    | Some f -> OpamFile.Filenames.write (updates local_repo) f; f
    | None   ->
      OpamGlobals.error_and_exit
        "The repository %s is not initialized correctly"
        (OpamFilename.Dir.to_string local_dir)

  let upload_dir ~address:_ dirname =
    log "upload_dir";
    let files = OpamFilename.rec_files dirname in
    try
      OpamSystem.commands [
        [ "darcs"; "add"; OpamFilename.Dir.to_string dirname; ];
        [ "darcs"; "record"; "--all"; "-m"; "upload new files" ];
        [ "darcs"; "push"; "--all"]
      ];
      OpamFilename.Set.of_list files
    with _ ->
      OpamFilename.Set.empty

end

let register () =
  OpamRepository.register_backend `bazaar (module B: OpamRepository.BACKEND)
