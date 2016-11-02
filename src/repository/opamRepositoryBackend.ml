(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

module type S = sig
  val name: OpamUrl.backend
  val pull_url: package -> dirname -> OpamHash.t option -> url ->
    generic_file download OpamProcess.job
  val pull_repo: repository_name -> dirname -> url -> unit OpamProcess.job
  val pull_archive:
    repository_name -> dirname -> url -> filename download OpamProcess.job
  val revision: dirname -> version option OpamProcess.job
end

let compare r1 r2 =
  match compare r2.repo_priority r1.repo_priority with
  | 0 -> compare r2.repo_name r1.repo_name
  | x -> x

let to_string r =
  Printf.sprintf "%s (%s)"
    (OpamRepositoryName.to_string r.repo_name)
    (OpamUrl.to_string r.repo_url)

let local dirname = {
  repo_name     = OpamRepositoryName.of_string "local";
  repo_root     = dirname;
  repo_url      = OpamUrl.empty;
  repo_priority = 0;
}

let to_json r =
  `O  [ ("name", OpamRepositoryName.to_json r.repo_name);
        ("kind", `String (OpamUrl.string_of_backend r.repo_url.OpamUrl.backend));
      ]

let check_digest filename = function
  | Some expected
    when OpamRepositoryConfig.(!r.force_checksums) <> Some false ->
    if OpamHash.check_file (OpamFilename.to_string filename) expected then true
    else
      (OpamConsole.error
         "Bad checksum for %s: expected %s\n\
          Metadata might be out of date, in this case run `opam update`."
         (OpamFilename.to_string filename) (OpamHash.to_string expected);
       false)
  | _ -> true
