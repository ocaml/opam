(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

module type S = sig
  val name: OpamUrl.backend
  val pull_url: package -> dirname -> string option -> url ->
    generic_file download OpamProcess.job
  val pull_repo: repository -> unit OpamProcess.job
  val pull_archive: repository -> url -> filename download OpamProcess.job
  val revision: repository -> version option OpamProcess.job
end

let compare r1 r2 =
  match compare r2.repo_priority r1.repo_priority with
  | 0 -> compare r2.repo_name r1.repo_name
  | x -> x

let to_string r =
  Printf.sprintf "%s(%d %s)"
    (OpamRepositoryName.to_string r.repo_name)
    r.repo_priority
    (OpamUrl.to_string r.repo_url)

let default_url = {
  OpamUrl.
  transport = "https";
  path = "opam.ocaml.org";
  hash = None;
  backend = `http;
}

let default () = {
  repo_name = OpamRepositoryName.default;
  repo_url = default_url;
  repo_priority = 0;
  repo_root =
    OpamFilename.Dir.of_string OpamRepositoryName.(to_string default);
}

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
    let actual = OpamFilename.digest filename in
    if actual = expected then true
    else
      (OpamConsole.error
         "Bad checksum for %s:\n\
         \  - %s [expected result]\n\
         \  - %s [actual result]\n\
          Metadata might be out of date, in this case run `opam update`.\n"
         (OpamFilename.to_string filename) expected actual;
       false)
  | _ -> true
