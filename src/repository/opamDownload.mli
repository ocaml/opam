(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration init and handling of downloading commands *)

(** downloads a file from an URL, using Curl, Wget, or a custom configured
    tool, to the given directory. Returns the downloaded filename.
    @raise Failure if the download failed or if the checksum is specified and
    doesn't match*)
val download:
  ?quiet:bool -> ?validate:bool -> overwrite:bool -> ?compress:bool ->
  ?checksum:OpamHash.t ->
  OpamUrl.t -> OpamFilename.Dir.t ->
  OpamFilename.t OpamProcess.job

(** As [download], but with a specified output filename. *)
val download_as:
  ?quiet:bool -> ?validate:bool -> overwrite:bool -> ?compress:bool ->
  ?checksum:OpamHash.t ->
  OpamUrl.t -> OpamFilename.t ->
  unit OpamProcess.job
