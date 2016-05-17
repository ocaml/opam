(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC <spiros@inhabitedtype.com>

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

let default_nv =
  match OpamPackage.of_string_opt "local-package.dev" with
  | Some nv -> nv
  | None -> assert false

let build t ?build_test ?build_doc dir =
  let name = OpamPackage.name default_nv in
  let pred dir = None <> OpamPinned.find_opam_file_in_source name dir in
  let dir = 
    match OpamFilename.find_ancestor_dir pred dir with
    | Some dir -> dir
    | None     ->
      OpamConsole.error "No valid package description found.";
      OpamStd.Sys.exit 1
  in
  (* XXX(seliopou): This whole dance is necessary because
   * OpamAction.build_package reads global state, rather than explicitly taking
   * the build_test and build_doc arguments *)
  let old_r = OpamStateConfig.(!r) in
  try
    OpamStateConfig.update ?build_test ?build_doc ();
    OpamAction.build_package t (`In_place dir) default_nv;
    OpamStateConfig.r := old_r
  with exn -> OpamStateConfig.r := old_r; raise exn
