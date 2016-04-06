(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC <spiros@inhabitedtype.com

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

type t = {
  sandbox_dir : OpamTypes.dirname option;
  local_file : OpamFile.Local.t option
}

type 'a options_fun =
  ?sandbox_dir:OpamTypes.dirname option ->
  ?local_file:OpamFile.Local.t option ->
  'a

let default =
  { sandbox_dir = None; local_file = None }

let setk k t
  ?sandbox_dir
  ?local_file
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    sandbox_dir = t.sandbox_dir + sandbox_dir;
    local_file = t.local_file + local_file;
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let sandbox_dir = OpamFile.Local.locate (OpamFilename.cwd ()) in
  let local_file =
    match sandbox_dir with
    | None -> None
    | Some dir ->
      let local_file = OpamFilename.Op.(dir // ".opamlocal") in
      try Some OpamFile.(Local.read (make local_file))
      with _ -> None
  in
  setk (setk (fun c -> r := c; k)) !r
    ~sandbox_dir
    ~local_file

let init ?noop:_ = initk (fun () -> ())
